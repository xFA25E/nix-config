;; -*- eval: (put 'with-prefix 'scheme-indent-function 1); -*-
;; List of modifier:
;;   Release, Control, Shift, Mod1 (Alt), Mod2 (NumLock),
;;   Mod3 (CapsLock), Mod4, Mod5 (Scroll).

;; Optional modifier state:
;; (set-numlock! #f or #t)
;; (set-scrolllock! #f or #t)
;; (set-capslock! #f or #t)
;; Other functions:
;; (remove-xbindkey key)
;; (run-command "foo-bar-command [args]")
;; (grab-all-keys)
;; (ungrab-all-keys)
;; (remove-all-keys)
;; (debug)

;; (use-modules (system repl server))
;; (spawn-server)

(use-modules (srfi srfi-1) (ice-9 format))

(define chain "")
(define global-map (make-hash-table))
(define current global-map)

(define (kbd kstring)
  "Similar to emacs's kbd function, but it does not handle sequences."

  (define (kbd-modifier modifier)
    (cond ((string= modifier "s") "Mod4")
          ((string= modifier "S") "Shift")
          ((string= modifier "M") "Alt")
          ((string= modifier "C") "Control")
          (else (error (format #f "Unknown kdb string \"~A\"" modifier)))))

  (when (string-contains kstring " ")
    (error (format #f "String \"~A\" has spaces!" kstring)))

  (let loop ((s (string-split kstring #\-)) (result '()))
    (if (null? (cdr s))
        (reverse! (cons (car s) result))
        (loop (cdr s) (cons (kbd-modifier (car s)) result)))))

(define (set-key key-map key-path command)
  (let ((next-key (car key-path))
        (rest-path (cdr key-path)))
    (if (null? rest-path)
        (hash-set! key-map next-key command)
        (let ((ref (hash-ref key-map next-key)))
          (when (not ref)
            (hash-set! key-map next-key (make-hash-table))
            (set! ref (hash-ref key-map next-key)))
          (set-key ref rest-path command)))))

(define (global-set-key key-path command)
  "key-path is a list of form '(key1 key2..);
command may be a string or a function"
  (set-key global-map (string-split key-path #\space) command))

(define (execute-key key)
  (let* ((val (hash-ref current key)))
    (cond ((string? val) (run-command val))
          ((procedure? val) (val)))
    (set! current (if (hash-table? val) val global-map))))

(define (hash-map-keys hm)
  (define (inner hm)
    (apply append! (hash-map->list
                    (lambda (key value) (cons key (if (hash-table? value) (inner value) '())))
                    hm)))
  (delete-duplicates! (inner hm)))

(define (bind-function key ksym)
  (xbindkey-function
   ksym
   (lambda ()
     (execute-key key)
     (if (eq? current global-map)
         (when (not (string= chain ""))
           (set! chain "")
           (run-command "sratus -u keyseq -v ''"))
         (begin
           (set! chain (string-trim (string-append chain " " key)))
           (run-command (string-append "sratus -u keyseq -v '" chain "'")))))))

(define (permutate lst)
  (define (inner pfx lst)
    (if (null? lst)
        (list pfx)
        (let ((op (car lst)) (tail (cdr lst)))
          (case op
            ((and) (handle-and pfx tail))
            ((or) (handle-or pfx tail))
            (else (error "List should start with and/or."))))))

  (define (handle-and pfx lst)
    (if (null? lst)
        (list pfx)
        (let ((head (car lst)) (tail (cdr lst)))
          (cond ((string? head)
                 (handle-and (string-append pfx head) tail))
                ((number? head)
                 (handle-and (string-append pfx (number->string head)) tail))
                ((symbol? head)
                 (handle-and (string-append pfx (symbol->string head)) tail))
                ((list? head)
                 (apply append! (map (lambda (p) (handle-and p tail)) (inner pfx head))))
                (else (error "Neither string or list or number in and"))))))

  (define (handle-or pfx lst)
    (if (null? lst)
        (list pfx)
        (apply append!
               (map (lambda (e) (cond ((string? e) (list (string-append pfx e)))
                                 ((number? e) (list (string-append pfx (number->string e))))
                                 ((symbol? e) (list (string-append pfx (symbol->string e))))
                                 ((list? e) (inner pfx e))
                                 (else (error "Neither string or list or number in or"))))
                    lst))))

  (if (null? (cdr lst))
      (list)
      (inner "" lst)))

(define (main)
  (for-each (lambda (key)
              (let ((ksym (kbd key)))
                (bind-function key ksym)
                (bind-function key (cons "Mod2" ksym))))
            (delete! "s-g" (hash-map-keys global-map)))
  (xbindkey-function (kbd "s-g")
                     (λ ()
                       (set! current global-map)
                       (when (not (string= chain ""))
                         (set! chain "")
                         (run-command "sratus -u keyseq -v ''")))))

(define prefix "")
(define-macro (define-key key cmd)
  ;; (global-set-key key cmd)
  (cond
   ((and (string? key) (string? cmd))
    `(global-set-key (string-trim (string-append prefix " " ,key)) ,cmd))
   ((and (list? key) (list? cmd))
    `(begin . ,(map (lambda (k c) `(global-set-key (string-trim (string-append prefix " " ,k)) ,c))
                    (permutate key) (permutate cmd))))
   (else (error "Neither string or list in define-key"))))

(define-macro (with-prefix prefix exp . body)
  `(let ((prefix (string-trim (string-append prefix " " ,prefix))))
     ,exp . ,body))

;;; bspwm indipendent keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run launcher
(define-key "s-space" "rofi -show run")
(define-key "s-S-space" "rofi -show window")

;; change brightness
(define-key "XF86MonBrightnessDown" "xbacklight -dec 3")
(define-key "XF86MonBrightnessUp" "xbacklight -inc 3")

;; change volume
(define-key (or XF86AudioLowerVolume XF86AudioMute XF86AudioRaiseVolume)
  (and "amixer -D pulse sset Master " (or 2%- toggle 2%+) " >/dev/null"))

;; change volume
(define-key (and s- (or 1 2 3 4 5 6 7 8 9 0))
  (and "amixer -D pulse sset Master " (or 1 2 3 4 5 6 7 8 9 10) "0% >/dev/null"))

;; open programs
(with-prefix "s-o"
  (define-key "s-b" "browser")
  (define-key "s-w" "rpass type")
  (define-key "s-S-w" "rpass")
  (define-key "s-S-m" "bm menu")
  (define-key "s-m" "main_menu")
  (define-key "s-c" "timer add")
  (define-key "s-S-c" "timer")
  (define-key "s-v" "$TERMINAL -e pulsemixer"))

;; emacs progs
(with-prefix "s-e"
  (define-key (or s-e s-r s-S-r) (or em emremember emremember-notes)))

;; mpd control
(with-prefix "s-s"
  (define-key (or s-n s-p s-t) (and "mpc -q " (or next prev toggle)))

  ;; mpd volume
  (define-key (and s- (or comma period 1 2 3 4 5 6 7 8 9 0))
    (and "mpc -q volume " (or -1 "+1" (and (or 1 2 3 4 5 6 7 8 9 10) 0)))))

;; keyboard layouts control
(define-key "s-k" "switch_keyboard next")
(define-key "s-S-k" "switch_keyboard full-next")

;; change panel mode
(with-prefix "s-p"
  (define-key (and s- (or d s h n w c))
    (and "runel -m " (or default mpd hardware network wifi corona))))

;;; nodes (windows) commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-prefix "s-w"
  (define-key "s-k" "bspc node --kill")
  (define-key "s-c" "bspc node --close")

  ;; states
  (define-key (and s- (or a o e u))
    (and "bspc node --state " (or tiled pseudo_tiled floating fullscreen)))

  ;; send to desktop by name
  (with-prefix "s-t"
    (define-key (and s- (or b c h m r s t v))
      (and "bspc node --to-desktop " (or B C H M R S T V)))

    (define-key (and s-S- (or b c h m r s t v))
      (and "bspc node --to-desktop " (or B C H M R S T V) " --follow"))))

;; focus by direction
(define-key (and s- (or c h n t))
  (and "bspc node --focus " (or north west east south)))
(define-key (or s-Tab s-S-Tab)
  (and "bspc node --focus " (and (or prev next) .local.!hidden.window)))

;; euclid mover
(define-key (and s-S- (or c h n t))
  (and "bspwm_move " (or north west east south) " 10"))

(define-key (and (or s- s-S-) bracket (or left right))
  (and "bspwm_resize " (or north south west east) " 5"))

;;; desktop commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-prefix "s-d"
  (define-key "s-Tab" "bspc desktop --layout next")

  ;; focus desktop
  (define-key (and s- (or b c h m r s t v))
    (and "bspc desktop --focus " (or B C H M R S T V))))

;; This one creates all bindings from global-map.
;; Should run at the end
(set-numlock! #t)
(main)
