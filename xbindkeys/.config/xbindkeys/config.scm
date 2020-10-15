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

(use-modules (srfi srfi-1)
             (ice-9 format))

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
  (define (hash-map-keys-inner hm)
    (apply
     append!
     (hash-map->list
      (lambda (key value)
        (cons key (if (hash-table? value) (hash-map-keys-inner value) '())))
      hm)))

  (delete-duplicates! (hash-map-keys-inner hm)))

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

(define (main)
  (for-each (lambda (key)
              (let ((ksym (kbd key)))
                (bind-function key ksym)
                (bind-function key (cons "Mod2" ksym))))
            (delete! "s-g" (hash-map-keys global-map)))

  (xbindkey-function
   (kbd "s-g")
   (λ ()
     (set! current global-map)
     (when (not (string= chain ""))
       (set! chain "")
       (run-command "sratus -u keyseq -v ''")))))

(define (define-key key cmd) (global-set-key key cmd))
(define prefix "")
(define-macro (with-prefix prefix exp . body)
  `(let* ((prefix (string-trim (string-append prefix " " ,prefix)))
          (define-key (lambda (key cmd) (global-set-key (string-append prefix " " key) cmd))))
     ,exp . ,body))

;;; bspwm indipendent keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run launcher
(define-key "s-space" "rofi -show run")
(define-key "s-S-space" "rofi -show window")

;; change brightness
(define-key "XF86MonBrightnessDown" "xbacklight -dec 3")
(define-key "XF86MonBrightnessUp" "xbacklight -inc 3")

;; change volume
(define-key "XF86AudioLowerVolume" "amixer -D pulse sset Master 2%- >/dev/null")
(define-key "XF86AudioMute" "amixer -D pulse sset Master toggle >/dev/null")
(define-key "XF86AudioRaiseVolume" "amixer -D pulse sset Master 2%+ >/dev/null")

;; change volume
(define-key "s-1" "amixer -D pulse sset Master  10% >/dev/null")
(define-key "s-2" "amixer -D pulse sset Master  20% >/dev/null")
(define-key "s-3" "amixer -D pulse sset Master  30% >/dev/null")
(define-key "s-4" "amixer -D pulse sset Master  40% >/dev/null")
(define-key "s-5" "amixer -D pulse sset Master  50% >/dev/null")
(define-key "s-6" "amixer -D pulse sset Master  60% >/dev/null")
(define-key "s-7" "amixer -D pulse sset Master  70% >/dev/null")
(define-key "s-8" "amixer -D pulse sset Master  80% >/dev/null")
(define-key "s-9" "amixer -D pulse sset Master  90% >/dev/null")
(define-key "s-0" "amixer -D pulse sset Master 100% >/dev/null")

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
  (define-key "s-e" "em")
  (define-key "s-r" "emremember")
  (define-key "s-S-r" "emremember-notes"))

;; mpd control
(with-prefix "s-s"
  (define-key "s-n" "mpc -q next")
  (define-key "s-p" "mpc -q prev")
  (define-key "s-t" "mpc -q toggle")

  ;; mpd volume
  (define-key "s-comma" "mpc -q volume -1")
  (define-key "s-period" "mpc -q volume +1")
  (define-key "s-1" "mpc -q volume 1")
  (define-key "s-2" "mpc -q volume 2")
  (define-key "s-3" "mpc -q volume 3")
  (define-key "s-4" "mpc -q volume 4")
  (define-key "s-5" "mpc -q volume 5")
  (define-key "s-6" "mpc -q volume 6")
  (define-key "s-7" "mpc -q volume 7")
  (define-key "s-8" "mpc -q volume 8")
  (define-key "s-9" "mpc -q volume 9")
  (define-key "s-0" "mpc -q volume 10"))

;; keyboard layouts control
(define-key "s-k" "switch_keyboard next")
(define-key "s-S-k" "switch_keyboard full-next")

;; change panel mode
(with-prefix "s-p"
  (define-key "s-d" "runel -m default")
  (define-key "s-s" "runel -m mpd")
  (define-key "s-h" "runel -m hardware")
  (define-key "s-n" "runel -m network")
  (define-key "s-w" "runel -m wifi")
  (define-key "s-c" "runel -m corona"))

;;; nodes (windows) commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-prefix "s-w"
  (define-key "s-k" "bspc node --kill")
  (define-key "s-c" "bspc node --close")

  ;; states
  (define-key "s-a" "bspc node --state tiled")
  (define-key "s-o" "bspc node --state pseudo_tiled")
  (define-key "s-e" "bspc node --state floating")
  (define-key "s-u" "bspc node --state fullscreen")

  ;; send to desktop by name
  (with-prefix "s-t"
    (define-key "s-b" "bspc node --to-desktop B")
    (define-key "s-c" "bspc node --to-desktop C")
    (define-key "s-h" "bspc node --to-desktop H")
    (define-key "s-m" "bspc node --to-desktop M")
    (define-key "s-r" "bspc node --to-desktop R")
    (define-key "s-s" "bspc node --to-desktop S")
    (define-key "s-t" "bspc node --to-desktop T")
    (define-key "s-v" "bspc node --to-desktop V")

    (define-key "s-S-b" "bspc node --to-desktop B --follow")
    (define-key "s-S-c" "bspc node --to-desktop C --follow")
    (define-key "s-S-h" "bspc node --to-desktop H --follow")
    (define-key "s-S-m" "bspc node --to-desktop M --follow")
    (define-key "s-S-r" "bspc node --to-desktop R --follow")
    (define-key "s-S-s" "bspc node --to-desktop S --follow")
    (define-key "s-S-t" "bspc node --to-desktop T --follow")
    (define-key "s-S-v" "bspc node --to-desktop V --follow")))


;; focus by direction
(define-key "s-c" "bspc node --focus north")
(define-key "s-h" "bspc node --focus west")
(define-key "s-n" "bspc node --focus east")
(define-key "s-t" "bspc node --focus south")
(define-key "s-Tab" "bspc node --focus prev.local.!hidden.window")
(define-key "s-S-Tab" "bspc node --focus next.local.!hidden.window")

;; euclid mover
(define-key "s-S-c" "bspwm_move north 10")
(define-key "s-S-h" "bspwm_move west 10")
(define-key "s-S-n" "bspwm_move east 10")
(define-key "s-S-t" "bspwm_move south 10")

(define-key "s-bracketleft" "bspwm_resize north 5")
(define-key "s-bracketright" "bspwm_resize south 5")
(define-key "s-S-bracketleft" "bspwm_resize west 5")
(define-key "s-S-bracketright" "bspwm_resize east 5")

;;; desktop commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-prefix "s-d"
  (define-key "s-Tab" "bspc desktop --layout next")

  ;; focus desktop
  (define-key "s-b" "bspc desktop --focus B")
  (define-key "s-c" "bspc desktop --focus C")
  (define-key "s-h" "bspc desktop --focus H")
  (define-key "s-m" "bspc desktop --focus M")
  (define-key "s-r" "bspc desktop --focus R")
  (define-key "s-s" "bspc desktop --focus S")
  (define-key "s-t" "bspc desktop --focus T")
  (define-key "s-v" "bspc desktop --focus V"))

;; This one creates all bindings from global-map.
;; Should run at the end
(set-numlock! #t)
(main)
