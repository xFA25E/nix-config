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

(use-modules (srfi srfi-1))

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

;; (with-prefix
;;  ""
;;  (define-key "s-s" "shell command")
;;  (with-prefix
;;   "s-t"
;;   (define-key "s-t" "cool")))

;; (let* ((prefix (string-trim (string-append prefix "s-s ")))
;;        (define-key (λ (key cmd)
;;                      (format #t "~a ~a~%" (string-append prefix key) cmd))))
;;   (define-key "s-t" "hello world")
;;   (let* ((prefix (string-append prefix " s-s"))
;;          (define-key (λ (key cmd)
;;                        (format #t "~a ~a~%"
;;                                (string-trim (string-append prefix " " key)) cmd))))
;;     (define-key "s-t" "hello boomer")))

;;; bspwm indipendent keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run launcher
(global-set-key "s-space" "rofi -show run")
(global-set-key "s-S-space" "rofi -show window")

;; change brightness
(global-set-key "XF86MonBrightnessDown" "xbacklight -dec 3")
(global-set-key "XF86MonBrightnessUp" "xbacklight -inc 3")

;; change volume
(global-set-key "XF86AudioLowerVolume" "amixer -D pulse sset Master 2%- >/dev/null")
(global-set-key "XF86AudioMute" "amixer -D pulse sset Master toggle >/dev/null")
(global-set-key "XF86AudioRaiseVolume" "amixer -D pulse sset Master 2%+ >/dev/null")

;; change volume
(global-set-key "s-1" "amixer -D pulse sset Master 10% >/dev/null")
(global-set-key "s-2" "amixer -D pulse sset Master 20% >/dev/null")
(global-set-key "s-3" "amixer -D pulse sset Master 30% >/dev/null")
(global-set-key "s-4" "amixer -D pulse sset Master 40% >/dev/null")
(global-set-key "s-5" "amixer -D pulse sset Master 50% >/dev/null")
(global-set-key "s-6" "amixer -D pulse sset Master 60% >/dev/null")
(global-set-key "s-7" "amixer -D pulse sset Master 70% >/dev/null")
(global-set-key "s-8" "amixer -D pulse sset Master 80% >/dev/null")
(global-set-key "s-9" "amixer -D pulse sset Master 90% >/dev/null")
(global-set-key "s-0" "amixer -D pulse sset Master 100% >/dev/null")

;; open programs
(global-set-key "s-o s-b" "browser")
(global-set-key "s-o s-w" "rpass type")
(global-set-key "s-o s-S-w" "rpass")
(global-set-key "s-o s-S-m" "bm menu")
(global-set-key "s-o s-m" "main_menu")
(global-set-key "s-o s-c" "timer add")
(global-set-key "s-o s-S-c" "timer")
(global-set-key "s-o s-v" "$TERMINAL -e pulsemixer")

;; emacs progs
(global-set-key "s-e s-e" "em")
(global-set-key "s-e s-r" "emremember")
(global-set-key "s-e s-S-r" "emremember-notes")

;; mpd control
(global-set-key "s-s s-n" "mpc -q next")
(global-set-key "s-s s-p" "mpc -q prev")
(global-set-key "s-s s-t" "mpc -q toggle")

;; mpd volume
(global-set-key "s-s s-comma" "mpc -q volume -1")
(global-set-key "s-s s-period" "mpc -q volume +1")
(global-set-key "s-s s-1" "mpc -q volume 10")
(global-set-key "s-s s-2" "mpc -q volume 20")
(global-set-key "s-s s-3" "mpc -q volume 30")
(global-set-key "s-s s-4" "mpc -q volume 40")
(global-set-key "s-s s-5" "mpc -q volume 50")
(global-set-key "s-s s-6" "mpc -q volume 60")
(global-set-key "s-s s-7" "mpc -q volume 70")
(global-set-key "s-s s-8" "mpc -q volume 80")
(global-set-key "s-s s-9" "mpc -q volume 90")
(global-set-key "s-s s-0" "mpc -q volume 100")

;; keyboard layouts control
(global-set-key "s-k" "switch_keyboard next")
(global-set-key "s-S-k" "switch_keyboard full-next")

;; change panel mode
(global-set-key "s-p s-d" "runel remote mode default")
(global-set-key "s-p s-s" "runel remote mode mpd")
(global-set-key "s-p s-h" "runel remote mode hardware")
(global-set-key "s-p s-n" "runel remote mode network")
(global-set-key "s-p s-w" "runel remote mode wifi")
(global-set-key "s-p s-c" "runel remote mode corona")

;;; nodes (windows) commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill and close
(global-set-key "s-w s-k" "bspc node --kill")
(global-set-key "s-w s-c" "bspc node --close")

;; states
(global-set-key "s-w s-a" "bspc node --state tiled")
(global-set-key "s-w s-o" "bspc node --state pseudo_tiled")
(global-set-key "s-w s-e" "bspc node --state floating")
(global-set-key "s-w s-u" "bspc node --state fullscreen")

;; send to desktop by name
(global-set-key "s-w s-t s-b" "bspc node --to-desktop B")
(global-set-key "s-w s-t s-c" "bspc node --to-desktop C")
(global-set-key "s-w s-t s-h" "bspc node --to-desktop H")
(global-set-key "s-w s-t s-m" "bspc node --to-desktop M")
(global-set-key "s-w s-t s-r" "bspc node --to-desktop R")
(global-set-key "s-w s-t s-s" "bspc node --to-desktop S")
(global-set-key "s-w s-t s-t" "bspc node --to-desktop T")
(global-set-key "s-w s-t s-v" "bspc node --to-desktop V")

(global-set-key "s-w s-t s-S-b" "bspc node --to-desktop B --follow")
(global-set-key "s-w s-t s-S-c" "bspc node --to-desktop C --follow")
(global-set-key "s-w s-t s-S-h" "bspc node --to-desktop H --follow")
(global-set-key "s-w s-t s-S-m" "bspc node --to-desktop M --follow")
(global-set-key "s-w s-t s-S-r" "bspc node --to-desktop R --follow")
(global-set-key "s-w s-t s-S-s" "bspc node --to-desktop S --follow")
(global-set-key "s-w s-t s-S-t" "bspc node --to-desktop T --follow")
(global-set-key "s-w s-t s-S-v" "bspc node --to-desktop V --follow")

;; focus by direction
(global-set-key "s-c" "bspc node --focus north")
(global-set-key "s-h" "bspc node --focus west")
(global-set-key "s-n" "bspc node --focus east")
(global-set-key "s-t" "bspc node --focus south")
(global-set-key "s-Tab" "bspc node --focus prev.local.!hidden.window")
(global-set-key "s-S-Tab" "bspc node --focus next.local.!hidden.window")

;; euclid mover
(global-set-key "s-S-c" "bspwm_move north 10")
(global-set-key "s-S-h" "bspwm_move west 10")
(global-set-key "s-S-n" "bspwm_move east 10")
(global-set-key "s-S-t" "bspwm_move south 10")

(global-set-key "s-bracketleft" "bspwm_resize north 5")
(global-set-key "s-bracketright" "bspwm_resize south 5")
(global-set-key "s-S-bracketleft" "bspwm_resize west 5")
(global-set-key "s-S-bracketright" "bspwm_resize east 5")

;;; desktop commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alternate monocle layout
(global-set-key "s-d s-Tab" "bspc desktop --layout next")

;; focus desktop
(global-set-key "s-d s-b" "bspc desktop --focus B")
(global-set-key "s-d s-c" "bspc desktop --focus C")
(global-set-key "s-d s-h" "bspc desktop --focus H")
(global-set-key "s-d s-m" "bspc desktop --focus M")
(global-set-key "s-d s-r" "bspc desktop --focus R")
(global-set-key "s-d s-s" "bspc desktop --focus S")
(global-set-key "s-d s-t" "bspc desktop --focus T")
(global-set-key "s-d s-v" "bspc desktop --focus V")

;; This one creates all bindings from global-map.
;; Should run at the end
(set-numlock! #t)

(for-each
 (lambda (key)
   (for-each
    (lambda (ksym)
      (xbindkey-function
       ksym
       (lambda ()
         (execute-key key)
         (set! chain (string-append chain (if (string-null? chain) "" " ") key))
         (if (eq? current global-map)
             (begin
               ;; (run-command (format #f "runel remote set timer \"~a\" && millisleep 200 && runel remote set timer \" \"" chain))
               (set! chain ""))
             ;; (run-command (format #f "runel remote set timer \"~a\"" chain))
             ))))
    (let ((ksym (kbd key)))
      (list ksym (cons "Mod2" ksym)))))

 (delete! "s-g" (hash-map-keys global-map)))

(xbindkey-function
 (kbd "s-g")
 (λ ()
   (set! current global-map)
   (set! chain "")
   ;; (run-command "runel remote set timer \" \"")
   ))
