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

;; should be higher than 1
(define timeout 3)

(define global-map '())
(define current '())
(define time (current-time))

(define (set-key key-map key-path command)
  (if (null? key-path)
      command
      (assoc-set! key-map
                  (car key-path)
                  (set-key (or (assoc-ref key-map (car key-path)) '())
                           (cdr key-path)
                           command))))

(define (global-set-key key-path command)
    "key-path is a list of form '(key1 key2..);
command may be a string or a function"
  (set! global-map (set-key global-map key-path command)))

(define (update-current val)
  (set! current val)
  (set! time (current-time)))

(define (execute-key-on-global-map key)
  (let ((val (assoc-ref global-map key)))
    (cond ((not val)
           (update-current global-map))
          ((string? val)
           (run-command val)
           (update-current global-map))
          ((procedure? val)
           (apply val '())
           (update-current global-map))
          (else
           (update-current val)))))

(define (execute-key key)
  (let ((val (assoc-ref current key)))
    (cond ((not (and val (< (- (current-time) time) timeout)))
           (execute-key-on-global-map key))
          ((string? val)
           (run-command val)
           (update-current global-map))
          ((procedure? val)
           (apply val '())
           (update-current global-map))
          (else
           (update-current val)))))

(define (alist-keys alst)
  (append-map (lambda (elm)
                (let ((rest (cdr elm)))
                  (cons (car elm)
                        (if (or (string? rest) (procedure? rest))
                            '()
                            (alist-keys rest)))))
              alst))

(global-set-key '((Mod2 Mod4 space)) "rofi -show run")
(global-set-key '((Shift Mod2 Mod4 space)) "rofi -show window")

(global-set-key '((Mod2 XF86MonBrightnessDown)) "xbacklight -dec 3")
(global-set-key '((Mod2 XF86MonBrightnessUp)) "xbacklight -inc 3")

(global-set-key '((Mod2 XF86AudioLowerVolume)) "amixer -D pulse sset Master 2%- >/dev/null")
(global-set-key '((Mod2 XF86AudioMute)) "amixer -D pulse sset Master toggle >/dev/null")
(global-set-key '((Mod2 XF86AudioRaiseVolume)) "amixer -D pulse sset Master 2%+ >/dev/null")

(global-set-key '((Mod2 Mod4 o) (Mod2 Mod4 b)) "browser")
(global-set-key '((Mod2 Mod4 o) (Mod2 Mod4 c)) "timer add")
(global-set-key '((Mod2 Mod4 o) (Mod2 Mod4 m)) "main_menu")
(global-set-key '((Mod2 Mod4 o) (Mod2 Mod4 s)) "emmingus")
(global-set-key '((Mod2 Mod4 o) (Mod2 Mod4 t)) "emshell")
(global-set-key '((Mod2 Mod4 o) (Mod2 Mod4 v)) "uxterm -e pulsemixer")
(global-set-key '((Mod2 Mod4 o) (Mod2 Mod4 w)) "rpass type")
(global-set-key '((Mod2 Mod4 o) (Shift Mod2 Mod4 c)) "timer")
(global-set-key '((Mod2 Mod4 o) (Shift Mod2 Mod4 m)) "bm menu")
(global-set-key '((Mod2 Mod4 o) (Shift Mod2 Mod4 w)) "rpass")

(global-set-key '((Mod2 Mod4 e) (Mod2 Mod4 b)) "emswitch-to-buffer")
(global-set-key '((Mod2 Mod4 e) (Mod2 Mod4 e)) "em")
(global-set-key '((Mod2 Mod4 e) (Mod2 Mod4 f)) "emfind-file")
(global-set-key '((Mod2 Mod4 e) (Mod2 Mod4 m)) "emman")
(global-set-key '((Mod2 Mod4 e) (Mod2 Mod4 p)) "emproced")
(global-set-key '((Mod2 Mod4 e) (Mod2 Mod4 r)) "emtransmission")
(global-set-key '((Mod2 Mod4 e) (Mod2 Mod4 s)) "emeshell")
(global-set-key '((Mod2 Mod4 e) (Shift Mod2 Mod4 s)) "emeshell t")

(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "1")) "mpc -q volume 10")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "2")) "mpc -q volume 20")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "3")) "mpc -q volume 30")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "4")) "mpc -q volume 40")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "5")) "mpc -q volume 50")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "6")) "mpc -q volume 60")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "7")) "mpc -q volume 70")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "8")) "mpc -q volume 80")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "9")) "mpc -q volume 90")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 "0")) "mpc -q volume 100")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 comma)) "mpc -q volume -1")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 n)) "mpc -q next")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 p)) "mpc -q prev")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 period)) "mpc -q volume +1")
(global-set-key '((Mod2 Mod4 s) (Mod2 Mod4 t)) "mpc -q toggle")

(global-set-key '((Mod2 Mod4 k)) "switch_keyboard next")
(global-set-key '((Shift Mod2 Mod4 k)) "switch_keyboard full-next")

(global-set-key '((Mod2 Mod4 p) (Mod2 Mod4 d)) "runel remote mode default")
(global-set-key '((Mod2 Mod4 p) (Mod2 Mod4 h)) "runel remote mode hardware")
(global-set-key '((Mod2 Mod4 p) (Mod2 Mod4 n)) "runel remote mode network")
(global-set-key '((Mod2 Mod4 p) (Mod2 Mod4 s)) "runel remote mode mpd")
(global-set-key '((Mod2 Mod4 p) (Mod2 Mod4 t)) "panel_toggle")
(global-set-key '((Mod2 Mod4 p) (Mod2 Mod4 w)) "runel remote mode wifi")

(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 c)) "bspc node --close")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 k)) "bspc node --kill")

(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 a)) "bspc node --state tiled")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 e)) "bspc node --state floating")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 o)) "bspc node --state pseudo_tiled")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 u)) "bspc node --state fullscreen")

(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 comma)) "bspc node --flag locked")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 p)) "bspc node --flag private")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 period)) "bspc node --flag sticky")

(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 b)) "bspc node --to-desktop B")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 c)) "bspc node --to-desktop C")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 h)) "bspc node --to-desktop H")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 m)) "bspc node --to-desktop M")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 r)) "bspc node --to-desktop R")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 s)) "bspc node --to-desktop S")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 t)) "bspc node --to-desktop T")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Mod2 Mod4 v)) "bspc node --to-desktop V")

(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 b)) "bspc node --to-desktop B --follow")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 c)) "bspc node --to-desktop C --follow")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 h)) "bspc node --to-desktop H --follow")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 m)) "bspc node --to-desktop M --follow")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 r)) "bspc node --to-desktop R --follow")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 s)) "bspc node --to-desktop S --follow")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 t)) "bspc node --to-desktop T --follow")
(global-set-key '((Mod2 Mod4 w) (Mod2 Mod4 t) (Shift Mod2 Mod4 v)) "bspc node --to-desktop V --follow")

(global-set-key '((Mod2 Mod4 c)) "bspc node --focus north")
(global-set-key '((Mod2 Mod4 h)) "bspc node --focus west")
(global-set-key '((Mod2 Mod4 n)) "bspc node --focus east")
(global-set-key '((Mod2 Mod4 t)) "bspc node --focus south")
(global-set-key '((Mod2 Mod4 Tab)) "bspc node --focus prev.local")
(global-set-key '((Shift Mod2 Mod4 Tab)) "bspc node --focus next.local")

(global-set-key '((Shift Mod2 Mod4 c)) "$XDG_CONFIG_HOME/bspwm/euclid_mover north 10")
(global-set-key '((Shift Mod2 Mod4 h)) "$XDG_CONFIG_HOME/bspwm/euclid_mover west 10")
(global-set-key '((Shift Mod2 Mod4 n)) "$XDG_CONFIG_HOME/bspwm/euclid_mover east 10")
(global-set-key '((Shift Mod2 Mod4 t)) "$XDG_CONFIG_HOME/bspwm/euclid_mover south 10")

(global-set-key '((Mod2 Mod4 bracketleft)) "$XDG_CONFIG_HOME/bspwm/bspwm_resize north 5")
(global-set-key '((Mod2 Mod4 bracketright)) "$XDG_CONFIG_HOME/bspwm/bspwm_resize south 5")
(global-set-key '((Shift Mod2 Mod4 bracketleft)) "$XDG_CONFIG_HOME/bspwm/bspwm_resize west 5")
(global-set-key '((Shift Mod2 Mod4 bracketright)) "$XDG_CONFIG_HOME/bspwm/bspwm_resize east 5")

(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 Tab)) "bspc desktop --layout next")

(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 b)) "bspc desktop --focus B")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 c)) "bspc desktop --focus C")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 h)) "bspc desktop --focus H")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 m)) "bspc desktop --focus M")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 n)) "bspc desktop --focus next")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 p)) "bspc desktop --focus prev")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 r)) "bspc desktop --focus R")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 s)) "bspc desktop --focus S")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 t)) "bspc desktop --focus T")
(global-set-key '((Mod2 Mod4 d) (Mod2 Mod4 v)) "bspc desktop --focus V")

;; This one creates all bindings from global-map.
;; Should run at the end
(update-current global-map)
(set-numlock! #t)
(for-each
 (lambda (key) (xbindkey-function key (lambda () (execute-key key))))
 (delete-duplicates (alist-keys global-map)))
