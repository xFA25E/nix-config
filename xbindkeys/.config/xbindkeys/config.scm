;; -*- eval: (put 'global-set-keys 'scheme-indent-function 1); -*-
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

(add-to-load-path (dirname (current-filename)))
(use-modules (utils))


;;; bspwm indipendent keys

;; run launcher
(global-set-key "s-space" "rofi -show run")
(global-set-key "s-S-space" "rofi -show window")

;; change brightness
(global-set-key "XF86MonBrightnessDown" "xbacklight -dec 3")
(global-set-key "XF86MonBrightnessUp" "xbacklight -inc 3")

;; change volume
(global-set-keys (and XF86 (or AudioLowerVolume AudioMute AudioRaiseVolume))
  (and "amixer -D pulse sset Master " (or 2%- toggle 2%+) " >/dev/null"))

;; change volume
(global-set-keys (and s- (or 1 2 3 4 5 6 7 8 9 0))
  (and "amixer -D pulse sset Master " (or 1 2 3 4 5 6 7 8 9 10) "0% >/dev/null"))

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
(global-set-keys (and "s-e " (or s-e s-r s-S-r))
  (or em emremember emremember-notes))

;; mpd control
(global-set-keys (and "s-s " (or s-n s-p s-t))
  (and "mpc -q " (or next prev toggle)))

;; mpd volume
(global-set-keys (and "s-s s-" (or comma period 1 2 3 4 5 6 7 8 9 0))
  (and "mpc -q volume " (or -1 "+1" (and (or 1 2 3 4 5 6 7 8 9 10) 0))))

;; keyboard layouts control
(global-set-key "s-k" "switch_keyboard next")
(global-set-key "s-S-k" "switch_keyboard full-next")

;; change panel mode
(global-set-keys (and "s-p s-" (or d s h n w c))
  (and "runel -m " (or default mpd hardware network wifi corona)))


;;; nodes (windows) commands
(global-set-key "s-w s-k" "bspc node --kill")
(global-set-key "s-w s-c" "bspc node --close")

;; states
(global-set-keys (and "s-w s-" (or a o e u))
  (and "bspc node --state " (or tiled pseudo_tiled floating fullscreen)))

;; send to desktop by name
(global-set-keys (and "s-w s-t s-" (or b c h m r s t v))
  (and "bspc node --to-desktop " (or B C H M R S T V)))

(global-set-keys (and "s-w s-t s-S-" (or b c h m r s t v))
  (and "bspc node --to-desktop " (or B C H M R S T V) " --follow"))

;; focus by direction
(global-set-keys (and s- (or c h n t))
  (and "bspc node --focus " (or north west east south)))

(global-set-key "s-Tab" "bspc node --focus prev.local.!hidden.window")
(global-set-key "s-S-Tab" "bspc node --focus next.local.!hidden.window")

;; euclid mover
(global-set-keys (and s-S- (or c h n t))
  (and "bspwm_move " (or north west east south) " 10"))

(global-set-keys (and (or s- s-S-) bracket (or left right))
  (and "bspwm_resize " (or north south west east) " 5"))


;;; desktop commands
(global-set-key "s-d s-Tab" "bspc desktop --layout next")

;; focus desktop
(global-set-keys (and "s-d s-" (or b c h m r s t v))
  (and "bspc desktop --focus " (or B C H M R S T V)))


;;; RUN
(main)
