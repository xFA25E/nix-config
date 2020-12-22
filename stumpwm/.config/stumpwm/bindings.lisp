(in-package :stumpwm)

(set-prefix-key (kbd "C-z"))

;;; MAPS

(defvar *open-map* (make-sparse-keymap))

;;; TOP

(define-key *top-map* (kbd "XF86AudioMute") "exec amixer -D pulse sset Master toggle >/dev/null")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer -D pulse sset Master 1%- >/dev/null")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer -D pulse sset Master 1%+ >/dev/null")

(define-key *top-map* (kbd "XF86AudioPrev") "exec mpc -q prev")
(define-key *top-map* (kbd "XF86AudioPlay") "exec mpc -q toggle")
(define-key *top-map* (kbd "XF86AudioNext") "exec mpc -q next")

(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec xbacklight -dec 1")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec xbacklight -inc 1")

;;; MENU-INPUT

(define-key *single-menu-map* (kbd "C-h") 'menu-backspace)
(define-key *batch-menu-map* (kbd "C-h") 'menu-backspace)

(define-key *menu-map* (kbd "C-m") 'menu-finish)

(define-key *input-map* (kbd "C-m") 'input-submit)
(define-key *input-map* (kbd "C-h") 'input-delete-backward-char)
(define-key *input-map* (kbd "C-w") 'input-backward-kill-word)

;;; ROOT

(define-key *root-map* (kbd "e") "em")
(define-key *root-map* (kbd "C-e") "em")

(define-key *root-map* (kbd "C-a") "mode-line")
(define-key *root-map* (kbd "a") "mode-line")

(define-key *root-map* (kbd "C-c") "exec uxterm")
(define-key *root-map* (kbd "c") "exec uxterm")

(define-key *root-map* (kbd "C-o") '*open-map*)

;;; OPEN

(define-key *open-map* (kbd "C-b") "brave")
(define-key *open-map* (kbd "C-f") "firefox")
(define-key *open-map* (kbd "C-q") "qutebrowser")
(define-key *open-map* (kbd "C-t") "telegram-desktop")

(define-key *open-map* (kbd "C-c") "menu-rimer")

(define-key *open-map* (kbd "C-w") "type-pass-entry")
(define-key *open-map* (kbd "w") "menu-pass")

(define-key *open-map* (kbd "C-v") "exec uxterm -e pulsemixer")
(define-key *open-map* (kbd "C-p") "exec uxterm -e htop")

(define-key *open-map* (kbd "C-s") "show-mpd")
(define-key *open-map* (kbd "C-k") "show-corona")
(define-key *open-map* (kbd "C-d") "show-hardware")

(define-key *open-map* (kbd "C-m") "show-menu")
