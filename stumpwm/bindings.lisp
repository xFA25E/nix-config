(in-package :stumpwm)

(set-prefix-key (kbd "C-z"))

;;; MAPS

(defvar *open-map* (make-sparse-keymap))

;;; MENU-INPUT

(define-key *single-menu-map* (kbd "C-h") 'menu-backspace)
(define-key *batch-menu-map* (kbd "C-h") 'menu-backspace)

(define-key *menu-map* (kbd "C-m") 'menu-finish)

(define-key *input-map* (kbd "C-m") 'input-submit)
(define-key *input-map* (kbd "C-h") 'input-delete-backward-char)
(define-key *input-map* (kbd "C-w") 'input-backward-kill-word)

;;; ROOT

(define-key *root-map* (kbd "C-c") (format nil "exec ~A" *uxterm*))
(define-key *root-map* (kbd "e") "emacs")
(define-key *root-map* (kbd "C-e") "emacs")
(define-key *root-map* (kbd "C-q") "send-raw-key")
(define-key *root-map* (kbd "C-o") '*open-map*)

;;; OPEN

(define-key *open-map* (kbd "C-c") "menu-rimer")

(define-key *open-map* (kbd "C-w") "type-pass-entry")
(define-key *open-map* (kbd "w") "menu-pass")

(define-key *open-map* (kbd "v") (format nil "exec ~A -e ~A" *uxterm* *pulsemixer*))
(define-key *open-map* (kbd "C-p") (format nil "exec ~A -e ~A" *uxterm* *htop*))
(define-key *open-map* (kbd "C-n") (format nil "exec ~A -e ~A" *uxterm* *nload*))

(define-key *open-map* (kbd "C-s") "mpd-controller-interactive")
(define-key *open-map* (kbd "C-l") "brightness-controller-interactive")
(define-key *open-map* (kbd "C-v") "alsa-controller-interactive")

(define-key *open-map* (kbd "C-k") "show-corona")
(define-key *open-map* (kbd "C-d") "show-hardware")

(define-key *open-map* (kbd "C-m") "show-menu")

(define-key *open-map* (kbd "C-f") "firefox")
(define-key *open-map* (kbd "C-q") "qutebrowser")
(define-key *open-map* (kbd "C-b") "chromium-incognito")
(define-key *open-map* (kbd "C-t") "telegram-desktop")

;;; Remapped keys

(define-remapped-keys
    '(("(Firefox|Chromium-browser|TelegramDesktop)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ("C-f"   . "Right")
       ("C-b"   . "Left")
       ("C-v"   . "Next")
       ("M-v"   . "Prior")
       ("M-w"   . "C-c")
       ("C-w"   . "C-x")
       ("C-y"   . "C-v")
       ("M-<"   . "Home")
       ("M->"   . "End")
       ("C-M-b" . "M-Left")
       ("C-M-f" . "M-Right")
       ("M-f"   . "C-Right")
       ("M-b"   . "C-Left")
       ("C-k"   . ("C-S-End" "C-x"))
       ("M-K"   . "C-w")
       ("C-m"   . "RET")
       ("C-s"   . "C-f")
       ("C-h"   . "DEL"))))
