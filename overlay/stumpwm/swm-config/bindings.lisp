(defpackage #:swm-config.bindings
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm)))
(in-package #:swm-config.bindings)

(defvar *open-map* (swm:make-sparse-keymap))
(defvar *timer-map* (swm:make-sparse-keymap))

(defun define-key (map key command)
  (swm:define-key map (swm:kbd key) command))

(defun define-keys (map &rest keys-and-commands)
  (loop :for (key command) :on keys-and-commands :by #'cddr
        :do (define-key map key command)))

;;; MENU-INPUT

(define-key swm:*single-menu-map* "C-h" 'swm:menu-backspace)
(define-key swm:*batch-menu-map* "C-h" 'swm:menu-backspace)
(define-key swm:*menu-map* "C-m" 'swm:menu-finish)

(define-keys swm:*input-map*
  "C-m" 'swm::input-submit
  "C-h" 'swm::input-delete-backward-char
  "C-w" 'swm::input-backward-kill-word)

;;; ROOT

(define-keys swm:*root-map*
  "C-c" "exec uxterm"
  "C-q" "send-raw-key"
  "C-o" '*open-map*)

;;; OPEN

(define-keys *open-map*
  "C-c" '*timer-map*
  "c"   "timer-menu"

  "C-w" "pass-type"
  "w"   "pass-menu"

  "v"   "exec uxterm -e pulsemixer"
  "C-p" "exec uxterm -e htop"
  "C-n" "exec uxterm -e nload"

  "C-s" "mpd-interactive"
  "C-l" "brightness-interactive"
  "C-v" "alsa-interactive"

  "C-k" "covid-19-italy"
  "C-d" "hardware"
  "C-m" "main-menu"

  "C-b" "brave-incognito"
  "C-e" "mpv"
  "C-f" "firefox"
  "C-t" "telegram-desktop")

;;; TIMER

(define-keys *timer-map*
  "C-c" "timer-add"
  "C-n" "timer-add-work-block")

;;; REMAPPED KEYS

(swm:define-remapped-keys
    '(("(Tor Browser|Brave-browser|firefox|discord|TelegramDesktop)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ("C-f"   . "Right")
       ("C-b"   . "Left")
       ("C-v"   . "Next")
       ("M-v"   . "Prior")
       ("M-w"   . "C-c")
       ("C-w"   . "C-DEL")
       ("M-d"   . "C-Delete")
       ("C-y"   . "C-v")
       ("M-less"   . "Home")
       ("M-greater"   . "End")
       ("C-M-b" . "M-Left")
       ("C-M-f" . "M-Right")
       ("M-f"   . "C-Right")
       ("M-b"   . "C-Left")
       ("C-k"   . ("C-S-End" "DEL"))
       ("M-K"   . "C-w")
       ("C-m"   . "RET")
       ("C-s"   . "C-f")
       ("C-h"   . "DEL")
       ("C-d"   . "Delete"))))
