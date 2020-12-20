(in-package :stumpwm)

(defvar *slynk-port* 4096)

(defvar *battery-device*
  (find "BAT" (split-string (run-shell-command "upower -e" t)) :test #'search))

(defvar *battery-percentage* 100)
(defvar *battery-state* nil)
(defvar *battery-time* nil)

(defvar *battery-previous-percentage* 100)

(defvar *battery-timer* nil)
(defvar *date-time-timer* nil)

(defvar *menu*
  '(("brightness" :command "set-brightness")
    ("lock screen" :shell "lock")
    ("suspend" :shell "systemctl suspend")
    ("mount" :shell "rmount")
    ("unmount" :shell "rumount")
    ("screenshot" :shell "screenshot")
    ("alsa volume" :command "set-alsa-volume")
    ("mpd volume" :command "set-mpd-volume")))
