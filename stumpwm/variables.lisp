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
  '(("screenshot" :command "screenshot")
    ("mount" :shell "rmount")
    ("unmount" :shell "rumount")
    ("suspend" :shell "systemctl suspend")))

(defvar *rimer-timers*
  '(("study" . "25")
    ("pause" . "5")
    ("eyes" . "30")))
