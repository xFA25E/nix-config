(in-package :stumpwm)

(defvar *battery-timer* nil)
(defvar *date-time-timer* nil)

(defvar *menu*
  '(("screenshot" :command "screenshot")
    ("mount" :shell ("rmount"))
    ("unmount" :shell ("rumount"))
    ("suspend" :shell ("systemctl" "suspend"))))

(defvar *rimer-timers*
  '(("study" . "25")
    ("pause" . "5")
    ("eyes" . "30")
    ("computer" . "1:00")))
