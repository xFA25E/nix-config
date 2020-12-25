(in-package :stumpwm)

(set-font "-*-terminus-medium-r-*-*-24-*-*-*-*-*-iso10646-1")

(clear-window-placement-rules)

(setf *screen-mode-line-format* "^B^70[%n] %t L%l V%a B%b %d")

(setf *mode-line-border-width* 0)
(setf *mode-line-pad-x* 0)
(setf *mode-line-pad-y* 0)

(setf *mouse-focus-policy* :click)

(setf *battery-timer* (stumpwm::run-with-timer 60 60 #'notify-battery-status))
(setf *date-time-timer* (stumpwm::run-with-timer 60 60 #'notify-date-time))

(run-shell-command "random_wallpaper")



(load-module "notify")
(notify:notify-server-toggle)
(setf notify:*notify-server-title-color* "^7^B")
(setf notify:*notify-server-body-color* "^7^B")
