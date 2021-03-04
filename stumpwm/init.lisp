(in-package :stumpwm)

(setf *colors*
      ;; '("black" "red"    "green"   "magenta" "blue"    "yellow"  "cyan"    "white"))
      '("#101010" "#ff0086" "#00c918" "#ad00a1" "#3777e6" "#aba800" "#1faaaa" "#ffffff"))

(dolist (s *screen-list*)
  (update-color-map s))

;; (let ((white (nth 7 *colors*))
;;       (black (nth 0 *colors*))
;;       (yellow (nth 5 *colors*))
;;       (blue (nth 4 *colors*))
;;       (green (nth 2 *colors*)))
;;   (set-bg-color black)
;;   (set-border-color white)
;;   (set-fg-color white)
;;   (set-float-focus-color yellow)
;;   (set-float-unfocus-color blue)
;;   (set-focus-color green)
;;   (set-unfocus-color white)
;;   (set-win-bg-color black))
;; (update-colors-all-screens)

(set-font "-*-terminus-medium-r-*-*-24-*-*-*-*-*-iso10646-1")

(clear-window-placement-rules)

(setf *mouse-focus-policy* :click)

(setf *battery-timer* (stumpwm::run-with-timer 60 60 #'notify-battery-status))
(setf *date-time-timer* (stumpwm::run-with-timer 60 60 #'notify-date-time))

(run-shell-command "random_wallpaper")



(load-module "notify")
(notify:notify-server-toggle)
(setf notify:*notify-server-title-color* "^7^B")
(setf notify:*notify-server-body-color* "^7^B")
