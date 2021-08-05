(in-package :stumpwm)

(setf *colors*
      ;; '("black" "red"    "green"   "magenta" "blue"    "yellow"  "cyan"    "white"))
      '("#101010" "#ff0086" "#00c918" "#ad00a1" "#3777e6" "#aba800" "#1faaaa" "#ffffff"))

(dolist (s *screen-list*)
  (update-color-map s))

(set-font "-*-terminus-medium-r-*-*-24-*-*-*-*-*-iso10646-1")

(clear-window-placement-rules)

(setf *mouse-focus-policy* :click)

(defvar *battery-timer* (run-with-timer 60 60 #'notify-battery-status))
(defvar *date-time-timer* (run-with-timer 30 1800 #'echo-date))

(defvar *default-layout* "dvorak")
(defvar *previous-layout* nil)
(defvar *nested-counter* 0)

(defun current-layout ()
  (uiop:run-program '("xkb-switch" "-p") :output '(:string :stripped t)))

(defun switch-layout (layout)
  (uiop:run-program (list "xkb-switch" "-s" layout)))

(defun maybe-save-layout ()
  (let ((current-layout (current-layout)))
    (when (string/= *default-layout* current-layout)
      (setf *previous-layout* current-layout)
      (switch-layout *default-layout*))))

(defun maybe-restore-layout ()
  (when *previous-layout*
    (let ((previous-layout *previous-layout*))
      (setf *previous-layout* nil)
      (switch-layout previous-layout))))

(defun swap-keyboard (key key-seq cmd)
  (setf *nested-counter* 0)
  (cond ((and (symbolp cmd) (null (cdr key-seq)))
         (maybe-save-layout))
        ((and (null cmd))
         (maybe-restore-layout))))

(add-hook *key-press-hook* 'swap-keyboard)

(defun increment-nested-counter (cmd)
  (incf *nested-counter*)
  (unless *previous-layout*
    (maybe-save-layout)))

(add-hook *pre-command-hook* 'increment-nested-counter)

(defun decrement-nested-counter (cmd)
  (decf *nested-counter*)
  (when (zerop *nested-counter*)
    (maybe-restore-layout)))

(add-hook *post-command-hook* 'decrement-nested-counter)

(uiop:launch-program '("systemctl" "--user" "start" "random-background.service"))
