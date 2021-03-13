(in-package :stumpwm)

(defun extract-first-regexp-group (scanner string)
  (let ((value (nth-value 1 (ppcre:scan-to-strings scanner string))))
    (when (and (vectorp value) (/= 0 (length value)))
      (aref value 0))))



(let ((time-scanner (ppcre:create-scanner "time to (?:full|empty): +([0-9]+\\.[0-9]+ +[a-zA-Z]+)"))
      (percentage-scanner (ppcre:create-scanner "percentage: +([0-9]+)"))
      (state-scanner (ppcre:create-scanner "state: +([-a-zA-Z]+)")))
  (defun read-battery-status ()
    (when *battery-device*
      (let ((output (run-shell-command (format nil "upower -i '~A'" *battery-device*) t)))
        (values (extract-first-regexp-group percentage-scanner output)
                (extract-first-regexp-group state-scanner output)
                (extract-first-regexp-group time-scanner output))))))

(defun read-brightness-status ()
  (flet ((read-number (s)
           (parse-integer (uiop:read-file-string (car (directory s))) :junk-allowed t)))
    (let* ((brightness-file "/sys/class/backlight/*/brightness")
           (max-brightness-file "/sys/class/backlight/*/max_brightness")
           (brightness (read-number brightness-file))
           (max-brightness (read-number max-brightness-file)))
      (write-to-string (round (* (/ brightness max-brightness) 100))))))

(let ((volume-scanner (ppcre:create-scanner "Front Left:[^[]+\\[([0-9]+)%\\]"))
      (state-scanner (ppcre:create-scanner "Front Left:[^[]+\\[[0-9]+%\\][^[]+\\[(on|off)\\]")))
 (defun read-alsa-volume-status ()
   (let ((output (run-shell-command "amixer -D pulse sget Master" t)))
     (values (extract-first-regexp-group volume-scanner output)
             (extract-first-regexp-group state-scanner output)))))

(defun read-mpd-volume-status ()
  (let ((output (run-shell-command "mpc -q volume" t)))
    (ppcre:scan-to-strings "[0-9]+" output)))



(defun update-battery-status-variables ()
  (multiple-value-bind (percentage state time) (read-battery-status)
    (setf *battery-percentage* (parse-integer percentage)
          *battery-state* state
          *battery-time* time)))



(defun notify-battery-status ()
  (update-battery-status-variables)
  (let* ((percentage-difference (- *battery-previous-percentage* *battery-percentage*))
         (criticalp (<= *battery-percentage* 15))
         (chargingp (<= percentage-difference 0))
         (notifyp (<= 10 percentage-difference)))
    (when (or criticalp chargingp notifyp)
      (setf *battery-previous-percentage* *battery-percentage*)
      (unless chargingp
        (message "BATTERY: ~D ~A ~A" *battery-percentage* *battery-state* *battery-time*))
      (when criticalp
        (run-shell-command "notify_sound")))))

(defun notify-date-time ()
  (when (zerop (rem (nth-value 1 (get-decoded-time)) 30))
    (echo-date)))



(define-stumpwm-type :rest-strings (input prompt)
  (declare (ignore prompt))
  (or (argument-pop-rest input) ""))
