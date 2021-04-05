(in-package :stumpwm)

(defun read-battery-status ()
  (let ((output (uiop:run-program '("acpi" "--battery") :output '(:string :stripped t))))
    (ppcre:register-groups-bind (state percentage time)
        ("[^:]+: ([^,]+), ([^%]+)%(?:, (.*?))?" output :sharedp t)
      (values percentage state time))))

(defun read-brightness-status ()
  (flet ((read-number (s)
           (parse-integer (uiop:read-file-string (car (directory s))) :junk-allowed t)))
    (let* ((brightness-file "/sys/class/backlight/*/brightness")
           (max-brightness-file "/sys/class/backlight/*/max_brightness")
           (brightness (read-number brightness-file))
           (max-brightness (read-number max-brightness-file)))
      (write-to-string (round (* (/ brightness max-brightness) 100))))))

(defun read-alsa-volume-status ()
  (let ((output (uiop:run-program `(,*amixer* "sget" "Master") :output :string)))
    (ppcre:register-groups-bind (volume state)
        ("Front Left:[^[]+\\[([0-9]+)%\\][^[]+\\[(on|off)\\]" output :sharedp t)
      (values volume state))))

(defun read-mpd-volume-status ()
  (let ((output (uiop:run-program '("mpc" "-q" "volume") :output :string)))
    (parse-integer output :start 8 :junk-allowed t)))



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
        (uiop:launch-program '("notify_sound"))))))

(defun notify-date-time ()
  (when (zerop (rem (nth-value 1 (get-decoded-time)) 30))
    (echo-date)))
