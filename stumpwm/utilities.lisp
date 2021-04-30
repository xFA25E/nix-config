(in-package :stumpwm)

(defun read-battery-status ()
  (let ((output (uiop:run-program '("acpi" "--battery") :output '(:string :stripped t))))
    (ppcre:register-groups-bind (state percentage time)
        ("[^:]+: ([^,]+), ([^%]+)%(?:, (.*))?" output :sharedp t)
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
  (let ((output (uiop:run-program '("amixer" "sget" "Master") :output :string)))
    (ppcre:register-groups-bind (volume state)
        ("Front Left:[^[]+\\[([0-9]+)%\\][^[]+\\[(on|off)\\]" output :sharedp t)
      (values volume state))))

(defun read-mpd-volume-status ()
  (let ((output (uiop:run-program '("mpc" "-q" "volume") :output :string)))
    (parse-integer output :start 8 :junk-allowed t)))



(defun notify-battery-status ()
  (multiple-value-bind (percentage state time) (read-battery-status)
    (when (string= "Discharging" state)
      (message "Battery: ~A ~A ~A" percentage state time)
      (when (<= (parse-integer percentage) 10)
        (uiop:launch-program '("notify_sound"))))))
