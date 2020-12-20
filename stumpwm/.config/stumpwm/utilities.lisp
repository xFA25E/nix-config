(in-package :stumpwm)

(defmacro define-run-or-raise (binary class)
  (check-type binary string)
  (check-type class string)
  `(defcommand ,(intern (string-upcase binary)) () ()
     ,(concatenate 'string "Start " binary " unless it is already running, in which case focus it.")
     (run-or-raise ,binary '(:class ,class))))

(defmacro define-stumpwm-type-number (label form)
  (check-type label keyword)
  `(define-stumpwm-type ,label (input prompt)
     (when-let ((n (or (argument-pop input)
                       (read-one-line (current-screen) prompt :initial-input ,form))))
       (handler-case (parse-integer n)
         (parse-error (c)
           (declare (ignore c))
           (throw 'error "Number required."))))))

(defmacro define-screen-mode-line-formatter (character name form)
  (check-type character character)
  (check-type name symbol)
  (let ((mode-line-symbol (gensym "MODE-LINE")))
    `(progn
       (defun ,name (,mode-line-symbol)
         (declare (ignore ,mode-line-symbol))
         (let ((result ,form))
           (if (stringp result) result "<>")))
       (add-screen-mode-line-formatter ,character ',name))))



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
           (parse-integer (uiop:read-file-string s) :junk-allowed t)))
    (let* ((path "/sys/class/backlight/intel_backlight/")
           (brightness-file (concatenate 'string path "brightness"))
           (max-brightness-file (concatenate 'string path "max_brightness"))
           (brightness (read-number brightness-file))
           (max-brightness (read-number max-brightness-file)))
      (write-to-string (round (* (/ brightness max-brightness) 100))))))

(let ((volume-scanner (ppcre:create-scanner "Front Left:[^[]+\\[([0-9]+)%\\]")))
 (defun read-alsa-volume-status ()
   (let ((output (run-shell-command "amixer -D pulse sget Master" t)))
     (extract-first-regexp-group volume-scanner output))))

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
  (let* ((*queue-messages-p* t)
         (percentage-difference (- *battery-previous-percentage* *battery-percentage*))
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
    (let ((*queue-messages-p* t))
      (echo-date))))



(define-stumpwm-type-number :brightness (read-brightness-status))
(define-stumpwm-type-number :alsa-volume (read-alsa-volume-status))
(define-stumpwm-type-number :mpd-volume (read-mpd-volume-status))

(define-screen-mode-line-formatter #\t fmt-window-title
  (if-let ((window (current-window))) (window-title window) "<no window>"))
(define-screen-mode-line-formatter #\l fmt-brightness
  (read-brightness-status))
(define-screen-mode-line-formatter #\a fmt-alsa-volume
  (read-alsa-volume-status))
(define-screen-mode-line-formatter #\b fmt-battery
  (read-battery-status))
