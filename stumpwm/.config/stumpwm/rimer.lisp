(in-package :stumpwm)

(defvar *timer-updater* (or (getenv "RIMER_CALLBACK") "rimer_callback"))

(let ((duration-scanner
        (ppcre:create-scanner "^(?:(t)?([0-1]?[0-9]|2[0-3]):)?([0-5][0-9])$")))
  (defun parse-duration (duration)
    (when-let ((groups (nth-value 1 (ppcre:scan-to-strings duration-scanner duration))))
      (let* ((hours (parse-integer (or (aref groups 1) "0")))
             (minutes (parse-integer (aref groups 2)))
             (total-seconds (+ (* hours 60 60) (* minutes 60))))
        (if (aref groups 0)
            (multiple-value-bind (s m h) (get-decoded-time)
              (let ((duration (- total-seconds (+ s (* m 60) (* h 60 60)))))
                (if (minusp duration)
                    (+ (* 60 60 24) duration)
                    duration)))
            total-seconds)))))

(defun parse-rimer-report (text)
  (delete-if
   #'null
   (mapcar
    (lambda (line)
      (when-let ((groups (nth-value 1 (ppcre:scan-to-strings "^(.*?) (\\d+) (\\d+) (running|paused|halted)$" line))))
        (list (aref groups 0)
              (parse-integer (aref groups 1))
              (parse-integer (aref groups 2))
              (let ((state (aref groups 3)))
                (cond ((string= "running" state) :running)
                      ((string= "paused" state) :paused)
                      ((string= "halted" state) :halted))))))
    (split-string text))))

(defun format-duration (tot-s)
  (let* ((s (rem tot-s 60))
         (tot-m (/ (- tot-s s) 60))
         (m (rem tot-m 60))
         (h (/ (- tot-m m) 60)))
    (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)))

(defun format-report (report)
  (when report
    (let* ((max (reduce #'max (mapcar #'length (mapcar #'first report))))
           (fmt (format nil "~~~DA  ~~A  ~~A  ~~A" max)))
      (format nil "~{~A~%~}"
              (mapcar
               (lambda (entry)
                 (destructuring-bind (name elapsed duration state) entry
                   (format nil fmt
                           name
                           (format-duration elapsed)
                           (if (= duration (1- (expt 2 64)))
                               " STPWTCH"
                               (format-duration duration))
                           state)))
               report)))))

(defun get-rimer-report (&optional (allowed-states '(:running :paused :halted)))
  (delete-if-not
   (lambda (timer) (member (nth 3 timer) allowed-states))
   (parse-rimer-report (run-shell-command "rimer report" t))))

(define-stumpwm-type :duration (input prompt)
  (if-let ((duration (or (argument-pop input)
                         (read-one-line (current-screen) prompt))))
    (if-let ((seconds (parse-duration duration)))
      seconds
      (throw 'error "Invalid duration."))
    (throw 'error "Duration required.")))

(defmacro define-stumpwm-type-rimer-timer (name allowed-states)
  `(define-stumpwm-type ,name (input prompt)
     (if-let ((menu (get-rimer-report ',allowed-states)))
       (if-let ((timer (or (argument-pop input)
                           (select-from-menu (current-screen) menu prompt))))
         (first timer)
         (throw 'error "No timer!"))
       (throw 'error "No timers!"))))

(define-stumpwm-type-rimer-timer :rimer-timer (:running :paused :halted))
(define-stumpwm-type-rimer-timer :rimer-timer-paused (:paused))
(define-stumpwm-type-rimer-timer :rimer-timer-running (:running))

(defcommand add-rimer-countdown (name duration)
    ((:string "Countdown name: ") (:duration "Countdown duration ([[t]HH:]MM): "))
  (message
   "~A"
   (run-shell-command
    (format nil "rimer add --name '~A' --duration '~D' --step '~D'" name duration duration)
    t)))

(defcommand add-rimer-stopwatch (name) ((:string "Stopwatch name: "))
  (add-rimer-countdown name (1- (expt 2 64))))

(defcommand pause-rimer-timer (timer) ((:rimer-timer-running "Running timer: "))
  (message "~A" (run-shell-command (format nil "rimer pause --name '~A'" timer) t)))

(defcommand resume-rimer-timer (timer) ((:rimer-timer-paused "Paused timer: "))
  (message "~A" (run-shell-command (format nil "rimer resume --name '~A'" timer) t)))

(defcommand report-rimer () ()
  (message "~A" (format-report (get-rimer-report))))

(defcommand halt-rimer-timer (timer) ((:rimer-timer "Timer: "))
  (message "~A" (run-shell-command (format nil "rimer halt --name '~A'" timer) t)))

(defcommand quit-rimer () ()
  (message "~A" (run-shell-command "rimer quit" t)))

(defcommand start-rimer () ()
  (message
   "~A"
   (run-shell-command
    (format nil "pidof rimer || rimer start '~A'" *timer-updater*)
    t)))

(defcommand menu-rimer () ()
  (let ((menu '(("countdown" :countdown) ("stopwatch" :stopwatch)
                ("pause" :pause) ("resume" :resume)
                ("report" :report) ("halt" :halt)
                ("quit" :quit) ("start" :start))))
    (case (cadr (select-from-menu (current-screen) menu "Rimer: "))
      ((:countdown) (eval-command "add-rimer-countdown" t))
      ((:stopwatch) (eval-command "add-rimer-stopwatch" t))
      ((:pause) (eval-command "pause-rimer-timer" t))
      ((:resume) (eval-command "resume-rimer-timer" t))
      ((:report) (eval-command "report-rimer" t))
      ((:halt) (eval-command "halt-rimer-timer" t))
      ((:quit) (eval-command "quit-rimer" t))
      ((:start) (eval-command "start-rimer" t)))))
