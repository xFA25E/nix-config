(in-package :stumpwm)

(defvar *timer-updater* (or (getenv "RIMER_CALLBACK") *rimer-callback*))

(defun parse-duration (duration)
  (ppcre:register-groups-bind (till hours minutes)
      ("^(?:(t)?([0-1]?[0-9]|2[0-3]):)?([0-5]?[0-9])$" duration)
    (let ((total-seconds (+ (* (parse-integer (or hours "0")) 60 60)
                            (* (parse-integer minutes) 60))))
      (if till
          (multiple-value-bind (s m h) (get-decoded-time)
            (let ((duration (- total-seconds (+ s (* m 60) (* h 60 60)))))
              (if (minusp duration)
                  (+ (* 60 60 24) duration)
                  duration)))
          total-seconds))))

(defun parse-rimer-report (text)
  (mapcar
   (lambda (obj)
     (list (jsown:val obj "name")
           (jsown:val (jsown:val obj "elapsed") "secs")
           (jsown:val (jsown:val obj "duration") "secs")
           (let ((state (jsown:val obj "state")))
             (cond ((string-equal "running" state) :running)
                   ((string-equal "paused" state) :paused)
                   ((string-equal "halted" state) :halted)))))
   (jsown:parse text)))

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
   (parse-rimer-report (uiop:run-program `(,*rimer* "report" "-j") :output :string))))

(let ((last-name nil))
  (define-stumpwm-type :rimer-name (input prompt)
    (if-let ((name (or (argument-pop input)
                       (completing-read (current-screen) prompt
                                        (mapcar #'car *rimer-timers*)))))
      (setf last-name name)
      (throw 'error "No name")))

  (define-stumpwm-type :rimer-duration (input prompt)
    (if-let ((duration
              (or (argument-pop input)
                  (read-one-line
                   (current-screen) prompt
                   :initial-input
                   (or (cdr (assoc last-name *rimer-timers* :test #'string=))
                       "")))))
      (if-let ((seconds (parse-duration duration)))
        seconds
        (throw 'error "Invalid duration."))
      (throw 'error "Duration required."))))

(defun show-rimer-output (output)
  (unless (zerop (length output))
    (message "~A" output)))

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
    ((:rimer-name "Countdown name: ")
     (:rimer-duration "Countdown duration ([[t]H:]M): "))
  (let ((dur (write-to-string duration)))
    (show-rimer-output
     (uiop:run-program `(,*rimer* "add" "--name" ,name "--duration" ,dur "--step" ,dur)
      :output :string))))

(defcommand add-rimer-stopwatch (name) ((:rimer-name "Stopwatch name: "))
  (let ((dur (write-to-string (1- (expt 2 64)))))
    (show-rimer-output
     (uiop:run-program `(,*rimer* "add" "--name" ,name "--duration" ,dur "--step" "3600")
      :output :string))))

(defcommand pause-rimer-timer (timer) ((:rimer-timer-running "Running timer: "))
  (show-rimer-output
   (uiop:run-program `(,*rimer* "pause" "--name" ,timer) :output :string)))

(defcommand resume-rimer-timer (timer) ((:rimer-timer-paused "Paused timer: "))
  (show-rimer-output
   (uiop:run-program `(,*rimer* "resume" "--name" ,timer) :output :string)))

(defcommand report-rimer () ()
  (show-rimer-output (format-report (get-rimer-report))))

(defcommand halt-rimer-timer (timer) ((:rimer-timer "Timer: "))
  (show-rimer-output
   (uiop:run-program `(,*rimer* "halt" "--name" ,timer) :output :string)))

(defcommand menu-rimer () ()
  (let ((menu '(("countdown" :countdown) ("stopwatch" :stopwatch)
                ("pause" :pause) ("resume" :resume)
                ("report" :report) ("halt" :halt))))
    (case (cadr (select-from-menu (current-screen) menu "Rimer: "))
      ((:countdown) (eval-command "add-rimer-countdown" t))
      ((:stopwatch) (eval-command "add-rimer-stopwatch" t))
      ((:pause) (eval-command "pause-rimer-timer" t))
      ((:resume) (eval-command "resume-rimer-timer" t))
      ((:report) (eval-command "report-rimer" t))
      ((:halt) (eval-command "halt-rimer-timer" t)))))
