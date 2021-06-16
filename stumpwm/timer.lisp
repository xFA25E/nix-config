(in-package :stumpwm)

(defvar *last-timer-name* nil)

(defvar *frequent-timers*
  '(("pomodoro" . "25")
    ("pause"    . "5")))

(defun time-left (timer)
  ;; Attention!  Internal sbcl function is used, maybe it is not a great idea.
  (round (/ (- (sb-impl::%timer-expire-time timer)
               (get-internal-real-time))
            internal-time-units-per-second)))

(defun parse-duration (duration)
  (trivia:match (mapcar #'parse-integer (ppcre:split ":" duration))
    ((list m) (* m 60))
    ((list h m) (+ (* h 60 60) (* m 60)))
    ((list h m s) (+ (* h 60 60) (* m 60) s))))

(defun format-duration (tot-s)
  (if (minusp tot-s)
      " ELAPSED"
      (let* ((s (rem tot-s 60))
             (tot-m (/ (- tot-s s) 60))
             (m (rem tot-m 60))
             (h (/ (- tot-m m) 60)))
        (format nil "~2,'0D:~2,'0D:~2,'0D" h m s))))

(defun format-timer (timer)
  (format nil "~15A~A" (sb-ext:timer-name timer)
          (format-duration (time-left timer))))

(defun format-timers (&optional (timers (sb-ext:list-all-timers)))
  (format nil "~{~A~%~}" (mapcar #'format-timer timers)))

(defun timer-callback (name duration)
  (let ((msg (format nil "~A ~A" name (format-duration duration))))
    (uiop:run-program (list "notify-send" "Timer" msg))
    (uiop:run-program (list "notify_sound"))))

(defun launch-timer (name duration)
  (let* ((callback (lambda () (timer-callback name duration)))
         (timer (sb-ext:make-timer callback :name name :thread t)))
    (sb-ext:schedule-timer timer duration))
  (message "Timer ~A ~A added" name (format-duration duration)))

(defun timer-exists-p (name)
  (find name (sb-ext:list-all-timers) :key #'sb-ext:timer-name :test #'string=))

(defun read-timer-name (prompt)
  (let ((frequent-names (mapcar #'car *frequent-timers*)))
    (when-let ((name (completing-read (current-screen) prompt frequent-names)))
      (setf *last-timer-name* name))))

(define-stumpwm-type :timer-name (input prompt)
  (or (argument-pop input)
      (read-timer-name prompt)
      (throw 'error "No name")))

(defun read-timer-duration (prompt)
  (let* ((timer (assoc *last-timer-name* *frequent-timers* :test #'string=))
         (duration (if timer (cdr timer) "")))
    (read-one-line (current-screen) prompt :initial-input duration)))

(define-stumpwm-type :timer-duration (input prompt)
  (if-let ((duration (or (argument-pop input) (read-timer-duration prompt))))
    (or (parse-duration duration)
        (throw 'error "Invalid duration."))
    (throw 'error "Duration required.")))

(defun read-timer (prompt)
  (flet ((make-entry (timer) (list (format-timer timer) timer)))
    (if-let ((menu (mapcar #'make-entry (sb-ext:list-all-timers))))
      (or (second (select-from-menu (current-screen) menu prompt))
          (throw 'error "No timer!"))
      (throw 'error "No timers!"))))

(define-stumpwm-type :timer (input prompt)
  (if-let ((name (argument-pop input)))
    (or (timer-exists-p name)
        (throw 'error (format nil "No timer ~A!" name)))
    (read-timer prompt)))

(defcommand timer-add (name duration)
    ((:timer-name "Timer name: ")
     (:timer-duration "Timer duration (H:M:S): "))
  (if (timer-exists-p name)
      (message "Timer ~A already exists!" name)
      (launch-timer name duration)))

(defcommand timer-kill (timer) ((:timer "Timer: "))
  (when (sb-ext:timer-scheduled-p timer)
    (sb-ext:unschedule-timer timer)
    (message "Timer ~A cancelled" (sb-ext:timer-name timer))))

(defcommand timer-list () ()
  (message "NAME          REMAINING~%~A" (format-timers)))

(defcommand timer-menu () ()
  (let ((menu '(("add" :add) ("kill" :kill) ("list" :list))))
    (case (second (select-from-menu (current-screen) menu "Timer: "))
      ((:add) (eval-command "timer-add" t))
      ((:kill) (eval-command "timer-kill" t))
      ((:list) (eval-command "timer-list" t)))))
