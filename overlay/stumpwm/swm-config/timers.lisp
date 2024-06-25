(defpackage #:swm-config.timers
  (:use #:cl)
  (:import-from #:alexandria #:if-let #:when-let #:compose #:curry)
  (:import-from #:trivia #:match)
  (:import-from #:uiop #:run-program)
  (:local-nicknames (#:swm #:stumpwm))
  (:export #:timer-add #:timer-kill #:timer-list #:timer-menu #:timer-add-sitting-timer #:init))
(in-package #:swm-config.timers)

;;; INIT

(defun init ()
  (local-time:define-timezone local-time:*default-timezone* #p"/etc/localtime" :load t))

;;; STUMPWM COMMANDS

(swm:defcommand timer-add (name when) ((:non-empty-string "Name: ")
                                       (:non-empty-string "When? "))
  (when-let ((timer (existsp name)))
    (throw 'error (format nil "Timer \"~A\" already exists!" (make-instance 'timer :timer timer))))

  (if-let ((timestamp (chronicity:parse (string-trim '(#\Space #\Tab #\Newline) when))))
    (launch name (local-time:timestamp-to-universal timestamp))
    (throw 'error (format nil "\"~A\" is an invalid time specification!" when))))

(swm:defcommand timer-kill (timer) ((:timer "Timer: "))
  (when (sb-ext:timer-scheduled-p timer)
    (sb-ext:unschedule-timer timer)
    (swm:message "Timer \"~A\" is killed!" (sb-ext:timer-name timer))))

(swm:defcommand timer-list () ()
  (swm:message "~A" (make-instance 'timers)))

(swm:defcommand timer-add-sitting-timer () ()
  (launch-sitting-timer
   (if (existsp :sitting-timer-sit)
       :sitting-timer-stand
       :sitting-timer-sit)))

(swm:defcommand timer-menu () ()
  (let ((menu '(("add" "timer-add")
                ("list" "timer-list")
                ("kill" "timer-kill"))))
    (match (swm:select-from-menu (swm:current-screen) menu "Timer: ")
      ((list _ command)
       (swm::eval-command command t)))))

;;; STUMPWM TYPES

(swm:define-stumpwm-type :non-empty-string (input prompt)
  (or (swm:argument-pop input)
      (when-let ((string (swm:read-one-line (swm:current-screen) prompt)))
        (unless (zerop (length string))
          string))
      (throw 'error "Empty input!")))

(swm:define-stumpwm-type :timer (input prompt)
  (if-let ((name (swm:argument-pop input)))
    (or (existsp name) (throw 'error (format nil "No \"~A\" timer!" name)))
    (read-timer prompt)))

(defun read-timer (&optional (prompt "Timer: "))
  (flet ((make-entry (timer) (list (format nil "~A" timer) (timer timer))))
    (if-let ((menu (mapcar #'make-entry (timers (make-instance 'timers)))))
      (match (swm:select-from-menu (swm:current-screen) menu prompt)
        ((list _ timer) timer)
        (_ (throw 'error "No timer!")))
      (throw 'error "No timers!"))))

;;; TIMER

(defclass timer ()
  ((timer :reader timer :initarg :timer)
   (max-name-length :initarg :max-name-length))
  (:default-initargs :max-name-length 0))

(defmethod print-object ((object timer) stream)
  (with-slots (timer max-name-length) object
    (let ((name (sb-ext:timer-name timer)))
      (format stream "~VA" max-name-length name)
      (ltd:human-readable-duration (ltd:duration :sec (seconds-left timer)) stream))))

;;; TIMERS

(defclass timers ()
  ((timers :accessor timers :initarg :timers))
  (:default-initargs :timers (sb-ext:list-all-timers)))

(defmethod initialize-instance :after ((instance timers) &key timers &allow-other-keys)
  (flet ((name-length (timer)
           (length (format nil "~A" (sb-ext:timer-name timer)))))
    (let* ((max-name-length (reduce #'max timers :initial-value 0 :key #'name-length))
           (make-timer (curry #'make-instance 'timer :max-name-length max-name-length :timer)))
      (setf (timers instance) (mapcar make-timer timers)))))

(defmethod print-object ((object timers) stream)
  (let ((seconds-left (compose #'seconds-left #'timer)))
    (format stream "~{~A~%~}" (sort (timers object) #'< :key seconds-left))))

;;; AUX

(defun existsp (name)
  (find name (sb-ext:list-all-timers) :key #'sb-ext:timer-name :test #'equal))

(defun callback (name)
  (lambda ()
    (run-program `("notify-send" ,(format nil "Timer \"~A\" is out!" name)))
    (run-program '("notify_ding"))))

(defun launch (name universal-time)
  (let ((timer (sb-ext:make-timer (callback name) :name name :thread t)))
    (sb-ext:schedule-timer timer universal-time :absolute-p t)
    (swm:message "Timer \"~A\" is added" (make-instance 'timer :timer timer))))

(defun seconds-left (timer)
  ;; Attention!  Internal sbcl function is used, maybe it is not a great idea.
  (round (/ (- (sb-impl::%timer-expire-time timer)
               (get-internal-real-time))
            internal-time-units-per-second)))

;;; SITTING

(defun launch-sitting-timer (name)
  (when-let ((timer (or (existsp :sitting-timer-sit)
                        (existsp :sitting-timer-stand))))
    (when (sb-ext:timer-scheduled-p timer)
      (sb-ext:unschedule-timer timer)))

  (let* ((text (ecase name
                 (:sitting-timer-sit "in 30 minutes")
                 (:sitting-timer-stand "in 5 minutes")))
         (time (local-time:timestamp-to-universal (chronicity:parse text)))
         (timer (sb-ext:make-timer (sitting-timer-callback name) :name name :thread t)))
    (sb-ext:schedule-timer timer time :absolute-p t)
    (run-program `("notify-send" ,(format nil "Timer \"~A\" is added" name)))))

(defun sitting-timer-callback (name)
  (lambda ()
    (funcall (callback name))
    (launch-sitting-timer
     (ecase name
       (:sitting-timer-sit :sitting-timer-stand)
       (:sitting-timer-stand :sitting-timer-sit)))))
