(defpackage #:swm-config.timers
  (:use #:cl)
  (:import-from #:alexandria #:if-let #:when-let #:compose #:curry)
  (:import-from #:trivia #:match)
  (:import-from #:uiop #:run-program)
  (:local-nicknames (#:swm #:stumpwm))
  (:export #:timer-add #:timer-kill #:timer-list #:timer-menu #:init))
(in-package #:swm-config.timers)

(defun seconds-left (timer)
  ;; Attention!  Internal sbcl function is used, maybe it is not a great idea.
  (round (/ (- (sb-impl::%timer-expire-time timer)
               (get-internal-real-time))
            internal-time-units-per-second)))

(defclass timer ()
  ((timer :reader timer :initarg :timer)
   (max-name-length :initarg :max-name-length))
  (:default-initargs :max-name-length 0))

(defmethod print-object ((object timer) stream)
  (with-slots (timer max-name-length) object
    (let ((name (sb-ext:timer-name timer)))
      (format stream "~VA" max-name-length name)
      (ltd:human-readable-duration (ltd:duration :sec (seconds-left timer)) stream))))

(defclass timers ()
  ((timers :accessor timers :initarg :timers))
  (:default-initargs :timers (sb-ext:list-all-timers)))

(defmethod initialize-instance :after ((instance timers) &key timers &allow-other-keys)
  (flet ((name-length (timer) (length (sb-ext:timer-name timer))))
    (let* ((max-name-length (reduce #'max timers :initial-value 0 :key #'name-length))
           (make-timer (curry #'make-instance 'timer :max-name-length max-name-length :timer)))
      (setf (timers instance) (mapcar make-timer timers)))))

(defmethod print-object ((object timers) stream)
  (format stream "~{~A~%~}" (timers object)))

(defun existsp (name)
  (find name (sb-ext:list-all-timers) :key #'sb-ext:timer-name :test #'string=))

(defun callback (name)
  (lambda ()
    (run-program `("notify-send" ,(format nil "Timer \"~A\" is out!" name)))
    (run-program '("notify_bruh"))))

(defun launch (name universal-time)
  (let ((timer (sb-ext:make-timer (callback name) :name name :thread t)))
    (sb-ext:schedule-timer timer universal-time :absolute-p t)
    (swm:message "Timer \"~A\" is added" (make-instance 'timer :timer timer))))

(swm:defcommand timer-add (name when) ((:string "Name: ") (:string "When? "))
  (when-let ((timer (existsp name)))
    (throw 'error (format nil "Timer \"~A\" already exists!" (make-instance 'timer :timer timer))))

  (if-let ((timestamp (chronicity:parse when)))
    (launch name (local-time:timestamp-to-universal timestamp))
    (throw 'error (format nil "\"~A\" is an invalid time specification!" when))))

(defun read-timer (&optional (prompt "Timer: "))
  (flet ((make-entry (timer) (list (format nil "~A" timer) (timer timer))))
    (if-let ((menu (mapcar #'make-entry (timers (make-instance 'timers)))))
      (match (swm:select-from-menu (swm:current-screen) menu prompt)
        ((list _ timer) timer)
        (_ (throw 'error "No timer!")))
      (throw 'error "No timers!"))))

(swm:define-stumpwm-type :timer (input prompt)
  (if-let ((name (swm:argument-pop input)))
    (or (existsp name) (throw 'error (format nil "No \"~A\" timer!" name)))
    (read-timer prompt)))

(swm:defcommand timer-kill (timer) ((:timer "Timer: "))
  (when (sb-ext:timer-scheduled-p timer)
    (sb-ext:unschedule-timer timer)
    (swm:message "Timer \"~A\" is killed!" (sb-ext:timer-name timer))))

(swm:defcommand timer-list () ()
  (swm:message "~A" (make-instance 'timers)))

(swm:defcommand timer-menu () ()
  (let ((menu '(("add" "timer-add")
                ("list" "timer-list")
                ("kill" "timer-kill"))))
    (match (swm:select-from-menu (swm:current-screen) menu "Timer: ")
      ((list _ command)
       (swm::eval-command command t)))))

(defun init ()
  (local-time:define-timezone local-time:*default-timezone* #p"/etc/localtime" :load t))
