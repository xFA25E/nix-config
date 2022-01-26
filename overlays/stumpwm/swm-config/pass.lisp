(defpackage #:swm-config.pass
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm)
                    (#:re #:cl-ppcre))
  (:import-from #:alexandria #:when-let)
  (:import-from #:trivia #:match)
  (:import-from #:uiop
                #:close-streams
                #:getenv-absolute-directory
                #:launch-program
                #:process-info-input
                #:run-program
                #:wilden)
  (:export #:pass-edit #:pass-type #:pass-insert #:pass-otp #:pass-menu))
(in-package #:swm-config.pass)

(defun store-directory ()
  (or (getenv-absolute-directory "PASSWORD_STORE_DIR")
      (merge-pathnames #p".password-store/" (user-homedir-pathname))))

(defun list-entries ()
  (let* ((store-directory (store-directory))
         (files (directory (make-pathname :type "gpg" :defaults (wilden store-directory))))
         (prefix-length (length (namestring store-directory))))
    (flet ((make-entry (pathname &aux (namestring (namestring pathname)))
             (subseq namestring prefix-length (- (length namestring) 4))))
      (mapcar #'make-entry files))))

(defun parse-entry-contents (contents)
  (let ((fields nil))
    (re:do-register-groups (pass) ("^(.*)\\n" contents)
      (push (list "password" pass) fields))
    (re:do-register-groups (key value) ("\\n([a-zA-Z_]+): (.*)" contents)
      (push (list key value) fields))
    (sort (delete-if #'null fields) #'string< :key #'first)))

(defun gpg-agent-running-p ()
  (multiple-value-bind (output output-error status)
      (run-program '("gpg" "--sign" "--armor" "--pinentry-mode" "cancel") :input '("unlock") :ignore-error-status t)
    (declare (ignore output output-error))
    (zerop status)))

(defun unlock-gpg-agent ()
  (let ((proc (launch-program '("gpg" "--sign" "--armor") :input :stream)))
    (write-string "unlock" (process-info-input proc))
    (close-streams proc)))

(defmacro with-running-gpg-agent (&body body)
  `(if (gpg-agent-running-p)
       (progn ,@body)
       (unlock-gpg-agent)))

(swm:define-stumpwm-type :pass (input prompt)
  (or (swm:argument-pop input)
      (match (swm:select-from-menu (swm:current-screen) (list-entries) prompt)
        ((list entry) entry)
        (_ (throw 'error "Pass entry required.")))))

(swm:defcommand pass-edit (pass) ((:pass "Pass edit: "))
  (launch-program `("pass" "edit" ,pass)))

(swm:defcommand pass-type (pass) ((:pass "Pass type: "))
  (with-running-gpg-agent
    (let* ((contents (run-program `("pass" "show" ,pass) :output :string))
           (menu (cons '("autotype" :autotype) (parse-entry-contents contents))))
      (match (swm:select-from-menu (swm:current-screen) menu "Pass type field: ")
         ((list _ :autotype)
         (when-let ((value (second (assoc "login" menu :test #'string=))))
           (swm:window-send-string value)
           (swm:window-send-string (string #\tab)))
         (when-let ((value (second (assoc "password" menu :test #'string=))))
           (swm:window-send-string value)))
        ((list _ value)
         (swm:window-send-string value))))))

(swm:defcommand pass-insert () ()
  (when-let ((entry (swm:completing-read (swm:current-screen) "Pass insert: " (list-entries))))
    (unless (zerop (length entry))
      (launch-program `("pass" "edit" ,entry)))))

(swm:defcommand pass-otp (pass) ((:pass "Pass otp: "))
  (with-running-gpg-agent
    (swm:window-send-string (run-program `("pass" "otp" ,pass) :output :string))))

(swm:defcommand pass-menu () ()
  (let ((menu '(("type" "pass-type")
                ("edit" "pass-edit")
                ("insert" "pass-insert")
                ("otp" "pass-otp"))))
    (match (swm:select-from-menu (swm:current-screen) menu "Pass menu: ")
      ((list _ command)
       (swm::eval-command command t)))))
