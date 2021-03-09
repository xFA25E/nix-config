(in-package :stumpwm)

(defvar *password-store-directory*
  (or (uiop:getenv-absolute-directory "PASSWORD_STORE_DIR")
      (merge-pathnames #p".password-store/" (user-homedir-pathname))))



(defun list-pass-entries ()
  (let ((prefix-length (length (namestring *password-store-directory*))))
    (mapcar
     (lambda (entry)
       (let ((namestring (namestring entry)))
         (subseq namestring prefix-length (- (length namestring) 4))))
     (directory (make-pathname
                 :directory `(,@(pathname-directory *password-store-directory*)
                              :wild-inferiors)
                 :name :wild
                 :type "gpg")))))

(defun select-pass-entry (&optional (prompt "Pass: "))
  (select-from-menu
   (current-screen) (mapcar #'list (list-pass-entries)) prompt))

(defun parse-pass-field (field)
  (when-let ((groups (nth-value 1 (ppcre:scan-to-strings "^([a-zA-Z_]+): (.*)$" field))))
    (list (aref groups 0) (aref groups 1))))

(defun parse-pass-entry-text (text)
  (let* ((lines (split-string text))
         (password-field `("password" ,(pop lines))))
    (when (string= "---" (car lines))
      (pop lines))
    (sort
     (cons password-field (delete-if #'null (mapcar #'parse-pass-field lines)))
     #'string< :key #'car)))



(define-stumpwm-type :pass-entry (input prompt)
  (if-let ((pass-entry (or (argument-pop input)
                           (first (select-pass-entry prompt)))))
    pass-entry
    (throw 'error "Pass entry required.")))



(defcommand edit-pass-entry (pass-entry) ((:pass-entry "Edit pass: "))
  (run-shell-command (format nil "pass edit '~A'" pass-entry)))

(defcommand type-pass-entry (pass-entry) ((:pass-entry "Type pass: "))
  (let* ((text (run-shell-command (format nil "pass show '~A'" pass-entry) t))
         (menu (cons (list "autotype" :autotype) (parse-pass-entry-text text)))
         (value (cadr (select-from-menu (current-screen) menu "Type field: "))))

    (cond ((eq :autotype value)
           (when-let ((value (cadr (assoc "login" menu :test #'string=))))
             (window-send-string value)
             (window-send-string (string #\tab)))
           (when-let ((value (cadr (assoc "password" menu :test #'string=))))
             (window-send-string value)))

          ((stringp value)
           (window-send-string value)))))

(defcommand otp-pass-entry (pass-entry) ((:pass-entry "Otp pass: "))
  (let ((text (run-shell-command (format nil "pass otp '~A'" pass-entry) t)))
    (window-send-string text)))

(defcommand menu-pass () ()
  (let ((menu '(("type" :type) ("edit" :edit) ("otp" :otp))))
    (case (cadr (select-from-menu (current-screen) menu "Pass: "))
      (:type (stumpwm::eval-command "type-pass-entry" t))
      (:edit (stumpwm::eval-command "edit-pass-entry" t))
      (:otp (stumpwm::eval-command "otp-pass-entry" t)))))
