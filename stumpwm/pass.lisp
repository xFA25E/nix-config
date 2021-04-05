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

(defun parse-pass-entry-text (text)
  (let ((fields nil))
    (ppcre:do-register-groups (pass) ("^(.*)\\n" text)
      (push (list "password" pass) fields))
    (ppcre:do-register-groups (key value) ("\\n([a-zA-Z_]+): (.*)" text)
      (push (list key value) fields))
    (sort (delete-if #'null fields) #'string< :key #'car)))



(define-stumpwm-type :pass-entry (input prompt)
  (if-let ((pass-entry (or (argument-pop input)
                           (first (select-pass-entry prompt)))))
    pass-entry
    (throw 'error "Pass entry required.")))



(defcommand edit-pass-entry (pass-entry) ((:pass-entry "Edit pass: "))
  (uiop:launch-program `("pass" "edit" ,pass-entry)))

(defcommand type-pass-entry (pass-entry) ((:pass-entry "Type pass: "))
  (let* ((text (uiop:run-program `("pass" "show" ,pass-entry) :output :string))
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

(defcommand insert-pass-entry () ()
  (if-let ((entry (completing-read (current-screen) "Insert or edit: " (list-pass-entries))))
    (unless (zerop (length entry))
      (uiop:launch-program `("pass" "edit" ,entry)))))

(defcommand otp-pass-entry (pass-entry) ((:pass-entry "Otp pass: "))
  (let ((text (uiop:run-program `("pass" "otp" ,pass-entry) :output :string)))
    (window-send-string text)))

(defcommand menu-pass () ()
  (let ((menu '(("type" :type) ("edit" :edit) ("insert" :insert) ("otp" :otp))))
    (case (cadr (select-from-menu (current-screen) menu "Pass: "))
      (:type (eval-command "type-pass-entry" t))
      (:edit (eval-command "edit-pass-entry" t))
      (:insert (insert-pass-entry))
      (:otp (eval-command "otp-pass-entry" t)))))
