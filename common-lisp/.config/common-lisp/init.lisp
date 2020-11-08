;; -*- mode: lisp; -*-
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".local/share/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#+quicklisp
(progn
  (pushnew (merge-pathnames "Documents/projects/common-lisp/"
                            (user-homedir-pathname))
           ql:*local-project-directories*))

;; (setf *print-circle* t)
