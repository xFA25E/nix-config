#-clisp (require "asdf")

#-quicklisp
(let ((quicklisp-init #-clisp (uiop:xdg-data-home "quicklisp" "setup.lisp")
                      #+clisp (concatenate 'string (ext:getenv "XDG_DATA_HOME") "/quicklisp/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#+quicklisp
(let ((docs-dir (uiop:run-program '("xdg-user-dir" "DOCUMENTS") :output '(:string :stripped t))))
  (pushnew (uiop:subpathname* docs-dir "projects/common-lisp/")
           ql:*local-project-directories*))
