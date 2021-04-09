#-clisp (require "asdf")

#-quicklisp
(let ((quicklisp-init (concatenate 'string
                                    #-clisp (uiop:getenv "QUICKLISP")
                                    #+clisp (ext:getenv "QUICKLISP")
                                    "/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#+quicklisp
(let ((docs-dir (uiop:run-program '("xdg-user-dir" "DOCUMENTS") :output '(:string :stripped t))))
  (pushnew (uiop:subpathname* docs-dir "projects/common-lisp/")
           ql:*local-project-directories*))
