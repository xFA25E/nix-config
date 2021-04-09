#-clisp (require "asdf")

#-quicklisp
(let ((quicklisp-init #-clisp (uiop:subpathname*
                               (or (uiop:getenv-pathname "QUICKLISP")
                                   (uiop:xdg-cache-home "quicklisp"))
                               "setup.lisp")
                      #+clisp (concatenate
                               'string
                               (or (ext:getenv "QUICKLISP")
                                   (concatenate
                                    'string
                                    (or (ext:getenv "XDG_CACHE_HOME")
                                        (namestring (merge-pathnames ".cache" (user-homedir-pathname))))
                                    "/quicklisp"))
                               "/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#+quicklisp
(let ((docs-dir (uiop:run-program '("xdg-user-dir" "DOCUMENTS") :output '(:string :stripped t))))
  (pushnew (uiop:subpathname* docs-dir "projects/common-lisp/")
           ql:*local-project-directories*))
