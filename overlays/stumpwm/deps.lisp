(require 'uiop)

(defun deps-urls (deps)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (pushnew (ql-dist:archive-url (ql-dist:release el)) acc :test #'string=)))
             acc))
    (rflatten (mapcar #'ql-dist:dependency-tree deps) nil)))

(defun url-hash (url)
  (uiop:run-program `("nix-prefetch-url" "--unpack" ,url) :output '(:string :stripped t)))

(defun read-deps (filepath)
  (let ((output (uiop:read-file-string filepath)))
    (uiop:split-string (uiop:stripln output) :separator '(#\Newline))))

(defun main ()
  (let ((urls (mapcar (lambda (u) (list u (url-hash u))) (deps-urls (read-deps #p"deps.txt")))))
    (with-open-file (file #p"deps.nix" :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
      (format file "[~%~:{  {~%    url = \"~A\";~%    sha256 = \"~A\";~%  }~%~}]~%" urls))))
