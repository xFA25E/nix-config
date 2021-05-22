;;; project-fixes.el --- Fixes for project from elpa  -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(defun project--process-file-region (start end program &optional buffer display &rest args)
  (if (not (file-remote-p default-directory))
      (apply #'call-process-region
             start end program nil buffer display args)
    (let ((infile (make-temp-file "ppfr")))
      (unwind-protect
          (progn
            (write-region start end infile nil 'silent)
            (apply #'process-file program infile buffer display args))
        (delete-file infile)))))

(provide 'project-fixes)
