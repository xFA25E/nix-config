(in-package #:stumpwm)

(defmethod typing-action :around ((menu single-menu) key-seq)
  "Fix the defualt method selecting wrong item-object.  It used SECOND instead
of CDR."
  (let ((input-char (and key-seq (get-input-char key-seq))))
    (when input-char
      (vector-push-extend input-char (single-menu-current-input menu)))
    (handler-case
        (when (or input-char (not key-seq))
          (labels ((match-p (table-item)
                     (funcall (single-menu-filter-pred menu)
                              (car table-item)
                              (cdr table-item)
                              (single-menu-current-input menu))))
            (setf (menu-table menu) (remove-if-not #'match-p (single-menu-unfiltered-table menu))
                  (menu-selected menu) 0)
            (bound-check-menu menu)))
      (cl-ppcre:ppcre-syntax-error ()))))
