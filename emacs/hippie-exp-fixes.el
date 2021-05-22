;;; hippie-exp-fixes.el --- Fixes for hippie-exp package  -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'hippie-exp)

(unless (string-match-p (rx "{") he-file-name-chars)
  (setf he-file-name-chars (concat he-file-name-chars "{")))

(unless (string-match-p (rx "}") he-file-name-chars)
  (setf he-file-name-chars (concat he-file-name-chars "}")))

(defun try-complete-file-name-with-env (old)
  "Just like `TRY-COMPLETE-FILE-NAME', but add support for env vars.
`OLD'."
  (unless old
    (he-init-string (he-file-name-beg) (point))
    (let ((name-part (file-name-nondirectory he-search-string))
          (dir-part (substitute-in-file-name
                     (expand-file-name (or (file-name-directory he-search-string) "")))))
      (unless (he-string-member name-part he-tried-table)
        (setq he-tried-table (cons name-part he-tried-table)))
      (if (and (not (equal he-search-string "")) (file-directory-p dir-part))
          (setq he-expand-list (sort (file-name-all-completions name-part dir-part) 'string-lessp))
        (setq he-expand-list ()))))

  (while (and he-expand-list (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if he-expand-list
      (let ((filename (he-concat-directory-file-name
                       (file-name-directory he-search-string)
                       (car he-expand-list))))
        (he-substitute-string filename)
        (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
        (setq he-expand-list (cdr he-expand-list))
        t)
    (when old (he-reset-string))
    nil))

(defun try-complete-file-name-partially-with-env (old)
  "Just like `TRY-COMPLETE-FILE-NAME-PARTIALLY', but add support for env vars.
`OLD'."
  (let ((expansion ()))
    (unless old
      (he-init-string (he-file-name-beg) (point))
      (let ((name-part (file-name-nondirectory he-search-string))
            (dir-part (substitute-in-file-name
                       (expand-file-name (or (file-name-directory he-search-string) "")))))
        (when (and (not (equal he-search-string "")) (file-directory-p dir-part))
          (setq expansion (file-name-completion name-part dir-part)))
        (when (or (eq expansion t) (string= expansion name-part) (he-string-member expansion he-tried-table))
          (setq expansion ()))))

    (if expansion
        (let ((filename (he-concat-directory-file-name (file-name-directory he-search-string) expansion)))
          (he-substitute-string filename)
          (setq he-tried-table (cons expansion (cdr he-tried-table)))
          t)
      (when old (he-reset-string))
      nil)))

(advice-add 'try-complete-file-name :override 'try-complete-file-name-with-env)
(advice-add 'try-complete-file-name-partially :override 'try-complete-file-name-partially-with-env)

(provide 'hippie-exp-fixes)
