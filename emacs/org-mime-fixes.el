;;; org-mime-fixes.el --- Fixes for org-mime package  -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'org-mime)

(defun org-mime-beautify-quoted-add-newlines (html)
  "Add <br> tags to newlines in `HTML' blockquotes if there is one."
  (let ((blockquote-count
         (save-match-data
           (with-temp-buffer
             (insert html)
             (goto-char (point-min))
             (how-many "blockquote" (point-min) (point-max))))))
    (if (/= 2 blockquote-count)
        html
      (replace-regexp-in-string
       "\n" "<br/>\n"
       (replace-regexp-in-string
        (rx (>= 3 "\n")) "\n\n"
        html)))))

(defun org-mime-replace-images-fix-cids-and-path (args)
  "Remove file:// from src in `ARGS'.
For some reason it is added twice to the src attribute."
  (cl-destructuring-bind (first . rest) args
    (cons (replace-regexp-in-string "src=\"file:///" "src=\"/" first) rest)))

(advice-add 'org-mime-beautify-quoted :filter-return #'org-mime-beautify-quoted-add-newlines)
(advice-add 'org-mime-replace-images :filter-args #'org-mime-replace-images-fix-cids-and-path)

(provide 'org-mime-fixes)
