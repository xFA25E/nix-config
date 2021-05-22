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

(defun org-mime-edit-mail-in-org-mode-up-to-signature ()
  "Dont add signature to org mime edit buffer."
  (interactive)
  ;; see `org-src--edit-element'
  (cond
   ((eq major-mode 'org-mode)
    (message "This command is not for `org-mode'."))
   (t
    (setq org-mime--saved-temp-window-config (current-window-configuration))
    (let* ((beg (copy-marker (org-mime-mail-body-begin)))
           (end (copy-marker (or (org-mime-mail-signature-begin)
                                 (point-max))))
           (bufname "OrgMimeMailBody")
           (buffer (generate-new-buffer bufname))
           (overlay (org-mime-src--make-source-overlay beg end))
           (text (buffer-substring-no-properties beg end)))

      (setq org-mime-src--beg-marker beg)
      (setq org-mime-src--end-marker end)
      ;; don't use local-variable because only user can't edit multiple emails
      ;; or multiple embedded org code in one mail
      (setq org-mime-src--overlay overlay)

      (save-excursion
        (delete-other-windows)
        (org-switch-to-buffer-other-window buffer)
        (erase-buffer)
        (insert org-mime-src--hint)
        (insert text)
        (goto-char (point-min))
        (org-mode)
        (org-mime-src-mode))))))

(advice-add 'org-mime-beautify-quoted :filter-return #'org-mime-beautify-quoted-add-newlines)
(advice-add 'org-mime-replace-images :filter-args #'org-mime-replace-images-fix-cids-and-path)
(advice-add 'org-mime-edit-mail-in-org-mode :override #'org-mime-edit-mail-in-org-mode-up-to-signature)

(provide 'org-mime-fixes)
