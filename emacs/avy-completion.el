;;; -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'avy)
(require 'subr-x)

(defun avy-action-choose (pt)
  "Choose completion at `PT'."
  (goto-char pt)
  (choose-completion))

(defun avy-completion ()
  "Jump to a completion candidate."
  (interactive)
  (if-let* ((wnd (get-buffer-window "*Completions*" 0))
            (wnd-start (window-start wnd))
            (wnd-end (window-end wnd t)))
      (with-current-buffer "*Completions*"
        (avy-with avy-completion
          (let ((avy-action #'avy-action-choose))
            (avy-process
             (save-excursion
               (cl-loop initially (goto-char wnd-start)
                        do (next-completion 1) while (< (point) wnd-end)
                        collect (cons (point) wnd)))))))
    (user-error "No *Completions* windows")))

(provide 'avy-completion)
