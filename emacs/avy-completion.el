;;; -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'avy)
(require 'subr-x)

(defun avy-completion ()
  "Jump to a completion candidate."
  (interactive)
  (if-let* ((wnd (get-buffer-window "*Completions*" 0))
            (wnd-start (window-start wnd))
            (wnd-end (window-end wnd t))
            (candidates
             (with-current-buffer "*Completions*"
               (save-excursion
                 (goto-char wnd-start)
                 (previous-completion 1)
                 (cl-loop do (next-completion 1)
                          while (< (point) wnd-end)
                          collect (cons (point) wnd))))))
      (avy-with avy-completion
        (let ((avy-action (lambda (pt) (goto-char pt) (choose-completion))))
          (avy-process candidates)))
    (user-error "No *Completions* windows")))

(provide 'avy-completion)
