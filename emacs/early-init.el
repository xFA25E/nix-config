;;; -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(let ((old-threshold gc-cons-threshold))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold old-threshold)))
  (setq gc-cons-threshold most-positive-fixnum))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections." (emacs-init-time) gcs-done)))
