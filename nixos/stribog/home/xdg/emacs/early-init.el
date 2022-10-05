;; -*- lexical-binding: t; -*-

(let ((old-threshold gc-cons-threshold))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (emacs-init-time) gcs-done)
              (setq gc-cons-threshold old-threshold)))
  (setq gc-cons-threshold most-positive-fixnum))
