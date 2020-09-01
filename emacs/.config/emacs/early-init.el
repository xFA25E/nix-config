;;; early-init --- summary
;;; Commentary:
;;; Code:

(defconst old-gc-cons-threshold gc-cons-threshold)

(defun restore-gc-cons-threshold ()
  "Set the `gc-cons-threshold' back to `old-gc-cons-threshold'.
Remember to save it there beforehand."
  (when (fixnump old-gc-cons-threshold)
    (setq gc-cons-threshold old-gc-cons-threshold)))

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook #'restore-gc-cons-threshold)

(defun show-startup-time ()
  "The name speaks for itself."
  (let ((startup-time (float-time (time-subtract after-init-time before-init-time))))
    (message "Emacs ready in %.2f seconds with %d garbage collections." startup-time gcs-done)))

(add-hook 'emacs-startup-hook #'show-startup-time)

;; (require 'xdg)
;; (require 'package)
;; (setq package-user-dir (expand-file-name "elpa" (xdg-data-home)))

;;; early-init ends here
