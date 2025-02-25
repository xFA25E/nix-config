(defpackage #:swm-config
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm))
  (:import-from #:uiop #:launch-program)
  (:export #:init))
(in-package #:swm-config)

(defun init ()
  (setf swm:*mouse-focus-policy* :click)
  ;; (setf swm:*input-completion-style* (make-input-completion-style-unambiguous))
  ;; (setf swm::*input-refine-candidates-fn* #'swm:input-refine-fuzzy)
  (swm:set-prefix-key (swm:kbd "C-z"))
  (swm:set-font "-*-terminus-medium-r-*-*-18-*-*-*-*-*-iso10646-1")
  (swm:clear-window-placement-rules)
  (launch-program '("systemctl" "--user" "start" "random-background.service"))
  (swm-config.reverse-im:init)
  (swm-config.timers:init))
