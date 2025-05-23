(defpackage #:swm-config
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm))
  (:import-from #:uiop #:launch-program #:run-program)
  (:export #:init))
(in-package #:swm-config)

(defun get-xresource (name)
  (run-program (list "xrdb" "-get" name) :output '(:string :stripped t)))

(defun init ()
  (setf swm:*mouse-focus-policy* :click)
  ;; (setf swm:*input-completion-style* (make-input-completion-style-unambiguous))
  ;; (setf swm::*input-refine-candidates-fn* #'swm:input-refine-fuzzy)
  (swm:set-prefix-key (swm:kbd "C-z"))
  (swm:set-font "-*-terminus-medium-r-*-*-16-*-*-*-*-*-iso10646-1")
  (swm:clear-window-placement-rules)
  (launch-program '("systemctl" "--user" "start" "random-background.service"))
  (swm-config.reverse-im:init)
  (swm-config.timers:init)

  (setf swm:*screen-mode-line-format* "[%n] %W^> %u %d")
  (setf swm:*mode-line-position* :bottom)
  (setf swm:*mode-line-border-width* 0)
  (setf swm:*mode-line-pad-x* 0)
  (setf swm:*mode-line-pad-y* 0)
  (setf swm:*mode-line-background-color* (get-xresource "xterm*color0"))
  (setf swm:*mode-line-foreground-color* (get-xresource "xterm*color7"))
  (setf swm:*mode-line-border-color* (get-xresource "xterm*color0")))
