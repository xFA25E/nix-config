(defpackage #:swm-config.reverse-im
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm))
  (:import-from #:uiop #:run-program)
  (:export #:init))
(in-package #:swm-config.reverse-im)

(defvar *default-layout* "us(dvorak)")
(defvar *current-layout* nil)
(defvar *nested-counter* 0)

(defun current-layout ()
  (run-program '("xkb-switch" "-p") :output '(:string :stripped t) :ignore-error-status t))

(defun set-layout (layout)
  (run-program `("xkb-switch" "-s" ,layout) :ignore-error-status t))

(defun set-default-saving-current-layout ()
  (let ((current-layout (current-layout)))
    (when (string/= *default-layout* current-layout)
      (set-layout *default-layout*)
      (setf *current-layout* current-layout))))

(defun restore-current-layout ()
  (when *current-layout*
    (set-layout *current-layout*)
    (setf *current-layout* nil)))

(defun maybe-swap-layout-on-key-press (key key-seq cmd)
  (declare (ignore key))
  (setf *nested-counter* 0)
  (cond ((and (symbolp cmd) (null (cdr key-seq)))
         (set-default-saving-current-layout))
        ((null cmd)
         (restore-current-layout))))

(defun maybe-swap-layout-before-command (cmd)
  (declare (ignore cmd))
  (incf *nested-counter*)
  (unless *current-layout*
    (set-default-saving-current-layout)))

(defun maybe-restore-layout-after-command (cmd)
  (declare (ignore cmd))
  (decf *nested-counter*)
  (when (zerop *nested-counter*)
    (restore-current-layout)))

(defun init ()
  (swm:add-hook swm:*key-press-hook* 'maybe-swap-layout-on-key-press)
  (swm:add-hook swm:*pre-command-hook* 'maybe-swap-layout-before-command)
  (swm:add-hook swm:*post-command-hook* 'maybe-restore-layout-after-command))
