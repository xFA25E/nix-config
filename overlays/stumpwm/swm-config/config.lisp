(defpackage #:swm-config
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm))
  (:import-from #:uiop #:launch-program #:run-program)
  (:import-from #:sdl-fonts #:load-font)
  (:export #:init))
(in-package #:swm-config)

(defun get-xresource (name)
  (run-program (list "xrdb" "-get" name) :output '(:string :stripped t) :ignore-error-status t))

(defun init ()
  (setf swm:*mouse-focus-policy* :click)
  ;; (setf swm:*input-completion-style* (make-input-completion-style-unambiguous))
  ;; (setf swm::*input-refine-candidates-fn* #'swm:input-refine-fuzzy)
  (swm:set-prefix-key (swm:kbd "C-z"))
  ;; DO NOT HARDCODE THIS
  (swm:set-font (load-font "/run/current-system/sw/share/X11/fonts/IosevkaAile-Light.ttc" 11))
  (swm:clear-window-placement-rules)
  (launch-program '("systemctl" "--user" "start" "random-background.service"))
  (swm-config.reverse-im:init)
  (swm-config.timers:init)

  (setf swm:*window-format* "%m%50t")
  (setf swm:*mode-line-highlight-template* "^[^09~A^]")
  (setf swm:*screen-mode-line-format* "[%n] %W^> %u %d")
  (setf swm:*mode-line-position* :bottom)
  (setf swm:*mode-line-border-width* 0)
  (setf swm:*mode-line-pad-x* 0)
  (setf swm:*mode-line-pad-y* 0)

  (setf swm:*mode-line-background-color* "#dfd9cf") ; bg-mode-line-inactive
  (setf swm:*mode-line-foreground-color* "#000000") ; fg-mode-line-active
  (setf swm:*mode-line-border-color* "#545454")     ; border-mode-line-active

  (swm:set-bg-color "#fbf7f0")          ; bg-main
  (swm:set-fg-color "#000000")          ; fg-main
  (swm:set-border-color "#9f9690")      ; border
  (swm:set-focus-color "#0031a9")       ; blue
  (swm:set-unfocus-color "#dfd5cf")     ; bg-inactive

  (setf swm:*colors* '("#000000"        ; 0 black
                       "#a60000"        ; 1 red
                       "#006800"        ; 2 green
                       "#6f5500"        ; 3 yellow
                       "#0031a9"        ; 4 blue
                       "#721045"        ; 5 magenta
                       "#005e8b"        ; 6 cyan
                       "#ffffff"        ; 7 white
                       "#bfefff"        ; 8 bg-cyan-subtle
                       "#e0f2fa"        ; 9 bg-cyan-nuanced
                       ))

  (mapc #'stumpwm:update-color-map stumpwm:*screen-list*))
