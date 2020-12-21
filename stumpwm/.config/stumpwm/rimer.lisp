(in-package :stumpwm)

(defvar *timer-updater* "panel_timer")

;; add

;; pause

;; resume

(defcommand report-rimer () ()
  (message "~A" (run-shell-command "rimer report" t)))

;; halt

(defcommand quit-rimer () ()
  (run-shell-command "rimer quit"))

(defcommand start-rimer () ()
  (run-shell-command
   (format nil "pidof rimer || rimer start '~A'" *timer-updater*)))

(defcommand menu-rimer () ()
  (let ((menu '(("add" . :add) ("pause" . :pause) ("resume" . :resume)
                ("report" . :report) ("halt" . :halt)
                ("quit" . :quit) ("start" . :start))))
    (case (cdr (select-from-menu (current-screen) menu "Rimer:"))
      (:add (message "Unimplemented!"))
      (:pause (message "Unimplemented!"))
      (:resume (message "Unimplemented!"))
      (:report (eval-command "report-rimer" t))
      (:halt (message "Unimplemented!"))
      (:quit (eval-command "quit-rimer" t))
      (:start (eval-command "start-rimer" t)))))
