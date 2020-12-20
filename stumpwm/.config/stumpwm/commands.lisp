(in-package :stumpwm)

(define-run-or-raise "telegram-desktop" "TelegramDesktop")
(define-run-or-raise "qutebrowser" "qutebrowser")
(define-run-or-raise "firefox" "firefox")
(define-run-or-raise  "brave" "Brave-browser")
(define-run-or-raise "em" "Emacs")



(defcommand slynk-start () ()
  (slynk:create-server :dont-close t :port *slynk-port*))

(defcommand slynk-stop () ()
  (slynk:stop-server *slynk-port*))



(defcommand set-brightness (brightness) ((:brightness "Brightness: "))
  (run-shell-command (format nil "xbacklight -set ~D" brightness)))

(defcommand set-alsa-volume (alsa-volume) ((:alsa-volume "Alsa volume: "))
  (run-shell-command (format nil "amixer -D pulse sset Master '~D%'" alsa-volume)))

(defcommand set-mpd-volume (mpd-volume) ((:mpd-volume "Mpd volume: "))
  (run-shell-command (format nil "mpc -q volume '~D'" mpd-volume)))



(defcommand show-mpd () ()
  (message (string-right-trim (string #\Newline) (run-shell-command "mpc" t))))

(defcommand show-corona () ()
  (let* ((link "https://corona-stats.online/Italy?format=json")
         (data (first (jsown:val (jsown:parse (dex:get link)) "data"))))
    (message "COVID-19 Italy~%Confirmed: ~:D~%Recovered: ~:D~%Deaths: ~:D~%Active: ~:D"
             (jsown:val data "cases")
             (jsown:val data "recovered")
             (jsown:val data "deaths")
             (jsown:val data "active"))))

(flet ((make-scanner (partition)
         (ppcre:create-scanner
          (concatenate 'string "([0-9]+(?:\\.[0-9]+)?[a-zA-Z]+) +[0-9]+% +" partition "\\n"))))
  (let ((root-scanner (make-scanner "/"))
        (home-scanner (make-scanner "/home"))
        (second-partition-scanner (make-scanner "/mnt/second_partition")))
    (defcommand show-hardware () ()
      (update-battery-status-variables)
      (let ((output (run-shell-command "df --si" t)))
        (message "R ~A~%H ~A~%W ~A~%B~A ~A ~A"
                 (extract-first-regexp-group root-scanner output)
                 (extract-first-regexp-group home-scanner output)
                 (extract-first-regexp-group second-partition-scanner output)
                 *battery-percentage* *battery-state* *battery-time*)))))



(defcommand show-menu () ()
  (let* ((menu '(("brightness" :command "set-brightness")
                 ("lock screen" :shell "lock")
                 ("suspend" :shell "systemctl suspend")
                 ("mount" :shell "rmount")
                 ("unmount" :shell "rumount")
                 ("screenshot" :shell "screenshot")
                 ("alsa volume" :command "set-alsa-volume")
                 ("mpd volume" :command "set-mpd-volume")))
         (selection (select-from-menu (current-screen) menu nil)))
    (destructuring-bind (name type cmd) selection
      (declare (ignore name))
      (ecase type
        (:shell (run-shell-command cmd))
        (:command (eval-command cmd t))))))
