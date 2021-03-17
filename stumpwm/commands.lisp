(in-package :stumpwm)

(defcommand slynk-start () ()
  (slynk:create-server :dont-close t :port *slynk-port*))

(defcommand slynk-stop () ()
  (slynk:stop-server *slynk-port*))



(defcommand mpd-controller (&optional command) ((:rest-strings))
  (when command
    (uiop:run-program (format nil "mpc -q ~A" command)))
  (message "~A
[<] prev    [N] volume -1  [n] volume -10  [f] seek +00:00:10
[>] next    [P] volume +1  [p] volume +10  [b] seek -00:00:10
[t] toggle  [r] repeat     [z] random
[.] single  [c] consume    [Z] shuffle"
           (uiop:run-program '("mpc") :output :string)))

(let ((timer nil))
  (define-interactive-keymap mpd-controller-interactive
      (:on-enter (lambda () (setf timer (run-with-timer 0.1 1 #'mpd-controller)))
       :on-exit (lambda () (cancel-timer timer)))
    ((kbd "<") "mpd-controller prev")
    ((kbd ">") "mpd-controller next")
    ((kbd "t") "mpd-controller toggle")
    ((kbd "r") "mpd-controller repeat")
    ((kbd "z") "mpd-controller random")
    ((kbd "Z") "mpd-controller shuffle")
    ((kbd "c") "mpd-controller consume")
    ((kbd ".") "mpd-controller single")
    ((kbd "N") "mpd-controller volume -1")
    ((kbd "n") "mpd-controller volume -10")
    ((kbd "P") "mpd-controller volume +1")
    ((kbd "p") "mpd-controller volume +10")
    ((kbd "f") "mpd-controller seek +00:00:10")
    ((kbd "b") "mpd-controller seek -00:00:10")))

(defcommand brightness-controller (&optional command) ((:rest-strings))
  (when command
    (run-shell-command (format nil "xbacklight ~A" command) t))
  (message "Brightness ~A
[N] -dec 1     [P] -inc 1
[n] -dec 10    [p] -inc 10
[M-n] -dec 40  [M-p] -inc 40"
           (read-brightness-status)))

(let ((timer nil))
  (define-interactive-keymap brightness-controller-interactive
      (:on-enter (lambda () (setf timer (run-with-timer 0.1 1 #'brightness-controller)))
       :on-exit (lambda () (cancel-timer timer)))
    ((kbd "N") "brightness-controller -dec 1")
    ((kbd "n") "brightness-controller -dec 10")
    ((kbd "M-n") "brightness-controller -dec 40")
    ((kbd "P") "brightness-controller -inc 1")
    ((kbd "p") "brightness-controller -inc 10")
    ((kbd "M-p") "brightness-controller -inc 40")))

(defcommand alsa-controller (&optional command) ((:rest-strings))
  (when command
    (run-shell-command (format nil "amixer ~A" command) t))
  (multiple-value-bind (value state) (read-alsa-volume-status)
    (message "Alsa-Volume ~A ~A
[N] 1%-     [P] 1%+
[n] 10%-    [p] 10%+
[t] toggle" value state)))

(let ((timer nil))
  (define-interactive-keymap alsa-controller-interactive
      (:on-enter (lambda () (setf timer (run-with-timer 0.1 1 #'alsa-controller)))
       :on-exit (lambda () (cancel-timer timer)))
    ((kbd "t") "alsa-controller -D pulse sset Master toggle")
    ((kbd "N") "alsa-controller -D pulse sset Master 1%-")
    ((kbd "n") "alsa-controller -D pulse sset Master 10%-")
    ((kbd "P") "alsa-controller -D pulse sset Master 1%+")
    ((kbd "p") "alsa-controller -D pulse sset Master 10%+")))

(defcommand screenshot (name selectp)
    ((:string "File name (w/o ext): ") (:y-or-n "Select? "))
  (let* ((directory
           (merge-pathnames #p"Pictures/screenshots/" (user-homedir-pathname)))
         (file-name (make-pathname :directory (pathname-directory directory)
                                   :name name :type "png")))
    (ensure-directories-exist file-name)
    (run-shell-command
     (format
      nil
      "scrot --overwrite --delay 1~:[~; -select~] '~A' --exec 'image_clipboard $f' && notify-send Scrot 'Done screenshot'"
      selectp (namestring file-name)))))



(defcommand show-corona () ()
  (let* ((link "https://corona-stats.online/Italy?format=json")
         (data (first (jsown:val (jsown:parse (dex:get link)) "data"))))
    (message "COVID-19 Italy~%Confirmed: ~:D~%Recovered: ~:D~%Deaths: ~:D~%Active: ~:D"
             (jsown:val data "cases")
             (jsown:val data "recovered")
             (jsown:val data "deaths")
             (jsown:val data "active"))))

(let* ((scanner (ppcre:create-scanner '(:sequence
                                        (:alternation
                                         "/" "/home" "/mnt/second_partition"
                                         "/mnt/backup" "/media/kindle"
                                         "/media/usb")
                                        :modeless-end-anchor)
                                      :case-insensitive-mode t))
       (needed-partition-p (curry #'ppcre:scan scanner)))
  (defcommand show-hardware () ()
    (update-battery-status-variables)
    (let ((output (run-shell-command "df --si --output=avail,target" t)))
      (destructuring-bind (head . body) (split-string output)
        (message "~A~%~{~A~%~}~%B~A ~A ~A"
                 head (delete-if-not needed-partition-p body)
                 *battery-percentage* *battery-state* *battery-time*)))))



(defcommand show-menu () ()
  (when-let ((selection (select-from-menu (current-screen) *menu* nil)))
    (destructuring-bind (name type cmd) selection
      (declare (ignore name))
      (ecase type
        (:shell (run-shell-command cmd))
        (:command (eval-command cmd t))))))

(defcommand type-clipboard (&optional (primary t))
    ((:y-or-n "Primary or clipboard? "))
  (let ((*default-selections* (if primary '(:primary) '(:clipboard))))
    (window-send-string (get-x-selection))))
