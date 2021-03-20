(in-package :stumpwm)

(defcommand emacs () () (run-or-raise *emacs* '(:class "Emacs")))
(defcommand firefox () () (run-or-raise *firefox* '(:class "Firefox")))
(defcommand qutebrowser () () (run-or-raise *qutebrowser* '(:class "qutebrowser")))
(defcommand chromium-incognito () () (run-or-raise *chromium-incognito* '(:class "Chromium-browser")))
(defcommand telegram-desktop () () (run-or-raise *telegram-desktop* '(:class "TelegramDesktop")))



(defcommand mpd-controller (&optional args) ((:rest))
  (when args
    (uiop:run-program (list* *mpc* "-q" (uiop:split-string args))))
  (message "~A~%~
            [<] prev    [N] volume -1  [n] volume -10  [f] seek +00:00:10~%~
            [>] next    [P] volume +1  [p] volume +10  [b] seek -00:00:10~%~
            [t] toggle  [r] repeat     [z] random~%~
            [.] single  [c] consume    [Z] shuffle"
           (uiop:run-program `(,*mpc*) :output :string)))

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

(defcommand brightness-controller (&optional args) ((:rest))
  (when args
    (uiop:run-program (list* *xbacklight* (uiop:split-string args))))
  (message "Brightness ~A~%~
            [N]   -dec 1   [P]   -inc 1~%~
            [n]   -dec 10  [p]   -inc 10~%~
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

(defcommand alsa-controller (&optional args) ((:rest))
  (when args
    (uiop:run-program (list* *amixer* (uiop:split-string args))))
  (multiple-value-bind (value state) (read-alsa-volume-status)
    (message "Alsa-Volume ~A ~A~%~
              [N] 1%-     [P] 1%+~%~
              [n] 10%-    [p] 10%+~%~
              [t] toggle"
             value state)))

(let ((timer nil))
  (define-interactive-keymap alsa-controller-interactive
      (:on-enter (lambda () (setf timer (run-with-timer 0.1 1 #'alsa-controller)))
       :on-exit (lambda () (cancel-timer timer)))
    ((kbd "t") "alsa-controller sset Master toggle")
    ((kbd "N") "alsa-controller sset Master 1%-")
    ((kbd "n") "alsa-controller sset Master 10%-")
    ((kbd "P") "alsa-controller sset Master 1%+")
    ((kbd "p") "alsa-controller sset Master 10%+")))

(defcommand screenshot (name selectp)
    ((:string "File name (w/o ext): ") (:y-or-n "Select? "))
  (let* ((name "hellotherehellothere")
         (selectp t)
         (*xdg-user-dir* "xdg-user-dir")
         (*image-clipboard* "image_clipboard")
         (*scrot* "scrot")
         (pic-dir (uiop:run-program `(,*xdg-user-dir* "PICTURES") :output '(:string :stripped t)))
         (directory (uiop:subpathname* pic-dir "screenshots/"))
         (file-name (make-pathname :directory (pathname-directory directory) :name name :type "png")))
    (ensure-directories-exist file-name)
    (uiop:launch-program (list* *scrot* "--overwrite" "--delay" "2"
                                "--exec" (format nil "~A Scrot 'Done Screenshot'; ~A $f"
                                                 *notify-send* *image-clipboard*)
                                (namestring file-name)
                                (when selectp '("--select"))))))



(defcommand show-corona () ()
  (let* ((link "https://corona-stats.online/Italy?format=json")
         (data (first (jsown:val (jsown:parse (dex:get link)) "data"))))
    (message "COVID-19 Italy~%Confirmed: ~:D~%Recovered: ~:D~%Deaths: ~:D~%Active: ~:D"
             (jsown:val data "cases")
             (jsown:val data "recovered")
             (jsown:val data "deaths")
             (jsown:val data "active"))))

(defcommand show-hardware () ()
  (update-battery-status-variables)
  (let ((output (uiop:run-program `(,*df* "--si" "--output=avail,target") :output :string))
        (max-length 0)
        (partitions nil))

    (ppcre:do-register-groups (avail target)
        (" *([^ ]+) +(/|/home|/mnt/second_partition|/run/media/val/backup)\\n"
         output nil :sharedp t)
      (push (cons avail target) partitions)
      (setf max-length (max max-length (length avail))))

    (message "~{~A~%~}~%B~A ~A ~A"
             (mapcar (lambda (partition)
                       (destructuring-bind (avail . target) partition
                         (format nil "~A~A ~A"
                                 (make-string (- max-length (length avail)) :initial-element #\Space)
                                 avail
                                 target)))
                     partitions)
             *battery-percentage* *battery-state* *battery-time*)))



(defcommand show-menu () ()
  (when-let ((selection (select-from-menu (current-screen) *menu* nil)))
    (destructuring-bind (name type cmd) selection
      (declare (ignore name))
      (ecase type
        (:shell (uiop:launch-program cmd))
        (:command (eval-command cmd t))))))

(defcommand type-clipboard (&optional (primary t))
    ((:y-or-n "Primary or clipboard? "))
  (let ((*default-selections* (if primary '(:primary) '(:clipboard))))
    (window-send-string (get-x-selection))))
