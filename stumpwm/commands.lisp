(in-package :stumpwm)

(defvar *menu*
  '(("screenshot" :command "screenshot")
    ("mount" :shell ("rmount"))
    ("unmount" :shell ("rumount"))
    ("suspend" :shell ("systemctl" "suspend"))))

(defcommand show-menu () ()
  (when-let ((selection (select-from-menu (current-screen) *menu* nil)))
    (destructuring-bind (name type cmd) selection
      (declare (ignore name))
      (ecase type
        (:shell (uiop:launch-program cmd))
        (:command (eval-command cmd t))))))

(defcommand screenshot (name selectp)
    ((:string "File name (w/o ext): ") (:y-or-n "Select? "))
  (let* ((pic-dir (uiop:run-program '("xdg-user-dir" "PICTURES") :output '(:string :stripped t)))
         (directory (uiop:subpathname* pic-dir "screenshots/"))
         (file-name (make-pathname :directory (pathname-directory directory) :name name :type "png")))
    (ensure-directories-exist file-name)
    (uiop:launch-program (list* "scrot" "--overwrite" "--delay" "2"
                                "--exec" "notify-send Scrot 'Done Screenshot'; image_clipboard $f"
                                (namestring file-name)
                                (when selectp '("--select"))))))

(defcommand show-corona () ()
  (if-let ((jsown-package (find-package "JSOWN"))
           (dexador-package (find-package "DEXADOR")))
    (let* ((parse (symbol-function (find-symbol "PARSE" jsown-package)))
           (val (symbol-function (find-symbol "VAL" jsown-package)))
           (get (symbol-function (find-symbol "GET" dexador-package)))
           (link "https://corona-stats.online/Italy?format=json")
           (data (first (funcall val (funcall parse (funcall get link)) "data"))))
      (message "COVID-19 Italy~%Confirmed: ~:D~%Recovered: ~:D~%Deaths: ~:D~%Active: ~:D"
               (funcall val data "cases")
               (funcall val data "recovered")
               (funcall val data "deaths")
               (funcall val data "active")))
    (progn (message "No jsown or dexador!") nil)))

(defcommand show-hardware () ()
  (multiple-value-bind (percentage state time) (read-battery-status)
    (message "~{~A~%~}B~A ~A ~A"
             (delete-if-not
              (alexandria:curry #'ppcre:scan "(/|/run/media/val/backup)$")
              (rest
               (uiop:split-string
                (uiop:run-program '("df" "--si" "--output=avail,target") :output '(:string :stripped t))
                :separator '(#\Newline))))
             percentage state time)))

(defcommand type-clipboard (&optional (primary t))
    ((:y-or-n "Primary or clipboard? "))
  (let ((*default-selections* (if primary '(:primary) '(:clipboard))))
    (window-send-string (get-x-selection))))

(defcommand mpd-controller (&optional args) ((:rest))
  (when args
    (uiop:run-program (list* "mpc" "-q" (uiop:split-string args))))
  (message "~A~%~
            [<] prev    [N] volume -1  [n] volume -10  [f] seek +00:00:10~%~
            [>] next    [P] volume +1  [p] volume +10  [b] seek -00:00:10~%~
            [t] toggle  [r] repeat     [z] random~%~
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

(defcommand brightness-controller (&optional args) ((:rest))
  (when args
    (uiop:run-program (list* "brightnessctl" (uiop:split-string args))))
  (message "Brightness ~A~%~
            [N]   set 1%-   [P]   set +1%~%~
            [n]   set 10%-  [p]   set +10%~%~
            [M-n] set 40%-  [M-p] set +40%"
           (read-brightness-status)))

(let ((timer nil))
  (define-interactive-keymap brightness-controller-interactive
      (:on-enter (lambda () (setf timer (run-with-timer 0.1 1 #'brightness-controller)))
       :on-exit (lambda () (cancel-timer timer)))
    ((kbd "N") "brightness-controller set 1%-")
    ((kbd "n") "brightness-controller set 10%-")
    ((kbd "M-n") "brightness-controller set 40%-")
    ((kbd "P") "brightness-controller set +1%")
    ((kbd "p") "brightness-controller set +10%")
    ((kbd "M-p") "brightness-controller set +40%")))

(defcommand alsa-controller (&optional args) ((:rest))
  (when args
    (uiop:run-program (list* "amixer" (uiop:split-string args))))
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
