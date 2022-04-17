(defpackage #:swm-config.commands
  (:use #:cl)
  (:local-nicknames (#:re #:cl-ppcre) (#:status #:swm-config.status) (#:swm #:stumpwm))
  (:import-from #:alexandria #:format-symbol #:with-gensyms)
  (:import-from #:trivia #:match #:lambda-match #:let-match #:plist)
  (:import-from #:uiop #:launch-program #:run-program #:subpathname*)
  (:export #:brave-incognito
           #:mpv
           #:firefox
           #:screenshot
           #:covid-19-italy
           #:hardware
           #:clipboard-type
           #:mpd
           #:mpd-interactive
           #:exit-mpd-interactive
           #:brightness
           #:brightness-interactive
           #:exit-brightness-interactive
           #:alsa
           #:alsa-interactive
           #:exit-alsa-interactive
           #:main-menu
           #:*main-menu*))
(in-package #:swm-config.commands)

;;; RUN OR RAISE COMMANDS

(swm:defcommand brave-incognito () ()
  (swm:run-or-raise "brave-incognito" '(:class "Brave-browser")))

(swm:defcommand mpv () ()
  (swm:run-or-raise "true" '(:class "mpv")))

(swm:defcommand firefox () ()
  (swm:run-or-raise "firefox" '(:class "firefox")))

;;; MISC

(swm:defcommand screenshot (name selectp) ((:string "Name (w/o ext): ") (:y-or-n "Select? "))
  (let* ((pictures-directory (run-program '("xdg-user-dir" "PICTURES") :output '(:string :stripped t)))
         (screenshots-directory (subpathname* pictures-directory "screenshots/"))
         (screenshot-pathname (make-pathname :name name :type "png" :defaults screenshots-directory))
         (arguments (list "--overwrite" "--delay" "2" "--exec"
                          "notify-send Scrot 'Done Screenshot'; image_clipboard $f"
                          (namestring screenshot-pathname))))
    (when selectp (push "--select" arguments))
    (ensure-directories-exist screenshot-pathname)
    (launch-program (cons "scrot" arguments))))

(swm:defcommand covid-19-italy () ()
  (let* ((json (run-program '("curl" "https://corona-stats.online/Italy?format=json") :output '(:string :stripped t)))
         (data (first (gethash "data" (jojo:parse json :as :hash-table)))))
    (swm:message "COVID-19 Italy~%~@?~%~@?~%~@?~%~@?~%"
                 "Confirmed: ~:D" (gethash "cases" data)
                 "Recovered: ~:D" (gethash "recovered" data)
                 "   Deaths: ~:D" (gethash "deaths" data)
                 "   Active: ~:D" (gethash "active" data))))

(swm:defcommand hardware () ()
  (let-match ((mounts (re:all-matches-as-strings
                       ".*?(?:/|/run/media/val/backup)(?=\\n)"
                       (run-program '("df" "--si" "--output=avail,target") :output :string)
                       :sharedp t))
              ((plist :percentage percentage :state state :time time) (status:battery)))
    (swm:message "Free space:~%~{~A~%~}~%Battery: ~A% ~A ~A" mounts percentage state time)))

(swm:defcommand clipboard-type (&optional (primary t)) ((:y-or-n "Primary or clipboard? "))
  (let ((swm:*default-selections* (if primary '(:primary) '(:clipboard))))
    (swm:window-send-string (swm:get-x-selection))))

;;; INTERACTIVE CONTROLLERS

(defmacro define-interactive-controller (name command format-string format-arguments keys-and-args)
  (let ((name-interactive (format-symbol t "~A-INTERACTIVE" name)))
    (with-gensyms (arguments timer)
      `(progn
         (swm:defcommand ,name (&optional ,arguments) ((:rest))
           (when ,arguments
             (run-program (list* ,@command (re:split " " ,arguments))))
           (swm:message "~?~%~:{[~A]~6T~A~%~}" ,format-string ,format-arguments ',keys-and-args))
         (let ((,timer nil))
           (swm:define-interactive-keymap ,name-interactive
               (:on-enter (lambda () (setf ,timer (swm:run-with-timer 0.1 1 #',name)))
                :on-exit (lambda () (swm:cancel-timer ,timer)))
             ,@(mapcar (lambda-match
                         ((list key args)
                          `((swm:kbd ,key) ,(format nil "~(~A~) ~A" name args))))
                       keys-and-args)))))))

(define-interactive-controller mpd ("mpc" "-q")
  "~A" (list (run-program '("mpc") :output :string))
  (("<" "prev")
   (">" "next")
   ("t" "toggle")
   ("n" "volume -10")
   ("p" "volume +10")
   ("f" "seek +00:00:10")
   ("b" "seek -00:00:10")
   ("r" "repeat")
   ("z" "random")
   ("Z" "shuffle")
   ("c" "consume")
   ("." "single")
   ("N" "volume -1")
   ("P" "volume +1")))

(define-interactive-controller brightness ("brightnessctl" "set")
  "Brightness ~D" (list (status:brightness))
  (("N"   "1%-")
   ("n"   "10%-")
   ("M-n" "40%-")
   ("P"   "+1%")
   ("p"   "+10%")
   ("M-p" "+40%")))

(define-interactive-controller alsa ("amixer" "sset" "Master")
  "Alsa ~A ~A" (let-match (((plist :volume volume :state state) (status:alsa))) (list volume state))
  (("t" "toggle")
   ("N" "1%-")
   ("n" "10%-")
   ("P" "1%+")
   ("p" "10%+")))

;;; MAIN MENU

(defvar *main-menu*
  '(("screenshot"     (:command "screenshot"))
    ("suspend"        (:shell ("systemctl" "suspend")))
    ("mount"          (:shell ("rmount")))
    ("unmount"        (:shell ("rumount")))
    ("clipboard-type" (:command "clipboard-type"))))

(swm:defcommand main-menu () ()
  (match (swm:select-from-menu (swm:current-screen) *main-menu* "Main menu: ")
    ((list _ (list :command command)) (swm::eval-command command t))
    ((list _ (list :shell shell)) (launch-program shell))))
