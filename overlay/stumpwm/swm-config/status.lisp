(defpackage #:swm-config.status
  (:use #:cl)
  (:local-nicknames (#:re #:cl-ppcre))
  (:import-from #:alexandria #:make-keyword)
  (:import-from #:uiop #:read-file-string #:run-program)
  (:export #:battery #:brightness #:alsa))
(in-package #:swm-config.status)

(defun battery ()
  (let ((output (run-program '("acpi" "--battery") :output '(:string :stripped t))))
    (re:register-groups-bind ((#'string-upcase state) (#'parse-integer percentage) time)
        ("[^:]+: ([a-zA-Z]+), ([0-9]+)%(?:, (.*))?" output :sharedp t)
      (list :percentage percentage :state (make-keyword state) :time time))))

(defun brightness ()
  (flet ((read-number-from-pathspec (pathspec)
           (parse-integer (read-file-string (first (directory pathspec))) :junk-allowed t)))
    (let ((brightness (read-number-from-pathspec #p"/sys/class/backlight/*/brightness"))
          (max-brightness (read-number-from-pathspec #p"/sys/class/backlight/*/max_brightness")))
      (round (* (/ brightness max-brightness) 100)))))

(defun alsa ()
  (let ((output (run-program '("amixer" "sget" "Master") :output :string)))
    (re:register-groups-bind ((#'parse-integer volume) (#'string-upcase state))
        ("Front Left:[^[]+\\[([0-9]+)%\\][^[]+\\[(on|off)\\]" output :sharedp t)
      (list :volume volume :state (make-keyword state)))))
