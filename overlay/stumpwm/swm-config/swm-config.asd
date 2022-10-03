(defsystem "swm-config"
  :depends-on ("stumpwm"
               "alexandria"
               "chronicity"
               "cl-ppcre"
               "jonathan"
               "local-time"
               "local-time-duration"
               "trivia"

               "flexi-streams")
  :components ((:file "timers")
               (:file "pass")
               (:file "reverse-im")
               (:file "status")
               (:file "commands" :depends-on ("status"))
               (:file "bindings" :depends-on ("commands" "timers" "pass"))
               (:file "config" :depends-on ("reverse-im"))))
