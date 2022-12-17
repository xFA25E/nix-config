;; -*- lexical-binding: t; -*-

(require 'tempo-ext)

(tempo-ext-define
 "switch" 'js-mode
 '("switch (" p ") {" n>
   (:while ("Pattern: " pat)
           "case " (s pat) ":" > n>
           p n>
           "break;" n>)
   "default:" > n>
   p n>
   "break;" n>
   "}" >))

(tempo-ext-define
 "function" 'js-mode
 '("function " p "(" p ") {" n>
   r> n>
   "}" >))

(tempo-ext-define
 "if" 'js-mode
 '("if (" p ") {" n>
   r> n>
   "}" >))

(tempo-ext-define
 "for" 'js-mode
 '("for (" p ") {" n>
   r> n>
   "}" >))

(tempo-ext-define
 "try" 'js-mode
 '("try {" n>
   r> n>
   "} catch (" p "error) {" > n>
   p n>
   "}" >))

(tempo-ext-define
 "clog" 'js-mode
 '("console.log(" r ")"))

(tempo-ext-define
 "ctime" 'js-mode
 '("console.time(\"" (P "Time name: " time) "\");" > n>
   r> n>
   "console.timeEnd(\"" (s time) "\");" >))

(provide 'tempo-ext-js)
;;; tempo-ext-js.el ends here
