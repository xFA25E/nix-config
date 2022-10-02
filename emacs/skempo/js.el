;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'skempo))

(declare-function skempo-define "skempo")
(declare-function skempo--define-tempo "skempo")

(skempo-define-tempo switch (:mode js-mode :tag t :abbrev t)
  "switch (" p ") {" n>
  (:while ("Pattern: " pat)
          "case " (s pat) ":" > n>
          p n>
          "break;" n>)
  "default:" > n>
  p n>
  "break;" n>
  "}" >)

(skempo-define-tempo function (:mode js-mode :tag t :abbrev t)
  "function " p "(" p ") {" n>
  r> n>
  "}" >)

(skempo-define-tempo if (:mode js-mode :tag t :abbrev t)
  "if (" p ") {" n>
  r> n>
  "}" >)

(skempo-define-tempo for (:mode js-mode :tag t :abbrev t)
  "for (" p ") {" n>
  r> n>
  "}" >)

(skempo-define-tempo try (:mode js-mode :tag t :abbrev t)
  "try {" n>
  r> n>
  "} catch (" p "error) {" > n>
  p n>
  "}" >)

(skempo-define-tempo clog (:mode js-mode :tag t :abbrev t)
  "console.log(" r ")")

(skempo-define-tempo ctime (:mode js-mode :tag t :abbrev t)
  "console.time(\"" (P "Time name: " time) "\");" > n>
  r> n>
  "console.timeEnd(\"" (s time) "\");" >)
