;;; skempo-js.el --- Js skempo templates -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'skempo)

(skempo-define-skeleton (switch :mode js-mode)
  "Expression: "
  "switch (" str ") {" > \n
  ("Pattern: "
   "case " str ":" > \n
   @ \n
   "break;" > \n \n)
  "default:" > \n
  @ \n
  "}" >)

(provide 'skempo-js)
