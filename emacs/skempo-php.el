;;; skempo-php.el --- Php skempo templates -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'skempo)

(skempo-define-tempo (vd :mode php-mode)
  "echo '<pre>'; var_dump(" p "); echo '</pre>';")

(provide 'skempo-php)
