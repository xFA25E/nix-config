;; -*- lexical-binding: t; -*-

(add-hook 'js-mode-hook 'abbrev-mode)

(with-eval-after-load 'compile
  (defun colorize-compilation ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'colorize-compilation))

(define-key global-map "\M-z" 'avy-goto-word-0)
(define-key goto-map "\M-g" 'avy-goto-line)

(define-key ctl-x-map "B" 'browse-url)

(define-key mode-specific-map "oy" 'browse-url-multi-youtube-search)

(with-eval-after-load 'bytecomp (async-bytecomp-package-mode))

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(define-key global-map "\M-H" 'consult-history)
(define-key goto-map "o" 'consult-outline)
(define-key goto-map "i" 'consult-imenu)
(define-key goto-map "E" 'consult-compile-error)
(define-key goto-map "F" 'consult-flymake)
(define-key project-prefix-map "i" 'consult-project-imenu)
(define-key kmacro-keymap "c" 'consult-kmacro)
(with-eval-after-load 'consult
  (add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode))

(defvar css-mode-map)
(with-eval-after-load 'css-mode (define-key css-mode-map "\C-cm" 'css-lookup-symbol))

(defvar cus-edit-map (make-sparse-keymap))
(define-key cus-edit-map "v" 'customize-option)
(define-key cus-edit-map "g" 'customize-group)
(define-key cus-edit-map "f" 'customize-face)
(define-key cus-edit-map "s" 'customize-saved)
(define-key cus-edit-map "u" 'customize-unsaved)
(define-key ctl-x-map "c" cus-edit-map)

(require 'cyrillic-dvorak-im)

(with-eval-after-load 'dired-aux
  (define-key dired-mode-map "\M-+" 'dired-create-empty-file)
  (add-to-list 'dired-compress-file-suffixes
               (list (rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

(with-eval-after-load 'dired (require 'dired-x))

(autoload 'dired-jump "dired-x" nil t)
(define-key ctl-x-map "\C-j" 'dired-jump)

(with-eval-after-load 'dired (dired-async-mode))

(with-eval-after-load 'dired
  (define-key dired-mode-map "\C-c\C-t" 'dired-tags-prefix-map))

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

(with-eval-after-load 'ebdb-com
  (define-key ebdb-mode-map "\C-cm" 'ebdb-complete-push-mail-and-quit-window)
  (define-key ebdb-mode-map "\C-cM" 'ebdb-complete-push-mail))

(with-eval-after-load 'message
  (require 'ebdb-message)
  (define-key message-mode-map "\C-ce" 'ebdb-complete))

(define-key ctl-x-map "E" 'edit-indirect-region)

(define-key emacs-lisp-mode-map "\C-cM" 'emacs-lisp-macroexpand)
(define-key lisp-interaction-mode-map "\C-cM" 'emacs-lisp-macroexpand)

(setq completion-ignore-case t)

(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(setenv "PAGER" "cat")

(with-eval-after-load 'eww
  (defun eww-browse-url-custom ()
    (interactive)
    (let ((browse-url-browser-function (default-value 'browse-url-browser-function)))
      (when-let ((url-at-point (car (eww-links-at-point))))
        (browse-url url-at-point))))
  (define-key eww-mode-map "V" 'eww-browse-url-custom))

(define-key search-map "n" 'find-name-dired)
(define-key search-map "N" 'find-dired)

(define-key ctl-x-map "L" 'find-library)
(define-key ctl-x-map "F" 'find-function)
(define-key ctl-x-map "K" 'find-function-on-key)
(define-key ctl-x-map "V" 'find-variable)

(dolist (fn '(find-library find-function find-function-on-key find-variable))
  (advice-add fn :before 'xref-push-marker-stack-ignore-args))

(define-key help-map "\M-c" 'finder-commentary)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-g\M-f" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-g\M-b" 'flymake-goto-prev-error))

(define-key search-map "g" 'rgrep)
(declare-function grep-expand-template@add-cut "grep")
(with-eval-after-load 'grep
  (define-advice grep-expand-template (:filter-return (cmd) add-cut)
    (concat cmd " | cut -c-500")))

(define-key global-map "\C-_" 'hippie-expand)

(add-hook 'csv-mode-hook 'hl-line-mode)
(add-hook 'grep-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'transmission-files-mode-hook 'hl-line-mode)
(add-hook 'transmission-mode-hook 'hl-line-mode)
(add-hook 'transmission-peers-mode-hook 'hl-line-mode)
(add-hook 'mpc-mode-hook 'hl-line-mode)

(define-key lisp-interaction-mode-map "\C-j" 'ipretty-last-sexp)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key isearch-mode-map "\C-?" isearch-help-map)

(defvar ledger-amount-regex)
(defvar ledger-commodity-regexp)
(with-eval-after-load 'ledger-regex
  (setq ledger-amount-regex
        (concat "\\(  \\|\t\\| \t\\)[ \t]*-?"
                "\\(?:" ledger-commodity-regexp " *\\)?"
                "\\([-=]?\\(?:[0-9]+\\|[0-9,.]+?\\)\\)"
                "\\([,.][0-9)]+\\)?"
                "\\(?: *" ledger-commodity-regexp "\\)?"
                "\\([ \t]*[@={]@?[^\n;]+?\\)?"
                "\\([ \t]+;.+?\\|[ \t]*\\)?$")))

(define-key goto-map "\M-l" 'link-hint-open-link)
(define-key goto-map "\M-L" 'link-hint-copy-link)
(with-eval-after-load 'link-hint
  (cl-pushnew 'rg-mode (get 'link-hint-compilation-link :vars)))

(defun change-pair (change-to)
  (interactive "cChange to:")
  (pcase (assq change-to insert-pair-alist)
    ((or `(,open ,close) `(,_ ,open ,close))
     (save-excursion
       (insert-pair 1 open close)
       (delete-pair)))))

(defun slurp-pair ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (save-excursion
      (pcase (assq (char-after) insert-pair-alist)
        ((or `(,open ,close) `(_ ,open ,close))
         (insert-pair 2 open close)
         (delete-pair))))
    (indent-sexp)))

(define-key global-map "\M-]" 'change-pair)
(define-key global-map "\M-[" 'delete-pair)
(define-key global-map [?\C-\)] 'slurp-pair)

(define-key search-map "l" 'locate)

(define-key project-prefix-map "m" 'magit-project-status)

(define-key help-map "\M-m" 'man)

(define-key completion-in-region-mode-map "\M-v" 'switch-to-completions)
(define-key minibuffer-local-must-match-map "\C-j" 'minibuffer-force-complete-and-exit)

(define-key mode-specific-map "os" 'mpc)

(defvar mpc-mode-map)
(defvar mpc-songs-mode-map)
(with-eval-after-load 'mpc
  (define-key mpc-mode-map "p" 'mpc-playlist)
  (define-key mpc-mode-map "u" 'mpc-update)
  (define-key mpc-mode-map "a" 'mpc-playlist-add)
  (define-key mpc-mode-map "c" 'mpc-toggle-consume)
  (define-key mpc-mode-map "r" 'mpc-toggle-repeat)
  (define-key mpc-mode-map "." 'mpc-toggle-single)
  (define-key mpc-mode-map "z" 'mpc-toggle-shuffle)
  (define-key mpc-mode-map "t" 'mpc-toggle-play)
  (define-key mpc-mode-map "s" 'mpc-songs-search)
  (define-key mpc-mode-map "k" 'mpc-songs-kill-search)
  (define-key mpc-mode-map "f" 'mpc-ffwd)
  (define-key mpc-mode-map "b" 'mpc-rewind)
  (define-key mpc-mode-map "D" 'mpc-playlist-delete)
  (define-key mpc-mode-map "m" 'mpc-select-toggle)
  (define-key mpc-mode-map "M" 'mpc-select-extend)
  (define-key mpc-mode-map "\M-m" 'mpc-select)
  (define-key mpc-mode-map "\C-m" 'mpc-songs-jump-to)
  (define-key mpc-songs-mode-map [remap mpc-select] nil))

(define-key mode-specific-map "nh" 'nslookup-host)
(define-key mode-specific-map "ni" 'ifconfig)
(define-key mode-specific-map "nn" 'netstat)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "nw" 'iwconfig)

(define-key mode-specific-map "on" 'newsticker-show-news)

(define-key mode-specific-map "om" 'notmuch)
(autoload 'notmuch-mua-mail "notmuch-mua")
(define-mail-user-agent 'notmuch-user-agent 'notmuch-mua-mail 'notmuch-mua-send-and-exit 'notmuch-mua-kill-buffer 'notmuch-mua-send-hook)

(add-to-list 'auto-mode-alist (cons (rx ".epub" eos) 'nov-mode))

(setq disabled-command-function nil)

(with-eval-after-load 'org
  (cl-pushnew
   '(const :tag "Http" http)
   (cdadr (memq :key-type (get 'org-babel-load-languages 'custom-type)))
   :test 'equal))

(with-eval-after-load 'org
  (define-key org-mode-map [?\C-c?\C-\S-t] 'org-todo-yesterday))

(define-advice org-show-notification (:after (&rest _) sound)
  (call-process "notify_sound" nil 0 nil))

(define-key mode-specific-map "Ga" 'org-agenda)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "T" 'org-agenda-todo-yesterday))

(define-key mode-specific-map "Gc" 'org-capture)

(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\M-o" 'org-mime-htmlize)
  (define-key message-mode-map "\C-c\M-e" 'org-mime-edit-mail-in-org-mode)
  (define-key message-mode-map "\C-c\M-t" 'org-mime-revert-to-plain-text-mail))

(define-key global-map [?\C-\M-\S-t] 'transpose-paragraphs)

(autoload 'pcomplete/pass "pcmpl-args")
(autoload 'pcomplete/parted "pcmpl-args-parted")

(pdf-loader-install t t)

(define-key emacs-lisp-mode-map "\C-cm" 'pp-macroexpand-last-sexp)
(define-key lisp-interaction-mode-map "\C-cm" 'pp-macroexpand-last-sexp)

(define-key mode-specific-map "op" 'proced)

(define-key mode-specific-map "ou" 'pueue)
(add-hook 'pueue-mode-hook 'hl-line-mode)

(define-key ctl-x-r-map "v" 'view-register)
(define-key ctl-x-r-map "L" 'list-registers)
(define-key ctl-x-r-map "p" 'prepend-to-register)
(define-key ctl-x-r-map "a" 'append-to-register)

(require 'reverse-im)
(reverse-im-activate "cyrillic-dvorak")

(define-key search-map "r" 'rg-menu)

(with-eval-after-load 'wid-edit
  (require 'rx-widget)
  (define-widget 'regexp 'rx-widget "A regular expression in rx form."))

(with-eval-after-load 'savehist
  (defun savehist-filter-file-name-history ()
    (let (result)
      (dolist (file-name file-name-history)
        (let ((f (string-trim-right (expand-file-name file-name) "/+")))
          (unless (string-empty-p f)
            (when (or (file-remote-p f)
                      (string-match-p "\\`http" f)
                      (file-exists-p f))
              (cl-pushnew f result :test #'string-equal)))))
      (setq file-name-history result))))

(define-key mode-specific-map "ot" 'sdcv-search-input)

(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char))

(define-key mode-specific-map "s" 'shell)

(define-key mode-specific-map "l" 'shell-pwd-list-buffers)
(with-eval-after-load 'shell
  (define-key shell-mode-map "\C-c\M-d" 'shell-pwd-change-directory))

(defun kill-region-dwim (&optional count)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word count)))

(define-key global-map "\C-h" 'backward-delete-char-untabify)
(define-key global-map "\M-K" 'kill-whole-line)
(define-key global-map "\M-c" 'capitalize-dwim)
(define-key global-map "\M-l" 'downcase-dwim)
(define-key global-map "\M-u" 'upcase-dwim)
(define-key global-map "\C-w" 'kill-region-dwim)
(define-key mode-specific-map "oP" 'list-processes)

(add-hook 'nix-mode-hook 'skempo-mode)
(add-hook 'js-mode-hook 'skempo-mode)

(with-eval-after-load 'skempo
  (define-key skempo-mode-map "\C-z" 'skempo-complete-tag-or-call-on-region)
  (define-key skempo-mode-map "\M-g\M-e" 'skempo-forward-mark)
  (define-key skempo-mode-map "\M-g\M-a" 'skempo-backward-mark)

(defun tempo-custom-user-elements (arg)
  (pcase arg
    (:nix-hash (make-string 52 ?1))
    (:elisp-namespace (string-trim-right (buffer-name) (rx ".el" eos)))
    (:elisp-group (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))
    (`(:lisp-with-parens . ,body)
     (if (or (not (eql (char-before) ?\()) (use-region-p))
         `(l "(" ,@body ")")
       `(l ,@body)))))

(add-to-list 'tempo-user-elements 'tempo-custom-user-elements)

(let ((enable-fn (lambda () (or (eq this-command 'expand-abbrev)
                                (eql ?\s last-command-event)))))
  (dolist (mode '(lisp-mode emacs-lisp-mode))
    (let ((table (skempo--abbrev-table mode)))
      (define-abbrev-table table nil :enable-function enable-fn))))

(skempo-define-tempo lambda (:mode lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "lambda (" p ") " r>))

(skempo-define-tempo let (:mode lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "let ((" p "))" n>
   r>))

(skempo-define-tempo let* (:mode lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "let* ((" p "))" n>
   r>))

(skempo-define-tempo defvar (:mode lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defvar " p n>
   r> n>
   "\"" p "\""))

(skempo-define-tempo defun (:mode lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defun " p " (" p ")" n>
   "\"" p "\"" n>
   r>))

(skempo-define-tempo defpackage (:mode lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defpackage #:" (P "Package name: " package) n>
   "(:use #:cl)" n>
   (:when ("Nickname: " nickname)
          "(:nicknames #:" (s nickname)
          (:while ("Nickname: " nickname) " #:" (s nickname))
          ")" n>)
   (:when ("Local nickname: " local-nickname)
          (:when ("For package: " local-package)
                 "(:local-nicknames (#:" (s local-nickname) " #:" (s local-package) ")"
                 (:while ("Local nickname: " local-nickname)
                         (:when ("For package: " local-package)
                                " (#:" (s local-nickname) " #:" (s local-package) ")"))
                 ")" n>))
   (:while ("Import from: " import-package)
           (:when ("Import symbol: " import-symbol)
                  "(:import-from #:" (s import-package) " #:" (s import-symbol)
                  (:while ("Import symbol: " import-symbol) " #:" (s import-symbol))
                  ")" n>))
   (:when ("Export: " export)
          "(:export #:" (s export)
          (:while ("Export: " export) " #:" (s export))
          ")" n>)
   "(:documentation \"" (P "Documentation: ") "\"))" n>
   "(in-package #:" (s package) ")" n>))

(skempo-define-tempo defsystem (:mode lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defsystem \"" (P "System: " system) "\"" n>
   (:when ("Long name: " long-name) ":long-name \"" (s long-name) "\"" n>)
   (:when ("Version: " version) ":version \""  (s version) "\"" n>)
   (:when ("Author: " author) ":author \"" (s author) "\"" n>)
   (:when ("Maintainer: " maintainer) ":maintainer \"" (s maintainer) "\"" n>)
   (:when ("Mailto: " mailto) ":mailto \"" (s mailto) "\"" n>)
   (:when ("License (ex: GPL3): " license) ":license \"" (s license) "\"" n>)
   (:when ("Homepage: " homepage) ":homepage \"" (s homepage) "\"" n>)
   (:when ("Bug tracker: " bug-tracker) ":bug-tracker \"" (s bug-tracker) "\"" n>)
   (:when ("Source control (ex: git): " source-control)
          (:when ("Link: " link) ":source-control (:" (s source-control) " \"" (s link) "\")" n>))
   (:when ("Description: " description) ":description \"" (s description) "\"" n>)
   ":long-description #.(let ((file (probe-file* (subpathname *load-pathname* \"README.md\")))) (when file (read-file-string file)))" n>
   (:when ("Dependency: " dependency)
          ":depends-on (" "\"" (s dependency) "\""
          (:while ("Dependency: " dependency) " \"" (s dependency) "\"")
          ")" n>)
   ":components ((:module \"src\" :components ((:file \"" (s system) "\"))))" n>
   ":in-order-to ((test-op (test-op \"" (s system) "/tests\"))))" n>
   n>
   "(defsystem \"" (s system) "/tests\"" n>
   ":depends-on (\"" (s system) "\" \"fiveam\")" n>
   ":components ((:module \"tests\" :components ((:file \"" (s system) "\"))))" n>
   ":perform (test-op (op c) (symbol-call '#:fiveam '#:run! (find-symbol* '#:" (s system) " '#:" (s system) ".tests)))"))

(skempo-define-tempo defvar (:mode emacs-lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defvar " :elisp-namespace "-" p n>
   r> n>
   "\"" p "\""))

(skempo-define-tempo defun (:mode emacs-lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defun " :elisp-namespace "-" p " (" p ")" n>
   "\"" p "\"" n>
   r>))

(skempo-define-tempo defgroup (:mode emacs-lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defgroup " :elisp-group " nil" n>
   "\"" p "\"" n>
   ":group " p "nil"))

(skempo-define-tempo defcustom (:mode emacs-lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defcustom " :elisp-namespace "-" p n>
   r> n>
   "\"" p "\"" n>
   ":type " p "nil" n>
   ":group '" :elisp-group))

(skempo-define-tempo defface (:mode emacs-lisp-mode :tag t :abbrev t)
  (:lisp-with-parens
   "defface " :elisp-namespace "-" p n>
   "'((t :inherit " p "nil))" n>
   "\"" p "\"" n>
   ":group '" :elisp-group))

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
  p n>
  "}" >)

(skempo-define-tempo if (:mode js-mode :tag t :abbrev t)
  "if (" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo for (:mode js-mode :tag t :abbrev t)
  "for (" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo try (:mode js-mode :tag t :abbrev t)
  "try {" n>
  p n>
  "} catch (" p "error) {" > n>
  p n>
  "}" >)

(skempo-define-tempo github (:mode nix-mode :tag t :abbrev t)
  "fetchFromGitHub {" n>
  "owner = \"" p "\";" n>
  "repo = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo url (:mode nix-mode :tag t :abbrev t)
  "fetchurl {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo zip (:mode nix-mode :tag t :abbrev t)
  "fetchzip {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo git (:mode nix-mode :tag t :abbrev t)
  "fetchgit {" n>
  "url = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo vd (:mode php-mode :tag t :abbrev t)
  "echo '<pre>'; var_dump(" r "); echo '</pre>';")

(skempo-define-tempo readmeorg (:mode org-mode :tag t :abbrev t)
  "#+TITLE: " (P "Project title: ") n
  (P "A short, one-line description of the project: ") n
  n
  "* Overview" n
  p "# A longer description of the project" n
  n
  "** Features" n
  "** History" n
  "** Motivation" n
  "* Usage" n
  p "# Examples of usage" n
  n
  "* Documentation" n
  "* License" n
  "Copyright (c) " (format-time-string "%Y") " " (P "Authors: ") n
  "Licensed under the " p "GPL3 License." n
  n
  "* COMMENT Local Variables" n
  "# Local Variables:" n
  "# eval: (add-hook 'after-save-hook #'org-md-export-to-markdown nil t)" p n
  "# End:")

)

(defvar sly-lisp-implementations)
(with-eval-after-load 'sly
  (setq sly-lisp-implementations
        '((sbcl  ("sbcl"))
          (ecl   ("ecl"))
          (ccl   ("ccl"))
          (clisp ("clisp"))
          (abcl  ("abcl")))))

(add-hook 'rust-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

(define-key mode-specific-map "or" 'transmission)
(defvar transmission-mode-map)
(defvar transmission-files-mode-map)
(defvar transmission-torrent-id)
(declare-function transmission-request-async "transmission")
(with-eval-after-load 'transmission
  (define-key transmission-mode-map "M" 'transmission-move)
  (define-key transmission-files-mode-map "R" 'transmission-files-rename-path)

  (defun transmission-files-rename-path (torrent-id old-path new-name)
    "Rename an OLD-PATH to NEW-NAME of TORRENT-ID.

TORRENT-ID is a hashString of torrent.

OLD-PATH is a path to file in a torrent.  It can be a directory
or a file.

NEW-NAME is a new name of a file at OLD-PATH.

When called interactively, values are taken from current buffer
with `transmission-files-mode'.  OLD-PATH can be set explicitly
with prefix argument, otherwise the file at point is taken.

Note: it is forbidden to move a file to other locations, only
renaming is allowed."
    (interactive
     (let* ((old-path (cdr (assq 'name (tabulated-list-get-id))))
            (old-path-prompt (format "Old path (default %s): " old-path))
            (old-path (if current-prefix-arg
                          (read-string old-path-prompt nil nil old-path)
                        old-path))
            (new-name (file-name-nondirectory old-path))
            (new-name-prompt (format "Rename %s to: " new-name))
            (new-name (read-string new-name-prompt nil nil new-name)))
       (list transmission-torrent-id old-path new-name)))

    (when (string= new-name (file-name-nondirectory old-path))
      (user-error "Cannot rename to the same name: %s" new-name))

    (let ((arguments (list :ids (list torrent-id) :path old-path :name new-name)))
      (transmission-request-async nil "torrent-rename-path" arguments))))

(with-eval-after-load 'url-parse
  (define-advice url-generic-parse-url (:around (fn &rest args) save-match-data)
    (save-match-data (apply fn args))))

(add-to-list 'auto-mode-alist (cons (rx ".twig" eos) 'web-mode))

(define-key global-map "\M-V" 'scroll-down-line)
(define-key global-map [?\C-\S-v] 'scroll-up-line)
(define-key global-map [?\C-\M-\S-b] 'previous-buffer)
(define-key global-map [?\C-\M-\S-f] 'next-buffer)
(define-key global-map "\M-Q" 'quit-window)
(define-key global-map "\M-o" 'other-window)

(autoload 'xref-push-marker-stack "xref")
(defun xref-push-marker-stack-ignore-args (&rest _)
  (xref-push-marker-stack))

(load (expand-file-name "nixpkgs/emacs/custom.el" (xdg-config-home)) nil nil t)
