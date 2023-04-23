;;; init --- Config -*- lexical-binding: t; outline-regexp: ";;;\\(;*\\)"; -*-

;;; Commentary:

;; Config

;;; Code:

(require 'cl-lib)
(require 'xdg)

(add-hook
 'after-init-hook
 (lambda ()
   (load (locate-user-emacs-file "custom.el") nil nil t)))

(define-prefix-command 'load-command-map)
(define-key ctl-x-map "\C-l" 'load-command-map)

;;; Abbrev

(add-hook 'js-mode-hook 'abbrev-mode)
(add-hook 'nix-mode-hook 'abbrev-mode)

;;; Affe

(define-key search-map "\M-g\M-z" 'affe-grep)
(define-key search-map "\M-f\M-z" 'affe-find)

;;; Apheleia

(autoload 'apheleia-mode "apheleia")
(add-hook 'css-mode-hook 'apheleia-mode)
(add-hook 'js-mode-hook 'apheleia-mode)
(add-hook 'nix-mode-hook 'apheleia-mode)
(add-hook 'nxml-mode-hook 'apheleia-mode)
(add-hook 'web-mode-hook 'apheleia-mode)

;;; Avy

(define-key global-map "\M-z" 'avy-goto-char-timer)
(define-key goto-map "\M-g" 'avy-goto-line)
(define-key isearch-mode-map "\M-z" 'avy-isearch)

;;; Browse Url

(define-key ctl-x-map "B" 'browse-url)

(defun browse-url-choices (url &rest args)
  "Function to browse urls that provides a choices menu.
See `browse-url' for URL and ARGS."
  (let* ((browse-url-ytdli (lambda (url &rest _)
                             (call-process "ytdli" nil 0 nil url)))
         (browse-url-mpvi (lambda (url &rest _)
                            (call-process "setsid" nil 0 nil "-f" "mpvi" url)))
         (answers `(("firefox" ?f "Open in firefox" browse-url-firefox)
                    ("eww" ?e "Open in eww" eww-browse-url)
                    ("brave" ?b "Open in brave" browse-url-generic)
                    ("ytdli" ?y "Download with ytdli" ,browse-url-ytdli)
                    ("mpvi" ?m "Open in mpvi" ,browse-url-mpvi)))
         (read-answer-short t)
         (answer (read-answer (concat url " ") answers)))
    (apply (nth 3 (assoc answer answers)) url args)))

;;; Bytecomp Async

(declare-function async-bytecomp-package-mode "async-bytecomp")
(with-eval-after-load 'bytecomp (async-bytecomp-package-mode))

;;; Cargo

(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; Comint

(define-key mode-specific-map "c" 'comint-run)

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-output-filter-functions 'comint-osc-process-output)

(define-advice comint-run (:override (name command) shell)
  (declare (interactive-only make-comint))
  (interactive
   (let* ((command (read-shell-command "Command: "))
          (name (car (split-string-shell-command command)))
          (name (if (not current-prefix-arg) name
                  (read-string (format-prompt "Name" name) nil nil name))))
     (list name command)))
  (switch-to-buffer
   (make-comint name shell-file-name nil shell-command-switch command))
  (run-hooks (intern-soft (concat "comint-" name "-hook"))))

;;; Compile

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defvar compilation-button-map)
(defvar compilation-mode-map)
(defvar compilation-shell-minor-mode-map)
(with-eval-after-load 'compile
  (define-key compilation-button-map "\C-m" 'compile-goto-error-same-window)
  (define-key compilation-button-map "o" 'compile-goto-error)
  (define-key compilation-mode-map "\C-m" 'compile-goto-error-same-window)
  (define-key compilation-mode-map "o" 'compile-goto-error)
  (define-key compilation-shell-minor-mode-map "\C-c\C-g" 'recompile-comint-maybe-in-project))

(declare-function compile-goto-error "compile")
(defun compile-goto-error-same-window ()
  "Run `compile-goto-error' in the same window."
  (interactive)
  (xref-push-marker-stack)
  (same-window-prefix)
  (compile-goto-error))

(defvar project-compilation-buffer-name-function)
(declare-function recompile "compile")
(defun recompile-comint-maybe-in-project (&optional edit-command)
  "Recompile comint in project if in project.
For EDIT-COMMAND see `recompile'."
  (interactive "P")
  (let ((compilation-buffer-name-function
         (if (project-current)
             project-compilation-buffer-name-function
           compilation-buffer-name-function)))
    (recompile edit-command)))

;;; Consult

(defvar kmacro-keymap)
(define-key ctl-x-4-map "b" 'consult-buffer-other-window)
(define-key ctl-x-map "b" 'consult-buffer)
(define-key ctl-x-r-map "b" 'consult-bookmark)
(define-key ctl-x-r-map "i" 'consult-register-load)
(define-key ctl-x-r-map "s" 'consult-register-store)
(define-key global-map "\M-H" 'consult-history)
(define-key global-map "\M-y" 'consult-yank-replace)
(define-key goto-map "E" 'consult-compile-error)
(define-key goto-map "F" 'consult-flymake)
(define-key goto-map "I" 'consult-imenu-multi)
(define-key goto-map "i" 'consult-imenu)
(define-key goto-map "o" 'consult-outline)
(define-key help-map "\M-i" 'consult-info)
(define-key help-map "\M-m" 'consult-man)
(define-key isearch-mode-map "\M-s\M-c\M-L" 'consult-line-multi)
(define-key isearch-mode-map "\M-s\M-c\M-l" 'consult-line)
(define-key kmacro-keymap "c" 'consult-kmacro)
(define-key project-prefix-map "b" 'consult-project-buffer)
(define-key project-prefix-map "i" 'consult-project-imenu)
(define-key search-map "\M-c\M-L" 'consult-line-multi)
(define-key search-map "\M-c\M-f" 'consult-focus-lines)
(define-key search-map "\M-c\M-k" 'consult-keep-lines)
(define-key search-map "\M-c\M-l" 'consult-line)
(define-key search-map "\M-f\M-f" 'consult-find)
(define-key search-map "\M-f\M-l" 'consult-locate)
(define-key search-map "\M-g\M-g" 'consult-grep)
(define-key search-map "\M-g\M-r" 'consult-ripgrep)
(define-key search-map "\M-g\M-t" 'consult-git-grep)
(define-key tab-prefix-map "b" 'consult-buffer-other-tab)

(with-eval-after-load 'consult
  (add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

(setq register-preview-function 'consult-register-format)
(advice-add 'register-preview :override 'consult-register-window)

(define-advice man (:around (fn man-args) faster)
  (interactive (user-error "Please call `consult-man'"))
  (if (string-match-p (rx "configuration.nix") man-args)
      (let ((buffer-name (format "*Faster Man - %s*" man-args)))
        (unless (get-buffer buffer-name)
          (let* ((buffer (get-buffer-create buffer-name t))
                 (cmd (format "man %s 2>/dev/null | col -b" man-args)))
            (with-current-buffer buffer
              (special-mode))
            (set-process-filter
             (start-process-shell-command "man" buffer cmd)
             (lambda (proc text)
               (with-current-buffer (process-buffer proc)
                 (let ((point (point)))
                   (internal-default-process-filter proc text)
                   (goto-char point)))))))

        (pop-to-buffer-same-window buffer-name))
    (funcall fn man-args)))

(defvar consult--buffer-display)
(declare-function consult-buffer "consult")
(defun consult-buffer-other-tab ()
  "Variant of `consult-buffer' which opens in other tab."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-tab))
    (consult-buffer)))

;;; Css Mode

(defvar css-mode-map)
(with-eval-after-load 'css-mode
  (define-key css-mode-map "\C-cm" 'css-lookup-symbol))

;;; Custom

(define-prefix-command 'cus-edit-map)
(define-key ctl-x-map "c" 'cus-edit-map)

(define-key 'cus-edit-map "v" 'customize-option)
(define-key 'cus-edit-map "g" 'customize-group)
(define-key 'cus-edit-map "f" 'customize-face)
(define-key 'cus-edit-map "s" 'customize-saved)
(define-key 'cus-edit-map "u" 'customize-unsaved)

;;; Cyrillic Dvorak Im

(require 'cyrillic-dvorak-im)

;;; Dictionary

(define-key mode-specific-map "oT" 'dictionary-search)

;;; Diff

(defvar diff-mode-map)
(with-eval-after-load 'diff-mode
  (define-key diff-mode-map "\M-o" nil))

;;; Dired

(defvar dired-mode-map)
(with-eval-after-load 'dired
  (require 'dired-x)
  (define-key dired-mode-map "\M-+" 'dired-create-empty-file)
  (define-key dired-mode-map "X" nil)
  (define-key dired-mode-map "&" nil))

;;; Dired Atool Transient

(with-eval-after-load 'dired
  (define-key dired-mode-map "c" 'dired-atool-transient-pack)
  (define-key dired-mode-map "Z" 'dired-atool-transient-unpack))

;;; Dired Tags

(with-eval-after-load 'dired
  (define-key dired-mode-map "\C-c\C-t" 'dired-tags-prefix-map))

;;; Disass

(define-key emacs-lisp-mode-map "\C-c\C-d" 'disassemble)
(define-key lisp-interaction-mode-map "\C-c\C-d" 'disassemble)

;;; Dumb Jump

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;;; Ebdb

(define-key mode-specific-map "oe" 'ebdb)

(defvar ebdb-mode-map)
(with-eval-after-load 'ebdb-com
  (define-key ebdb-mode-map "\C-cm" 'ebdb-complete-push-mail-and-quit-window)
  (define-key ebdb-mode-map "\C-cM" 'ebdb-complete-push-mail))

(defvar message-mode-map)
(with-eval-after-load 'message
  (require 'ebdb-message)
  (define-key message-mode-map "\C-ce" 'ebdb-complete))

;;; Ediff

(require 'transient)
(transient-define-prefix ediff-commands ()
  [["Buffers"
    ("bb" "Buffers" ediff-buffers)
    ("bm" "Merge" ediff-merge-buffers)
    ("bp" "Patch" ediff-patch-buffer)]
   ["Regions"
    ("rl" "Linewise" ediff-regions-linewise)
    ("rw" "Wordwise" ediff-regions-wordwise)]
   ["Windows"
    ("wl" "Linewise" ediff-windows-linewise)
    ("ww" "Wordwise" ediff-windows-wordwise)]]
  [["Files"
    ("fc" "Current" ediff-current-file)
    ("fb" "Backup" ediff-current-file)
    ("ff" "Files" ediff-files)
    ("fm" "Merge" ediff-merge-files)
    ("fp" "Patch" ediff-patch-file)]
   ["Directories"
    ("dd" "Directories" ediff-current-file)
    ("dm" "Merge" ediff-merge-directories)]
   ["Revisions"
    ("vv" "Revision" ediff-revision)
    ("vm" "Merge" ediff-merge-revisions)
    ("vdd" "Directory" ediff-directory-revisions)
    ("vdm" "Merge directory" ediff-merge-directory-revisions)]])

(define-key ctl-x-map "\C-d" 'ediff-commands)

;;; Edit Indirect

(define-key ctl-x-map "E" 'edit-indirect-region)

;;; Eglot

(defvar eglot-mode-map)
(defvar eglot-server-programs)
(defvar eglot-stay-out-of)
(declare-function project-files "project")

(defun eglot-csharp-server-program (_)
  "Return a command for csharp language server."
  (let* ((files (project-files (project-current)))
         (slns (cl-remove (rx ".sln" eos) files :test-not #'string-match-p))
         (sln (if (cdr slns)
                  (completing-read "Select solution: " slns nil t)
                (car slns))))
    (list "CSharpLanguageServer" "-s" sln)))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map "\C-c\C-l" 'eglot-code-actions)

  (mapc (apply-partially 'add-to-list 'eglot-server-programs)
        '((js-mode . ("typescript-language-server" "--tsserver-path" "tsserver" "--stdio"))
          (csharp-mode . eglot-csharp-server-program)))

  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy))

(define-advice eglot-xref-backend (:override () dumb) 'eglot+dumb)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
  "Xref backend that combines eglot and dumb-jump."
  (cons (xref-backend-identifier-at-point 'eglot)
        (xref-backend-identifier-at-point 'dumb-jump)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
  "Xref backend that combines eglot and dumb-jump."
  (xref-backend-identifier-completion-table 'eglot))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot+dumb)) identifier)
  "Xref backend that combines eglot and dumb-jump.
See `xref-backend-definitions' docs for IDENTIFIER."
  (or (xref-backend-definitions 'eglot (car identifier))
      (xref-backend-definitions 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-references ((_backend (eql eglot+dumb)) identifier)
  "Xref backend that combines eglot and dumb-jump.
See `xref-backend-references' docs for IDENTIFIER."
  (or (xref-backend-references 'eglot (car identifier))
      (xref-backend-references 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot+dumb)) pattern)
  "Xref backend that combines eglot and dumb-jump.
See `xref-backend-apropos' docs for PATTERN."
  (xref-backend-apropos 'eglot pattern))

;;; Eldoc

(add-hook 'nix-mode-hook 'eldoc-mode)

;;; Elisp Mode

(define-key emacs-lisp-mode-map [?\C-c ?\C-\S-m] 'emacs-lisp-macroexpand)
(define-key lisp-interaction-mode-map [?\C-c ?\C-\S-m] 'emacs-lisp-macroexpand)
(setq elisp-flymake-byte-compile-load-path (cons "./" load-path))

;;; Emacs

(setq completion-ignore-case t)
(define-key ctl-x-map "\C-\M-t" 'transpose-regions)

;;; Embark

(define-key global-map [?\C-.] 'embark-act)

;;; Emmet Mode

(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;; Env

(setenv "PAGER" "cat")
(define-key global-map [?\C-\M-$] 'getenv)

;;; Envrc

(defvar envrc-mode-map)
(with-eval-after-load 'envrc
  (define-key envrc-mode-map "\C-xd" 'envrc-command-map)
  (define-key 'envrc-command-map "R" 'envrc-reload-all))

;;; Eww

(defun eww-restore-browse-url-browser-function ()
  "Restore `browse-url-browser-function' original value."
  (kill-local-variable 'browse-url-browser-function))
(add-hook 'eww-mode-hook 'eww-restore-browse-url-browser-function)

;;; Files

(define-key 'load-command-map "\C-f" 'load-file)
(define-key 'load-command-map "\C-l" 'load-library)

;;; Files X

(define-key ctl-x-x-map "ld" 'add-dir-local-variable)
(define-key ctl-x-x-map "lf" 'add-file-local-variable)
(define-key ctl-x-x-map "lF" 'add-file-local-variable-prop-line)

;;; Find Dired

(defvar find-ls-option)
(defun find-dired-command (dir cmd)
  "Run `find-dired' with CMD in DIR.
CMD must output files delimited by zero byte."
  (interactive
   (let ((cmd (read-shell-command "Shell command (zsf): "))
         (dir (read-directory-name "Run find in directory: " nil "" t)))
     (list dir cmd)))
  (let ((find-program "true")
        (find-ls-option
         (cons
          (concat "; " cmd " | xargs -0 ls -ldF --si --quoting-style=literal")
          "-ldhF")))
    (find-dired dir "")))

(defun find-dired-fd--execute ()
  "Interactive command used as a transient suffix."
  (declare (completion ignore) (interactive-only t))
  (interactive)
  (let ((args (transient-args 'find-dired-fd)))
    (find-dired-command
     (transient-arg-value "--directory=" args)
     (mapconcat
      (lambda (arg)
        (pcase arg
          ((rx bos "--directory=") "")
          ((rx bos "--pattern=" (let pattern (* any)))
           (shell-quote-argument pattern))
          ((rx bos "--contains-regexp=" (let regexp (* any)))
           (concat "--exec rg --color never --regexp"
                   " " (shell-quote-argument regexp)
                   " -0 -ls " (shell-quote-argument ";")))
          (_ (shell-quote-argument arg))))
      (cl-list* "fd" "-0" "-c" "never" args) " "))))

(transient-define-prefix find-dired-fd ()
  ["Flags"
   ("-H" "Include hidden files" "--hidden")
   ("-I" "Include ignored files" "--no-ignore")
   ("-g" "Glob pattern" "--glob")
   ("-F" "Literal pattern" "--fixed-strings")
   ("-L" "Follow symlinks" "--follow")
   ("-p" "Match against the full path" "--full-path")]
  ["Options"
   ("-D" "Directory" "--directory="
    transient-read-existing-directory
    :init-value (lambda (o) (oset o value default-directory))
    :always-read t)
   ("-P" "Pattern" "--pattern=")
   ("-d" "Limit traversal" "--max-depth=" transient-read-number-N+)
   ("-t" "Filter by type" "--type="
    :multi-value repeat
    :choices ("file" "directory" "symlink" "socket" "pipe" "executable" "empty"))
   ("-e" "Filter by extension" "--extension=" :multi-value repeat)
   ("-E" "Exclude files/directories" "--exclude="
    :multi-value repeat
    :prompt "(glob) --exclude=")
   ("-S" "Limit based on the size" "--size="
    :prompt "[-+]?[0-9]+[bkmgt] --size=")
   ("-cw" "File changed within" "--changed-within="
    :prompt "(YYYY-MM-DD( HH:MM:SS)?|[0-9]+[smhdwMy]) --changed-within=")
   ("-cb" "File changed before" "--changed-before="
    :prompt "(YYYY-MM-DD( HH:MM:SS)?|[0-9]+[smhdwMy]) --changed-before=")
   ("-o" "User and/or group" "--owner="
    :prompt "!?(user|uid)?(:(group|gid))? --owner=")
   ("-cr" "File contains regexp" "--contains-regexp=")]
  ["Actions" ("x" "Execute" find-dired-fd--execute)])

(define-key search-map "\M-fd" 'find-dired-fd)

(defun find-dired-locate--execute ()
  "Interactive command used as a transient suffix."
  (declare (completion ignore) (interactive-only t))
  (interactive)
  (let ((args (transient-args 'find-dired-locate)))
    (find-dired-command
     "/"
     (mapconcat
      (lambda (arg)
        (pcase arg
          ((rx bos "--pattern=" (let pattern (* any)))
           (shell-quote-argument pattern))
          (_ (shell-quote-argument arg))))
      (cl-list* "locate" "-0" args) " "))))

(transient-define-prefix find-dired-locate ()
  ["Flags"
   ("-b" "Basename" "--basename")
   ("-i" "Ignore case" "--ignore-case")
   ("-r" "POSIX extended regex" "--regex")]
  ["Options"
   ("-P" "Pattern" "--pattern="
    :prompt "Locate pattern: "
    :init-value (lambda (o) (oset o value (read-string (oref o prompt))))
    :always-read t)]
  ["Actions" ("x" "Execute" find-dired-locate--execute)])

(define-key search-map "\M-fl" 'find-dired-locate)

;;; Find Func

(define-key ctl-x-map "F" 'find-function)
(define-key ctl-x-map "K" 'find-function-on-key)
(define-key ctl-x-map "L" 'find-library)
(define-key ctl-x-map "V" 'find-variable)

(dolist (fn '(find-library find-function find-function-on-key find-variable))
  (advice-add fn :before 'xref-push-marker-stack-ignore-args))

;;; Finder

(define-key help-map "\M-c" 'finder-commentary)

;;; Flymake

(add-hook 'nix-mode-hook 'flymake-mode)
(add-hook 'nxml-mode-hook 'flymake-mode)

(defvar flymake-mode-map)
(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-g\M-b" 'flymake-goto-prev-error)
  (define-key flymake-mode-map "\M-g\M-f" 'flymake-goto-next-error))

;;; Flymake Collection

(declare-function flymake-collection-hook-setup "flymake-collection-hook")
(flymake-collection-hook-setup)

(defvar nix-mode-map)
(with-eval-after-load 'nix-mode
  (with-eval-after-load 'flymake-collection-statix
    (define-key nix-mode-map "\C-c\C-x" 'flymake-collection-statix-fix)))

;;; Grep

(define-key search-map "\M-gg" 'rgrep)

(define-advice grep-expand-template (:filter-return (cmd) cut)
  (concat cmd " | cut -c-500"))

;;; Help

(define-key ctl-x-map "h" 'help-command)

;;; Help Fns

(define-key help-map "\M-f" 'describe-face)
(define-key help-map "\M-k" 'describe-keymap)

;;; Hippie Exp

(define-key ctl-x-map [?\C-\;] 'hippie-expand)

;;; Hl Line

(define-key ctl-x-x-map "h" 'hl-line-mode)

(add-hook 'csv-mode-hook 'hl-line-mode)
(add-hook 'grep-mode-hook 'hl-line-mode)
(add-hook 'mpc-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'transmission-files-mode-hook 'hl-line-mode)
(add-hook 'transmission-mode-hook 'hl-line-mode)
(add-hook 'transmission-peers-mode-hook 'hl-line-mode)

;;; Ibuffer

(defvar ibuffer-mode-map)
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map "\M-o" nil))

;;; Ipretty

(define-key lisp-interaction-mode-map "\C-j" 'ipretty-last-sexp)

;;; Isearch

(fset 'isearch-help-map isearch-help-map)
(define-key isearch-mode-map (kbd "C-?") 'isearch-help-map)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; Js

(defvar js-mode-map)
(with-eval-after-load 'js
  (define-key js-mode-map "\M-." nil))

;;; Ledger

(defvar ledger-amount-regex)
(defvar ledger-commodity-regexp)
(with-eval-after-load 'ledger-regex
  (setq ledger-amount-regex
        (rx (group (or "  " "\t" " \t")) (* (in " \t")) (? "-")
            (? (regexp ledger-commodity-regexp) (* " "))
            (group (? (in "=-")) (or (+ (in "0-9")) (+? (in "0-9,."))))
            (? (group (in ",.") (+ (in "0-9)"))))
            (? (* " ") (regexp ledger-commodity-regexp))
            (? (group (* (in " \t")) (in "=@{") (? "@") (+? (not (in "\n;")))))
            (? (group (or (seq (+ (in " \t")) ";" (+? nonl))
                          (* (in " \t")))))
            eol)))

;;; Link Hint

(define-key goto-map "\M-l" 'link-hint-open-link)
(define-key goto-map "\M-L" 'link-hint-copy-link)
(with-eval-after-load 'link-hint
  (cl-pushnew 'rg-mode (get 'link-hint-compilation-link :vars)))

;;; Lisp

(define-key global-map "\M-[" 'delete-pair)
(define-key global-map "\M-]" 'change-pair)
(define-key global-map [?\C-\)] 'slurp-pair)

(defun change-pair (change-to)
  "Change pair at point to CHANGE-TO."
  (interactive "cChange to:")
  (pcase (assq change-to insert-pair-alist)
    ((or `(,open ,close) `(,_ ,open ,close))
     (save-excursion
       (insert-pair 1 open close)
       (delete-pair)))))

(defun slurp-pair ()
  "Slurp the next pair into the current one."
  (interactive)
  (save-excursion
    (backward-up-list)
    (save-excursion
      (pcase (assq (char-after) insert-pair-alist)
        ((or `(,open ,close) `(_ ,open ,close))
         (insert-pair 2 open close)
         (delete-pair))))
    (indent-sexp)))

;;; Loadhist

(define-key 'load-command-map "\C-u" 'unload-feature)

;;; Magit

(define-key project-prefix-map "m" 'magit-project-status)

;;; Menu Bar

(define-key ctl-x-map "`" 'toggle-debug-on-error)

;;; Minibuffer

(setq completion-category-defaults nil
      completion-in-region-function 'consult-completion-in-region
      minibuffer-allow-text-properties t)

(define-key completion-in-region-mode-map "\M-v" 'switch-to-completions)
(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map "\C-j" 'minibuffer-force-complete-and-exit)

;;; Mpc

(define-key mode-specific-map "os" 'mpc)

(defvar mpc-mode-map)
(defvar mpc-songs-mode-map)
(with-eval-after-load 'mpc
  (define-key mpc-mode-map "." 'mpc-toggle-single)
  (define-key mpc-mode-map "D" 'mpc-playlist-delete)
  (define-key mpc-mode-map "M" 'mpc-select-extend)
  (define-key mpc-mode-map "\C-m" 'mpc-songs-jump-to)
  (define-key mpc-mode-map "\M-m" 'mpc-select)
  (define-key mpc-mode-map "a" 'mpc-playlist-add)
  (define-key mpc-mode-map "b" 'mpc-rewind)
  (define-key mpc-mode-map "c" 'mpc-toggle-consume)
  (define-key mpc-mode-map "f" 'mpc-ffwd)
  (define-key mpc-mode-map "j" 'mpc-dired-jump)
  (define-key mpc-mode-map "k" 'mpc-songs-kill-search)
  (define-key mpc-mode-map "m" 'mpc-select-toggle)
  (define-key mpc-mode-map "p" 'mpc-playlist)
  (define-key mpc-mode-map "r" 'mpc-toggle-repeat)
  (define-key mpc-mode-map "s" 'mpc-songs-search)
  (define-key mpc-mode-map "t" 'mpc-toggle-play)
  (define-key mpc-mode-map "u" 'mpc-update)
  (define-key mpc-mode-map "z" 'mpc-toggle-shuffle)
  (define-key mpc-songs-mode-map "V" 'mpc-move-backward)
  (define-key mpc-songs-mode-map "v" 'mpc-move-forward)
  (define-key mpc-songs-mode-map [remap mpc-select] nil))

(defvar mpc-mpd-music-directory)
(declare-function mpc-tagbrowser-all-p "mpc")
(defun mpc-dired-jump ()
  "Call `dired-jump' on the sond at point."
  (interactive)
  (when-let ((file (or (get-text-property (point) 'mpc-file)
                       (unless (mpc-tagbrowser-all-p)
                         (let ((file (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (1- (line-beginning-position 2)))))
                           (unless (or (string-empty-p file)
                                       (string= "\n" file))
                             file))))))
    (dired-jump nil (expand-file-name file mpc-mpd-music-directory))))

(declare-function mpc-cmd-move "mpc")
(declare-function mpc-songs-refresh "mpc")
(defun mpc-move-forward (n)
  "Move song at point to the N's relative position."
  (interactive "p")
  (let ((point-max (point-max)))
    (unless (= 1 point-max)
      (when-let ((last-pos (get-text-property (1- point-max) 'mpc-file-pos))
                 (cur-pos (get-text-property (point) 'mpc-file-pos)))
        (let ((new-pos (+ cur-pos n)))
          (when (<= 0 new-pos last-pos)
            (mpc-cmd-move (list cur-pos) new-pos)
            (mpc-songs-refresh)))))))

(defun mpc-move-backward (n)
  "Like `mpc-move-forward' but backwards.
See its documentiation for N."
  (interactive "p")
  (mpc-move-forward (- n)))

;;; Net Utils

(define-key mode-specific-map "nh" 'nslookup-host)
(define-key mode-specific-map "ni" 'ifconfig)
(define-key mode-specific-map "nn" 'netstat)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "nw" 'iwconfig)

;;; Newcomment

(define-key global-map [?\C-\;] 'comment-line)

;;; Newsticker

(define-key mode-specific-map "on" 'newsticker-show-news)

;;; Nix Mode

(add-hook 'proced-mode-hook 'nix-prettify-mode)

(define-key mode-specific-map "e" 'nix-edit)
(define-key mode-specific-map "r" 'nix-repl)
(define-key mode-specific-map "S" 'nix-search-transient)
(define-key mode-specific-map "T" 'nix-store-show-path)

(define-advice nix-edit (:override () flake)
  (interactive)
  (let ((cmd (read-shell-command "Nix edit command: " "nix edit ")))
    (find-file
     (with-temp-buffer
       (let ((process-environment (cons "EDITOR=echo" process-environment)))
         (call-process-shell-command cmd nil (list (current-buffer) nil) nil))
       (buffer-substring-no-properties (point-min) (1- (point-max)))))))

(declare-function cl-delete-duplicates "cl-lib")
(declare-function cl-remove "cl-lib")
(declare-function nix-flake--registry-list "nix-flake")
(define-advice nix-flake--registry-refs (:override () all)
  (cl-delete-duplicates
   (cl-remove
    "path:"
    (flatten-list (mapcar #'cdr (nix-flake--registry-list)))
    :test #'string-prefix-p)
   :test #'string=))

(defvar nix-flake-ref)
(declare-function nix-flake--installable-command "nix-flake")
(declare-function nix-flake--options "nix-flake")
(declare-function nix-flake--run-attribute-names "nix-flake")
(declare-function project-root "project")
(define-advice nix-flake-run-attribute
    (:override (options flake-ref attribute command-args &optional comint)
               shell)
  (interactive (list (nix-flake--options)
                     nix-flake-ref
                     (completing-read "Nix app/package: "
                                      (nix-flake--run-attribute-names))
                     nil
                     (consp current-prefix-arg)))
  (compile (nix-flake--installable-command "run" options flake-ref attribute
                                           command-args)
           comint))

(declare-function nix-flake--build-attribute-names "nix-flake")
(defun nix-flake-log-attribute (options flake-ref attribute)
  "Log a derivation in the current flake.

For OPTIONS, FLAKE-REF, and ATTRIBUTE, see the documentation of
`nix-flake-run-attribute'."
  (interactive (list (nix-flake--options)
                     nix-flake-ref
                     (completing-read "Nix package: "
                                      (nix-flake--build-attribute-names))))
  (compile (nix-flake--installable-command "log" options flake-ref attribute)))

(with-eval-after-load 'nix-flake
  (transient-append-suffix 'nix-flake-dispatch '(2 -1)
    '("l" "Log attribute" nix-flake-log-attribute))
  (transient-append-suffix 'nix-flake-dispatch '(2 -1)
    '("o" "Rebuild attribute" nix-flake-rebuild-attribute)))

(define-advice nix-read-flake (:override () always-prompt)
  (let ((default "nixpkgs"))
    (read-string (format-prompt "Nix flake" default) nil nil default)))

(defun nix-compile-in-project-advice (fn &rest args)
  "Change compilation buffer name in FN with ARGS.
Used as an advice."
  (let ((default-directory nix-flake-ref)
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (apply fn args)))

(dolist (fn '(nix-flake-log-attribute
              nix-flake-run-attribute
              nix-flake-run-default
              nix-flake-build-attribute
              nix-flake-build-default
              nix-flake-check
              nix-flake-lock
              nix-flake-update
              nix-flake-rebuild-attribute))
  (advice-add fn :around 'nix-compile-in-project-advice))

(declare-function nix-flake "nix-flake")
(defun nix-flake-project ()
  "Run command `nix-flake' in project root."
  (interactive)
  (nix-flake (project-root (project-current t))))

(define-key project-prefix-map "l" 'nix-flake-project)

(defvar nix-flake-outputs)
(defun nix-flake-rebuild-attribute (options flake-ref attribute)
  "Command `nix-flake' suffix that builds nixos configurations.
OPTIONS is a list of options passed to nixos-rebuild.  FLAKE-REF
is a flake url.  ATTRIBUTE specifies which nixos configuration to
build."
  (interactive
   (list
    (nix-flake--options)
    nix-flake-ref
    (completing-read
     "Nixos configuration: "
     (mapcar (lambda (nc) (symbol-name (car nc)))
             (alist-get 'nixosConfigurations nix-flake-outputs)))))
  (compile (concat "nixos-rebuild build --flake "
                   (mapconcat 'shell-quote-argument
                              (cons (concat flake-ref "#" attribute) options)
                              " "))))

(declare-function nix--process-json-nocheck "nix")
(declare-function nix-search--display "nix-search")
(defun nix-search-transient-execute ()
  "Interactive command used as a transient suffix."
  (declare (completion ignore) (interactive-only t))
  (interactive)
  (let* ((args (transient-args 'nix-search-transient))
         (search (transient-arg-value "--regexp=" args))
         (flake (transient-arg-value "--flake=" args))
         (exclude (transient-arg-value "--exclude=" args)))
    (nix-search--display
     (apply #'nix--process-json-nocheck "search" "--json" flake search
            (when exclude (list "--exclude" exclude)))
     (get-buffer-create "*Nix-Search*")
     t search flake)))

(transient-define-prefix nix-search-transient ()
  ["Options"
   ("-r" "Regexp" "--regexp="
    :prompt "Regexp: "
    :init-value (lambda (o) (oset o value (read-regexp (oref o prompt))))
    :always-read t)
   ("-f" "Flake" "--flake="
    nix-flake--read-flake-ref
    :prompt "Flake: "
    :init-value (lambda (o) (oset o value "flake:nixpkgs"))
    :always-read t)
   ("-e" "Exclude regexp" "--exclude=" :prompt "Exclude regexp: ")]
  ["Actions" ("x" "Execute" nix-search-transient-execute)])


;;; Notmuch

(define-key mode-specific-map "om" 'notmuch)

(autoload 'notmuch-mua-mail "notmuch-mua")
(define-mail-user-agent 'notmuch-user-agent
  'notmuch-mua-mail
  'notmuch-mua-send-and-exit
  'notmuch-mua-kill-buffer
  'notmuch-mua-send-hook)

;;; Nov

(add-to-list 'auto-mode-alist (cons (rx ".epub" eos) 'nov-mode))

;;; Novice

(setq disabled-command-function nil)

;;; Nxml Mode

(add-to-list 'auto-mode-alist (cons (rx ".axaml" eos) 'nxml-mode))

;;; Orderless

(cl-pushnew '(const orderless)
            (cdr (nth 3 (get 'completion-styles 'custom-type)))
            :test #'equal)

;;; Org

(defvar org-mode-map)
(with-eval-after-load 'org
  (define-key org-mode-map [?\C-c?\C-\S-t] 'org-todo-yesterday))

(define-advice org-show-notification (:after (&rest _) sound)
  (call-process "notify_ding" nil 0 nil))

(define-key mode-specific-map "Ga" 'org-agenda)

(defvar org-agenda-mode-map)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "T" 'org-agenda-todo-yesterday))

(define-key mode-specific-map "Gc" 'org-capture)

;;; Org Mime

(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\M-o" 'org-mime-htmlize)
  (define-key message-mode-map "\C-c\M-e" 'org-mime-edit-mail-in-org-mode)
  (define-key message-mode-map "\C-c\M-t" 'org-mime-revert-to-plain-text-mail))

;;; Org Roam

(define-key mode-specific-map "Gf" 'org-roam-node-find)
(define-key mode-specific-map "Gi" 'org-roam-node-insert)
(define-key mode-specific-map "Gl" 'org-roam-buffer-toggle)
(define-key mode-specific-map "Gs" 'org-roam-db-sync)

(declare-function org-roam-db-autosync-mode "org-roam-db")
(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode))

;;; Paragraphs

(define-key global-map [?\C-\M-\S-t] 'transpose-paragraphs)

;;; Pdf Tools

(declare-function pdf-loader-install "pdf-loader")
(pdf-loader-install t t)

;;; Pp

(define-key emacs-lisp-mode-map "\C-c\C-m" 'pp-macroexpand-last-sexp)
(define-key lisp-interaction-mode-map "\C-c\C-m" 'pp-macroexpand-last-sexp)

;;; Proced

(define-key mode-specific-map "op" 'proced)

;;; Project

(define-key project-prefix-map "&" nil)

;;; Pueue

(define-key mode-specific-map "ou" 'pueue)
(add-hook 'pueue-mode-hook 'hl-line-mode)

;;; Register

(define-key ctl-x-r-map "L" 'list-registers)
(define-key ctl-x-r-map "a" 'append-to-register)
(define-key ctl-x-r-map "p" 'prepend-to-register)
(define-key ctl-x-r-map "v" 'view-register)

;;; Re Builder

(define-key emacs-lisp-mode-map "\C-c\C-r" 're-builder)
(define-key lisp-interaction-mode-map "\C-c\C-r" 're-builder)

;;; Reverse Im

(require 'reverse-im)
(reverse-im-activate "cyrillic-dvorak")

;;; Rg

(define-key search-map "\M-gr" 'rg-menu)

;;; Rx Widget

(with-eval-after-load 'wid-edit
  (require 'rx-widget)
  (define-widget 'regexp 'rx-widget "A regular expression in rx form."))

;;; Savehist

(defvar savehist-minibuffer-history-variables)
(declare-function cl-delete-if-not "cl-lib")
(defun savehist-cleanup-histories ()
  "Cleanup savehist histories.
Remove duplicates.  Remove inexistent files from
`file-name-history'."
  (interactive)
  (dolist (sym savehist-minibuffer-history-variables)
    (when (boundp sym)
      (set sym (cl-delete-duplicates (symbol-value sym) :test #'equal))))
  (setq file-name-history (cl-delete-if-not #'file-exists-p file-name-history)))

;;; Sdcwoc

(define-key mode-specific-map "ot" 'sdcwoc)

;;; Sgml mode

(add-hook 'nxml-mode-hook 'sgml-electric-tag-pair-mode)

(defvar html-mode-map)
(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key html-mode-map "\M-o" nil)
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char))

;;; Shell

(define-key mode-specific-map "s" 'shell)
(define-key mode-specific-map "l" 'shell-list)

(defun shell-list (&optional other-window-p)
  "Open shell buffers in ibuffer.
`OTHER-WINDOW-P' is like in `ibuffer'."
  (interactive "P")
  (let ((buffer-name "*Shells-List*"))
    (ibuffer other-window-p buffer-name `((mode . shell-mode)) nil nil
             '(("Project shells" (name . "-shell\\*\\'"))
               ("Shells" (name . "\\`\\*shell\\*"))
               ("Async shell commands" (name . "\\`\\*Async Shell Command\\*")))
             '((mark " " (name 40 50 :left :elide) " " filename-and-process)))
    (with-current-buffer buffer-name
      (setq-local ibuffer-use-header-line nil)
      (hl-line-mode t))))

;;; Simple

(define-key ctl-x-map "w" 'mark-whole-buffer)
(define-key ctl-x-x-map "f" 'auto-fill-mode)
(define-key ctl-x-x-map "v" 'visual-line-mode)
(define-key ctl-x-x-map "w" 'whitespace-mode)
(define-key esc-map "&" nil)
(define-key global-map "\C-h" 'backward-delete-char-untabify)
(define-key global-map "\C-w" 'kill-region-dwim)
(define-key global-map "\M- " 'cycle-spacing-fast)
(define-key global-map "\M-K" 'kill-whole-line)
(define-key global-map "\M-\\" 'delete-indentation)
(define-key global-map "\M-c" 'capitalize-dwim)
(define-key global-map "\M-l" 'downcase-dwim)
(define-key global-map "\M-u" 'upcase-dwim)
(define-key mode-specific-map "oP" 'list-processes)

(defun cycle-spacing-fast (&optional n)
  "Like `cycle-spacing' but with fast mode.
See its documentation for N."
  (interactive "*p")
  (cycle-spacing n nil 'fast))

(defun kill-region-dwim (&optional count)
  "Kill word or kill region if it's active.
See `backward-kill-word' for COUNT."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word count)))

;;; Subr X

(put 'thread-first 'lisp-indent-function 1)
(put 'thread-last 'lisp-indent-function 1)

;;; Subword

(add-hook 'csharp-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)
(add-hook 'rust-mode-hook 'subword-mode)

;; Tab Bar

(define-key tab-prefix-map "\M-b" 'tab-bar-history-back)
(define-key tab-prefix-map "\M-f" 'tab-bar-history-forward)

(easy-mmode-defmap tab-bar-history-repeat-map
  '(("\M-b" . tab-bar-history-back)
    ("\M-f" . tab-bar-history-forward))
  nil)

(put 'tab-bar-history-back 'repeat-map 'tab-bar-history-repeat-map)
(put 'tab-bar-history-forward 'repeat-map 'tab-bar-history-repeat-map)

;;; Tempo

(with-eval-after-load 'tempo
  (define-key goto-map "\M-e" 'tempo-forward-mark)
  (define-key goto-map "\M-a" 'tempo-backward-mark))

;;; Tempo Extra

(with-eval-after-load 'abbrev-hook
  (define-key global-map "\C-z" 'abbrev-hook-call))
(with-eval-after-load 'csharp-mode (require 'tempo-extra-csharp))
(with-eval-after-load 'elisp-mode (require 'tempo-extra-elisp))
(with-eval-after-load 'js (require 'tempo-extra-js))
(with-eval-after-load 'lisp-mode (require 'tempo-extra-lisp))
(with-eval-after-load 'nix-mode (require 'tempo-extra-nix))
(with-eval-after-load 'org (require 'tempo-extra-org))

;;; Tex Mode

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

;;; Text Mode

(autoload 'center-region "text-mode")

;;; Term

(define-key mode-specific-map "t" 'term)

;;; Tramp

(define-key ctl-x-x-map "T" 'tramp-cleanup-all-buffers)

;;; Transient

(transient-define-prefix region-commands ()
  [["Other"
    ("a" "Align Regexp" align-regexp)
    ("r" "Reverse Region" reverse-region)
    ("c" "Center Region" center-region)]
   ["Filter"
    ("k" "Keep Lines" keep-lines)
    ("f" "Flush Lines" flush-lines)
    ("d" "Delete Duplicates" delete-duplicate-lines)]
   ["Sort"
    ("s" "Sort Lines" sort-lines)
    ("x" "Sort Regexp Fields" sort-regexp-fields)
    ("m" "Sort Columns" sort-columns)]
   [""
    ("l" "Sort Fields" sort-fields)
    ("n" "Sort Numeric Fields" sort-numeric-fields)]])

(define-key ctl-x-map "\C-r" 'region-commands)

;;; Transmission

(define-key mode-specific-map "or" 'transmission)

(defvar transmission-mode-map)
(with-eval-after-load 'transmission
  (define-key transmission-mode-map "M" 'transmission-move))

;;; Tree Sitter

(add-hook 'csharp-mode-hook 'tree-sitter-mode)
(add-hook 'css-mode-hook 'tree-sitter-mode)
(add-hook 'js-mode-hook 'tree-sitter-mode)
(add-hook 'mhtml-mode-hook 'tree-sitter-mode)
(add-hook 'nix-mode-hook 'tree-sitter-mode)
(add-hook 'python-mode-hook 'tree-sitter-mode)
(add-hook 'rust-mode-hook 'tree-sitter-mode)

;;; Url Parse

(define-advice url-generic-parse-url (:around (fn &rest args) save-match-data)
  (save-match-data (apply fn args)))

;;; Vertico

(defvar vertico-map)
(with-eval-after-load 'vertico
  (define-key vertico-map "\C-m" 'vertico-directory-enter)
  (define-key vertico-map "\C-h" 'vertico-directory-delete-char)
  (define-key vertico-map "\C-w" 'vertico-directory-delete-word)

  (define-key vertico-map "\M-z" 'vertico-quick-exit)
  (define-key vertico-map "\M-Z" 'vertico-quick-insert))

(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

;;; Web Mode

(add-to-list 'auto-mode-alist (cons (rx ".twig" eos) 'web-mode))

;;; Widget

(defvar widget-field-keymap)
(defvar widget-text-keymap)
(with-eval-after-load 'wid-edit
  (define-key widget-field-keymap "\C-xnf" 'widget-narrow-to-field)
  (define-key widget-text-keymap "\C-xnf" 'widget-narrow-to-field))

;;; Window

(define-key global-map "\M-Q" 'quit-window)
(define-key global-map "\M-V" 'scroll-down-line)
(define-key global-map "\M-o" 'other-window)
(define-key global-map [?\C-\M-\S-b] 'previous-buffer)
(define-key global-map [?\C-\M-\S-f] 'next-buffer)
(define-key global-map [?\C-\S-v] 'scroll-up-line)

(with-eval-after-load 'window
  (put 'other-window 'repeat-map nil))

;;; With Editor

(define-key global-map [?\C-\M-!] 'with-editor-shell-command)

(define-advice with-editor-shell-command-read-args
    (:filter-args (args) prefix-prompt)
  (cons (concat "WE " (car args)) (cdr args)))

;;; Xref

(defvar xref--xref-buffer-mode-map)
(with-eval-after-load 'xref
  (define-key xref--xref-buffer-mode-map "\C-m" 'xref-quit-and-goto-xref)
  (define-key xref--xref-buffer-mode-map "o" 'xref-goto-xref))

(autoload 'xref-push-marker-stack "xref")
(defun xref-push-marker-stack-ignore-args (&rest _)
  "Like `xref-push-marker-stack', but ignore arguments.
Used as an advice in goto functions."
  (xref-push-marker-stack))

(provide 'init)

;;; init.el ends here
