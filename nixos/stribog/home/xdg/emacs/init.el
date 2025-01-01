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

(define-keymap :prefix 'load-command-map)
(keymap-set ctl-x-map "C-l" 'load-command-map)

;;; Abbrev

(add-hook 'js-ts-mode-hook 'abbrev-mode)
(add-hook 'nix-mode-hook 'abbrev-mode)
(add-hook 'nix-ts-mode-hook 'abbrev-mode)

;;; Affe

(define-keymap :keymap search-map
  "M-g M-z" 'affe-grep
  "M-f M-z" 'affe-find)

;;; Ansi Color

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; Ansi Osc

(add-hook 'compilation-filter-hook 'ansi-osc-compilation-filter)

;;; Apheleia

(autoload 'apheleia-mode "apheleia")
(add-hook 'css-ts-mode-hook 'apheleia-mode)
(add-hook 'js-ts-mode-hook 'apheleia-mode)
(add-hook 'nix-mode-hook 'apheleia-mode)
(add-hook 'nix-ts-mode-hook 'apheleia-mode)
(add-hook 'nxml-mode-hook 'apheleia-mode)
(add-hook 'web-mode-hook 'apheleia-mode)

;;; Avy

(keymap-global-set "M-z" 'avy-goto-char-timer)
(keymap-set goto-map "M-g" 'avy-goto-line)
(keymap-set isearch-mode-map "M-z" 'avy-isearch)

;;; Battery

(require 'battery)
(require 'notifications)

(defvar battery-previous-percentage
  (thread-last battery-status-function
    funcall
    (alist-get ?p)
    string-to-number))

(defun battery-alarm-on-low-level (data)
  "Alarm when battery DATA percentage is low."
  (let ((status (alist-get ?B data))
        (percentage (string-to-number (alist-get ?p data))))
    (when (and (equal status "Discharging")
               (< percentage 15)
               (< 1 (- battery-previous-percentage percentage)))
      (call-process "notify_ding" nil 0 nil)
      (notifications-notify
       :title "Battery"
       :body (format-spec "%B %p%% %t" data)))
    (setq battery-previous-percentage percentage)))

;;; Browse Url

(keymap-set ctl-x-map "B" 'browse-url)

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

(add-hook 'rust-ts-mode-hook 'cargo-minor-mode)

;;; Comint

(keymap-set mode-specific-map "c" 'comint-run)

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

(defvar compilation-button-map)
(defvar compilation-mode-map)
(defvar compilation-shell-minor-mode-map)
(with-eval-after-load 'compile
  (dolist (keymap (list compilation-button-map compilation-mode-map))
    (define-keymap :keymap keymap
      "RET" 'compile-goto-error-same-window
      "o" 'compile-goto-error))
  (keymap-set compilation-shell-minor-mode-map "C-c C-g"
              'recompile-comint-maybe-in-project))

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

(keymap-set 'kmacro-keymap "c" 'consult-kmacro)

(keymap-set ctl-x-4-map "b" 'consult-buffer-other-window)
(keymap-set ctl-x-map "b" 'consult-buffer)

(define-keymap :keymap ctl-x-r-map
  "b" 'consult-bookmark
  "i" 'consult-register-load
  "s" 'consult-register-store)

(define-keymap :keymap (current-global-map)
  "M-H" 'consult-history
  "M-y" 'consult-yank-replace)

(define-keymap :keymap goto-map
  "E" 'consult-compile-error
  "F" 'consult-flymake
  "I" 'consult-imenu-multi
  "i" 'consult-imenu
  "o" 'consult-outline)

(define-keymap :keymap help-map
  "M-i" 'consult-info
  "M-m" 'consult-man)

(define-keymap :keymap isearch-mode-map
  "M-s M-c M-L" 'consult-line-multi
  "M-s M-c M-l" 'consult-line)

(define-keymap :keymap project-prefix-map
  "b" 'consult-project-buffer
  "i" 'consult-project-imenu)

(define-keymap :keymap search-map
  "M-c M-L" 'consult-line-multi
  "M-c M-f" 'consult-focus-lines
  "M-c M-k" 'consult-keep-lines
  "M-c M-l" 'consult-line
  "M-f M-f" 'consult-find
  "M-f M-l" 'consult-locate
  "M-g M-g" 'consult-grep
  "M-g M-r" 'consult-ripgrep
  "M-g M-t" 'consult-git-grep)

(keymap-set tab-prefix-map "b" 'consult-buffer-other-tab)

(with-eval-after-load 'consult
  (add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

(setq register-preview-function 'consult-register-format)
(advice-add 'register-preview :override 'consult-register-window)

;; (define-advice man (:around (fn man-args) faster)
;;   (interactive (user-error "Please call `consult-man'"))
;;   (if (string-match-p (rx "configuration.nix") man-args)
;;       (let ((buffer-name (format "*Faster Man - %s*" man-args)))
;;         (unless (get-buffer buffer-name)
;;           (let* ((buffer (get-buffer-create buffer-name t))
;;                  (cmd (format "man %s 2>/dev/null | col -b" man-args)))
;;             (with-current-buffer buffer
;;               (special-mode))
;;             (set-process-filter
;;              (start-process-shell-command "man" buffer cmd)
;;              (lambda (proc text)
;;                (with-current-buffer (process-buffer proc)
;;                  (let ((point (point)))
;;                    (internal-default-process-filter proc text)
;;                    (goto-char point)))))))

;;         (pop-to-buffer-same-window buffer-name))
;;     (funcall fn man-args)))

(defvar consult--buffer-display)
(declare-function consult-buffer "consult")
(defun consult-buffer-other-tab ()
  "Variant of `consult-buffer' which opens in other tab."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-tab))
    (consult-buffer)))

;;; Custom

(define-keymap :prefix 'cus-edit-map
  "v" 'customize-option
  "g" 'customize-group
  "f" 'customize-face
  "s" 'customize-saved
  "u" 'customize-unsaved)

(keymap-set ctl-x-map "c" 'cus-edit-map)

;;; Cyrillic Dvorak Im

(require 'cyrillic-dvorak-im)

;;; Dictionary

(keymap-set mode-specific-map "o T" 'dictionary-search)

;;; Diff

(defvar diff-mode-map)
(with-eval-after-load 'diff-mode
  (keymap-unset diff-mode-map "M-o" t))

;;; Dired

(defvar dired-mode-map)
(with-eval-after-load 'dired
  (require 'dired-x)
  (define-keymap :keymap dired-mode-map
    "M-+" 'dired-create-empty-file
    "* i" 'dired-mark-images
    "* v" 'dired-mark-videos)
  (dolist (key '("X" "&"))
    (keymap-unset dired-mode-map key t)))

(defvar dired-marker-char)
(declare-function dired-mark-extension "dired-x")
(defun dired-mark-images (&optional marker-char)
  "Mark images.
For MARKER-CHAR see `dired-mark-extension'."
  (interactive
   (list
    (pcase current-prefix-arg
      ('(4) ?\s)
      ('(16)
       (let* ((dflt (char-to-string dired-marker-char))
              (input (read-string
                      (format-prompt "Marker character to use" dflt)
                      nil nil dflt)))
         (aref input 0)))
      (_ dired-marker-char))))
  (dired-mark-extension
   '("bmp" "gif" "jfif" "jpeg" "jpg" "nef" "png" "thm" "tif" "webp" "xpm")
   marker-char))

(defun dired-mark-videos (&optional marker-char)
  "Mark videos.
For MARKER-CHAR see `dired-mark-extension'."
  (interactive
   (list
    (pcase current-prefix-arg
      ('(4) ?\s)
      ('(16)
       (let* ((dflt (char-to-string dired-marker-char))
              (input (read-string
                      (format-prompt "Marker character to use" dflt)
                      nil nil dflt)))
         (aref input 0)))
      (_ dired-marker-char))))
  (dired-mark-extension
   '("3gp" "aiff" "avi" "flac" "flv" "m4a" "mkv" "mov" "mp3" "mp4" "mpg" "ogg" "ogv" "opus" "vob" "wav" "webm" "wmv" "mka" "m4v")
   marker-char))

;;; Dired Atool Transient

(with-eval-after-load 'dired
  (define-keymap :keymap dired-mode-map
    "c" 'dired-atool-transient-pack
    "Z" 'dired-atool-transient-unpack))

;;; Dired Tags

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "C-c C-t" 'dired-tags-prefix-map))

;;; Disass

(dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set keymap "C-c C-d" 'disassemble))

;;; Dumb Jump

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;;; Ebdb

(keymap-set mode-specific-map "o e" 'ebdb)

(defvar ebdb-mode-map)
(with-eval-after-load 'ebdb-com
  (define-keymap :keymap ebdb-mode-map
    "C-c m" 'ebdb-complete-push-mail-and-quit-window
    "C-c M" 'ebdb-complete-push-mail))

(defvar message-mode-map)
(with-eval-after-load 'message
  (require 'ebdb-message)
  (keymap-set message-mode-map "C-c e" 'ebdb-complete))

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

(keymap-set ctl-x-map "C-d" 'ediff-commands)

;;; Edit Indirect

(keymap-set ctl-x-map "E" 'edit-indirect-region)

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
  (keymap-set eglot-mode-map "C-c C-l" 'eglot-code-actions)

  (mapc (apply-partially 'add-to-list 'eglot-server-programs)
        '((js-ts-mode . ("typescript-language-server" "--tsserver-path" "tsserver" "--stdio"))
          (csharp-mode . eglot-csharp-server-program)))

  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy))

(define-advice eglot-xref-backend (:override () dumb) 'eglot+dumb)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
  "Xref backend that combines eglot and dumb-jump."
  (cons (xref-backend-identifier-at-point 'eglot)
        (xref-backend-identifier-at-point 'dumb-jump)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
  "Xref backend that combines eglot and dumb-jump."
  (lambda (string pred _action)
    (or (all-completions
         string (xref-backend-identifier-completion-table 'eglot) pred)
        (all-completions
         string (xref-backend-identifier-completion-table 'dumb-jump) pred))))

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
  (or (xref-backend-apropos 'eglot pattern)
      (xref-backend-apropos 'dumb-jump pattern)))

;;; Eldoc

(add-hook 'nix-mode-hook 'eldoc-mode)
(add-hook 'nix-ts-mode-hook 'eldoc-mode)

;;; Elisp Mode

(setq elisp-flymake-byte-compile-load-path (cons "./" load-path))

(dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set keymap "C-c S-RET" 'emacs-lisp-macroexpand))

;;; Emacs

(setq completion-ignore-case t)
(keymap-set ctl-x-map "C-M-t" 'transpose-regions)

;;; Embark

(keymap-global-set "C-." 'embark-act)

;;; Emmet Mode

(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;; Env

(setenv "PAGER" "cat")
(keymap-global-set "C-M-$" 'getenv)

;;; Envrc

(defvar envrc-mode-map)
(with-eval-after-load 'envrc
  (keymap-set 'envrc-command-map "R" 'envrc-reload-all)
  (keymap-set envrc-mode-map "C-x d" 'envrc-command-map))

;;; Eww

(defun eww-restore-browse-url-browser-function ()
  "Restore `browse-url-browser-function' original value."
  (kill-local-variable 'browse-url-browser-function))
(add-hook 'eww-mode-hook 'eww-restore-browse-url-browser-function)

;;; Files

(define-keymap :keymap 'load-command-map
  "C-f" 'load-file
  "C-l" 'load-library)

(keymap-set ctl-x-map "C-z" 'find-sibling-file)
(put 'find-sibling-rules 'custom-type '(repeat (cons regexp (repeat regexp))))

(keymap-set ctl-x-x-map "R" 'rename-visited-file)

;;; Files X

(define-keymap :keymap ctl-x-x-map
  "l d" 'add-dir-local-variable
  "l f" 'add-file-local-variable
  "l F" 'add-file-local-variable-prop-line)

;;; Find Dired

(autoload 'find-dired--escaped-ls-option "find-dired")
(defun find-dired-append-xargs-pipe (cmd)
  "Append xargs pipe to CMD."
  (thread-last (find-dired--escaped-ls-option)
    (string-remove-prefix "-print0")
    (concat cmd)))

(defun find-dired-fd--execute ()
  "Interactive command used as a transient suffix."
  (declare (completion ignore) (interactive-only t))
  (interactive)
  (let* ((args (transient-args 'find-dired-fd))
         (dir (transient-arg-value "--directory=" args))
         (fd-cmd (mapconcat
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
                  (cl-list* "fd" "-0" "-c" "never" args) " "))
         (cmd (find-dired-append-xargs-pipe fd-cmd)))
    (find-dired-with-command dir cmd)))

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

(defun find-dired-locate--execute ()
  "Interactive command used as a transient suffix."
  (declare (completion ignore) (interactive-only t))
  (interactive)
  (let* ((args (transient-args 'find-dired-locate))
         (locate-cmd (mapconcat
                      (lambda (arg)
                        (pcase arg
                          ((rx bos "--pattern=" (let pattern (* any)))
                           (shell-quote-argument pattern))
                          (_ (shell-quote-argument arg))))
                      (cl-list* "locate" "-0" args) " "))
         (cmd (find-dired-append-xargs-pipe locate-cmd)))
    (find-dired-with-command "/" cmd)))

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

(define-keymap :keymap search-map
  "M-f d" 'find-dired-fd
  "M-f l" 'find-dired-locate)

;;; Find Func

(find-function-setup-keys)
(add-hook 'find-function-after-hook 'xref-push-previous-buffer-marker-stack)

;;; Finder

(keymap-set help-map "M-c" 'finder-commentary)

;;; Flymake

(add-hook 'nix-mode-hook 'flymake-mode)
(add-hook 'nix-ts-mode-hook 'flymake-mode)
(add-hook 'nxml-mode-hook 'flymake-mode)

(defvar flymake-mode-map)
(with-eval-after-load 'flymake
  (define-keymap :keymap flymake-mode-map
    "M-g M-b" 'flymake-goto-prev-error
    "M-g M-f" 'flymake-goto-next-error))

(with-eval-after-load 'flymake-proc
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;;; Flymake Collection

(declare-function flymake-collection-hook-setup "flymake-collection-hook")
(flymake-collection-hook-setup)

(defvar nix-mode-map)
(defvar nix-ts-mode-map)

(with-eval-after-load 'flymake-collection-statix
  (with-eval-after-load 'nix-mode
    (keymap-set nix-mode-map "C-c C-x" 'flymake-collection-statix-fix))
  (with-eval-after-load 'nix-ts-mode
    (keymap-set nix-ts-mode-map "C-c C-x" 'flymake-collection-statix-fix)))

;;; Grep

(keymap-set search-map "M-g g" 'rgrep)

(define-advice grep-expand-template (:filter-return (cmd) cut)
  (concat cmd " | cut -c-500"))

;;; Help

(keymap-set ctl-x-map "h" 'help-command)

;;; Help Fns

(define-keymap :keymap help-map
  "M-f" 'describe-face
  "M-k" 'describe-keymap)

;;; Hippie Exp

(keymap-set ctl-x-map "C-;" 'hippie-expand)

;;; Hl Line

(keymap-set ctl-x-x-map "h" 'hl-line-mode)

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
  (keymap-unset ibuffer-mode-map "M-o" t))

;;; Ipretty

(keymap-set lisp-interaction-mode-map "C-j" 'ipretty-last-sexp)

;;; Isearch

(fset 'isearch-help-map isearch-help-map)
(define-keymap :keymap isearch-mode-map
  "C-?" 'isearch-help-map
  "C-h" 'isearch-delete-char)

;;; Js

(defvar js-ts-mode-map)
(with-eval-after-load 'js
  (keymap-unset js-ts-mode-map "M-." t))

;;; Ledger

(add-hook 'ledger-mode-hook 'ledger-flymake-enable)

;;; Link Hint

(define-keymap :keymap goto-map
  "M-l" 'link-hint-open-link
  "M-L" 'link-hint-copy-link)

(with-eval-after-load 'link-hint
  (cl-pushnew 'rg-mode (get 'link-hint-compilation-link :vars)))

;;; Lisp

(define-keymap :keymap (current-global-map)
  "M-[" 'delete-pair
  "M-]" 'change-pair
  "C-)" 'slurp-pair)

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

(keymap-set 'load-command-map "C-u" 'unload-feature)

;;; Magit

(keymap-set project-prefix-map "m" 'magit-project-status)

;;; Menu Bar

(keymap-set ctl-x-map "`" 'toggle-debug-on-error)

;;; Minibuffer

(setq completion-category-defaults nil
      completion-in-region-function 'consult-completion-in-region
      minibuffer-allow-text-properties t)

(keymap-set completion-in-region-mode-map "M-v" 'switch-to-completions)
(keymap-set minibuffer-local-must-match-map "C-j" 'minibuffer-force-complete-and-exit)
(keymap-unset minibuffer-local-completion-map "SPC" t)

;;; Misc

(keymap-set ctl-x-map "o" 'duplicate-dwim)

(defvar-keymap duplicate-dwim-repeat-map
  :repeat t
  "o" 'duplicate-dwim)

;;; Mpc

(keymap-set mode-specific-map "o s" 'mpc)

(defvar mpc-mode-map)
(defvar mpc-songs-mode-map)
(with-eval-after-load 'mpc
  (define-keymap :keymap mpc-mode-map
    "." 'mpc-toggle-single
    "D" 'mpc-playlist-delete
    "M" 'mpc-select-extend
    "RET" 'mpc-songs-jump-to
    "M-m" 'mpc-select
    "a" 'mpc-playlist-add
    "b" 'mpc-rewind
    "c" 'mpc-toggle-consume
    "f" 'mpc-ffwd
    "j" 'mpc-dired-jump
    "k" 'mpc-songs-kill-search
    "m" 'mpc-select-toggle
    "p" 'mpc-playlist
    "r" 'mpc-toggle-repeat
    "s" 'mpc-songs-search
    "t" 'mpc-toggle-play
    "u" 'mpc-update
    "z" 'mpc-toggle-shuffle)

  (define-keymap :keymap mpc-songs-mode-map
    "V" 'mpc-move-backward
    "v" 'mpc-move-forward)

  (keymap-unset mpc-songs-mode-map "<remap> <mpc-select>" t))

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

(define-keymap :keymap mode-specific-map
  "n h" 'nslookup-host
  "n i" 'ifconfig
  "n n" 'netstat
  "n p" 'ping
  "n w" 'iwconfig)

;;; Newcomment

(keymap-global-set "C-;" 'comment-line)

;;; Newsticker

(keymap-set mode-specific-map "o n" 'newsticker-show-news)

;;; Nix Mode

(add-hook 'proced-mode-hook 'nix-prettify-mode)

(define-keymap :keymap mode-specific-map
  "e" 'nix-edit
  "r" 'nix-repl
  "S" 'nix-search-transient
  "T" 'nix-store-show-path)

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

(declare-function nix-flake--command "nix-flake")
(define-advice nix-flake-update (:override (options flake-ref) fix-flag)
  (interactive (list (nix-flake--options) nix-flake-ref))
  (compile (nix-flake--command '("flake" "update" "--flake") options flake-ref)))

(with-eval-after-load 'nix-flake
  (transient-append-suffix 'nix-flake-dispatch '(2 -1)
    '("l" "Log attribute" nix-flake-log-attribute))
  (transient-append-suffix 'nix-flake-dispatch '(2 -1)
    '("o" "Rebuild attribute" nix-flake-rebuild-attribute))

  (add-to-list 'auto-mode-alist '("\\flake.lock\\'" . json-ts-mode)))

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

(keymap-set project-prefix-map "l" 'nix-flake-project)

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

(keymap-set mode-specific-map "o m" 'notmuch)

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
  (keymap-set org-mode-map "C-c C-S-t" 'org-todo-yesterday))

(define-advice org-show-notification (:after (&rest _) sound)
  (call-process "notify_ding" nil 0 nil))

(keymap-set mode-specific-map "G a" 'org-agenda)

(defvar org-agenda-mode-map)
(with-eval-after-load 'org-agenda
  (keymap-set org-agenda-mode-map "T" 'org-agenda-todo-yesterday))

(keymap-set mode-specific-map "G c" 'org-capture)

;;; Org Mime

(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(with-eval-after-load 'message
  (define-keymap :keymap message-mode-map
    "C-c M-o" 'org-mime-htmlize
    "C-c M-e" 'org-mime-edit-mail-in-org-mode
    "C-c M-t" 'org-mime-revert-to-plain-text-mail))

;;; Org Roam

(define-keymap :keymap mode-specific-map
  "G f" 'org-roam-node-find
  "G i" 'org-roam-node-insert
  "G l" 'org-roam-buffer-toggle
  "G s" 'org-roam-db-sync)

(declare-function org-roam-db-autosync-mode "org-roam-db")
(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode))

;;; Paragraphs

(keymap-global-set "C-M-S-t" 'transpose-paragraphs)

;;; Pdf Tools

(declare-function pdf-loader-install "pdf-loader")
(pdf-loader-install t t)

;;; Pp

(dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set keymap "C-c RET" 'pp-macroexpand-last-sexp))

;;; Proced

(keymap-set mode-specific-map "o p" 'proced)

;;; Project

(keymap-unset project-prefix-map "&" t)

;;; Pueue

(keymap-set mode-specific-map "o u" 'pueue)
(add-hook 'pueue-mode-hook 'hl-line-mode)

;;; Register

(define-keymap :keymap ctl-x-r-map
  "L" 'list-registers
  "a" 'append-to-register
  "p" 'prepend-to-register
  "v" 'view-register)

;;; Re Builder

(dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (keymap-set keymap "C-c C-r" 're-builder))

;;; Reverse Im

(require 'reverse-im)
(reverse-im-activate "cyrillic-dvorak")

;;; Rg

(keymap-set search-map "M-g r" 'rg-menu)

;;; Rust Ts Mode

(add-to-list 'auto-mode-alist (cons (rx ".rs" eos) 'rust-ts-mode))

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

(keymap-set mode-specific-map "o t" 'sdcwoc)

;;; Sgml mode

(add-hook 'nxml-mode-hook 'sgml-electric-tag-pair-mode)

(defvar html-mode-map)
(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (keymap-unset html-mode-map "M-o" t)
  (define-keymap :keymap sgml-mode-map
    "C-M-n" 'sgml-skip-tag-forward
    "C-M-p" 'sgml-skip-tag-backward
    "C-c C-r" 'sgml-namify-char))

;;; Sh Script

(add-hook 'bash-ts-mode-hook 'sh-electric-here-document-mode)
(add-hook 'bash-ts-mode-hook 'flymake-mode)

;;; Shell

(define-keymap :keymap mode-specific-map
  "s" 'shell
  "l" 'shell-list)

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

(keymap-set ctl-x-map "u" 'mark-whole-buffer)

(define-keymap :keymap ctl-x-x-map
  "f" 'auto-fill-mode
  "v" 'visual-line-mode
  "w" 'whitespace-mode)

(define-keymap :keymap (current-global-map)
  "C-h" 'backward-delete-char-untabify
  "C-w" 'kill-region-dwim
  "M-K" 'kill-whole-line
  "M-\\" 'delete-indentation
  "M-c" 'capitalize-dwim
  "M-l" 'downcase-dwim
  "M-u" 'upcase-dwim)

(keymap-set mode-specific-map "o P" 'list-processes)

(keymap-unset esc-map "&" t)

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
(add-hook 'js-ts-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)
(add-hook 'nix-ts-mode-hook 'subword-mode)
(add-hook 'rust-ts-mode-hook 'subword-mode)

;;; Tab Bar

(define-keymap :keymap tab-prefix-map
  "M-b" 'tab-bar-history-back
  "M-f" 'tab-bar-history-forward
  "1" nil)

(defvar-keymap tab-bar-history-repeat-map
  :repeat t
  "M-b" 'tab-bar-history-back
  "M-f" 'tab-bar-history-forward)

;;; Tempo

(with-eval-after-load 'tempo
  (define-keymap :keymap goto-map
    "M-e" 'tempo-forward-mark
    "M-a" 'tempo-backward-mark))

;;; Tempo Extra

(with-eval-after-load 'abbrev-hook
  (keymap-global-set "C-z" 'abbrev-hook-call))

(with-eval-after-load 'csharp-mode (require 'tempo-extra-csharp))
(with-eval-after-load 'elisp-mode (require 'tempo-extra-elisp))
(with-eval-after-load 'js (require 'tempo-extra-js))
(with-eval-after-load 'lisp-mode (require 'tempo-extra-lisp))
(with-eval-after-load 'nix-mode (require 'tempo-extra-nix))
(with-eval-after-load 'org (require 'tempo-extra-org))

(declare-function tempo-extra-define "tempo-extra")
(with-eval-after-load 'nix-ts-mode
  (require 'tempo-extra)

  (tempo-extra-define "fetchurl" 'nix-ts-mode
    '("fetchurl {" n>
      "url = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >))

  (tempo-extra-define "fetchzip" 'nix-ts-mode
    '("fetchzip {" n>
      "url = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >))

  (tempo-extra-define "fetchgit" 'nix-ts-mode
    '("fetchgit {" n>
      "url = \"" p "\";" n>
      "rev = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >))

  (tempo-extra-define "fetchFromGitHub" 'nix-ts-mode
    '("fetchFromGitHub {" n>
      "owner = \"" p "\";" n>
      "repo = \"" p "\";" n>
      "rev = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >)))

;;; Tex Mode

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

;;; Text Mode

(autoload 'center-region "text-mode")

;;; Term

(keymap-set mode-specific-map "t" 'term)

;;; Tramp

(keymap-set ctl-x-x-map "T" 'tramp-cleanup-all-buffers)

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

(keymap-set ctl-x-map "C-r" 'region-commands)

;;; Transmission

(keymap-set mode-specific-map "o r" 'transmission)

(defvar transmission-mode-map)
(with-eval-after-load 'transmission
  (keymap-set transmission-mode-map "M" 'transmission-move))

;;; Url Parse

(define-advice url-generic-parse-url (:around (fn &rest args) save-match-data)
  (save-match-data (apply fn args)))

;;; Vertico

(defvar vertico-map)
(with-eval-after-load 'vertico
  (define-keymap :keymap vertico-map
    "RET" 'vertico-directory-enter
    "C-h" 'vertico-directory-delete-char
    "C-w" 'vertico-directory-delete-word
    "M-z" 'vertico-quick-exit
    "M-Z" 'vertico-quick-insert))

(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

;;; Web Mode

(add-to-list 'auto-mode-alist (cons (rx ".twig" eos) 'web-mode))

;;; Widget

(defvar widget-field-keymap)
(defvar widget-text-keymap)
(with-eval-after-load 'wid-edit
  (dolist (keymap (list widget-field-keymap widget-text-keymap))
    (keymap-set keymap "C-x n f" 'widget-narrow-to-field)))

;;; Window

(define-keymap :keymap (current-global-map)
  "M-Q" 'quit-window
  "M-V" 'scroll-down-line
  "M-o" 'other-window
  "C-M-S-b" 'previous-buffer
  "C-M-S-f" 'next-buffer
  "C-S-v" 'scroll-up-line)

(with-eval-after-load 'window
  (put 'other-window 'repeat-map nil))

;;; With Editor

(keymap-global-set "C-M-!" 'with-editor-shell-command)

(define-advice with-editor-shell-command-read-args
    (:filter-args (args) prefix-prompt)
  (cons (concat "WE " (car args)) (cdr args)))

;;; Xref

(defvar xref--xref-buffer-mode-map)
(with-eval-after-load 'xref
  (define-keymap :keymap xref--xref-buffer-mode-map
    "RET" 'xref-quit-and-goto-xref
    "o" 'xref-goto-xref))

(autoload 'xref-push-marker-stack "xref")
(defun xref-push-previous-buffer-marker-stack ()
  "Push xref marker of previous buffer."
  (previous-buffer)
  (xref-push-marker-stack)
  (next-buffer))

(provide 'init)

;;; init.el ends here
