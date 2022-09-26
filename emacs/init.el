;; -*- lexical-binding: t; -*-

(define-prefix-command 'region-commands-map)
(define-prefix-command 'load-command-map)
(define-prefix-command 'ediff-command-map)

(add-hook 'js-mode-hook 'abbrev-mode)

(define-key region-commands-map "\C-a" 'align-regexp)

(define-key global-map "\M-z" 'avy-goto-word-0)
(define-key goto-map "\M-g" 'avy-goto-line)

(define-key ctl-x-map "B" 'browse-url)

(with-eval-after-load 'browse-url
  (defun browse-url-choices (url &rest args)
    (let* ((answers '(("firefox" ?f "Open in firefox" browse-url-firefox)
                      ("eww" ?e "Open in eww" eww-browse-url)
                      ("brave" ?b "Open in brave" browse-url-generic)
                      ("ytdli" ?y "Download with ytdli"
                       (lambda (url &rest _args)
                         (call-process "ytdli" nil 0 nil url)))
                      ("mpvi" ?m "Open in mpvi"
                       (lambda (url &rest _args)
                         (call-process "setsid" nil 0 nil "-f" "mpvi" url)))))
           (read-answer-short t)
           (answer (read-answer (concat url " ") answers)))
      (apply (nth 3 (assoc answer answers)) url args))))

(declare-function async-bytecomp-package-mode "async-bytecomp" (&optional arg))
(with-eval-after-load 'bytecomp (async-bytecomp-package-mode))

(add-hook 'rust-mode-hook 'cargo-minor-mode)

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

(with-eval-after-load 'compile
  (define-key compilation-shell-minor-mode-map "\C-c\C-g" 'recompile)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(defvar kmacro-keymap)
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

(define-key mode-specific-map "oT" 'dictionary-search)

(defvar dired-mode-map)
(defvar dired-compress-file-suffixes)
(defvar dired-compress-files-alist)
(defvar dired-log-buffer)
(declare-function dired-log "dired" (log &rest args))
(declare-function dired-get-marked-files "dired" (&optional localp arg filter distinguish-one-marked error))

(with-eval-after-load 'dired (require 'dired-x))

(declare-function dired-do-compress-to@async "dired-aux" (&optional arg))
(declare-function dired-relist-file "dired-aux" (file))
(with-eval-after-load 'dired-aux
  (define-key dired-mode-map "\M-+" 'dired-create-empty-file)
  (add-to-list 'dired-compress-file-suffixes
               (list (rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -"))

  (defun dired-do-compress-to@async (&optional arg)
    (interactive "P")
    (require 'format-spec)
    (let* ((in-files (dired-get-marked-files nil arg nil nil t))
           (out-file (expand-file-name (read-file-name "Compress to: ")))
           (rule (cl-find-if (lambda (x) (string-match-p (car x) out-file))
                             dired-compress-files-alist)))
      (cond
       ((not rule)
        (error "No compression rule found for %s, see `dired-compress-files-alist'" out-file))
       ((and (file-exists-p out-file)
             (not (y-or-n-p (format "%s exists, overwrite?" (abbreviate-file-name out-file)))))
        (message "Compression aborted"))
       (t
        (let* ((in-count 0)
               (proc-name (concat "compress " out-file))
               (qout-file (shell-quote-argument out-file))
               (qin-files (mapconcat (lambda (file) (cl-incf in-count)
                                       (shell-quote-argument (file-name-nondirectory file)))
                                     in-files " "))
               (cmd (format-spec (cdr rule) `((?\o . ,qout-file) (?\i . ,qin-files))))
               (buffer (generate-new-buffer "*dired-async-do-compress-to*"))
               (proc (start-file-process-shell-command proc-name buffer cmd))
               (sentinel
                (lambda (process event)
                  (pcase event
                    ("finished\n"
                     (message "Compressed %d file%s to %s" in-count
                              (ngettext "" "s" in-count)
                              (file-name-nondirectory out-file))
                     (kill-buffer (process-buffer process))
                     (dired-relist-file out-file))
                    ((rx bos "exited abnormally with code")
                     (dired-log (process-buffer process))
                     (dired-log t)
                     (message "Compress %s %s\nInspect %s buffer" out-file event dired-log-buffer)
                     (kill-buffer (process-buffer process)))))))
          (set-process-sentinel proc sentinel)))))))

(with-eval-after-load 'dired
  (define-key dired-mode-map "\C-c\C-t" 'dired-tags-prefix-map))

(with-eval-after-load 'disass
  (define-key emacs-lisp-mode-map "\C-c\C-d" 'disassemble)
  (define-key lisp-interaction-mode-map "\C-c\C-d" 'disassemble))

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

(define-key mode-specific-map "oe" 'ebdb)

(defvar ebdb-mode-map)
(defvar message-mode-map)

(with-eval-after-load 'ebdb-com
  (define-key ebdb-mode-map "\C-cm" 'ebdb-complete-push-mail-and-quit-window)
  (define-key ebdb-mode-map "\C-cM" 'ebdb-complete-push-mail))

(with-eval-after-load 'message
  (require 'ebdb-message)
  (define-key message-mode-map "\C-ce" 'ebdb-complete))

(define-key ediff-command-map "\C-k" 'ediff-backup)
(define-key ediff-command-map "\C-b" 'ediff-buffers)
(define-key ediff-command-map "\C-c" 'ediff-current-file)
(define-key ediff-command-map "\C-d" 'ediff-directories)
(define-key ediff-command-map [?\C-\S-v] 'ediff-directory-revisions)
(define-key ediff-command-map "\C-f" 'ediff-files)
(define-key ediff-command-map "\C-m\C-b" 'ediff-merge-buffers)
(define-key ediff-command-map "\C-m\C-d" 'ediff-merge-directories)
(define-key ediff-command-map [?\C-m ?\C-\S-v] 'ediff-merge-directory-revisions)
(define-key ediff-command-map "\C-m\C-f" 'ediff-merge-files)
(define-key ediff-command-map "\C-m\C-v" 'ediff-merge-revisions)
(define-key ediff-command-map "\C-p\C-b" 'ediff-patch-buffer)
(define-key ediff-command-map "\C-p\C-f" 'ediff-patch-file)
(define-key ediff-command-map "\C-r\C-l" 'ediff-regions-linewise)
(define-key ediff-command-map "\C-r\C-w" 'ediff-regions-wordwise)
(define-key ediff-command-map "\C-v" 'ediff-revision)
(define-key ediff-command-map "\C-w\C-l" 'ediff-windows-linewise)
(define-key ediff-command-map "\C-w\C-w" 'ediff-windows-wordwise)

(define-key ctl-x-map "E" 'edit-indirect-region)

(defvar eglot-mode-map)
(defvar eglot-server-programs)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map "\C-c\C-l" 'eglot-code-actions)
  (setf (alist-get '(js-mode typescript-mode) eglot-server-programs nil nil 'equal)
        '("typescript-language-server" "--tsserver-path" "tsserver" "--stdio"))

  (advice-add 'eglot-xref-backend :override 'xref-eglot+dumb-backend)

  (defun xref-eglot+dumb-backend () 'eglot+dumb)

  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
    (cons (xref-backend-identifier-at-point 'eglot)
          (xref-backend-identifier-at-point 'dumb-jump)))

  (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
    (xref-backend-identifier-completion-table 'eglot))

  (cl-defmethod xref-backend-definitions ((_backend (eql eglot+dumb)) identifier)
    (or (xref-backend-definitions 'eglot (car identifier))
        (xref-backend-definitions 'dumb-jump (cdr identifier))))

  (cl-defmethod xref-backend-references ((_backend (eql eglot+dumb)) identifier)
    (or (xref-backend-references 'eglot (car identifier))
        (xref-backend-references 'dumb-jump (cdr identifier))))

  (cl-defmethod xref-backend-apropos ((_backend (eql eglot+dumb)) pattern)
    (xref-backend-apropos 'eglot pattern)))

(add-hook 'nix-mode-hook 'eldoc-mode)

(define-key emacs-lisp-mode-map [?\C-c ?\C-\S-m] 'emacs-lisp-macroexpand)
(define-key lisp-interaction-mode-map [?\C-c ?\C-\S-m] 'emacs-lisp-macroexpand)
(with-eval-after-load 'elisp-mode
  (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

(setq completion-ignore-case t)
(define-key ctl-x-map "\C-\M-t" 'transpose-regions)
(define-key ctl-x-map "\C-l" 'load-command-map)

(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(setenv "PAGER" "cat")
(with-eval-after-load 'env
  (define-key global-map [?\C-\M-$] 'getenv))

(with-eval-after-load 'envrc
  (define-key envrc-command-map "R" 'envrc-reload-all)
  (define-key envrc-mode-map "\C-xd" 'envrc-command-map))

(with-eval-after-load 'files
  (define-key ctl-x-map "\C-r" 'region-commands-map)
  (define-key load-command-map "\C-l" 'load-library)
  (define-key load-command-map "\C-f" 'load-file)
  (define-key ctl-x-map "\C-d" 'ediff-command-map))

(with-eval-after-load 'files-x
  (define-key ctl-x-x-map "ad" 'add-dir-local-variable)
  (define-key ctl-x-x-map "aa" 'add-file-local-variable)
  (define-key ctl-x-x-map "ap" 'add-file-local-variable-prop-line))

(define-key search-map "n" 'find-name-dired)
(define-key search-map "N" 'find-dired)

(with-eval-after-load 'find-dired
  (let ((type '(const :tag "Sort file names by video duration" find-dired-sort-by-video-duration))
        (choices (cdr (get 'find-dired-refine-function 'custom-type))))
    (cl-pushnew type choices :test #'equal)
    (put 'find-dired-refine-function 'custom-type (cons 'choice choices)))

  (defun find-dired-sort-by-video-duration ()
    "Sort entries in *Find* buffer by video duration."
    (sort-subr nil 'forward-line 'end-of-line
               (lambda ()
                 (let ((file-name
                        (buffer-substring-no-properties
                         (next-single-property-change
                          (point) 'dired-filename)
                         (line-end-position))))
                   (with-temp-buffer
                     (call-process "video_seconds" nil '(t nil) nil file-name)
                     (string-to-number (buffer-string))))))))

(define-key ctl-x-map "L" 'find-library)
(define-key ctl-x-map "F" 'find-function)
(define-key ctl-x-map "K" 'find-function-on-key)
(define-key ctl-x-map "V" 'find-variable)

(dolist (fn '(find-library find-function find-function-on-key find-variable))
  (advice-add fn :before 'xref-push-marker-stack-ignore-args))

(define-key help-map "\M-c" 'finder-commentary)

(add-hook 'nix-mode-hook 'flymake-mode)
(defvar flymake-mode-map)
(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-g\M-f" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-g\M-b" 'flymake-goto-prev-error))

(add-hook 'js-mode-hook 'flymake-eslint-enable)

(add-hook 'nix-mode-hook 'flymake-statix-setup)
(with-eval-after-load 'nix-mode
  (with-eval-after-load 'flymake-statix
    (define-key nix-mode-map "\C-c\C-x" 'flymake-statix-fix)))

(add-hook 'nix-mode-hook 'format-all-mode)
(add-hook 'js-mode-hook 'format-all-mode)

(define-key search-map "g" 'rgrep)
(declare-function grep-expand-template@add-cut "grep" (cmd))
(with-eval-after-load 'grep
  (define-advice grep-expand-template (:filter-return (cmd) add-cut)
    (concat cmd " | cut -c-500")))

(define-key ctl-x-map "h" 'help-command)

(with-eval-after-load 'help-fns
  (define-key help-map "\M-f" 'describe-face)
  (define-key help-map "\M-k" 'describe-keymap))

(define-key global-map "\M-\\" 'hippie-expand)

(add-hook 'csv-mode-hook 'hl-line-mode)
(add-hook 'grep-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'transmission-files-mode-hook 'hl-line-mode)
(add-hook 'transmission-mode-hook 'hl-line-mode)
(add-hook 'transmission-peers-mode-hook 'hl-line-mode)
(add-hook 'mpc-mode-hook 'hl-line-mode)

(defvar ibuffer-mode-map)
(with-eval-after-load 'ibuffer (define-key ibuffer-mode-map "\M-o" nil))

(define-key lisp-interaction-mode-map "\C-j" 'ipretty-last-sexp)

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key isearch-mode-map "\C-?" isearch-help-map)

(defvar js-mode-map)
(with-eval-after-load 'js
  (define-key js-mode-map "\M-." nil))

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

(define-key load-command-map "\C-u" 'unload-feature)

(define-key search-map "l" 'locate)

(define-key project-prefix-map "m" 'magit-project-status)

(define-key help-map "\M-m" 'man)

(with-eval-after-load 'menu-bar
  (define-key ctl-x-map "`" 'toggle-debug-on-error))

(setq minibuffer-allow-text-properties t)
(define-key completion-in-region-mode-map "\M-v" 'switch-to-completions)
(define-key minibuffer-local-completion-map " " nil)
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
  (define-key mpc-songs-mode-map [remap mpc-select] nil)
  (define-key mpc-songs-mode-map "v" 'mpc-move-forward)
  (define-key mpc-songs-mode-map "V" 'mpc-move-backward))

(declare-function mpc-move-forward "mpc")
(declare-function mpc-songs-refresh "mpc")
(declare-function mpc-cmd-move "mpc")
(with-eval-after-load 'mpc
  (defun mpc-move-forward (n)
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
    (interactive "p")
    (mpc-move-forward (- n))))

(define-key mode-specific-map "nh" 'nslookup-host)
(define-key mode-specific-map "ni" 'ifconfig)
(define-key mode-specific-map "nn" 'netstat)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "nw" 'iwconfig)

(with-eval-after-load 'newcomment
  (define-key global-map [?\C-\;] 'comment-line))

(define-key mode-specific-map "on" 'newsticker-show-news)

(add-hook 'proced-mode-hook 'nix-prettify-mode)

(with-eval-after-load 'nix-mode
  (define-key nix-mode-map "\C-c\C-e" 'nix-edit)
  (define-key nix-mode-map "\C-c\C-f" 'nix-flake)
  (define-key nix-mode-map "\C-c\C-r" 'nix-repl)
  (define-key nix-mode-map "\C-c\C-s" 'nix-search)
  (define-key nix-mode-map "\C-c\C-p" 'nix-store-show-path))

(declare-function nix-edit@flake "nix-edit")
(with-eval-after-load 'nix-edit
  (define-advice nix-edit (:override () flake)
    (interactive)
    (let ((cmd (read-shell-command "Nix edit command: " "nix edit "))
          (process-environment (cons "EDITOR=echo" process-environment)))
      (find-file
       (with-temp-buffer
         (call-process-shell-command cmd nil (list (current-buffer) nil) nil)
         (buffer-substring-no-properties (point-min) (1- (point-max))))))))

(defvar nix-flake-ref)
(declare-function nix-flake--installable-command "nix-flake")
(declare-function nix-flake--build-attribute-names "nix-flake")
(declare-function nix-flake--options "nix-flake")
(declare-function nix-flake--registry-refs@all "nix-flake")
(declare-function nix-flake--registry-list "nix-flake")
(declare-function nix-flake-run-attribute@shell "nix-flake")
(declare-function nix-flake--run-attribute-names "nix-flake")
(with-eval-after-load 'nix-flake
  (define-advice nix-flake--registry-refs (:override () all)
    (cl-delete-duplicates
     (cl-remove
      "path:"
      (flatten-list (mapcar #'cdr (nix-flake--registry-list)))
      :test #'string-prefix-p)
     :test #'string=))

  (define-advice nix-flake-run-attribute
      (:override (options flake-ref attribute command-args &optional comint)
                 shell)
    (interactive (list (nix-flake--options)
                       nix-flake-ref
                       (completing-read "Nix app/package: "
                                        (nix-flake--run-attribute-names))
                       nil
                       (consp current-prefix-arg)))
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (compile (nix-flake--installable-command "run" options flake-ref attribute
                                               command-args)
               comint)))

  (defun nix-flake-log-attribute (options flake-ref attribute)
    "Log a derivation in the current flake.

For OPTIONS, FLAKE-REF, and ATTRIBUTE, see the documentation of
`nix-flake-run-attribute'."
    (interactive (list (nix-flake--options)
                       nix-flake-ref
                       (completing-read "Nix package: "
                                        (nix-flake--build-attribute-names))))
    (compile (nix-flake--installable-command "log" options flake-ref attribute)))

  (transient-append-suffix 'nix-flake-dispatch '(2 -1)
    '("l" "Log attribute" nix-flake-log-attribute)))

(declare-function nix-search--display@display-buffer "nix-search")
(with-eval-after-load 'nix-search
  (define-advice nix-search--display (:filter-args (args) display-buffer)
    (list (car args) (get-buffer-create "*Nix-Search*") (cddr args))))

(with-eval-after-load 'nix-shell
  (define-advice nix-read-flake (:override () always-prompt)
    (let ((default "nixpkgs"))
      (read-string (format-prompt "Nix flake" default) nil nil default))))

(defun nix-compile-in-project-advice (fn &rest args)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (apply fn args)))

(dolist (fn '(nix-flake-log-attribute
              nix-flake-run-default
              nix-flake-build-attribute
              nix-flake-build-default
              nix-flake-check
              nix-flake-lock
              nix-flake-update))
  (advice-add fn :around 'nix-compile-in-project-advice))

(add-hook 'nixos-options-mode-hook 'nix-prettify-mode)
(with-eval-after-load 'nix-mode
  (define-key nix-mode-map "\C-c\C-o" 'nixos-options))

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

(defvar org-mode-map)
(with-eval-after-load 'org
  (define-key org-mode-map [?\C-c?\C-\S-t] 'org-todo-yesterday))

(define-advice org-show-notification (:after (&rest _) sound)
  (call-process "notify_bruh" nil 0 nil))

(define-key mode-specific-map "Ga" 'org-agenda)
(defvar org-agenda-mode-map)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "T" 'org-agenda-todo-yesterday))

(define-key mode-specific-map "Gc" 'org-capture)

(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\M-o" 'org-mime-htmlize)
  (define-key message-mode-map "\C-c\M-e" 'org-mime-edit-mail-in-org-mode)
  (define-key message-mode-map "\C-c\M-t" 'org-mime-revert-to-plain-text-mail))

(define-key mode-specific-map "Gf" 'org-roam-node-find)
(define-key mode-specific-map "Gi" 'org-roam-node-insert)
(define-key mode-specific-map "Gl" 'org-roam-buffer-toggle)
(define-key mode-specific-map "Gs" 'org-roam-db-sync)
(declare-function org-roam-db-autosync-mode "org-roam-db" (&optional arg))
(with-eval-after-load 'org-roam (org-roam-db-autosync-mode))

(define-key global-map [?\C-\M-\S-t] 'transpose-paragraphs)

(declare-function pdf-loader-install "pdf-loader" (&optional no-query-p skip-dependencies-p no-error-p force-dependencies-p))
(pdf-loader-install t t)

(define-key emacs-lisp-mode-map "\C-c\C-m" 'pp-macroexpand-last-sexp)
(define-key lisp-interaction-mode-map "\C-c\C-m" 'pp-macroexpand-last-sexp)

(define-key mode-specific-map "op" 'proced)

(define-key mode-specific-map "ou" 'pueue)
(add-hook 'pueue-mode-hook 'hl-line-mode)

(define-key ctl-x-r-map "v" 'view-register)
(define-key ctl-x-r-map "L" 'list-registers)
(define-key ctl-x-r-map "p" 'prepend-to-register)
(define-key ctl-x-r-map "a" 'append-to-register)

(define-key emacs-lisp-mode-map "\C-c\C-r" 're-builder)
(define-key lisp-interaction-mode-map "\C-c\C-r" 're-builder)

(define-key region-commands-map "\C-k" 'keep-lines)
(define-key region-commands-map "\C-f" 'flush-lines)

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

(define-key mode-specific-map "ot" 'sdcwoc)

(defvar html-mode-map)
(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char)
  (define-key html-mode-map "\M-o" nil))

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

(defun cycle-spacing-fast (&optional n)
  (interactive "*p")
  (cycle-spacing n nil 'fast))

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
(define-key global-map "\M- " 'cycle-spacing-fast)
(define-key global-map "\M-\\" 'delete-indentation)
(define-key ctl-x-map "w" 'mark-whole-buffer)
(define-key ctl-x-x-map "f" 'auto-fill-mode)
(define-key ctl-x-x-map "v" 'visual-line-mode)
(define-key ctl-x-x-map "w" 'whitespace-mode)
(define-key mode-specific-map "oP" 'list-processes)

(add-hook 'nix-mode-hook 'skempo-mode)
(add-hook 'js-mode-hook 'skempo-mode)

(defvar skempo-mode-map)
(with-eval-after-load 'skempo
  (define-key skempo-mode-map "\C-z" 'skempo-complete-tag-or-call-on-region)
  (define-key skempo-mode-map "\M-g\M-e" 'skempo-forward-mark)
  (define-key skempo-mode-map "\M-g\M-a" 'skempo-backward-mark)
  (load (expand-file-name "emacs/skempo-templates.el" (xdg-config-home))))

(define-key region-commands-map "\C-d" 'delete-duplicate-lines)
(define-key region-commands-map "\C-l" 'sort-fields)
(define-key region-commands-map "\C-m" 'sort-columns)
(define-key region-commands-map "\C-n" 'sort-numeric-fields)
(define-key region-commands-map "\C-r" 'reverse-region)
(define-key region-commands-map "\C-s" 'sort-lines)
(define-key region-commands-map "\C-x" 'sort-regexp-fields)

(add-hook 'rust-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

(autoload 'center-region "text-mode")
(define-key region-commands-map "\C-c" 'center-region)

(define-key mode-specific-map "t" 'term)

(define-key ctl-x-x-map "T" 'tramp-cleanup-all-buffers)

(define-key mode-specific-map "or" 'transmission)
(defvar transmission-mode-map)
(declare-function transmission-request "transmission" (method &optional arguments tag))
(declare-function transmission-torrents "transmission" (response))
(declare-function transmission-draw-info@comment "transmission" (id))
(with-eval-after-load 'transmission
  (define-key transmission-mode-map "M" 'transmission-move)
  (define-advice transmission-draw-info (:after (id) comment)
    (let* ((arguments `(:ids ,id :fields ["comment"]))
           (response (transmission-request "torrent-get" arguments))
           (torrent (aref (transmission-torrents response) 0)))
      (insert "\nComment: " (or (cdr (assq 'comment torrent)) "")))))

(add-hook 'css-mode-hook 'tree-sitter-mode)
(add-hook 'js-mode-hook 'tree-sitter-mode)
(add-hook 'mhtml-mode-hook 'tree-sitter-mode)
(add-hook 'nix-mode-hook 'tree-sitter-mode)
(add-hook 'python-mode-hook 'tree-sitter-mode)
(add-hook 'rust-mode-hook 'tree-sitter-mode)

(declare-function url-generic-parse-url@save-match-data "url-parse" (fn &rest args))
(with-eval-after-load 'url-parse
  (define-advice url-generic-parse-url (:around (fn &rest args) save-match-data)
    (save-match-data (apply fn args))))

(add-to-list 'auto-mode-alist (cons (rx ".twig" eos) 'web-mode))

(defvar widget-field-keymap)
(defvar widget-text-keymap)
(with-eval-after-load 'wid-edit
  (define-key widget-field-keymap "\C-xnf" 'widget-narrow-to-field)
  (define-key widget-text-keymap "\C-xnf" 'widget-narrow-to-field))

(define-key global-map "\M-V" 'scroll-down-line)
(define-key global-map [?\C-\S-v] 'scroll-up-line)
(define-key global-map [?\C-\M-\S-b] 'previous-buffer)
(define-key global-map [?\C-\M-\S-f] 'next-buffer)
(define-key global-map "\M-Q" 'quit-window)
(define-key global-map "\M-o" 'other-window)

(define-key global-map [?\C-\M-&] 'with-editor-async-shell-command)

(autoload 'xref-push-marker-stack "xref")
(defun xref-push-marker-stack-ignore-args (&rest _)
  (xref-push-marker-stack))

(declare-function xdg-config-home "xdg" ())
(load (expand-file-name "emacs/custom.el" (xdg-config-home)) nil nil t)
