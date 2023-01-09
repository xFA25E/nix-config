;; -*- lexical-binding: t; outline-regexp: ";;;\\(;*\\)"; -*-

(require 'cl-lib)
(require 'xdg)

(add-hook
 'after-init-hook
 (lambda ()
   (load (locate-user-emacs-file "custom.el") nil nil t)))

(define-prefix-command 'load-command-map)
(define-key ctl-x-map "\C-l" 'load-command-map)

(define-prefix-command 'region-commands-map)
(define-key ctl-x-map "\C-r" 'region-commands-map)

;;; Abbrev

(add-hook 'js-mode-hook 'abbrev-mode)

;;; Align

(define-key 'region-commands-map "\C-a" 'align-regexp)

;;; Apheleia

(autoload 'apheleia-mode "apheleia")
(add-hook 'js-mode-hook 'apheleia-mode)
(add-hook 'nix-mode-hook 'apheleia-mode)
(add-hook 'css-mode-hook 'apheleia-mode)
(add-hook 'html-mode-hook 'apheleia-mode)
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
  (define-key compilation-shell-minor-mode-map "\C-c\C-g" 'recompile))

(declare-function compile-goto-error "compile")
(defun compile-goto-error-same-window ()
  "Run `compile-goto-error' in the same window."
  (interactive)
  (xref-push-marker-stack)
  (same-window-prefix)
  (compile-goto-error))

;;; Consult

(defvar kmacro-keymap)
(define-key global-map "\M-H" 'consult-history)
(define-key goto-map "E" 'consult-compile-error)
(define-key goto-map "F" 'consult-flymake)
(define-key goto-map "i" 'consult-imenu)
(define-key goto-map "o" 'consult-outline)
(define-key kmacro-keymap "c" 'consult-kmacro)
(define-key project-prefix-map "i" 'consult-project-imenu)

(with-eval-after-load 'consult
  (add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode))

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
  (define-key dired-mode-map "\M-+" 'dired-create-empty-file))

(defvar dired-compress-file-suffixes)
(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               (list (rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

(defvar dired-compress-files-alist)
(defvar dired-log-buffer)
(declare-function cl-find-if "cl-lib")
(declare-function dired-get-marked-files "dired")
(declare-function dired-log "dired")
(declare-function dired-relist-file "dired-aux")
(defun dired-do-compress-to@async (&optional arg)
  "Like `dired-do-compress-to', but asynchronous.
See the original function for ARG."
  (interactive "P")
  (require 'format-spec)
  (let* ((in-files (dired-get-marked-files nil arg nil nil t))
         (out-file (expand-file-name (read-file-name "Compress to: ")))
         (rule (cl-find-if (lambda (x) (string-match-p (car x) out-file))
                           dired-compress-files-alist)))
    (cond
     ((not rule)
      (error
       "No compression rule found for %s, see `dired-compress-files-alist'"
       out-file))
     ((and (file-exists-p out-file)
           (not (y-or-n-p (format "%s exists, overwrite?"
                                  (abbreviate-file-name out-file)))))
      (message "Compression aborted"))
     (t
      (let* ((in-count 0)
             (proc-name (concat "compress " out-file))
             (qout-file (shell-quote-argument out-file))
             (qin-files (mapconcat
                         (lambda (file) (cl-incf in-count)
                           (shell-quote-argument (file-name-nondirectory file)))
                         in-files " "))
             (cmd (format-spec (cdr rule) `((?\o . ,qout-file)
                                            (?\i . ,qin-files))))
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
                   (message "Compress %s %s\nInspect %s buffer"
                            out-file event dired-log-buffer)
                   (kill-buffer (process-buffer process)))))))
        (set-process-sentinel proc sentinel))))))

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

(define-prefix-command 'ediff-command-map)
(define-key ctl-x-map "\C-d" 'ediff-command-map)
(define-key 'ediff-command-map "\C-b" 'ediff-buffers)
(define-key 'ediff-command-map "\C-c" 'ediff-current-file)
(define-key 'ediff-command-map "\C-d" 'ediff-directories)
(define-key 'ediff-command-map "\C-f" 'ediff-files)
(define-key 'ediff-command-map "\C-k" 'ediff-backup)
(define-key 'ediff-command-map "\C-m\C-b" 'ediff-merge-buffers)
(define-key 'ediff-command-map "\C-m\C-d" 'ediff-merge-directories)
(define-key 'ediff-command-map "\C-m\C-f" 'ediff-merge-files)
(define-key 'ediff-command-map "\C-m\C-v" 'ediff-merge-revisions)
(define-key 'ediff-command-map "\C-p\C-b" 'ediff-patch-buffer)
(define-key 'ediff-command-map "\C-p\C-f" 'ediff-patch-file)
(define-key 'ediff-command-map "\C-r\C-l" 'ediff-regions-linewise)
(define-key 'ediff-command-map "\C-r\C-w" 'ediff-regions-wordwise)
(define-key 'ediff-command-map "\C-v" 'ediff-revision)
(define-key 'ediff-command-map "\C-w\C-l" 'ediff-windows-linewise)
(define-key 'ediff-command-map "\C-w\C-w" 'ediff-windows-wordwise)
(define-key 'ediff-command-map [?\C-\S-v] 'ediff-directory-revisions)
(define-key 'ediff-command-map [?\C-m ?\C-\S-v] 'ediff-merge-directory-revisions)

;;; Edit Indirect

(define-key ctl-x-map "E" 'edit-indirect-region)

;;; Eglot

(defvar eglot-mode-map)
(defvar eglot-server-programs)
(declare-function eglot-alternatives "eglot")
(with-eval-after-load 'eglot
  (define-key eglot-mode-map "\C-c\C-l" 'eglot-code-actions)

  (setf (cdr (assoc '(js-mode typescript-mode) eglot-server-programs))
        '("typescript-language-server" "--tsserver-path" "tsserver" "--stdio"))

  (setf (cdr (assq 'csharp-mode eglot-server-programs))
        (eglot-alternatives '("CSharpLanguageServer" ("OmniSharp" "-lsp")))))

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

;;; Files

(define-key 'load-command-map "\C-f" 'load-file)
(define-key 'load-command-map "\C-l" 'load-library)

;;; Files X

(define-key ctl-x-x-map "ad" 'add-dir-local-variable)
(define-key ctl-x-x-map "aa" 'add-file-local-variable)
(define-key ctl-x-x-map "ap" 'add-file-local-variable-prop-line)

;;; Find Dired

(define-key search-map "N" 'find-dired)
(define-key search-map "n" 'find-name-dired)

(with-eval-after-load 'find-dired
  (cl-pushnew '(const :tag "Sort file names by video duration"
                      find-dired-sort-by-video-duration)
              (cdr (get 'find-dired-refine-function 'custom-type))
              :test #'equal))

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
                   (string-to-number (buffer-string)))))))

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

(define-key search-map "g" 'rgrep)

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
        (concat "\\(  \\|\t\\| \t\\)[ \t]*-?"
                "\\(?:" ledger-commodity-regexp " *\\)?"
                "\\([-=]?\\(?:[0-9]+\\|[0-9,.]+?\\)\\)"
                "\\([,.][0-9)]+\\)?"
                "\\(?: *" ledger-commodity-regexp "\\)?"
                "\\([ \t]*[@={]@?[^\n;]+?\\)?"
                "\\([ \t]+;.+?\\|[ \t]*\\)?$")))

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

;;; Locate

(define-key search-map "l" 'locate)

;;; Magit

(define-key project-prefix-map "m" 'magit-project-status)

;;; Man

(define-key help-map "\M-m" 'man)

;;; Menu Bar

(define-key ctl-x-map "`" 'toggle-debug-on-error)

;;; Minibuffer

(setq minibuffer-allow-text-properties t)

(define-key completion-in-region-mode-map "\M-v" 'switch-to-completions)
(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map "\C-j" 'minibuffer-force-complete-and-exit)

(defun regexp-try-completion (string table pred point)
  (let ((completions (all-completions "" table pred)))
    (when (cl-some (apply-partially #'string-match-p string) completions)
      (cons string point))))

(defun regexp-all-completions (string table pred _point)
  (cl-loop
   for completion in (all-completions "" table pred)
   if (string-match string completion)
   do (add-face-text-property (match-beginning 0) (match-end 0)
                              'completions-common-part nil completion)
   and collect completion))

(add-to-list 'completion-styles-alist
             '(regexp regexp-try-completion regexp-all-completions "regexp"))
(cl-pushnew '(const regexp)
            (cdar (cdddr (get 'completion-styles 'custom-type))))

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

(defvar mpc-cover-image-re)
(declare-function mpc-constraints-push "mpc")
(declare-function mpc-constraints-restore "mpc")
(declare-function mpc-file-local-copy "mpc")
(declare-function mpc-secs-to-time "mpc")
(declare-function mpc-tempfiles-add "mpc")
(define-advice mpc-format (:override (format-spec info &optional hscroll) hash)
  (let* ((pos 0)
         (start (point))
         (col (if hscroll (- hscroll) 0))
         (insert (lambda (str)
                   (cond
                    ((>= col 0) (insert str))
                    (t (insert (substring str (min (length str) (- col))))))))
         (pred #'always))
    (while (string-match "%\\(?:%\\|\\(-\\)?\\([0-9]+\\)?{\\([[:alpha:]][[:alnum:]]*\\)\\(?:-\\([^}]+\\)\\)?}\\)" format-spec pos)
      (let ((pre-text (substring format-spec pos (match-beginning 0))))
        (funcall insert pre-text)
        (setq col (+ col (string-width pre-text))))
      (setq pos (match-end 0))
      (if (null (match-end 3))
          (progn
            (funcall insert "%")
            (setq col (+ col 1)))
        (let* ((size (match-string 2 format-spec))
               (tag (intern (match-string 3 format-spec)))
               (post (match-string 4 format-spec))
               (right-align (match-end 1))
               (text
                (if (eq info 'self) (symbol-name tag)
                  (pcase tag
                    ((or 'Time 'Duration)
                     (let ((time (cdr (or (assq 'time info) (assq 'Time info)))))
                       (setq pred #'ignore) ;Just assume it's never eq.
                       (when time
                         (mpc-secs-to-time (if (and (eq tag 'Duration)
                                                    (string-match ":" time))
                                               (substring time (match-end 0))
                                             time)))))
                    ('Cover
                     (let ((dir (file-name-directory (cdr (assq 'file info)))))
                       ;; (debug)
                       (setq pred
                             ;; We want the closure to capture the current
                             ;; value of `pred' and not a reference to the
                             ;; variable itself.
                             (let ((oldpred pred))
                               (lambda (info)
                                 (and (funcall oldpred info)
                                      (equal dir (file-name-directory
                                                  (cdr (assq 'file info))))))))
                       (if-let* ((covers '(".folder.png" "folder.png" "cover.jpg" "folder.jpg"))
                                 (cover (cl-loop for file in (directory-files (mpc-file-local-copy dir))
                                                 if (or (member (downcase file) covers)
                                                        (and mpc-cover-image-re
                                                             (string-match mpc-cover-image-re file)))
                                                 return (concat dir file)))
                                 (file (with-demoted-errors "MPC: %s"
                                         (mpc-file-local-copy cover))))
                           (let (image)
                             (if (null size)
                                 (setq image (create-image file))
                               (let* ((hash (with-temp-buffer
                                              (insert-file-contents-literally file)
                                              (md5 (current-buffer))))
                                      (dir (expand-file-name "emacs/mpc" (xdg-cache-home)))
                                      (scaled-file (expand-file-name (concat hash ".jpg") dir)))
                                 (unless (file-exists-p scaled-file)
                                   (make-directory dir t)
                                   ;; FIXME: Use native image scaling instead.
                                   (call-process "convert" nil nil nil
                                                 "-scale" size file scaled-file))
                                 (setq image (create-image scaled-file))
                                 (mpc-tempfiles-add image scaled-file)))
                             (setq size nil)
                             (propertize dir 'display image))
                         ;; Make sure we return something on which we can
                         ;; place the `mpc--uptodate-p' property, as
                         ;; a negative-cache.  We could also use
                         ;; a default cover.
                         (progn (setq size nil) " "))))
                    (_ (let ((val (cdr (assq tag info))))
                         ;; For Streaming URLs, there's no other info
                         ;; than the URL in `file'.  Pretend it's in `Title'.
                         (when (and (null val) (eq tag 'Title))
                           (setq val (cdr (assq 'file info))))
                         (setq pred
                               ;; We want the closure to capture the current
                               ;; value of `pred' and not a reference to the
                               ;; variable itself.
                               (let ((oldpred pred))
                                 (lambda (info)
                                   (and (funcall oldpred info)
                                        (equal val (cdr (assq tag info)))))))
                         (cond
                          ((not (and (eq tag 'Date) (stringp val))) val)
                          ;; For "date", only keep the year!
                          ((string-match "[0-9]\\{4\\}" val)
                           (match-string 0 val))
                          (t val)))))))
               (space (when size
                        (setq size (string-to-number size))
                        (propertize " " 'display
                                    (list 'space :align-to (+ col size)))))
               (textwidth (if text (string-width text) 0))
               (postwidth (if post (string-width post) 0)))
          (when text
            (let ((display
                   (if (and size
                            (> (+ postwidth textwidth) size))
                       (propertize
                        (truncate-string-to-width text size nil nil "…")
                        'help-echo text)
                     text)))
              (when (memq tag '(Artist Album Composer)) ;FIXME: wrong list.
                (setq display
                      (propertize display
                                  'mouse-face 'highlight
                                  'follow-link t
                                  'keymap `(keymap
                                            (mouse-2
                                             . ,(lambda ()
                                                  (interactive)
                                                  (mpc-constraints-push 'noerror)
                                                  (mpc-constraints-restore
                                                   ',(list (list tag text)))))))))
              (funcall insert
                       (concat (when size
                                 (propertize " " 'display
                                             (list 'space :align-to
                                                   (+ col
                                                      (if (and size right-align)
                                                          (- size postwidth textwidth)
                                                        0)))))
                               display post))))
          (if (null size) (setq col (+ col textwidth postwidth))
            (insert space)
            (setq col (+ col size))))))
    ;; Print the rest of format-spec, in case there is text after the
    ;; last actual format specifier.
    (insert (substring format-spec pos))
    (put-text-property start (point) 'mpc--uptodate-p pred)))

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

(with-eval-after-load 'nix-mode
  (define-key nix-mode-map "\C-c\C-e" 'nix-edit)
  (define-key nix-mode-map "\C-c\C-f" 'nix-flake)
  (define-key nix-mode-map "\C-c\C-r" 'nix-repl)
  (define-key nix-mode-map "\C-c\C-s" 'nix-search)
  (define-key nix-mode-map "\C-c\C-p" 'nix-store-show-path))

(define-advice nix-edit (:override () flake)
  (interactive)
  (let ((cmd (read-shell-command "Nix edit command: " "nix edit "))
        (process-environment (cons "EDITOR=echo" process-environment)))
    (find-file
     (with-temp-buffer
       (call-process-shell-command cmd nil (list (current-buffer) nil) nil)
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
(defvar project-compilation-buffer-name-function)
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
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (compile (nix-flake--installable-command "run" options flake-ref attribute
                                             command-args)
             comint)))

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
    '("l" "Log attribute" nix-flake-log-attribute)))

(define-advice nix-search--display (:filter-args (args) display-buffer)
  (list (car args) (get-buffer-create "*Nix-Search*") (cddr args)))

(define-advice nix-read-flake (:override () always-prompt)
  (let ((default "nixpkgs"))
    (read-string (format-prompt "Nix flake" default) nil nil default)))

(defun nix-compile-in-project-advice (fn &rest args)
  "Change compilation buffer name in FN with ARGS.
Used as an advice."
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

;;; Nixos Options

(add-hook 'nixos-options-mode-hook 'nix-prettify-mode)
(with-eval-after-load 'nix-mode
  (define-key nix-mode-map "\C-c\C-o" 'nixos-options))

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

;;; Ob Http

(with-eval-after-load 'org
  (cl-pushnew
   '(const :tag "Http" http)
   (cdadr (memq :key-type (get 'org-babel-load-languages 'custom-type)))
   :test 'equal))

;;; Org

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

;;; Replace

(define-key 'region-commands-map "\C-k" 'keep-lines)
(define-key 'region-commands-map "\C-f" 'flush-lines)

;;; Reverse Im

(require 'reverse-im)
(reverse-im-activate "cyrillic-dvorak")

;;; Rg

(define-key search-map "r" 'rg-menu)

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

;;; Sort

(define-key 'region-commands-map "\C-d" 'delete-duplicate-lines)
(define-key 'region-commands-map "\C-l" 'sort-fields)
(define-key 'region-commands-map "\C-m" 'sort-columns)
(define-key 'region-commands-map "\C-n" 'sort-numeric-fields)
(define-key 'region-commands-map "\C-r" 'reverse-region)
(define-key 'region-commands-map "\C-s" 'sort-lines)
(define-key 'region-commands-map "\C-x" 'sort-regexp-fields)

;;; Subr X

(put 'thread-first 'lisp-indent-function 1)
(put 'thread-last 'lisp-indent-function 1)

;;; Subword

(add-hook 'csharp-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)
(add-hook 'rust-mode-hook 'subword-mode)

;;; Tempo Abbrev

(declare-function tempo-save-named "tempo")
(declare-function tempo-abbrev-abbrev-table "tempo-abbrev")
(declare-function tempo-abbrev-define "tempo-abbrev")

(define-advice tempo-define-template (:filter-return (fn) compile)
  (cl-callf byte-compile (symbol-function fn))
  fn)

(with-eval-after-load 'tempo-abbrev
  (define-key global-map "\C-z" 'tempo-abbrev-call)
  (define-key goto-map "\M-e" 'tempo-forward-mark)
  (define-key goto-map "\M-a" 'tempo-backward-mark)
  (add-hook 'tempo-user-elements 'tempo-abbrev-user-elements))

(defun tempo-abbrev-lisp-enable ()
  (or (eq this-command 'expand-abbrev) (eql ?\s last-command-event)))

(defun tempo-abbrev-user-elements (element)
  (pcase element
    ;; Skeleton constructs
    (`(:if (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) ,then ,else)
     (let ((input (read-from-minibuffer prompt)))
       (if (string-empty-p input)
           else
         (tempo-save-named var input)
         then)))

    (`(:when (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:if (,prompt ,var) (l ,@body) (l)))

    (`(:while (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:when (,prompt ,var) ,@body ,element))

    ;; Lisp
    (`(:with-parens . ,body)
     (if (or (not (eql (char-before) ?\()) (use-region-p))
         `(l "(" ,@body ")")
       `(l ,@body)))

    (:elisp-namespace
     (thread-last (if-let ((bfn (buffer-file-name)))
                      (file-name-nondirectory bfn)
                    (buffer-name))
       downcase
       (string-remove-suffix ".el")
       (replace-regexp-in-string (rx (+ (not (any "a-z")))) "-")
       (replace-regexp-in-string (rx (* "-") eos) "")
       (replace-regexp-in-string (rx bos (* "-")) "")))

    (:elisp-prefix
     (let ((prefix (concat (tempo-abbrev-user-elements :elisp-namespace) "-")))
       (or (car (cl-find prefix read-symbol-shorthands
                         :key #'cdr :test #'string=))
           prefix)))

    (:elisp-group
     (thread-last :elisp-namespace
       tempo-abbrev-user-elements
       (string-remove-suffix "-mode")))

    ;; Nix
    (:nix-hash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")

    ;; Org
    (:changelog-last-version
     (save-excursion
       (goto-char (point-min))
       (search-forward-regexp (rx bol "** [" (group (+ (not (in "]\n")))) "]"))
       (substring-no-properties (match-string 1))))

    (:date (format-time-string "%Y-%m-%d"))))

;;;; Tempo Abbrev Elisp

(with-eval-after-load 'elisp-mode
  (require 'tempo-abbrev)

  (define-abbrev-table (tempo-abbrev-abbrev-table 'emacs-lisp-mode)
    nil :enable-function 'tempo-abbrev-lisp-enable)

  (tempo-abbrev-define "defvar" 'emacs-lisp-mode
    '((:with-parens
       "defvar " :elisp-prefix p n>
       r> n>
       "\"" p "\"")))

  (tempo-abbrev-define "defun" 'emacs-lisp-mode
    '((:with-parens
       "defun " :elisp-prefix p " (" p ")" n>
       "\"" p "\"" n>
       r>)))

  (tempo-abbrev-define "defgroup" 'emacs-lisp-mode
    '((:with-parens
       "defgroup " :elisp-group " nil" n>
       "\"" p "\"" n>
       ":group " p "nil")))

  (tempo-abbrev-define "defcustom" 'emacs-lisp-mode
    '((:with-parens
       "defcustom " :elisp-prefix p n>
       r> n>
       "\"" p "\"" n>
       ":type " p "nil" n>
       ":group '" :elisp-group)))

  (tempo-abbrev-define "defface" 'emacs-lisp-mode
    '((:with-parens
       "defface " :elisp-prefix p n>
       "'((t :inherit " p "nil))" n>
       "\"" p "\"" n>
       ":group '" :elisp-group))))


;;;; Tempo Abbrev Js

(with-eval-after-load 'js
  (require 'tempo-abbrev)

  (tempo-abbrev-define "switch" 'js-mode
    '("switch (" p ") {" n>
      (:while ("Pattern: " pat)
              "case " (s pat) ":" > n>
              p n>
              "break;" n>)
      "default:" > n>
      p n>
      "break;" n>
      "}" >))

  (tempo-abbrev-define "function" 'js-mode
    '("function " p "(" p ") {" n>
      r> n>
      "}" >))

  (tempo-abbrev-define "if" 'js-mode
    '("if (" p ") {" n>
      r> n>
      "}" >))

  (tempo-abbrev-define "for" 'js-mode
    '("for (" p ") {" n>
      r> n>
      "}" >))

  (tempo-abbrev-define "try" 'js-mode
    '("try {" n>
      r> n>
      "} catch (" p "error) {" > n>
      p n>
      "}" >))

  (tempo-abbrev-define "clog" 'js-mode '("console.log(" r ")"))

  (tempo-abbrev-define "ctime" 'js-mode
    '("console.time(\"" (P "Time name: " time) "\");" > n>
      r> n>
      "console.timeEnd(\"" (s time) "\");" >)))

;;;; Tempo Abbrev Lisp

(with-eval-after-load 'lisp-mode
  (require 'tempo-abbrev)

  (define-abbrev-table (tempo-abbrev-abbrev-table 'lisp-mode)
    nil :enable-function 'tempo-abbrev-lisp-enable)

  (tempo-abbrev-define "lambda" 'lisp-mode
    '((:with-parens "lambda (" p ") " r>)))

  (tempo-abbrev-define "let" 'lisp-mode
    '((:with-parens
       "let ((" p "))" n>
       r>)))

  (tempo-abbrev-define "defvar" 'lisp-mode
    '((:with-parens
       "defvar " p n>
       r> n>
       "\"" p "\"")))

  (tempo-abbrev-define "defun" 'lisp-mode
    '((:with-parens
       "defun " p " (" p ")" n>
       "\"" p "\"" n>
       r>)))

  (tempo-abbrev-define "defpackage" 'lisp-mode
    '((:with-parens
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
       "(in-package #:" (s package) ")" n>)))

  (tempo-abbrev-define "defsystem" 'lisp-mode
    '((:with-parens
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
       ":perform (test-op (op c) (symbol-call '#:fiveam '#:run! (find-symbol* '#:" (s system) " '#:" (s system) ".tests)))"))))

;;;; Tempo Abbrev Nix

(with-eval-after-load 'nix-mode
  (require 'tempo-abbrev)

  (tempo-abbrev-define "fetchurl" 'nix-mode
    '("fetchurl {" n>
      "url = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >))

  (tempo-abbrev-define "fetchzip" 'nix-mode
    '("fetchzip {" n>
      "url = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >))

  (tempo-abbrev-define "fetchgit" 'nix-mode
    '("fetchgit {" n>
      "url = \"" p "\";" n>
      "rev = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >))

  (tempo-abbrev-define "fetchFromGitHub" 'nix-mode
    '("fetchFromGitHub {" n>
      "owner = \"" p "\";" n>
      "repo = \"" p "\";" n>
      "rev = \"" p "\";" n>
      "hash = \"" p :nix-hash "\";" n>
      "}" p >)))

;;;; Tempo Abbrev Org

(with-eval-after-load 'org
  (require 'tempo-abbrev)

  (tempo-abbrev-define "changelog" 'org-mode
    '("** [" :changelog-last-version p "] - Unreleased" n n
      "*** Added" n n
      "*** Changed" n n
      "*** Deprecated" n n
      "*** Removed" n n
      "*** Fixed" n n
      "*** Security"))

  (tempo-abbrev-define "Unreleased" 'org-mode '(:date)))

;;; Tex Mode

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

;;; Text Mode

(autoload 'center-region "text-mode")
(define-key 'region-commands-map "\C-c" 'center-region)

;;; Term

(define-key mode-specific-map "t" 'term)

;;; Tramp

(define-key ctl-x-x-map "T" 'tramp-cleanup-all-buffers)

;;; Transmission

(define-key mode-specific-map "or" 'transmission)

(defvar transmission-mode-map)
(with-eval-after-load 'transmission
  (define-key transmission-mode-map "M" 'transmission-move))

(declare-function transmission-request "transmission")
(declare-function transmission-torrents "transmission")
(define-advice transmission-draw-info (:after (id) comment)
  (let* ((arguments `(:ids ,id :fields ["comment"]))
         (response (transmission-request "torrent-get" arguments))
         (torrent (aref (transmission-torrents response) 0)))
    (insert "\nComment: " (or (cdr (assq 'comment torrent)) ""))))

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

(define-key global-map [?\C-\M-&] 'with-editor-async-shell-command)

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
