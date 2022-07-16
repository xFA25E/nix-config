;; -*- lexical-binding: t; -*-

(add-hook 'js-mode-hook 'abbrev-mode)

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

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-output-filter-functions 'comint-osc-process-output)

(with-eval-after-load 'compile
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

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

(defvar ebdb-mode-map)
(defvar message-mode-map)

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

(defvar flymake-mode-map)
(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-g\M-f" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-g\M-b" 'flymake-goto-prev-error))

(define-key search-map "g" 'rgrep)
(declare-function grep-expand-template@add-cut "grep" (cmd))
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

(define-key global-map [?\C-\M-\S-t] 'transpose-paragraphs)

(declare-function pdf-loader-install "pdf-loader" (&optional no-query-p skip-dependencies-p no-error-p force-dependencies-p))
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

(define-key mode-specific-map "ot" 'sdcwoc)

(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char))

(define-key mode-specific-map "s" 'shell)

(define-key mode-specific-map "l" 'shell-pwd-list-buffers)
(defvar shell-mode-map)
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

(defvar skempo-mode-map)
(with-eval-after-load 'skempo
  (define-key skempo-mode-map "\C-z" 'skempo-complete-tag-or-call-on-region)
  (define-key skempo-mode-map "\M-g\M-e" 'skempo-forward-mark)
  (define-key skempo-mode-map "\M-g\M-a" 'skempo-backward-mark)
  (load (expand-file-name "emacs/skempo-templates.el" (xdg-config-home))))

(add-hook 'rust-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

(define-key mode-specific-map "or" 'transmission)
(defvar transmission-mode-map)
(with-eval-after-load 'transmission
  (define-key transmission-mode-map "M" 'transmission-move))

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

(autoload 'xref-push-marker-stack "xref")
(defun xref-push-marker-stack-ignore-args (&rest _)
  (xref-push-marker-stack))

(declare-function xdg-config-home "xdg" ())
(load (expand-file-name "emacs/custom.el" (xdg-config-home)) nil nil t)
