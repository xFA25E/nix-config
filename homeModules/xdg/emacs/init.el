;;; init --- Config -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: you can conditionally load packages if binaries are present
;; TODO: maybe we should defer by default
;; TODO: deal intelligently with faces
;; TODO: see if you can parametrize image/video extensions
;; TODO: see what org modules are useful

;;;; WINDOWS

;; archive tooling: zip? xz? gz? tar?
;; xmlint sucks, find another
;; do the google calendar and org agenda thingie
;; something to write docstrings
;; set powershell eldoc def files
;; find powershell tree sitter
;; set completion for powershell scripts and shell
;; use psscriptanalyzer with flymake to highlight warnings
;; something to open c# documentation from emacs

;; connect to remote desktop using emacs

;; try omnisharp

;; you can override csharp treesit bug instead of recreating entire list.  also
;; you can customize features

;; dotnet transient wrapper (I WANT THIS!)

;; there is jarchive to open jar files with eglot.  See if there is something
;; for .NET.

;; fix ^M issues with eglot, maybe before-save-hook

;; since you've installed git, you can just use your ssh key to push changes to
;; init.el file

;; maybe you can repait flymake after aphelia saved file

;; Config

;;; Code:

(require 'cl-lib)
(require 'xdg)

(define-keymap :prefix 'load-command-map)
(keymap-set ctl-x-map "C-l" 'load-command-map)

(setq package-archives nil)
(setq package-selected-packages nil)

(when (eq 'windows-nt system-type)
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")
          ("jcs-emacs" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

  (setq package-vc-selected-packages
        '((tempo-extra :vc-backend Git :url "https://github.com/xFA25E/tempo-extra")
          (abbrev-hook :vc-backend Git :url "https://github.com/xFA25E/abbrev-hook")
          (rx-widget :vc-backend Git :url "https://github.com/xFA25E/rx-widget")))

  (when-let ((font (cl-first (member "Segoe UI Emoji" (font-family-list)))))
    (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

(use-package abbrev
  :hook
  csharp-mode csharp-ts-mode
  emacs-lisp-mode
  js-ts-mode
  lisp-mode
  nix-mode nix-ts-mode
  shell-mode
  text-mode
  :custom
  (abbrev-file-name "~/Documents/projects/nix-config/homeModules/xdg/emacs/abbrev_defs")
  (abbrev-suggest t))

(use-package abbrev-hook
  :vc "https://github.com/xFA25E/abbrev-hook"
  :bind ("C-z" . abbrev-hook-call))

;; (use-package ace-window
;;   :ensure t
;;   :bind ("M-o" . ace-window)

;;   :custom
;;   (aw-keys (string-to-list "htnsaoeuid"))
;;   (aw-scope 'frame)
;;   (aw-background t)
;;   (aw-leading-char-style 'path)
;;   (aw-dispatch-always t)
;;   (aw-minibuffer-flag t)
;;   (aw-fair-aspect-ratio 6)
;;   (aw-dispatch-alist
;;    '((?m aw-swap-window "Swap Windows")
;;      (?M aw-move-window "Move Window")
;;      (?r aw-flip-window)
;;      (?f aw-find-file-in-window "Find File In Window")
;;      (?F aw-find-file-other-window "Find File Other Window")
;;      (?b aw-switch-buffer-in-window "Select Buffer")
;;      (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
;;      (?w aw-split-window-fair "Split Fair Window")
;;      (?k aw-delete-window "Delete Window")
;;      (?K delete-other-windows "Delete Other Windows")
;;      (?? aw-show-dispatch-help)))

;;   :config
;;   (defun aw-find-file-in-window (window)
;;     "Find file in WINDOW."
;;     (aw-switch-to-window window)
;;     (call-interactively #'find-file))

;;   (defun aw-find-file-other-window (window)
;;     "Find file other WINDOW."
;;     (aw-switch-to-window window)
;;     (call-interactively #'find-file)
;;     (aw-flip-window)))

(use-package affe
  :ensure t
  :bind
  (:map search-map
        ("M-g M-z" . affe-grep)
        ("M-f M-z" . affe-find)))

(use-package amded
  :vc "https://github.com/xFA25E/amded"
  :defer t
  :custom
  (amded-editable-tags '("album" "artist" "genre" "track-number" "track-title" "year")))

(use-package ange-ftp
  :defer t
  :custom (ange-ftp-netrc-filename "~/.authinfo.gpg"))

(use-package ansi-color
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  (shell-mode . ansi-color-for-comint-mode-on))

(use-package ansi-osc
  :hook (compilation-filter . ansi-osc-compilation-filter))

(use-package apheleia
  :ensure t
  :hook
  csharp-mode
  csharp-ts-mode
  css-ts-mode
  html-mode
  js-ts-mode
  nix-mode
  nix-ts-mode
  nxml-mode
  web-mode)

(use-package apheleia-formatters
  :ensure apheleia
  :defer t
  :custom (apheleia-mode-lighter nil)
  :config
  (setf (alist-get 'alejandra apheleia-formatters) (list "alejandra"))
  (setf (alist-get 'csharpier apheleia-formatters) (list "dotnet-csharpier"))
  (setf (alist-get 'xmllint apheleia-formatters) (list "xmllint" "--format" "-" "--pretty" "2"))

  (setf (alist-get 'nxml-mode apheleia-mode-alist) 'xmllint)
  (setf (alist-get 'csharp-mode apheleia-mode-alist) 'csharpier)
  (setf (alist-get 'csharp-ts-mode apheleia-mode-alist) 'csharpier)
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'alejandra)
  (setf (alist-get 'nix-ts-mode apheleia-mode-alist) 'alejandra)
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'shfmt))

(use-package apropos
  :defer t
  :custom (apropos-sort-by-scores t))

(use-package autoinsert
  :defer t
  :custom (auto-insert-mode t))

(use-package auth-source
  :defer t
  :custom (auth-sources '("~/.authinfo.gpg" "~/.netrc" "~/.authinfo")))

(use-package autorevert
  :defer t
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-mode-text " AR")
  (auto-revert-remote-files t)
  (global-auto-revert-mode t))

(use-package avy
  :ensure t
  :bind
  ("M-z" . avy-goto-char-timer)
  (:map goto-map ("M-g" . avy-goto-line))
  (:map isearch-mode-map ("M-z" . avy-isearch))
  :custom
  (avy-background t)
  (avy-goto-word-0-regexp "\\_<\\(\\sw\\|\\s_\\)")
  (avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (avy-style 'words)
  (avy-timeout-seconds 0.3))

(use-package avy-embark-collect
  :ensure t
  :defer t)

;; TODO: handle absence of battery
(use-package battery
  :preface
  (require 'notifications)
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

  :custom
  (battery-update-functions '(battery-alarm-on-low-level))
  (display-battery-mode t)

  :config
  (defvar battery-previous-percentage
    (if battery-status-function
        (thread-last battery-status-function
          funcall
          (alist-get ?p)
          string-to-number)
      100.0)))

(use-package bindings
  :defer t
  :init
  (keymap-unset esc-map "&" t)
  (add-to-list 'completion-ignored-extensions ".dir-locals.el")
  (add-to-list 'completion-ignored-extensions ".direnv/")
  (add-to-list 'completion-ignored-extensions ".editorconfig")
  (add-to-list 'completion-ignored-extensions ".eldev/")
  (add-to-list 'completion-ignored-extensions ".envrc")
  (add-to-list 'completion-ignored-extensions ".gitignore")
  (add-to-list 'completion-ignored-extensions ".prettierignore")
  (add-to-list 'completion-ignored-extensions "Eldev")
  (add-to-list 'completion-ignored-extensions "Eldev-local")
  (add-to-list 'completion-ignored-extensions "LICENSE")
  (add-to-list 'completion-ignored-extensions "flake.lock"))

(use-package bookmark
  :defer t
  :custom
  (bookmark-default-file (expand-file-name "emacs/bookmarks" (xdg-data-home)))
  (bookmark-fontify nil)
  (bookmark-menu-confirm-deletion t)
  (bookmark-save-flag 1))

(use-package browse-url
  :bind (:map ctl-x-map ("B" . browse-url))
  :custom
  (browse-url-browser-function 'browse-url-choices)
  (browse-url-generic-program "brave-incognito")
  (browse-url-secondary-browser-function 'browse-url)
  :config
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
      (apply (nth 3 (assoc answer answers)) url args))))

(use-package buffer
  :defer t
  :custom
  (fill-column 80)
  (indicate-buffer-boundaries 'left)
  (indicate-empty-lines t)
  (tab-width 4)
  (truncate-lines t)

  :hook ((csharp-mode csharp-ts-mode) . set-fill-column-in-csharp)
  :config
  (defun set-fill-column-in-csharp ()
    "Set `fill-column'."
    (setq-local fill-column 120)))

(use-package async-bytecomp
  :ensure async
  :after bytecomp
  :config (async-bytecomp-package-mode))

(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode)
  :custom
  (cargo-process--command-build "build --color never")
  (cargo-process--command-check "check --color never")
  (cargo-process--command-clippy "clippy --color never -Zunstable-options")
  (cargo-process--command-current-file-tests "test --color never")
  (cargo-process--command-current-test "test --color never")
  (cargo-process--command-rm "rm --color never")
  (cargo-process--command-run "run --color never")
  (cargo-process--command-test "test --color never"))

(use-package calendar
  :defer t
  :custom
  (calendar-time-zone-style 'numeric)
  (calendar-week-start-day 1))

(use-package cc-vars
  :defer t
  :custom (c-default-style '((java-mode . "java") (other . "awk"))))

(use-package cider
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package comint
  :bind (:map mode-specific-map ("c" . comint-run))

  :custom
  (comint-buffer-maximum-size 10000)
  (comint-input-ignoredups t)
  (comint-input-ring-size 10000)

  :config
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)

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

  (defun comint-set-local-buffer-maximum-size (n)
    "Set locally `comint-buffer-maximum-size' to N."
    (interactive (list (read-number "Buffer maximum size: " 1024)) comint-mode)
    (setq-local comint-buffer-maximum-size n)))

(use-package compile
  :bind
  (:map compilation-button-map
        ("RET" . compile-goto-error-same-window)
        ("o" . compile-goto-error))
  (:map compilation-mode-map
        ("RET" . compile-goto-error-same-window)
        ("o" . compile-goto-error))
  (:map compilation-shell-minor-mode-map
        ("C-c C-g" . recompile-comint-maybe-in-project))

  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)

  :config
  (add-to-list
   'compilation-error-regexp-alist
   (list (rx bol (1+ " ") "at" (1+ (not (any "/")))
             (group (1+ (not (any ":")))) ":"
             (group (1+ digit)) ":"
             (group (1+ digit)))
         1 2 3))

  (defun compile-goto-error-same-window ()
    "Run `compile-goto-error' in the same window."
    (interactive)
    (xref-push-marker-stack)
    (same-window-prefix)
    (compile-goto-error))

  (defun recompile-comint-maybe-in-project (&optional edit-command)
    "Recompile comint in project if in project.
For EDIT-COMMAND see `recompile'."
    (interactive "P")
    (let ((compilation-buffer-name-function
           (if (project-current)
               project-compilation-buffer-name-function
             compilation-buffer-name-function)))
      (recompile edit-command))))

(use-package conf-mode :mode ((rx ".aip" eos) . conf-windows-mode))

(use-package consult
  :ensure t
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  (embark-collect-mode . consult-preview-at-point-mode)

  :bind
  ("M-H" . consult-history)
  ("M-y" . consult-yank-replace)
  (:map kmacro-keymap
        ("c" . consult-kmacro))
  (:map ctl-x-4-map
        ("b" . consult-buffer-other-window))
  (:map ctl-x-map
        ("b" . 'consult-buffer))
  (:map ctl-x-r-map
        ("b" . consult-bookmark)
        ("i" . consult-register-load)
        ("s" . consult-register-store))
  (:map goto-map
        ("E" . consult-compile-error)
        ("F" . consult-flymake)
        ("I" . consult-imenu-multi)
        ("i" . consult-imenu)
        ("o" . consult-outline))
  (:map help-map
        ("M-i" . consult-info)
        ("M-m" . consult-man))
  (:map isearch-mode-map
        ("M-s M-c M-L" . consult-line-multi)
        ("M-s M-c M-l" . consult-line))
  (:map project-prefix-map
        ("b" . consult-project-buffer)
        ("i" . consult-project-imenu))
  (:map search-map
        ("M-c M-L" . consult-line-multi)
        ("M-c M-f" . consult-focus-lines)
        ("M-c M-k" . consult-keep-lines)
        ("M-c M-l" . consult-line)
        ("M-f M-f" . consult-find)
        ("M-f M-l" . consult-locate)
        ("M-g M-g" . consult-grep)
        ("M-g M-r" . consult-ripgrep)
        ("M-g M-t" . consult-git-grep))
  (:map tab-prefix-map
        ("b" . consult-buffer-other-tab))

  :config
  (defun consult-buffer-other-tab ()
    "Variant of `consult-buffer' which opens in other tab."
    (interactive)
    (let ((consult--buffer-display #'switch-to-buffer-other-tab))
      (consult-buffer))))

(use-package consult
  :ensure t
  :when (eq 'windows-nt system-type)
  :defer t
  :custom (consult-find-args '(find-program "." "-not" "(" "-path" "*/.[A-Za-z]*" "-prune" ")")))

(use-package consult-register
  :defer t
  :init
  (setq register-preview-function 'consult-register-format)
  (advice-add 'register-preview :override 'consult-register-window))

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

(use-package csharp-mode
  :hook (csharp-ts-mode . csharp-ts-mode-enable-clean-RETs)

  :config
  (defun csharp-ts-mode-clean-RETs ()
    "Clean ^M."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match "\n"))))

  (defun csharp-ts-mode-enable-clean-RETs ()
    "Enable by buffer-local before-save hook."
    (add-hook 'before-save-hook 'csharp-ts-mode-clean-RETs nil t)))

(use-package csharp-mode
  :disabled t
  :defer t
  :config
  (setq csharp-ts-mode--font-lock-settings
        (treesit-font-lock-rules
         :language 'c-sharp
         :feature 'expression
         '((conditional_expression (identifier) @font-lock-variable-use-face)
           (postfix_unary_expression (identifier)* @font-lock-variable-use-face)
           (initializer_expression (assignment_expression left: (identifier) @font-lock-variable-use-face)))

         :language 'c-sharp
         :feature 'bracket
         '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

         :language 'c-sharp
         :feature 'delimiter
         '((["," ":" ";"]) @font-lock-delimiter-face)

         :language 'c-sharp
         :feature 'error
         '((ERROR) @font-lock-warning-face)

         :language 'c-sharp
         :override t
         :feature 'comment
         '((comment) @font-lock-comment-face)

         :language 'c-sharp
         :override t
         :feature 'keyword
         `([,@csharp-ts-mode--keywords] @font-lock-keyword-face
           (modifier) @font-lock-keyword-face
           (this_expression) @font-lock-keyword-face)

         :language 'c-sharp
         :override t
         :feature 'property
         `((attribute (identifier) @font-lock-property-use-face (attribute_argument_list))
           (attribute (identifier) @font-lock-property-use-face))

         :language 'c-sharp
         :override t
         :feature 'escape-sequence
         '((escape_sequence) @font-lock-escape-face)

         :language 'c-sharp
         :override t
         :feature 'literal
         `((integer_literal) @font-lock-number-face
           (real_literal) @font-lock-number-face
           (null_literal) @font-lock-constant-face
           (boolean_literal) @font-lock-constant-face)

         :language 'c-sharp
         :override t
         :feature 'string
         `([(string_literal)
            (verbatim_string_literal)
            (interpolated_string_text)
            (interpolated_verbatim_string_text)
            (character_literal)
            "\""
            "$\""
            "@$\""
            "$@\""] @font-lock-string-face)

         :language 'c-sharp
         :override t
         :feature 'type
         '((predefined_type) @font-lock-type-face
           (implicit_type) @font-lock-type-face
           (nullable_type) @font-lock-type-face
           (type_parameter
            (identifier) @font-lock-type-face)
           (type_argument_list
            (identifier) @font-lock-type-face)
           (type_argument_list
            (generic_name
             (identifier) @font-lock-type-face))
           (base_list
            (generic_name
             (identifier) @font-lock-type-face))
           (array_type
            (identifier) @font-lock-type-face)
           (cast_expression (identifier) @font-lock-type-face)
           (cast_expression (generic_name (identifier) @font-lock-type-face))
           ["operator"] @font-lock-type-face
           (type_parameter_constraints_clause
            target: (identifier) @font-lock-type-face)
           (type_constraint type: (identifier) @font-lock-type-face)
           (type_constraint type: (generic_name (identifier) @font-lock-type-face))
           (type_of_expression (identifier) @font-lock-type-face)
           (object_creation_expression
            type: (identifier) @font-lock-type-face)
           (object_creation_expression
            type: (generic_name (identifier) @font-lock-type-face))
           (as_expression right: (identifier) @font-lock-type-face)
           (as_expression right: (generic_name (identifier) @font-lock-type-face)))

         :language 'c-sharp
         :feature 'definition
         :override t
         '((qualified_name (identifier) @font-lock-type-face)
           (using_directive (identifier) @font-lock-type-face)
           (using_directive (name_equals
                             (identifier) @font-lock-type-face))

           (enum_declaration (identifier) @font-lock-type-face)
           (enum_member_declaration (identifier) @font-lock-variable-name-face)

           (interface_declaration (identifier) @font-lock-type-face)

           (struct_declaration (identifier) @font-lock-type-face)

           (record_declaration (identifier) @font-lock-type-face)
           (namespace_declaration (identifier) @font-lock-type-face)
           (base_list (identifier) @font-lock-type-face)
           (property_declaration
            type: (nullable_type) @font-lock-type-face
            name: (identifier) @font-lock-variable-name-face)
           (property_declaration
            type: (predefined_type) @font-lock-type-face
            name: (identifier) @font-lock-variable-name-face)
           (property_declaration
            type: (identifier) @font-lock-type-face
            name: (identifier) @font-lock-variable-name-face)
           (class_declaration (identifier) @font-lock-type-face)

           (constructor_declaration name: (_) @font-lock-type-face)

           (method_declaration type: [(identifier) (predefined_type)] @font-lock-type-face)
           (method_declaration type: (generic_name (identifier) @font-lock-type-face))
           (method_declaration name: (_) @font-lock-function-name-face)

           (catch_declaration
            ((identifier) @font-lock-type-face))
           (catch_declaration
            ((identifier) @font-lock-type-face
             (identifier) @font-lock-variable-name-face))

           (variable_declaration (identifier) @font-lock-type-face)
           (variable_declaration (generic_name (identifier) @font-lock-type-face))
           (variable_declarator (identifier) @font-lock-variable-name-face)

           (parameter type: (identifier) @font-lock-type-face)
           (parameter type: (generic_name (identifier) @font-lock-type-face))
           (parameter name: (identifier) @font-lock-variable-name-face)

           (lambda_expression (identifier) @font-lock-variable-name-face)

           (declaration_expression type: (identifier) @font-lock-type-face)
           (declaration_expression name: (identifier) @font-lock-variable-name-face))

         :language 'c-sharp
         :feature 'function
         '((invocation_expression
            function: (member_access_expression
                       name: (identifier) @font-lock-function-call-face))
           (invocation_expression
            function: (identifier) @font-lock-function-call-face)
           (invocation_expression
            function: (member_access_expression
                       name: (generic_name (identifier) @font-lock-function-call-face)))
           (invocation_expression
            function: (generic_name (identifier) @font-lock-function-call-face)))

         :language 'c-sharp
         :feature 'escape-sequence
         :override t
         '((escape_sequence) @font-lock-escape-face)

         :language 'c-sharp
         :feature 'directives
         :override t
         '((if_directive
            "if" @font-lock-preprocessor-face
            (identifier) @font-lock-variable-use-face)
           (elif_directive
            "elif" @font-lock-preprocessor-face
            (identifier) @font-lock-variable-use-face)
           (else_directive) @font-lock-preprocessor-face
           (endif_directive) @font-lock-preprocessor-face
           (define_directive
            "define" @font-lock-preprocessor-face
            (identifier) @font-lock-variable-use-face)
           (nullable_directive) @font-lock-preprocessor-face
           (pragma_directive) @font-lock-preprocessor-face
           (region_directive) @font-lock-preprocessor-face
           (endregion_directive) @font-lock-preprocessor-face
           (region_directive
            (preproc_message) @font-lock-variable-use-face)
           (endregion_directive
            (preproc_message) @font-lock-variable-use-face)))))

(use-package csproj-mode
  :ensure t
  :defer t)

(use-package csv-mode
  :ensure t
  :defer t)

(use-package cus-edit
  :defer t
  :custom (custom-file null-device))

(use-package custom
  :bind
  (:map ctl-x-map
        ("c v" . customize-option)
        ("c g" . customize-group)
        ("c f" . customize-face)
        ("c s" . customize-saved)
        ("c u" . customize-unsaved)))

(use-package cyrillic-dvorak-im)

(use-package dabbrev :bind (:map ctl-x-map ("C-/" . dabbrev-expand)))

(use-package dictionary
  :bind (:map mode-specific-map ("o T" . dictionary-search))
  :custom (dictionary-use-single-buffer t))

(use-package diff-mode
  :defer t
  :config (keymap-unset diff-mode-map "M-o" t))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-free-space 'separate)
  (dired-guess-shell-alist-user
   (list (list (rx "." (or "csv" "doc" "docx" "odp" "ods" "odt" "ppt" "pptx" "xls" "xlsx") eos)
               "setsid -f libreoffice * >/dev/null 2>&1"
               "libreoffice --invisible --headless --convert-to pdf * &")
         (list (rx "." (or "bmp" "gif" "jfif" "jpeg" "jpg" "nef" "png" "thm" "tif" "webp" "xpm") eos)
               "setsid -f nsxiv * >/dev/null 2>&1" "setsid -f gimp * >/dev/null 2>&1")
         (list (rx "." (or "ai" "eps") eos)
               "setsid -f inkscape * >/dev/null 2>&1"
               "setsid -f gimp * >/dev/null 2>&1")
         (list (rx "." (or "djvu" "fb2") eos)
               "ebook-convert ? .epub &")
         (list (rx ".pdf" eos)
               "setsid -f libreoffice * >/dev/null 2>&1"
               "setsid -f gimp * >/dev/null 2>&1")
         (list (rx "." (or "3gp" "aiff" "avi" "flac" "flv" "m4a" "mkv" "mov" "mp3" "mp4" "mpg" "ogg" "ogv" "opus" "vob" "wav" "webm" "wmv" "mka" "m4v") eos)
               "setsid -f mpv --profile=gui * >/dev/null 2>&1"
               "for vid in * ; do dur=$(video_seconds \"$vid\"); sum=$((sum + dur)); done; format_seconds \"%02h:%02m:%02s\" \"$sum\""
               "recode_video hevc * &"
               '(pcase file
                  ((rx ".webm" eos) "strip_video opus * &")
                  ((rx "." (or "mp4" "m4v") eos) "strip_video m4a * &")
                  (_ "strip_video mp3 * &"))
               "resize_video 360 * &"
               "mediainfo"
               "mpv -vo=drm"
               "sort_videos_by_duration *")
         (list "\\.cue\\'" "setsid -f mpv --profile=gui * >/dev/null 2>&1")
         (list "\\.rar\\'"
               '(let ((dir (shell-quote-argument (file-name-sans-extension file))))
                  (concat "mkdir " dir "; unrar x * " dir)))
         (list "\\.torrent\\'" "transmission-show")
         (list "\\.epub\\'" "ebook-convert ? .mobi &")))
  (dired-listing-switches "-lFAv --si --group-directories-first")
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-copies 'always)
  :config
  (require 'dired-x)
  (keymap-unset dired-mode-map "X" t)
  (keymap-unset dired-mode-map "&" t))

(use-package dired
  :when (eq 'windows-nt system-type)
  :defer t
  :custom (dired-listing-switches "-lFAvh"))

(use-package dired-atool-transient
  :ensure t
  :vc "https://github.com/xFA25E/dired-atool-transient"
  :bind
  (:map dired-mode-map
        ("c" . dired-atool-transient-pack)
        ("Z" . dired-atool-transient-unpack)))

(use-package dired-async
  :ensure async
  :defer t
  :custom (dired-async-mode-lighter "")
  :custom-face
  (dired-async-message ((t (:foreground "dark orange"))))
  (dired-async-mode-message ((t (:foreground "dark orange")))))

(use-package dired-aux
  :bind (:map dired-mode-map ("M-+" . dired-create-empty-file))
  :custom
  (dired-compress-directory-default-suffix ".tar.xz")
  (dired-compress-file-default-suffix ".xz")
  (dired-create-destination-dirs 'ask))

(use-package dired-tags
  :vc "https://github.com/xFA25E/dired-tags"
  :bind (:map dired-mode-map ("C-c C-t" . dired-tags-prefix-map)))

(use-package dired-x
  :bind
  (:map dired-mode-map
        ("* i" . dired-mark-images)
        ("* v" . dired-mark-videos))

  :config
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
     '("mp4" "mkv" "png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg")
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
     marker-char)))

(use-package disass
  :bind
  (:map emacs-lisp-mode-map ("C-c C-d" . disassemble))
  (:map lisp-interaction-mode-map ("C-c C-d" . disassemble)))

(use-package discomfort
  :ensure t
  :defer t)

(use-package djvu
  :ensure t
  :defer t)

(use-package dotnet
  :ensure t
  :hook csharp-ts-mode)

(use-package dumb-jump
  :ensure t
  :after xref
  :defer t
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package ebdb
  :ensure t
  :bind (:map mode-specific-map ("o e" . ebdb))
  :custom
  (ebdb-complete-mail nil)
  (ebdb-complete-mail-allow-cycling nil)
  (ebdb-completion-display-record nil)
  (ebdb-record-self "5ecfc8f5-f490-4745-8cf1-86b1964e4ab7")
  (ebdb-sources (expand-file-name "emacs/ebdb" (xdg-data-home)))
  (ebdb-user-mail-address-re 'self))

(use-package ebdb-complete
  :ensure ebdb
  :bind
  (:map ebdb-mode-map
        ("C-c m" . ebdb-complete-push-mail-and-quit-window)
        ("C-c M" . ebdb-complete-push-mail)))

(use-package ebdb-complete
  :ensure ebdb
  :after message
  :init (require 'ebdb-message)
  :bind (:map message-mode-map ("C-c e" . ebdb-complete)))

(use-package ede/base
  :defer t
  :custom (ede-project-placeholder-cache-file (expand-file-name "emacs/ede/projects.el" (xdg-cache-home))))

(use-package ediff
  :bind (:map ctl-x-map ("C-d" . ediff-commands))
  :config
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
      ("vdm" "Merge directory" ediff-merge-directory-revisions)]]))

(use-package ediff-init
  :defer t
  :custom (ediff-autostore-merges nil))

(use-package ediff-wind
  :defer t
  :custom (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package edit-indirect
  :ensure t
  :bind (:map ctl-x-map ("E" . edit-indirect-region)))

(use-package editfns
  :defer t
  :custom (user-full-name "Valeriy Litkovskyy"))

(use-package eglot
  :bind (:map eglot-mode-map ("C-c C-l" . eglot-code-actions))

  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-connect-timeout 60)
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider :inlayHintProvider))
  (eglot-sync-connect nil)

  :config
  (defun eglot-csharp-server-program (_)
    "Return a command for csharp language server."
    (let* ((files (project-files (project-current)))
           (slns (cl-remove (rx ".sln" eos) files :test-not #'string-match-p))
           (sln (if (cdr slns)
                    (completing-read "Select solution: " slns nil t)
                  (car slns))))
      (list "CSharpLanguageServer" "-s" sln)))

  (add-to-list 'eglot-server-programs '(js-ts-mode . ("typescript-language-server" "--tsserver-path" "tsserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(csharp-mode . eglot-csharp-server-program))
  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)

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
        (xref-backend-apropos 'dumb-jump pattern))))

(use-package eldoc
  :hook nix-mode nix-ts-mode
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-minor-mode-string ""))

(use-package elec-pair
  :defer t
  :custom (electric-pair-mode t))

(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map ("C-c S-RET" . emacs-lisp-macroexpand))
  (:map lisp-interaction-mode-map ("C-c S-RET" . emacs-lisp-macroexpand))
  :config
  (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

(use-package emacs
  :bind (:map ctl-x-map ("C-M-t" . transpose-regions))
  :init (setq completion-ignore-case t)
  :custom (create-lockfiles nil))

(use-package embark
  :ensure t
  :bind ("C-." . embark-act))

(use-package embark-consult
  :ensure t
  :defer t)

(use-package emmet-mode
  :ensure t
  :hook html-mode mhtml-mode nxml-mode web-mode)

(use-package env
  :bind ("C-M-$" . getenv)
  :init (setenv "PAGER" "cat"))

(use-package envrc
  :ensure t
  :bind
  (:map envrc-command-map ("R" . envrc-reload-all))
  (:map envrc-mode-map ("C-x d" . envrc-command-map))
  :custom
  (envrc-error-lighter '(" " (:propertize "envrc" face envrc-mode-line-error-face)))
  (envrc-global-mode t)
  (envrc-none-lighter nil)
  (envrc-on-lighter '(" " (:propertize "envrc" face envrc-mode-line-on-face))))

(use-package enwc
  :ensure t
  :defer t
  :custom (enwc-display-mode-line nil))

(use-package enwc
  :ensure t
  :defer t
  :when (string= "stribog" (nth 0 (process-lines "hostname")))
  :custom
  (enwc-wired-device "eno1")
  (enwc-wireless-device "wlan0"))

(use-package enwc
  :ensure t
  :defer t
  :when (string= "veles" (nth 0 (process-lines "hostname")))
  :custom
  (enwc-wired-device "enp0s31f6")
  (enwc-wireless-device "wlp3s0"))

(use-package enwc-backend
  :ensure enwc
  :defer t
  :custom (enwc-default-backend 'nm))

(use-package esh-mode
  :defer t
  :custom (eshell-directory-name (expand-file-name "emacs/eshell/" (xdg-cache-home))))

(use-package eww
  :hook (eww-mode . eww-restore-browse-url-browser-function)
  :custom
  (eww-bookmarks-directory (expand-file-name "emacs/" (xdg-data-home)))
  (eww-browse-url-new-window-is-tab nil)
  :config
  (defun eww-restore-browse-url-browser-function ()
    "Restore `browse-url-browser-function' original value."
    (kill-local-variable 'browse-url-browser-function)))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  :custom (executable-chmod 64))

(use-package faces
  :defer t
  :custom-face
  (default ((t (:inherit nil :extend nil :stipple nil :background "white smoke" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 98 :width normal :foundry "UKWN" :family "Iosevka"))))
  (header-line ((t (:inherit default :background "grey90" :foreground "grey20"))))
  (mode-line ((t (:inherit default :background "white smoke" :foreground "black" :box (:line-width (1 . 1) :color "grey75") :height 0.83))))
  (mode-line-inactive ((t (:inherit mode-line :background "dark gray" :foreground "grey20" :weight light))))
  (region ((t (:extend t :background "LemonChiffon2" :distant-foreground "gtk_selection_fg_color"))))
  (tab-bar ((t (:inherit (variable-pitch default) :background "black" :foreground "white smoke" :height 0.8)))))

(use-package files
  :bind
  (:map load-command-map
        ("C-f" . load-file)
        ("C-l" . load-library))
  (:map ctl-x-map ("C-z" . find-sibling-file))
  (:map ctl-x-x-map ("R" . rename-visited-file))

  :custom
  (auto-save-file-name-transforms
   (list (list ".*" (expand-file-name "emacs/auto-saves/" (xdg-cache-home)) t)))
  (backup-by-copying t)
  (backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
  (byte-count-to-string-function (lambda (s) (file-size-human-readable s 'si)))
  (delete-old-versions t)
  (find-sibling-rules
   (rx-let ((file-name (+ (not "/"))))
     (list
      (list (rx (group file-name) ".axaml.cs" eos)
            (rx (backref 1) ".axaml"))
      (list (rx (group file-name) ".axaml" eos)
            (rx (backref 1) ".axaml.cs"))
      (list (rx "UserControls/Dialog/" (group file-name) ".xaml" eos)
            (rx "ViewModels/" (backref 1) "ViewModel.cs"))
      (list (rx "UserControls/" (group file-name) "View.xaml" eos)
            (rx "ViewModels/" (backref 1) "ViewModel.cs"))
      (list (rx "ViewModels/" (group file-name) "ViewModel.cs" eos)
            (rx "UserControls/" (backref 1) "View.xaml")
            (rx "UserControls/" (backref 1) ".xaml")
            (rx "UserControls/Dialog/" (backref 1) ".xaml"))
      (list (rx (group file-name) ".xaml.cs" eos)
            (rx (backref 1) ".xaml"))
      (list (rx (group file-name) "ViewModel.cs" eos)
            (rx (backref 1) "View.xaml"))
      (list (rx (group file-name) ".xaml.cs" eos)
            (rx (backref 1) ".xaml"))
      (list (rx (group file-name) "View.xaml" eos)
            (rx (backref 1) "ViewModel.cs")
            (rx (backref 1) "View.xaml.cs"))
      (list (rx (group file-name) ".xaml" eos)
            (rx (backref 1) "ViewModel.cs")
            (rx (backref 1) ".xaml.cs")))))
  (kept-new-versions 10)
  (major-mode-remap-alist
   '((csharp-mode . csharp-ts-mode)
     (css-mode . css-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-mode . js-ts-mode)
     (js-json-mode . json-ts-mode)
     (nix-mode . nix-ts-mode)
     (sh-mode . bash-ts-mode)))
  (safe-local-eval-forms
   '((add-hook 'write-file-hooks 'time-stamp)
     (add-hook 'write-file-functions 'time-stamp)
     (add-hook 'before-save-hook 'time-stamp nil t)
     (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
     (add-hook 'after-save-hook
               (lambda nil (org-babel-tangle)
                 (byte-recompile-directory (expand-file-name "./")))
               nil t)))
  (safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval add-hook 'after-save-hook 'org-md-export-to-markdown nil t)
     (eval apheleia-mode t)))
  (small-temporary-file-directory "/dev/shm/")
  (version-control t))

(use-package files-x
  :bind
  (:map ctl-x-x-map
        ("l d" . add-dir-local-variable)
        ("l f" . add-file-local-variable)
        ("l F" . add-file-local-variable-prop-line)))

(use-package find-dired
  :bind
  (:map search-map
        ("M-f d" . find-dired-fd)
        ("M-f l" . find-dired-locate))

  :custom
  (find-ls-option
   '("-print0 | xargs -0 ls -ldF --si --quoting-style=literal" . "-ldhF"))

  :config
  (defun find-dired-append-xargs-pipe (cmd)
    "Append xargs pipe to CMD."
    (thread-last (find-dired--escaped-ls-option)
      (string-remove-prefix "-print0")
      (concat cmd)))

  (require 'transient)

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

  (defun find-dired-is-chromashop-translations ()
    "Find all files with translations in ChromaShop project."
    (interactive)
    (find-dired
     "~/Projects/intex/ChromaShop/"
     "-iname \"TranslationDictionary.xaml\" -o -iname \"ScriptInsert_Translations*.sql\""
     ;; "-iname \"ScriptInsert_Translations*.sql\""
     )))

(use-package find-func
  :bind
  (:map ctl-x-map
        ("F" . find-function)
        ("K" . find-function-on-key)
        ("L" . find-library)
        ("V" . find-variable))
  (:map ctl-x-4-map
        ("F" . find-function-other-window)
        ("K" . find-function-on-key-other-window)
        ("L" . find-library-other-window)
        ("V" . find-variable-other-window))
  (:map  ctl-x-5-map
         ("F" . find-function-other-frame)
         ("K" . find-function-on-key-other-frame)
         ("L" . find-library-other-frame)
         ("V" . find-variable-other-frame)))

(use-package finder :bind (:map help-map ("M-c" . finder-commentary)))

(use-package flymake
  :hook
  bash-ts-mode
  emacs-lisp-mode
  nix-mode nix-ts-mode
  nxml-mode
  sh-mode
  :bind
  (:map flymake-mode-map
        ("M-g M-b" . flymake-goto-prev-error)
        ("M-g M-f" . flymake-goto-next-error))
  :custom
  (flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters)))

(use-package flymake-proc
  :defer t
  :config (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package flymake-collection
  :ensure t
  :init (flymake-collection-hook-setup))

(use-package flymake-collection-hook
  :ensure flymake-collection
  :defer t
  :config (cl-pushnew #'flymake-collection-statix (alist-get 'nix-ts-mode flymake-collection-hook-config)))

(use-package flymake-collection-statix
  :ensure flymake-collection
  :after nix-mode
  :bind (:map nix-mode-map ("C-c C-x" . flymake-collection-statix-fix)))

(use-package flymake-collection-statix
  :ensure flymake-collection
  :after nix-ts-mode
  :bind (:map nix-ts-mode-map ("C-c C-x" . flymake-collection-statix-fix)))

(use-package fns
  :defer t
  :custom (use-dialog-box nil))

(use-package frame
  :defer t
  :custom
  (blink-cursor-mode nil)
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (use-system-tooltips nil))

(use-package gdb-mi
  :defer t
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package grep
  :bind (:map search-map ("M-g g" . rgrep))
  :config
  (setf (alist-get "php" grep-files-aliases nil nil #'equal) "*.php *.phtml")
  (setf (alist-get "all" grep-files-aliases nil nil #'equal) "* .[!.]* ..?*")

  (define-advice grep-expand-template (:filter-return (cmd) cut)
    (concat cmd " | cut -c-500")))

(use-package grep
  :when (eq 'windows-nt system-type)
  :defer t
  :custom (find-program "C:\\ProgramData\\chocolatey\\bin\\find.exe"))

(use-package help
  :bind (:map ctl-x-map ("h" . help-command))
  :custom
  (describe-bindings-outline t)
  (help-window-keep-selected t)
  (help-window-select t))

(use-package help-fns
  :bind
  (:map help-map
        ("M-f" . describe-face)
        ("M-k" . describe-keymap))
  :custom (help-enable-variable-value-editing t))

(use-package hippie-exp
  :bind (:map ctl-x-map ("C-;" . hippie-expand))
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name-partially
     try-complete-file-name
     try-expand-list
     try-expand-line
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-expand-all-abbrevs)))

(use-package hl-line
  :bind (:map ctl-x-x-map ("h" . hl-line-mode))
  :hook
  csv-mode
  dired-mode
  grep-mode
  mpc-mode
  pueue-mode-hook
  tar-mode
  transmission-files-mode
  transmission-mode
  transmission-peers-mode)

(use-package htmlize
  :ensure t
  :defer t)

(use-package ibuffer
  :defer t
  :custom (ibuffer-default-sorting-mode 'major-mode)
  :config (keymap-unset ibuffer-mode-map "M-o" t))

(use-package ibuf-ext
  :defer t
  :custom (ibuffer-show-empty-filter-groups nil))

(use-package image-dired
  :defer t
  :custom
  (image-dired-db-file (expand-file-name "emacs/image-dired-db" (xdg-data-home)))
  (image-dired-dir (expand-file-name "emacs/image-dired/thumbnails/" (xdg-cache-home)))
  (image-dired-external-viewer "image-dired-external-viewer")
  (image-dired-gallery-dir (expand-file-name "emacs/image-dired/gallery/" (xdg-cache-home)))
  (image-dired-tags-db-file (expand-file-name "emacs/image-dired-db" (xdg-data-home)))
  (image-dired-temp-image-file (expand-file-name "emacs/image-dired/temp" (xdg-cache-home))))

(use-package image
  :defer t
  :custom (image-use-external-converter t))

(use-package image-dired-external
  :defer t
  :custom
  (image-dired-temp-rotate-image-file (expand-file-name "emacs/image-dired/rotate_temp" (xdg-cache-home))))

(use-package image-file
  :defer t
  :custom
  (image-file-name-extensions
   '("mp4" "mkv" "png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg")))

(use-package imenu
  :defer t
  :custom
  (imenu-auto-rescan t)
  (imenu-level-separator "/")
  (imenu-space-replacement " ")
  (imenu-use-popup-menu nil))

(use-package inf-lisp
  :defer t
  :custom (inferior-lisp-program "nix-shell -p sbcl --run sbcl"))

(use-package inspector
  :ensure t
  :defer t)

(use-package ipretty
  :ensure t
  :after elisp-mode
  :bind (:map lisp-interaction-mode-map ("C-j" . ipretty-last-sexp)))

(use-package isearch
  :init (fset 'isearch-help-map isearch-help-map)
  :bind
  (:map isearch-mode-map
        ("C-?" . isearch-help-map)
        ("C-h" . isearch-delete-char))
  :custom
  (isearch-allow-motion t)
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (search-whitespace-regexp (rx (*? nonl))))

(use-package ispell
  :hook (tex-mode . (lambda () (setq-local ispell-parser 'tex)))
  :custom (ispell-program-name "enchant-2"))

(use-package keyboard
  :defer t
  :custom (translate-upper-case-key-bindings nil))

(use-package js
  :defer t
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  :config
  (keymap-unset js-ts-mode-map "M-." t))

(use-package json-navigator
  :ensure t
  :defer t)

(use-package json-ts-mode :mode (rx "flake.lock" eos))

(use-package ledger-init
  :ensure ledger-mode
  :defer t
  :custom (ledger-default-date-format "%Y-%m-%d"))

(use-package ledger-report
  :ensure ledger-mode
  :defer t
  :custom
  (ledger-reports
   '(("intex kilometers"
      "%(binary) -f %(ledger-file) balance 'Income:Job:Intex System:KR' --limit 'a < 0' ")
     ("register"
      "%(binary) -f %(ledger-file) register --sort '-date' --period 'from 1 month ago' ")
     ("expenses monthly"
      "%(binary) -f %(ledger-file) register '^Expenses' --monthly --empty --collapse --average --period 'from 2023-4'")
     ("groceries weekly"
      "%(binary) -f %(ledger-file) register 'Expenses:Groceries' --period 'from 2023-7-24' --average --weekly")
     ("intex trips"
      "%(binary) -f %(ledger-file) register 'Liabilities:Intex System' --limit 'amount > 0' --period %(month)")
     ("account monthly collapsed"
      "%(binary) -f %(ledger-file) --monthly --empty --collapse register %(account)")
     ("cash flow monthly"
      "%(binary) -f %(ledger-file) register ^Income ^Expenses --monthly --empty --collapse --average --period 'from 2023-4'")
     ("cash flow"
      "%(binary) -f %(ledger-file) balance ^Income ^Expenses")
     ("net worth monthly"
      "%(binary) -f %(ledger-file) register ^Assets ^Liabilities --monthly --empty --collapse --average --period 'from 2023-4'")
     ("net worth"
      "%(binary) -f %(ledger-file) balance ^Assets ^Liabilities")
     ("balance"
      "%(binary) -f %(ledger-file) balance")
     ("payee"
      "%(binary) -f %(ledger-file) register @%(payee)")
     ("account"
      "%(binary) -f %(ledger-file) register %(account)")
     ("account monthly"
      "%(binary) -f %(ledger-file) --monthly register %(account)"))))

(use-package ledger-flymake
  :ensure ledger-mode
  :after ledger-mode
  :hook (ledger-mode . ledger-flymake-enable))

(use-package link-hint
  :ensure t
  :bind
  (:map goto-map
        ("M-l" . link-hint-open-link)
        ("M-L" . link-hint-copy-link))
  :config
  (cl-pushnew 'rg-mode (get 'link-hint-compilation-link :vars)))

(use-package lisp
  :init (provide 'lisp)
  :bind
  ("M-[" . delete-pair)
  ("M-]" . change-pair)
  ("C-)" . slurp-pair)

  :custom
  (delete-pair-blink-delay 0)

  :config
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
      (indent-sexp))))

(use-package loadhist :bind (:map load-command-map ("C-u" . unload-feature)))

(use-package locate
  :defer t
  :custom (locate-update-command "systemctl --user start updatedb.service"))

(use-package man
  :defer t
  :custom (Man-notify-method 'pushy))

(use-package magit
  :ensure t
  :defer t
  :custom (magit-define-global-key-bindings 'recommended))

(use-package magit-diff
  :ensure magit
  :defer t
  :custom-face
  (magit-diff-revision-summary-highlight ((t (:inherit magit-diff-hunk-heading-highlight :background "misty rose" :underline t)))))

(use-package magit-extras
  :ensure magit
  :bind (:map project-prefix-map ("m" . magit-project-status)))

(use-package magit-process
  :ensure magit
  :defer t
  :custom (magit-credential-cache-daemon-socket (expand-file-name "git/credential/socket" (xdg-cache-home))))

(use-package marginalia
  :ensure t
  :defer t
  :custom (marginalia-mode t))

(use-package mb-depth
  :defer t
  :custom (minibuffer-depth-indicate-mode t))

(use-package menu-bar :bind (:map ctl-x-map ("`" . toggle-debug-on-error)))

(use-package message
  :defer t
  :custom
  (message-directory "~/.mail/")
  (message-kill-buffer-on-exit t)
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (message-subject-re-regexp
   (rx bol (* blank)
       (* (or "R" "RE" "Re" "Ris")
          (* "[" (* digit) "]")
          (? " ") ":" (* blank)))))

(use-package minibuf
  :defer t
  :custom
  (enable-recursive-minibuffers t)
  (history-delete-duplicates t)
  (history-length 1000)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (read-buffer-completion-ignore-case t)
  (read-minibuffer-restore-windows nil))

(use-package minibuffer
  :defer t

  :init
  (setq completion-category-defaults nil)
  (setq completion-in-region-function 'consult-completion-in-region)
  (setq minibuffer-allow-text-properties t)

  :bind
  (:map minibuffer-local-must-match-map ("C-j" . minibuffer-force-complete-and-exit))

  ;; TODO: move orderless
  :custom
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-cycle-threshold 2)
  (completion-styles '(orderless partial-completion basic))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)
  (minibuffer-beginning-of-buffer-movement t)
  (minibuffer-default-prompt-format " [%s]")
  (read-file-name-completion-ignore-case t)

  :config
  (keymap-unset minibuffer-local-completion-map "SPC" t))

(use-package minibuf-eldef
  :defer t
  :custom (minibuffer-electric-default-mode t))

(use-package misc
  :bind
  (:map ctl-x-map ("o" . duplicate-dwim))
  (:repeat-map duplicate-dwim-repeat-map ("o" . duplicate-dwim)))

(use-package mouse
  :defer t
  :custom (context-menu-mode t))

(use-package mpc
  :bind
  (:map mode-specific-map ("o s" . mpc))
  (:map mpc-mode-map
        ("." . mpc-toggle-single)
        ("D" . mpc-playlist-delete)
        ("M" . mpc-select-extend)
        ("RET" . mpc-songs-jump-to)
        ("M-m" . mpc-select)
        ("a" . mpc-playlist-add)
        ("b" . mpc-rewind)
        ("c" . mpc-toggle-consume)
        ("f" . mpc-ffwd)
        ("j" . mpc-dired-jump)
        ("k" . mpc-songs-kill-search)
        ("m" . mpc-select-toggle)
        ("p" . mpc-playlist)
        ("r" . mpc-toggle-repeat)
        ("s" . mpc-songs-search)
        ("t" . mpc-toggle-play)
        ("u" . mpc-update)
        ("z" . mpc-toggle-shuffle))
  (:map mpc-songs-mode-map
        ("V" . mpc-move-backward)
        ("v" . mpc-move-forward))

  :custom
  (mpc-browser-tags '(file))
  (mpc-cover-image-re "[Ff]ront\\.jpg")
  (mpc-data-directory (expand-file-name "emacs/mpc" (xdg-cache-home)))
  (mpc-mpd-music-directory "~/Music")
  (mpc-songs-format "%-6{Time} %{file}")

  :config
  (keymap-unset mpc-songs-mode-map "<remap> <mpc-select>" t)

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
    (mpc-move-forward (- n))))

(use-package net-utils
  :bind
  (:map mode-specific-map
        ("n h" . nslookup-host)
        ("n i" . ifconfig)
        ("n n" . netstat)
        ("n p" . ping)
        ("n w" . iwconfig)))

(use-package newcomment :bind ("C-;" . comment-line))

(use-package newsticker :bind (:map mode-specific-map ("o n" . newsticker-show-news)))

(use-package newst-backend
  :defer t
  :custom
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old nil)
  (newsticker-dir (expand-file-name "emacs/newsticker" (xdg-cache-home)))
  (newsticker-obsolete-item-max-age 31536000)
  (newsticker-retrieval-interval 0)
  (newsticker-retrieval-method 'extern)
  (newsticker-url-list
   '(("The Alternative Hypothesis Substack" "https://thealternativehypothesis.substack.com/feed" nil nil nil)
     ("Информационное насилие" "https://www.youtube.com/feeds/videos.xml?channel_id=UCwpF2sTk7VLnCImJtYr45oQ" nil nil nil)
     ("the way i see things" "https://www.youtube.com/feeds/videos.xml?channel_id=UCFQYVdJHRHxmzffGKCqeZgQ" nil nil nil)
     ("Ideas and Data Substack" "https://seanlast.substack.com/feed" nil nil nil)
     ("Uebermarginal Twitch" "https://twitchrss.appspot.com/vod/uebermarginal" nil nil nil)
     ("uebermarginal" "https://www.youtube.com/feeds/videos.xml?channel_id=UCJ10M7ftQN7ylM6NaPiEB6w" nil nil nil)
     ("Dysphorra" "https://www.youtube.com/feeds/videos.xml?channel_id=UCsfHjT6FKyJHm39qWIO3rVg" nil nil nil)
     ("Неотеник" "https://www.youtube.com/feeds/videos.xml?channel_id=UC_4kLff8VIDAZ8NmUAIa_4w" nil nil nil)
     ("Лучшее с Алиповым" "https://www.youtube.com/feeds/videos.xml?channel_id=UCC4ImDJs7sSxlMVytsE09Qw" nil nil nil)
     ("Нейронаука с Владимиром Алиповым" "https://www.youtube.com/feeds/videos.xml?channel_id=UCmEG9pKNZaE6QG_SaBBt93Q" nil nil nil)
     ("itpedia" "https://www.youtube.com/feeds/videos.xml?channel_id=UC6bTF68IAV1okfRfwXIP1Cg" nil nil nil)
     ("Joseph Bronski YouTube" "https://www.youtube.com/feeds/videos.xml?channel_id=UCiMwMDPJBB_mcBPknjPrfGA" nil nil nil)
     ("Joseph Bronski Substack" "https://josephbronski.substack.com/feed" nil nil nil)
     ("Максим Вердикт" "https://www.youtube.com/feeds/videos.xml?channel_id=UC44oy3QadzoUZt7G0DDf5DQ" nil nil nil)
     ("Biopolitics" "https://biopolitics.substack.com/feed" nil nil nil)
     ("AltHype Cozy" "https://cozysounds.kanyecode.repl.co/feed/althype" nil nil nil)
     ("dickmao" "https://www.youtube.com/feeds/videos.xml?channel_id=UCyf-xqc0ovYSDgaCcB3CUoA" nil nil nil)
     ("Joseph Everett’s Newsletter" "https://josepheverettwil.substack.com/feed" nil nil nil)
     ("Just Emil Kirkegaard Things" "https://www.emilkirkegaard.com/feed" nil nil nil)
     ("UBERSOY" "https://www.youtube.com/feeds/videos.xml?channel_id=UCqBN8cSd6hfwqq0BM2biXOg" nil nil nil)
     ("Wags" "https://www.youtube.com/feeds/videos.xml?channel_id=UCA5Zo2kmdP9ig4f9fsvyRgg" nil nil nil)
     ("thuletide" "https://thuletide.wordpress.com/feed/" nil nil nil)
     ("Алексей Шевцов" "https://www.youtube.com/feeds/videos.xml?channel_id=UCM7-8EfoIv0T9cCI4FhHbKQ" nil nil nil)
     ("White Mage" "https://www.youtube.com/feeds/videos.xml?channel_id=UC1k29QaI7FKn5wki72Lyy7w" nil nil nil)
     ("What I've Learned" "https://www.youtube.com/feeds/videos.xml?channel_id=UCqYPhGiB9tkShZorfgcL2lA" nil nil nil)
     ("The Alternative Hypothesis YouTube" "https://www.youtube.com/feeds/videos.xml?user=fringeelements" nil nil nil)
     ("The Alternative Hypothesis Website" "http://thealternativehypothesis.org/index.php/feed" nil nil nil)
     ("The Alt Hype Bitchute" "https://www.bitchute.com/feeds/rss/channel/thealthype" nil nil nil)
     ("Sean Last YouTube" "https://www.youtube.com/feeds/videos.xml?channel_id=UCK1Uk2f36aglexxLkfOWnEQ" nil nil nil)
     ("Sacha Chua Emacs News" "https://sachachua.com/blog/category/emacs-news/feed/atom/" nil nil nil)
     ("Luke Smith YouTube" "https://youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" nil nil nil)
     ("Luke Smith PeerTube" "https://lukesmith.xyz/peertube" nil nil nil)
     ("Luke Smith Blog" "https://lukesmith.xyz/rss.xml" nil nil nil)
     ("Knight's Move" "https://www.youtube.com/feeds/videos.xml?channel_id=UC63HcOlghFQ3pcursLUp3NQ" nil nil nil)
     ("Ideas And Data" "https://ideasanddata.wordpress.com/feed/" nil nil nil)
     ("Fringe Elements" "https://odysee.com/$/rss/@TheAltHype:6" nil nil nil)
     ("American Renaissance" "https://odysee.com/$/rss/@AmericanRenaissance:7" nil nil nil)
     ("Alt Hype Livestream Archive" "https://odysee.com/$/rss/@AltHypeLiveArchive:9" nil nil nil)))
  (newsticker-url-list-defaults nil))

(use-package newst-treeview
  :defer t
  :custom
  (newsticker-treeview-automatically-mark-displayed-items-as-old nil)
  (newsticker-treeview-listwindow-height 6)
  (newsticker-treeview-treewindow-width 30)
  :custom-face
  (newsticker-treeview-new-face ((t (:underline t :weight bold)))))

(use-package mule-cmds
  :defer t
  :when (eq 'windows-nt system-type)
  :init (prefer-coding-system 'utf-8-unix))

(use-package nix-edit
  :ensure nix-mode
  :bind (:map mode-specific-map ("e" . nix-edit))
  :config
  (define-advice nix-edit (:override () flake)
    (interactive)
    (let ((cmd (read-shell-command "Nix edit command: " "nix edit ")))
      (find-file
       (with-temp-buffer
         (let ((process-environment (cons "EDITOR=echo" process-environment)))
           (call-process-shell-command cmd nil (list (current-buffer) nil) nil))
         (buffer-substring-no-properties (point-min) (1- (point-max))))))))

(use-package nix-flake
  :ensure nix-mode
  :bind (:map project-prefix-map ("l" . nix-flake-project))
  :custom (nix-flake-add-to-registry nil)
  :config
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
    (compile (nix-flake--installable-command "run" options flake-ref attribute
                                             command-args)
             comint))

  (defun nix-flake-log-attribute (options flake-ref attribute)
    "Log a derivation in the current flake.

For OPTIONS, FLAKE-REF, and ATTRIBUTE, see the documentation of
`nix-flake-run-attribute'."
    (interactive (list (nix-flake--options)
                       nix-flake-ref
                       (completing-read "Nix package: "
                                        (nix-flake--build-attribute-names))))
    (compile (nix-flake--installable-command "log" options flake-ref attribute)))

  (define-advice nix-flake-update (:override (options flake-ref) fix-flag)
    (interactive (list (nix-flake--options) nix-flake-ref))
    (compile (nix-flake--command '("flake" "update" "--flake") options flake-ref)))

  (transient-append-suffix 'nix-flake-dispatch '(2 -1)
    '("l" "Log attribute" nix-flake-log-attribute))
  (transient-append-suffix 'nix-flake-dispatch '(2 -1)
    '("o" "Rebuild attribute" nix-flake-rebuild-attribute))

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

  (defun nix-flake-project ()
    "Run command `nix-flake' in project root."
    (interactive)
    (nix-flake (project-root (project-current t))))


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
                                " ")))))

(use-package nix-prettify-mode
  :ensure nix-mode
  :hook proced-mode)

(use-package nix-search
  :ensure nix-mode
  :bind (:map mode-specific-map ("S" . nix-search-transient))
  :config
  (require 'transient)

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
       t search flake))))

(use-package nix-store
  :ensure nix-mode
  :bind (:map mode-specific-map ("T" . nix-store-show-path)))

(use-package nix-ts-mode
  :ensure t
  :defer t)

(use-package notmuch
  :ensure t
  :bind (:map mode-specific-map ("o m" . notmuch)))

(use-package notmuch-address
  :ensure notmuch
  :defer t
  :custom
  (notmuch-address-internal-completion '(received nil))
  (notmuch-address-use-company nil))

(use-package notmuch-draft
  :ensure notmuch
  :defer t
  :custom (notmuch-draft-tags '("+drafts")))

(use-package notmuch-hello
  :ensure notmuch
  :defer t
  :custom
  (notmuch-saved-searches
   '((:name "unread all" :query "tag:unread" :key [117 97])
     (:name "unread litkov" :query "tag:unread and tag:litkov" :key [117 108])
     (:name "unread polimi" :query "tag:unread and tag:polimi" :key [117 112])
     (:name "unread nonsolocodice" :query "tag:unread and tag:nonsolocodice"
            :key [117 110])
     (:name "inbox all" :query "tag:inbox" :key [105 97])
     (:name "inbox litkov" :query "tag:inbox and tag:litkov" :key [105 108])
     (:name "inbox polimi" :query "tag:polimi and tag:inbox" :key [105 112])
     (:name "inbox nonsolocodice" :query "tag:nonsolocodice and tag:inbox" :key
            [105 110])
     (:name "sent all" :query "tag:sent" :key [115 97])
     (:name "sent litkov" :query "tag:litkov and tag:sent" :key [115 108])
     (:name "sent polimi" :query "tag:polimi and tag:sent" :key [115 112])
     (:name "sent nonsolocodice" :query "tag:nonsolocodice and tag:sent" :key
            [115 110])
     (:name "archive all" :query "tag:archive" :key [97 97])
     (:name "archive litkov" :query "tag:litkov and tag:archive" :key [97 108])
     (:name "archive polimi" :query "tag:polimi and tag:archive" :key [97 112])
     (:name "archive nonsolocodice" :query "tag:nonsolocodice and tag:archive"
            :key [97 110])
     (:name "unread spam" :query "tag:unread and tag:spam" :key [117 115])
     (:name "flagged" :query "tag:flagged" :key [102])
     (:name "drafts" :query "tag:drafts" :key [100])))
  (notmuch-show-all-tags-list t)
  (notmuch-show-empty-saved-searches t))

(use-package notmuch-lib
  :ensure notmuch
  :defer t
  :custom
  (notmuch-archive-tags '("+archive" "-flagged" "-inbox" "-spam" "-trash" "-deleted"))
  (notmuch-search-oldest-first nil))

 (use-package notmuch-maildir-fcc
  :ensure notmuch
  :defer t
  :custom
  (notmuch-fcc-dirs
   '(("polimi\\.it" . "polimi/sent +sent +polimi")
     ("litkov\\.one" . "litkov/sent +sent +litkov")
     ("nonsolocodice\\.it" . "nonsolocodice/sent +sent +nonsolocodice"))))

(use-package notmuch-mua
   :ensure notmuch
   :commands notmuch-mua-mail
   :init
   (define-mail-user-agent 'notmuch-user-agent
     'notmuch-mua-mail
     'notmuch-mua-send-and-exit
     'notmuch-mua-kill-buffer
     'notmuch-mua-send-hook)
   :custom
   (notmuch-always-prompt-for-sender t)
   (notmuch-mua-cite-function #'message-cite-original-without-signature)
   (notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full))

(use-package notmuch-show
  :ensure notmuch
  :defer t
  :custom (notmuch-show-all-multipart/alternative-parts t))

(use-package notmuch-tag
  :ensure notmuch
  :defer t
  :custom
  (notmuch-tagging-keys
   '(([?a] notmuch-archive-tags "Archive")
     ([?u] notmuch-show-mark-read-tags "Mark read")
     ([?f] ("-archive" "+flagged" "-inbox" "-spam" "-trash" "-deleted") "Flag")
     ([?s] ("-archive" "-flagged" "-inbox" "+spam" "-trash" "-deleted") "Mark as spam")
     ([?t] ("-archive" "-flagged" "-inbox" "-spam" "+trash" "-deleted") "Trash")
     ([?d] ("+deleted") "Delete")
     ([?i] ("-archive" "-flagged" "+inbox" "-spam" "-trash" "-deleted") "Inbox")
     ([?p] ("-litkov" "+polimi") "Polimi"))))

(use-package nov
  :ensure t
  :mode ((rx ".epub" eos) . nov-mode)
  :custom
  (nov-save-place-file (expand-file-name "emacs/nov-places" (xdg-cache-home)))
  (nov-text-width 80))

(use-package novice
  :defer t
  :init (setq disabled-command-function nil))

(use-package nsm
  :defer t
  :custom (nsm-settings-file (expand-file-name "emacs/network-security.data" (xdg-cache-home))))

(use-package nxml-mode
  :mode (rx ".axaml" eos) (rx ".xaml" eos)
  :custom (nxml-child-indent 4))

(use-package orderless
  :ensure t
  :defer t
  :init
  (cl-pushnew '(const orderless)
              (cdr (nth 3 (get 'completion-styles 'custom-type)))
              :test #'equal)
  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes)))

(use-package org
  :bind (:map org-mode-map ("C-c C-S-t" . org-todo-yesterday))
  :custom
  (org-adapt-indentation nil)
  (org-babel-load-languages
   '((calc . t)
     (emacs-lisp . t)
     (sql . t)
     (shell . t)))
  (org-export-backends '(ascii html icalendar latex md odt))
  (org-log-into-drawer t)
  (org-log-reschedule 'note)
  (org-modules
   '(ol-bbdb
     ol-bookmark
     ol-docview
     ol-doi
     ;; ol-elisp-symbol
     ol-eww
     ;; ol-git-link
     ol-info
     ol-irc
     ;; ol-notmuch
     ol-w3m
     ;; org-annotate-file
     org-checklist
     ;; org-choose
     ;; org-collector
     ;; org-effectiveness
     ;; org-eldoc
     ;; org-eval-light
     ;; org-eval
     ;; org-expiry
     org-id
     ;; org-invoice
     org-habit
     ;; org-learn
     ol-man
     ;; org-notify
     ;; org-panel
     ;; org-registry
     ;; org-screenshot
     ;; org-secretary
     ;; orgtbl-sqlinsert
     ;; org-toc
     org-tempo))
  (org-startup-folded t)
  (org-tags-column 0))

(use-package org-agenda
  :bind
  (:map mode-specific-map ("G a" . org-agenda))
  (:map org-agenda-mode-map ("T" . org-agenda-todo-yesterday))
  :custom
  (org-agenda-files '("~/org/study.org" "~/org/life.org"))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'fortnight)
  (org-agenda-start-on-weekday nil))

(use-package org-annotate-file
  :defer t
  :custom (org-annotate-file-storage-file (expand-file-name "emacs/org-annotate-file.org" (xdg-data-home))))

(use-package org-capture
  :bind (:map mode-specific-map ("G c" . org-capture))
  :custom
  (org-capture-templates
   '(("r" "Remember" entry (file+headline "~/org/life.org" "Remember") "* %?"))))

(use-package org-clock
  :defer t
  :custom (org-clock-display-default-range 'untilnow)
  :config
  (define-advice org-show-notification (:after (&rest _) sound)
    (call-process "notify_ding" nil 0 nil)))

(use-package org-duration
  :defer t
  :custom (org-duration-format 'h:mm))

(use-package org-faces
  :defer t
  :custom-face
  (org-mode-line-clock ((t nil)))
  (org-mode-line-clock-overrun ((t (:background "red")))))

(use-package org-id
  :defer t
  :custom (org-id-locations-file (expand-file-name "emacs/org-id-locations" (xdg-data-home))))

(use-package org-habit
  :defer t
  :custom
  (org-habit-graph-column 54)
  (org-habit-show-done-always-green t))

(use-package org-mime
  :ensure t
  :after message
  :bind
  (:map message-mode-map
        ("C-c M-o" . org-mime-htmlize)
        ("C-c M-e" . org-mime-edit-mail-in-org-mode)
        ("C-c M-t" . org-mime-revert-to-plain-text-mail)))

(use-package org-refile
  :defer t
  :custom
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((org-agenda-files :level . 1)))
  (org-refile-use-outline-path 'file))

(use-package org-roam
  :ensure t
  :bind
  (:map mode-specific-map
        ("G f" . org-roam-node-find)
        ("G i" . org-roam-node-insert)
        ("G l" . org-roam-buffer-toggle)
        ("G s" . org-roam-db-sync))
  :custom
  (org-roam-directory "~/roam/")
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-capture
  :ensure org-roam
  :defer t
  :custom
  (org-roam-capture-templates
   '(("l" "Library" entry
      "* TODO ${title}%? %^g\12:PROPERTIES:\12:AUTHOR: %^{AUTHOR}\12:YEAR: %^{YEAR}\12:ID: %(org-id-uuid)\12:END:"
      :prepend t
      :empty-lines 1
      :target (node "d97e3562-627c-4e0e-906b-e9f1958937a9"))
     ("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\12")
      :unnarrowed t))))

(use-package org-roam-db
  :ensure org-roam
  :defer t
  :custom (org-roam-db-location (expand-file-name "emacs/org-roam.db" (xdg-cache-home))))

(use-package org-src
  :defer t
  :custom (org-edit-src-content-indentation 0))

(use-package ox-html
  :defer t
  :custom (org-html-htmlize-output-type 'css))

(use-package ox-odt
  :disabled
  :defer t
  :config
  (define-advice org-odt-export-to-odt (:around (fn &rest args) ignore-errors)
    (let ((dfn (symbol-function 'delete-directory)))
      (cl-letf (((symbol-function 'delete-directory)
                 (lambda (&rest dargs) (ignore-errors (apply dfn dargs)))))
        (apply fn args)))))

(use-package paragraphs :bind ("C-M-S-t" . transpose-paragraphs))

(use-package paren
  :defer t
  :custom (show-paren-context-when-offscreen 'overlay))

(use-package pcmpl-args
  :ensure t
  :defer t)

(use-package pdf-loader
  :ensure pdf-tools
  :defer t
  :init (pdf-loader-install t t))

(use-package php-mode
  :ensure t
  :defer t
  :custom (php-mode-coding-style 'php))

(use-package pp
  :after elisp-mode
  :bind
  (:map emacs-lisp-mode-map ("C-c RET" . pp-macroexpand-last-sexp))
  (:map lisp-interaction-mode-map ("C-c RET" . pp-macroexpand-last-sexp)))

(use-package proced
  :bind (:map mode-specific-map ("o p" . proced))
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t))

(use-package project
  :defer t
  :custom
  (project-compilation-buffer-name-function #'project-prefixed-buffer-name)
  (project-list-file (expand-file-name "emacs/project.list" (xdg-cache-home)))
  (project-switch-commands
   '((magit-project-status "Magit" nil)
     (project-find-file "Find file" nil)
     (project-dired "Dired" nil)
     (nix-flake-project "Nix flake" nil)
     (project-find-dir "Find directory" nil)
     (project-vc-dir "VC-Dir" nil)
     (project-find-regexp "Find regexp" nil)))
  (project-vc-ignores '("bin/" "obj/" ".vs/"))
  :config
  (keymap-unset project-prefix-map "&" t))

(use-package pueue
  :ensure t
  :bind (:map mode-specific-map ("o u" . pueue)))

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package re-builder
  :after elisp-mode
  :bind
  (:map emacs-lisp-mode-map ("C-c C-r" . re-builder))
  (:map lisp-interaction-mode-map ("C-c C-r" . re-builder)))

(use-package recentf
  :defer t
  :custom
  (recentf-mode t)
  (recentf-save-file (expand-file-name "emacs/recentf" (xdg-cache-home))))

(use-package register
  :bind
  (:map ctl-x-r-map
        ("L" . list-registers)
        ("a" . append-to-register)
        ("p" . prepend-to-register)
        ("v" . view-register))

  :hook
  (ediff-before-setup . (lambda (&rest _) (window-configuration-to-register ?w)))

  :custom
  (register-preview-delay 0.5)
  (register-separator 43))

(use-package repeat
  :defer t
  :custom (repeat-mode t))

(use-package reverse-im
  :ensure t
  :requires cyrillic-dvorak-im
  :config (reverse-im-activate "cyrillic-dvorak"))

(use-package rg-menu
  :ensure rg
  :bind (:map search-map ("M-g r" . rg-menu)))

(use-package rust-ts-mode :mode (rx ".rs" eos))

(use-package rx-widget
  :vc "https://github.com/xFA25E/rx-widget"
  :after wid-edit
  :config (define-widget 'regexp 'rx-widget "A regular expression in rx form."))

(use-package savehist
  :defer t
  :init
  (defun savehist-cleanup-histories ()
    "Cleanup savehist histories.
Remove duplicates.  Remove inexistent files from
`file-name-history'."
    (interactive)
    (dolist (sym savehist-minibuffer-history-variables)
      (when (boundp sym)
        (set sym (cl-delete-duplicates (symbol-value sym) :test #'equal))))
    (setq file-name-history (cl-delete-if-not #'file-exists-p file-name-history)))
  :custom
  (savehist-file (expand-file-name "emacs/savehist" (xdg-cache-home)))
  (savehist-mode t))

(use-package saveplace
  :defer t
  :custom
  (save-place-abbreviate-file-names t)
  (ansave-place-file (expand-file-name "emacs/saveplace" (xdg-cache-home)))
  (save-place-limit 1000)
  (save-place-mode t)
  (save-place-skip-check-regexp
   "\\`\\(?:http\\|/\\(?:cdrom\\|floppy\\|mnt\\|\\(?:[^/:@]*@\\)?[^/:@]*[^./:@]:\\)\\)"))

(use-package scheme
  :defer t
  :custom (scheme-program-name "guile"))

(use-package scroll-bar
  :defer t
  :custom (scroll-bar-mode nil))

(use-package sdcwoc
  :vc "https://github.com/xFA25E/sdcwoc"
  :bind (:map mode-specific-map ("o t" . sdcwoc))
  :custom
  (sdcwoc-categories
   '(("english" "Accounting (En-Ru)" "American Heritage Dictionary 4th Ed. (En-En)"
      "American_Idioms 2nd Ed" "Americana (En-Ru)" "Apresyan (En-Ru)"
      "Auto (En-Ru)" "Biology (En-Ru)" "Building (En-Ru)"
      "CMU American English spelling"
      "Cambridge Advanced Learners Dictionary 3th Ed. (En-En)"
      "Cambridge Dictionary of American Idioms (En-En)"
      "Chambers Dictionary 11th Ed. (En-En)" "Civil Aviation (En-Ru) LE"
      "Collins Cobuild 5" "Collins Cobuild English Dictionary"
      "Collins Thesaurus (En-En)" "Computers (En-Ru)"
      "Concise Oxford English Dictionary (En-En)"
      "Concise Oxford Thesaurus 2nd Ed. (Eng-Eng)" "Electrical (En-Ru) LE"
      "Engineering (En-Ru)" "English - Italian" "English - Russian"
      "English Etymology" "English Thesaurus" "English-Italian"
      "English-Russian" "Essential (En-Ru)" "FinancialManagement (En-Ru)"
      "FinancialMarkets (En-Ru)" "Free On-Line Dictionary of Computing"
      "Full English-Russian"
      "GNU Collaborative International Dictionary of English"
      "GNU/Linux English-English Dictionary" "GreatBritain (En-Ru)"
      "Informal (En-Ru)" "Jargon File" "Law (En-Ru)" "LingvoComputer (En-Ru)"
      "LingvoEconomics (En-Ru)" "LingvoGrammar (En-Ru)" "LingvoScience (En-Ru)"
      "LingvoUniversal (En-Ru)" "Longman Dictionary of Common Errors (En-En)"
      "Longman Dictionary of Contemporary English 5th Ed. (En-En)"
      "Longman Dictionary of Contemporary English Extras 5th Ed. (En-En)"
      "Longman Dictionary of Contemporary English"
      "Longman Language Activator 2nd Ed. (En-En)"
      "Macmillan English Dictionary (En-En)"
      "Macmillan English Thesaurus (En-En)" "Management (En-Ru)"
      "Maritime Technical (En-Ru) LE" "Marketing (En-Ru)"
      "MechanicalEngineering (En-Ru)" "Medical (En-Ru)"
      "Merriam-Webster's Advanced Learner's Dictionary (En-En)"
      "Merriam-Webster's Collegiate 11th Ed. (En-En)"
      "Merriam-Webster's Collegiate Thesaurus (En-En)"
      "Merrian Webster 10th dictionary" "Military (En-Ru) LE"
      "Moby Thesaurus II" "Naval (En-Ru) LE" "Obscene language (En-Ru)"
      "OilAndGas (En-Ru)" "Oxford Advanced Learner's Dictionary 8th Ed."
      "Oxford Advanced Learner's Dictionary"
      "Oxford English Dictionary 2nd Ed. P1"
      "Oxford English Dictionary 2nd Ed. P2"
      "Oxford Guide to British and American Culture (En-En)" "Patents (En-Ru)"
      "Physics (En-Ru)" "Polytechnical (En-Ru)"
      "Random House Webster's Unabridged Dictionary (En-En)"
      "Refrence Dictionary for Linux Enviroment Translation"
      "Roget's II The New Thesaurus 3th Ed. (En-En)"
      "Soule's Dictionary of English Synonyms (En-En)" "Telecoms (En-Ru)"
      "The BBI Combinatory Dictionary of English (En-En)"
      "The Britannica Concise" "The Chambers Thesaurus (En-En)"
      "The Idiom Connection (En-En)" "Urban Dictionary P1 (En-En)"
      "Urban Dictionary P2 (En-En)" "Virtual Entity of Relevant Acronyms"
      "Webster's Revised Unabridged Dictionary (1913)"
      "Webster's Third New International Dictionary, Unabridged (En-En)"
      "Wine (En-Ru)" "computer-dictionary" "dictd_www.dict.org_devils"
      "dictd_www.dict.org_gcide" "dictd_www.mova.org_geology_enru"
      "dictd_www.mova.org_korolew_enru" "dictd_www.mova.org_slovnyk_en-ru"
      "dictd_www.mova.org_sokrat_enru" "en-ru-bars" "pc-user-dictionary"
      "quick_english-italian" "quick_english-russian" "wtf (acronyms)"
      "Современный свободный словарь компьютерных терминов.")
     ("english->russian" "Accounting (En-Ru)" "Americana (En-Ru)"
      "Apresyan (En-Ru)" "Auto (En-Ru)" "Biology (En-Ru)" "Building (En-Ru)"
      "Civil Aviation (En-Ru) LE" "Computers (En-Ru)" "Electrical (En-Ru) LE"
      "Engineering (En-Ru)" "English - Russian" "English-Russian"
      "Essential (En-Ru)" "FinancialManagement (En-Ru)"
      "FinancialMarkets (En-Ru)" "Full English-Russian" "GreatBritain (En-Ru)"
      "Informal (En-Ru)" "Law (En-Ru)" "LingvoComputer (En-Ru)"
      "LingvoEconomics (En-Ru)" "LingvoGrammar (En-Ru)" "LingvoScience (En-Ru)"
      "LingvoUniversal (En-Ru)" "Management (En-Ru)"
      "Maritime Technical (En-Ru) LE" "Marketing (En-Ru)"
      "MechanicalEngineering (En-Ru)" "Medical (En-Ru)" "Military (En-Ru) LE"
      "Naval (En-Ru) LE" "Obscene language (En-Ru)" "OilAndGas (En-Ru)"
      "Patents (En-Ru)" "Physics (En-Ru)" "Polytechnical (En-Ru)"
      "Telecoms (En-Ru)" "Wine (En-Ru)" "computer-dictionary"
      "dictd_www.mova.org_geology_enru" "dictd_www.mova.org_korolew_enru"
      "dictd_www.mova.org_slovnyk_en-ru" "dictd_www.mova.org_sokrat_enru"
      "en-ru-bars" "pc-user-dictionary" "quick_english-russian"
      "Современный свободный словарь компьютерных терминов.")
     ("english->italian" "English - Italian" "English-Italian"
      "quick_english-italian")
     ("russian" "Auto (Ru-En)" "Auto (Ru-It)" "Biology (Ru-En)"
      "Building (Ru-En)" "Computers (Ru-En)" "Economics (Ru-It)"
      "Engineering (Ru-En)" "Essential (Ru-En)" "Full Russian-English"
      "Law (Ru-En)" "LingvoComputer (Ru-En)" "LingvoEconomics (Ru-En)"
      "LingvoScience (Ru-En)" "LingvoUniversal (Ru-En)"
      "MechanicalEngineering (Ru-En)" "Medical (Ru-En)" "Medical (Ru-It)"
      "OilAndGas (Ru-En)" "Ozhegov Shvedova (Ru-Ru)" "Patents (Ru-En)"
      "PhraseBook (Ru-En)" "PhraseBook (Ru-It)" "Physics (Ru-En)"
      "Polytechnical (Ru-En)" "Polytechnical (Ru-It)" "Russian-English"
      "Telecoms (Ru-En)" "Ushakov's Dictionary (Ru-Ru)" "Vasmer (Ru-Ru)"
      "dictd_www.mova.org_geology_ruen" "dictd_www.mova.org_korolew_ruen"
      "dictd_www.mova.org_slovnyk_ru-en" "dictd_www.mova.org_sokrat_ruen"
      "myspell Russian grammar forms" "quick_russian-english" "Словарь Даля")
     ("russian->english" "Auto (Ru-En)" "Biology (Ru-En)" "Building (Ru-En)"
      "Computers (Ru-En)" "Engineering (Ru-En)" "Essential (Ru-En)"
      "Full Russian-English" "Law (Ru-En)" "LingvoComputer (Ru-En)"
      "LingvoEconomics (Ru-En)" "LingvoScience (Ru-En)"
      "LingvoUniversal (Ru-En)" "MechanicalEngineering (Ru-En)"
      "Medical (Ru-En)" "OilAndGas (Ru-En)" "Patents (Ru-En)"
      "PhraseBook (Ru-En)" "Physics (Ru-En)" "Polytechnical (Ru-En)"
      "Russian-English" "Telecoms (Ru-En)" "dictd_www.mova.org_geology_ruen"
      "dictd_www.mova.org_korolew_ruen" "dictd_www.mova.org_slovnyk_ru-en"
      "dictd_www.mova.org_sokrat_ruen" "quick_russian-english")
     ("russian->italian" "Auto (Ru-It)" "Economics (Ru-It)" "Medical (Ru-It)"
      "PhraseBook (Ru-It)" "Polytechnical (Ru-It)")
     ("italian" "Auto (It-Ru)" "Economics (It-Ru)" "Essential (It-Ru)"
      "Italian-English" "Medical (It-Ru)" "Polytechnical (It-Ru)"
      "Universal (It-Ru)" "quick_italian-english")
     ("italian->english" "Italian-English" "quick_italian-english")
     ("italian->russian" "Auto (It-Ru)" "Economics (It-Ru)" "Essential (It-Ru)"
      "Medical (It-Ru)" "Polytechnical (It-Ru)" "Universal (It-Ru)"))))

(use-package semantic/symref/grep
  :defer t
  :config
  (setf (alist-get 'go-ts-mode semantic-symref-filepattern-alist) (list "*.go")))

(use-package sendmail
  :defer t
  :custom
  (mail-envelope-from 'header)
  (send-mail-function #'message-send-mail-with-sendmail)
  (sendmail-program "msmtp"))

(use-package sgml-mode
  :hook (nxml-mode . sgml-electric-tag-pair-mode)
  :bind
  (:map sgml-mode-map
    ("C-M-n" . sgml-skip-tag-forward)
    ("C-M-p" . sgml-skip-tag-backward)
    ("C-c C-r" . sgml-namify-char))
  :custom
  (sgml-basic-offset 4)
  :config
  (keymap-unset html-mode-map "M-o" t))

(use-package sh-script
  :hook ((bash-ts-mode sh-mode) . sh-electric-here-document-mode))

(use-package shell
  :bind
  (:map mode-specific-map
        ("s" . shell)
        ("l" . shell-list))
  :config
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
        (hl-line-mode t)))))

(use-package shr
  :defer t
  :custom
  (shr-max-image-proportion 0.7)
  (shr-use-fonts nil))

(use-package simple
  :bind
  ("C-h" . backward-delete-char-untabify)
  ("C-w" . kill-region-dwim)
  ("M-K" . kill-whole-line)
  ("M-\\" . delete-indentation)
  ("M-c" . capitalize-dwim)
  ("M-l" . downcase-dwim)
  ("M-u" . upcase-dwim)
  ([remap back-to-indentation] . back-to-indentation-or-beginning)
  (:map ctl-x-map
        ("u" . mark-whole-buffer))
  (:map ctl-x-x-map
        ("f" . auto-fill-mode)
        ("v" . visual-line-mode))
  (:map mode-specific-map
        ("o P" . list-processes))
  (:repeat-map transpose-lines-repeat-map
               ("t" . transpose-lines)
               ("T" . transpose-lines-backward))

  :hook (before-save . delete-trailing-whitespace)

  :custom
  (async-shell-command-buffer 'new-buffer)
  (column-number-mode t)
  (completion-show-help nil)
  (eval-expression-print-length t)
  (eval-expression-print-level t)
  (goto-line-history-local t)
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t)
  (kill-read-only-ok t)
  (mail-user-agent 'notmuch-user-agent)
  (next-error-found-function 'next-error-quit-window)
  (next-error-message-highlight t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (save-interprogram-paste-before-kill t)
  (shift-select-mode nil)
  (size-indication-mode t)

  :config
  (defun kill-region-dwim (&optional count)
    "Kill word or kill region if it's active.
See `backward-kill-word' for COUNT."
    (interactive "p")
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word count)))

  (defun back-to-indentation-or-beginning (arg)
    "Move point to indentation or to beginning of line if it's alreade there.
ARG as in `move-beginning-of-line'."
    (interactive "^p")
    (when (= (point) (progn (back-to-indentation) (point)))
      (move-beginning-of-line arg)))

  (defun transpose-lines-backward (arg)
    "`transpose-lines' with negative ARG."
    (interactive "*p")
    (transpose-lines (- arg))))

(use-package simple
  :after minibuffer
  :bind (:map completion-in-region-mode-map ("M-v" . switch-to-completions)))

(use-package sly
  :ensure t
  :hook (lisp-mode . sly-editing-mode)
  :custom
  (sly-default-lisp 'sbcl)
  (sly-lisp-implementations
   '((sbcl ("sbcl"))
     (ecl ("ecl"))
     (ccl ("ccl"))
     (clisp ("clisp"))
     (abcl ("abcl")))))

(use-package sly-asdf
  :ensure t
  :defer t)

(use-package sly-quicklisp
  :ensure t
  :defer t)

(use-package so-long
  :defer t
  :custom (global-so-long-mode t))

(use-package solar
  :defer t
  :custom
  (calendar-latitude 45.724458)
  (calendar-longitude 9.038057))

(use-package sql
  :defer t
  :custom
  (sql-input-ring-file-name (expand-file-name "emacs/sql_history" (xdg-cache-home)))
  (sql-sqlite-options '("-column" "-header" "-cmd" "PRAGMA foreign_keys = ON;"))
  (sql-sqlite-program "sqlite3"))

(use-package sql-indent
  :ensure t
  :defer t)

(use-package sqlup-mode
  :ensure t
  :hook sql-mode)

(use-package startup
  :defer t
  :custom
  (auto-save-list-file-prefix
   (expand-file-name "emacs/auto-saves-list/.saves-" (xdg-cache-home)))
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil))

(use-package subr-x
  :defer t
  :init
  (put 'thread-first 'lisp-indent-function 1)
  (put 'thread-last 'lisp-indent-function 1))

(use-package subword
  :hook
  csharp-mode
  java-mode
  js-ts-mode
  nix-mode nix-ts-mode
  php-mode
  rust-ts-mode)

(use-package tab-bar
  :bind
  (:map tab-prefix-map
        ("M-b" . tab-bar-history-back)
        ("M-f" . tab-bar-history-forward))
  (:repeat-map tab-bar-history-repeat-map
               ("M-b" . tab-bar-history-back)
               ("M-f" . tab-bar-history-forward))

  :custom
  (tab-bar-auto-width nil)
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))
  (tab-bar-history-mode t)
  (tab-bar-new-button-show nil)
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-show 1)
  (tab-bar-tab-hints t)

  :config
  (keymap-unset tab-prefix-map "1" t)

  :custom-face
  (tab-bar-tab ((t (:inherit tab-bar :background "white smoke" :foreground "black" :box (:line-width (1 . 1) :color "white smoke")))))
  (tab-bar-tab-group-current ((t (:weight bold :box nil :inverse-video t :inherit tab-bar-tab))))
  (tab-bar-tab-group-inactive ((t (:inherit tab-bar-tab-inactive))))
  (tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "dark grey" :foreground "black" :box (:line-width (1 . 1) :color "black")))))
  (tab-bar-tab-ungrouped ((t (:inherit tab-bar-tab-inactive)))))

(use-package tempo
  :bind
  (:map goto-map
        ("M-e" . tempo-forward-mark)
        ("M-a" . tempo-backward-mark)))

(use-package tempo-extra
  :vc "https://github.com/xFA25E/tempo-extra"
  :after csharp-mode
  :defer t
  :init
  (require 'tempo-extra)
  (require 'tempo-extra-csharp)

  (defun tempo-extra-custom-csharp-elements (element)
    "Additional custom elements.
For ELEMENT see `tempo-define-template'."
    (pcase element
      (:csharp-file-class
       (thread-last (if-let ((bfn (buffer-file-name)))
                        (file-name-nondirectory bfn)
                      (buffer-name))
         (string-remove-suffix ".cs")
         (string-remove-suffix ".xaml")))
      (:csharp-current-class
       (when-let ((node (treesit-parent-until
                         (treesit-node-at (point) 'c-sharp)
                         (lambda (node)
                           (member
                            (treesit-node-type node)
                            '("class_declaration" "record_declaration"))))))

         (treesit-node-text (treesit-node-child-by-field-name node "name") t)))))

  (add-hook 'tempo-user-elements #'tempo-extra-custom-csharp-elements)

  (tempo-extra-define "ctor" 'csharp-ts-mode
    '("public " :csharp-current-class "(" p ")" > n
      "{" > n
      p > n
      "}">))

  (dolist (mode '(csharp-mode csharp-ts-mode))

    (tempo-extra-define "wl" mode
      '("Console.WriteLine(\"{0}\", " p ");" >))

    (tempo-extra-define "puc" mode
      '("public static class " p :csharp-file-class > n
        "{" > n
        p > n
        "}" >))

    (tempo-extra-define "pur" mode
      '("public record class " p :csharp-file-class "();"))

    (tempo-extra-define "prm" mode
      '("private static async Task " p "M()" > n
        "{" > n
        p > n
        "}" >))

    (tempo-extra-define "namespace" mode
      '("namespace "
        (if-let* ((project (project-current))
                  (project-root (expand-file-name (project-root project)))
                  (file-path (string-remove-prefix project-root (buffer-file-name)))
                  (dir-path (file-name-directory file-path)))
            (thread-last dir-path
              (string-remove-prefix "trunk/")
              (string-remove-suffix "/")
              (string-replace "/" "."))
          (file-name-sans-extension (buffer-name)))
        ";"))

    (tempo-extra-define "if" mode
      '("if (" p ")" > n
        "{" > n
        p r > n
        "}" >))

    (tempo-extra-define "foreach" mode
      '("foreach (var " p " in " p ")" > n
        "{" > n
        p > n
        "}" >))

    (tempo-extra-define "try" mode
      '("try" > n
        "{" > n
        p r > n
        "} catch {" p "}" >))

    (tempo-extra-define "vmp" mode
      '((P "Type: " property-type noinsert)
        (P "Property name: " property-name noinsert)

        (ignore
         (let ((property-name (tempo-lookup-named 'property-name)))
           (tempo-save-named
            'field-name
            (if (zerop (length property-name))
                property-name
              (concat "_" (downcase (substring property-name 0 1))
                      (substring property-name 1))))))

        "private " (s property-type) " " (s field-name) p ";" > n
        "public " (s property-type) " " (s property-name) > n
        "    {" > n
        (l "        get => " (s field-name) ";" > n
           "        private set => SetField(ref " (s field-name) ", value);" > n)
        "    }" >))))

(use-package tempo-extra
  :vc "https://github.com/xFA25E/tempo-extra"
  :after elisp-mode
  :defer t
  :init (require 'tempo-extra-elisp))

(use-package tempo-extra
  :vc "https://github.com/xFA25E/tempo-extra"
  :after js
  :defer t
  :init (require 'tempo-extra-js))

(use-package tempo-extra
  :vc "https://github.com/xFA25E/tempo-extra"
  :after lisp-mode
  :defer t
  :init (require 'tempo-extra-lisp))

(use-package tempo-extra
  :vc "https://github.com/xFA25E/tempo-extra"
  :after nix-mode
  :defer t
  :init (require 'tempo-extra-nix))

(use-package tempo-extra
  :vc "https://github.com/xFA25E/tempo-extra"
  :after org
  :defer t
  :init (require 'tempo-extra-org))

(use-package tempo-extra
  :vc "https://github.com/xFA25E/tempo-extra"
  :after nix-ts-mode
  :defer t
  :init
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

(use-package text-mode
  :commands center-region
  :hook (text-mode . text-mode-hook-identify))

(use-package term :bind (:map mode-specific-map ("t" . term)))

(use-package time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-mail-function
   (lambda ()
     (ignore-errors
       (let ((count (car (process-lines "notmuch" "count" "tag:unread"))))
         (unless (string= "0" count) count)))))
  (display-time-mode t)
  (display-time-string-forms
   '((format-time-string "%a %b %e %H:%M" now) (if mail (concat " @" mail) "") " ")))

(use-package tramp
  :bind (:map ctl-x-x-map ("T" . tramp-cleanup-all-buffers))
  :custom
  (tramp-default-method "ssh")
  :config
  (customize-set-variable
   'tramp-remote-path
   (cl-remove-duplicates
    (cl-list* "/run/wrappers/bin" "/run/current-system/sw/bin" tramp-remote-path)
    :test #'equal))

  (customize-set-variable
   'tramp-remote-process-environment
   (cons "ENV=~/.profile" (cl-remove "ENV=" tramp-remote-process-environment :test #'string-prefix-p))))

(use-package tramp-cache
  :defer t
  :custom
  (tramp-persistency-file-name (expand-file-name "emacs/tramp" (xdg-cache-home))))

(use-package tramp-message
  :defer t
  :custom (tramp-debug-to-file t))

(use-package transient
  :bind (:map ctl-x-map ("C-r" . region-commands))

  :config
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

  :custom
  (transient-enable-popup-navigation t)
  (transient-history-file (expand-file-name "emacs/transient/history.el" (xdg-cache-home)))
  (transient-levels-file (expand-file-name "emacs/transient-levels.el" (xdg-data-home)))
  (transient-values-file (expand-file-name "emacs/transient/values.el" (xdg-cache-home))))

(use-package transmission
  :ensure t
  :bind
  (:map mode-specific-map ("o r" . transmission))
  (:map transmission-mode-map ("M" . transmission-move))
  :custom
  (transmission-pieces-function 'transmission-format-pieces-brief)
  (transmission-units 'si))

(use-package transmission
  :ensure t
  :defer t
  :after browse-url
  :init
  (setf
   (alist-get (rx ".torrent" eos) browse-url-handlers nil nil #'equal)
   (lambda (url &rest _) (transmission-add url (read-directory-name "Target directory: ")))))

(use-package treesit
  :defer t
  :custom (treesit-font-lock-level 4))

(use-package treesit
  :when (eq 'windows-nt system-type)
  :defer t
  :config
  (setf (alist-get 'c-sharp treesit-language-source-alist) '("https://github.com/tree-sitter/tree-sitter-c-sharp"))
  (setf (alist-get 'json treesit-language-source-alist) '("https://github.com/tree-sitter/tree-sitter-json")))

(use-package undo
  :defer t
  :custom
  (undo-limit 200000)
  (undo-strong-limit 300000))

(use-package uniquify
  :defer t
  :custom (uniquify-ignore-buffers-re (rx bol "*")))

(use-package url
  :defer t
  :custom
  (url-configuration-directory (expand-file-name "emacs/url/" (xdg-cache-home))))

(use-package url-handles
  :defer t
  :custom (url-handler-mode t))

(use-package url-parse
  :defer t
  :config
  (define-advice url-generic-parse-url (:around (fn &rest args) save-match-data)
    (save-match-data (apply fn args))))

(use-package vc-dir
  :bind
  (:map vc-dir-mode-map
        ("* u" . vc-dir-mark-needs-update-files)
        ("* m" . vc-dir-mark-needs-merge-files)
        ("K" . vc-dir-kill-marked-files))
  :config
  (defun vc-dir-kill-marked-files ()
    "Kill marked files."
    (interactive)
    (ewoc-filter vc-ewoc (lambda (data) (not (vc-dir-fileinfo->marked data)))))

  (defun vc-dir-mark-needs-update-files ()
    "Mark files that are in needs-update state."
    (interactive)
    (vc-dir-mark-state-files 'needs-update))

  (defun vc-dir-mark-needs-merge-files ()
    "Mark files that are in needs-merge state."
    (interactive)
    (vc-dir-mark-state-files 'needs-merge)))

(use-package vc-hooks
  :defer t
  :custom (vc-handled-backends '(Git))
  :config
  (customize-set-variable
   'vc-directory-exclusion-list
   (cons ".eldev" (cl-remove ".eldev" vc-directory-exclusion-list :test #'equal))))

(use-package verb
  :ensure t
  :defer t)

(use-package vertico
  :ensure t
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("C-h" . vertico-directory-delete-char)
        ("C-w" . vertico-directory-delete-word)
        ("M-z" . vertico-quick-exit)
        ("M-Z" . vertico-quick-insert))
  :custom
  (vertico-mode t))

(use-package vertico-directory
  :ensure vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vlf
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :mode (rx ".twig" eos)
  :custom (web-mode-markup-indent-offset 2))

(use-package wgrep
  :ensure t
  :defer t
  :custom (wgrep-auto-save-buffer t))

(use-package whitespace
  :bind (:map ctl-x-x-map ("w" . whitespace-mode))
  :hook (before-save . whitespace-cleanup))

(use-package wid-edit
  :bind
  (:map widget-field-keymap ("C-x n f" . widget-narrow-to-field))
  (:map widget-text-keymap ("C-x n f" . widget-narrow-to-field)))

(use-package window
  :bind
  ("M-Q" . quit-window)
  ("M-V" . scroll-down-line)
  ("M-o" . other-window)
  ("C-M-S-b" . previous-buffer)
  ("C-M-S-f" . next-buffer)
  ("C-S-v" . scroll-up-line)

  :hook (xref-after-jump . recenter)

  :custom
  (display-buffer-alist
   (list (cons (rx (or "*Pueue*" "*SDCWOC*"))
               '((display-buffer-reuse-window display-buffer-same-window)))
         (cons (rx "*org-roam*")
               '((display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))))
  (next-screen-context-lines 10)
  (recenter-positions '(top middle bottom))

  :config
  (put 'other-window 'repeat-map nil))

(use-package with-editor
  :ensure t
  :bind ("C-M-!" . with-editor-shell-command)
  :config
  (define-advice with-editor-shell-command-read-args
      (:filter-args (args) prefix-prompt)
    (cons (concat "WE " (car args)) (cdr args))))

(use-package xdisp
  :defer t
  :custom
  (hscroll-step 1)
  (max-mini-window-height 0.5)
  (mode-line-compact 'long)
  (scroll-conservatively 10000)
  (scroll-step 1)
  (x-stretch-cursor t))

(use-package xref
  :bind
  (:map xref--xref-buffer-mode-map
        ("RET" . xref-quit-and-goto-xref)
        ("o" . xref-goto-xref))

  :hook
  (find-function-after . xref-push-previous-buffer-marker-stack)
  (xref-after-jump . xref-pulse-momentarily)

  :custom
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom)

  :config
  (defun xref-push-previous-buffer-marker-stack ()
    "Push xref marker of previous buffer."
    (previous-buffer)
    (xref-push-marker-stack)
    (next-buffer)))

(provide 'init)

;;; init.el ends here
