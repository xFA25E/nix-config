;; -*- lexical-binding: t; -*-

;;; USE-PACKAGE INIT

(require 'xdg)


;;;; PACKAGE

(customize-set-variable
 'nsm-settings-file (expand-file-name "emacs/network-security.data" (xdg-cache-home)))
(customize-set-variable
 'package-user-dir (expand-file-name "emacs/elpa" (xdg-cache-home)))
(customize-set-variable
 'gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(customize-set-variable
 'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("org"   . "https://orgmode.org/elpa/")))

(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;;; USE-PACKAGE VARIABLES

(customize-set-variable 'use-package-enable-imenu-support t)
(customize-set-variable 'use-package-expand-minimally t)
(customize-set-variable 'use-package-always-defer t)
(customize-set-variable 'use-package-hook-name-suffix nil)
(require 'use-package)


;;; UTILS

(use-package xdg
  :commands xdg-download-dir xdg-music-dir

  :config
  (defun xdg-download-dir () (or (getenv "XDG_DOWNLOAD_DIR") "~/Downloads"))
  (defun xdg-music-dir () (or (getenv "XDG_MUSIC_DIR") "~/Music")))

(use-package diminish :ensure t)

(use-package quelpa
  :ensure t
  :demand t

  :custom
  (quelpa-build-dir (expand-file-name "emacs/quelpa/build" (xdg-cache-home)))
  (quelpa-dir (expand-file-name "emacs/quelpa" (xdg-cache-home)))
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :demand t
  :custom (quelpa-use-package-inhibit-loading-quelpa t))

(use-package rx :config (rx-define ext (&rest exts) (and "." (or exts) string-end)))

(use-package subr
  :init (provide 'subr)
  :commands
  add-to-list
  alist-get
  derived-mode-p
  error
  replace-regexp-in-string
  shell-quote-argument
  split-string
  start-process
  with-current-buffer
  :config (defalias 'yes-or-no-p 'y-or-n-p))

(use-package subr-x :commands when-let thread-last)


;;; SETTINGS

(use-package emacs
  :bind
  ("C-S-SPC" . insert-space-after-point)

  :custom
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (enable-recursive-minibuffers t)
  (history-delete-duplicates t)
  (history-length 300)
  (hscroll-step 1)
  (indent-tabs-mode nil)
  (indicate-buffer-boundaries 'left)
  (indicate-empty-lines t)
  (next-screen-context-lines 10)
  (tab-width 4)
  (undo-limit 200000)
  (undo-outer-limit 20000000)
  (undo-strong-limit 300000)
  (use-dialog-box nil)
  (visible-bell nil)
  (x-gtk-use-system-tooltips nil)
  (x-stretch-cursor t)
  (fill-column 80)
  (help-char (aref (kbd "C-l") 0))
  (kill-buffer-query-functions
   (remq #'process-kill-buffer-query-function kill-buffer-query-functions))
  (user-full-name "Valeriy Litkovskyy")
  (read-process-output-max (* 1024 1024))
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)

  :config
  (setq-default line-spacing 0.2)

  (defun insert-space-after-point ()
    (interactive)
    (save-excursion (insert " "))))


;;;; FACES

(use-package mb-depth :hook (after-init-hook . minibuffer-depth-indicate-mode))

(use-package so-long :hook (after-init-hook . global-so-long-mode))

(use-package rainbow-mode :ensure t)

(use-package paren
  :custom (show-paren-style 'parentheses)
  :hook (after-init-hook . show-paren-mode))

(use-package hl-line
  :hook
  ((csv-mode-hook
    dired-mode-hook
    grep-mode-hook
    mingus-browse-hook
    mingus-playlist-hooks
    tar-mode-hook
    transmission-files-mode-hook
    transmission-mode-hook
    transmission-peers-mode-hook
    ytel-mode-hook) . hl-line-mode))

(use-package diff-hl
  :ensure t

  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (prog-mode-hook          . diff-hl-mode)
  (org-mode-hook           . diff-hl-mode)
  (dired-mode-hook         . diff-hl-dired-mode))

(use-package ansi-color
  :hook
  (shell-mode-hook         . ansi-color-for-comint-mode-on)
  (compilation-filter-hook . colorize-compilation)

  :config
  (defun colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point)))))

(use-package form-feed
  :ensure t
  :diminish form-feed-mode
  :hook ((emacs-lisp-mode-hook scheme-mode-hook) . form-feed-mode))


;;;;; THEMES

(use-package custom :commands load-theme custom-theme-enabled-p)

(use-package acme-theme
  :ensure t
  :init (load-theme 'acme t))

(use-package faces
  :bind (:map help-map ("F" . list-faces-display))

  :config
  (set-face-attribute 'default nil :family "Iosevka")
  (set-face-attribute 'default nil :height 165)
  (set-face-attribute 'mode-line nil :family "DejaVu Sans")
  (set-face-attribute 'mode-line nil :height 125)
  (set-face-attribute 'mode-line-inactive nil :family "DejaVu Sans" )
  (set-face-attribute 'mode-line-inactive nil :height 125)
  (set-face-attribute 'fixed-pitch-serif nil :family "DejaVu Serif")
  (set-face-attribute 'header-line nil :inverse-video nil :family "Iosevka")

  (with-eval-after-load 'man
    (set-face-attribute 'Man-overstrike nil :inherit 'font-lock-variable-name-face :bold t)
    (set-face-attribute 'Man-underline nil :inherit 'font-lock-negation-char-face :underline t))

  (when (custom-theme-enabled-p 'acme)
    (with-eval-after-load 'comint
      (set-face-attribute 'comint-highlight-input nil :inherit 'diff-added)
      (set-face-attribute 'comint-highlight-prompt nil :inherit 'diff-hl-reverted-hunk-highlight))
    (with-eval-after-load 'isearch
      (set-face-attribute 'isearch-fail nil :background "LightSalmon1")))

  (when (custom-theme-enabled-p 'leuven)
    (with-eval-after-load 'comint
      (set-face-attribute 'comint-highlight-input nil :inherit 'diff-added)
      (set-face-attribute 'comint-highlight-prompt nil :inherit 'diff-hl-change))
    (with-eval-after-load 'compile
      (set-face-attribute 'compilation-info nil :foreground "deep sky blue")
      (set-face-attribute 'compilation-mode-line-exit nil :foreground "lawn green"))
    (with-eval-after-load 'mu4e
      (set-face-attribute 'mu4e-context-face nil :foreground "orange")
      (set-face-attribute 'mu4e-modeline-face nil :foreground "green"))
    (with-eval-after-load 'org
      (set-face-attribute 'org-list-dt nil :foreground "sky blue"))))


;;;;; OUTLINE

(use-package outline
  :diminish outline-minor-mode
  :hook (emacs-lisp-mode-hook . outline-minor-mode)

  :config
  (defun outline-show-after-jump ()
    (when outline-minor-mode
      (outline-show-entry)))

  (with-eval-after-load 'xref
    (add-hook 'xref-after-jump-hook #'outline-show-after-jump))

  (with-eval-after-load 'imenu
    (add-hook 'imenu-after-jump-hook #'outline-show-after-jump)))

(use-package bicycle
  :ensure t
  :after outline

  :bind
  (:map outline-minor-mode-map
        ("<C-tab>" . bicycle-cycle)
        ("<backtab>" . bicycle-cycle-global)))

(use-package outline-minor-faces
  :ensure t
  :after outline
  :hook (outline-minor-mode-hook . outline-minor-faces-add-font-lock-keywords))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (emacs-lisp-mode-hook . hs-minor-mode)
  :bind (:map hs-minor-mode-map ("<C-M-tab>" . #'hs-toggle-hiding)))

(use-package bicycle
  :ensure t
  :after hideshow

  :bind
  (:map hs-minor-mode-map
        ("<C-tab>" . bicycle-cycle)
        ("<C-M-tab>" . #'hs-toggle-hiding)
        ("<backtab>" . bicycle-cycle-global)))


;;;; GUI

(use-package tool-bar :config (tool-bar-mode -1))

(use-package scroll-bar
  :custom
  (scroll-step 1)
  (scroll-conservatively 10000)

  :config (scroll-bar-mode -1))

(use-package menu-bar
  :bind ("<f10>" . menu-bar-mode)
  :config (menu-bar-mode -1))

(use-package tooltip :config (tooltip-mode -1))

(use-package frame
  :config
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (define-advice suspend-frame (:override ()) nil))


;;;; AUTH

(use-package auth-source
  :custom (auth-sources '("~/.authinfo.gpg" "~/.netrc" "~/.authinfo")))

(use-package auth-source-pass
  :custom
  (auth-source-pass-filename
   (or (getenv "PASSWORD_STORE_DIR") (expand-file-name "pass" (xdg-data-home)))))


;;;; URL

(use-package browse-url
  :custom (browse-url-secondary-browser-function #'browse-url-firefox)
  :bind (:map ctl-x-map ("B" . browse-url)))

(use-package bruh
  :after browse-url
  :quelpa (bruh :repo "a13/bruh" :fetcher github)

  :custom
  (bruh-default-browser #'eww-browse-url)
  (bruh-videos-browser-function #'bruh-mpvi-ytdli-or-browse)
  (browse-url-browser-function #'bruh-browse-url)
  (bruh-mpvi-get-title-functions nil)

  :config
  (defun bruh-mpvi-ytdli-or-browse (url &rest rest)
    (cond
     ((yes-or-no-p (format "Mpv %s ?" url))
      (let ((process-environment (browse-url-process-environment)))
        (start-process (concat "mpv " url) nil "mpvi" url)))
     ((yes-or-no-p (format "Download %s ?" url))
      (let ((process-environment (browse-url-process-environment)))
        (start-process
         (concat "ytdl " url) nil "ytdli" url
         (funcall
          (or (seq-some
               (lambda (args) (when (derived-mode-p (car args)) (cdr args)))
               bruh-mpvi-get-title-functions)
              (lambda () (read-from-minibuffer "Title: ")))))))
     ((and
       (string-match
        (rx (or (and "youtube.com/watch?" (*? any) "v=") "youtu.be/")
            (group (= 11 (any "-_A-Za-z0-9"))))
        url)
        (yes-or-no-p (format "Show %s in ytel-show?" url)))
      (ytel-show (vector (match-string 1 url))))
     (t
      (apply bruh-default-browser url rest))))

  (dolist (re `(,(rx bos "http" (? "s") "://" (? "www.") "bitchute.com/" (or "embed" "video" "channel"))
                ,(rx bos "http" (? "s") "://videos.lukesmith.xyz/" (or "static/webseed" "videos/watch"))))
    (add-to-list 'bruh-videos-re re)))

(use-package url
  :custom
  (url-configuration-directory (expand-file-name "emacs/url/" (xdg-cache-home))))

(use-package url-handlers :hook (after-init-hook . url-handler-mode))

(use-package url-util
  :commands encode-url-entities decode-url-entities

  :config
  (defun decode-url-entities (beg end)
    (interactive "r")
    (let ((text (url-unhex-string (buffer-substring beg end))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text))))

  (defun encode-url-entities (beg end)
    (interactive "r")
    (let ((text (url-encode-url (buffer-substring beg end))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))


;;;; CACHE

(use-package savehist
  :custom (savehist-file (expand-file-name "emacs/savehist" (xdg-data-home)))

  :hook
  (after-init-hook    . savehist-mode)
  (savehist-save-hook . savehist-filter-file-name-history)

  :config
  (defun savehist-filter-file-name-history ()
    (setq
     file-name-history
     (cl-delete-if
      #'string-empty-p
      (cl-delete-duplicates
       (mapcar
        (lambda (s) (string-trim-right (expand-file-name s) (rx (+ "/"))))
        file-name-history)
       :test #'string-equal)))))

(use-package saveplace
  :hook (after-init-hook . save-place-mode)

  :custom
  (save-place-file (expand-file-name "emacs/saveplace" (xdg-data-home)))
  (save-place-forget-unreadable-files t))


;;;; FILES

(use-package autorevert
  :custom
  (auto-revert-remote-files t)
  (auto-revert-avoid-polling t))

(use-package files
  :commands read-directory-name custom-backup-enable-predicate

  :bind
  ("M-~"         . nil)
  ("C-S-x C-S-c" . save-buffers-kill-emacs)
  (:map ctl-x-map ("R" . revert-buffer-no-confirm))

  :custom
  (backup-by-copying t)
  (confirm-nonexistent-file-or-buffer nil)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (require-final-newline nil)
  (version-control t)
  (backup-enable-predicate #'custom-backup-enable-predicate)
  ;; (remote-file-name-inhibit-cache nil "Speed up tramp, caution!")
  (auto-save-file-name-transforms
   `((,(rx (* any)) ,(expand-file-name "emacs/auto-saves/" (xdg-cache-home)) t)))
  (backup-directory-alist
   `((,(rx (* any)) . ,(expand-file-name "emacs/backups" (xdg-data-home)))))

  :config
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

  (defun custom-backup-enable-predicate (name)
    (let ((regexp (rx (or (and string-start (or "/tmp/" "/dev/shm/"))
                          (ext "vcf")))))
      (or (not (string-match-p regexp name))
          (normal-backup-enable-predicate name)))))


;;;; SHR

(use-package shr
  :custom
  (shr-use-fonts nil)
  (shr-use-colors nil)
  (shr-max-image-proportion 0.7)
  (shr-image-animate nil)
  (shr-width (current-fill-column)))

(use-package shr-tag-pre-highlight
  :ensure t
  :after shr

  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))


;;;; CLIPBOARD

(use-package select
  :custom
  (selection-coding-system 'utf-8)
  (select-enable-clipboard t))

(use-package clipmon
  :ensure t
  :hook (after-init-hook . clipmon-mode))


;;;; OTHER

(use-package startup
  :init (provide 'startup)

  :custom
  (auto-save-list-file-prefix
   (expand-file-name (format-time-string "emacs/auto-saves/list/%y-%m-%d-")
                     (xdg-cache-home)))
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil))

(use-package window
  :init (provide 'window)

  :bind
  ("M-V"     . scroll-down-line)
  ("C-S-v"   . scroll-up-line)
  ("C-M-S-b" . previous-buffer)
  ("C-M-S-f" . next-buffer)
  ("M-Q"     . quit-window)
  (:map ctl-x-map ("C-b" . switch-to-buffer))

  :config
  (add-to-list 'display-buffer-alist `(,(rx (* any)) (display-buffer-same-window))))

(use-package time :custom (display-time-24hr-format t))

(use-package novice :custom (disabled-command-function nil))

(use-package cus-edit :custom (custom-file null-device))

(use-package uniquify :custom (uniquify-ignore-buffers-re "^\\*"))

(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment "UTF-8"))

(use-package gamegrid
  :custom
  (gamegrid-user-score-file-directory
   (expand-file-name "emacs/games/" (xdg-cache-home))))

(use-package ede/base
  :custom
  (ede-project-placeholder-cache-file
   (expand-file-name "emacs/ede/projects.el" (xdg-cache-home))))

(use-package async
  :ensure t
  :after bytecomp
  :init (async-bytecomp-package-mode))

(use-package byte-compile
  :hook (after-save-hook . byte-recompile-current-file)

  :config
  (defun byte-recompile-current-file ()
    (interactive)
    (when (derived-mode-p 'emacs-lisp-mode)
      (byte-recompile-file (buffer-file-name)))))

(use-package transient
  :custom
  (transient-history-file
   (expand-file-name "emacs/transient/history.el" (xdg-cache-home)))
  (transient-levels-file
   (expand-file-name "emacs/transient/levels.el" (xdg-cache-home)))
  (transient-values-file
   (expand-file-name "emacs/transient/values.el" (xdg-cache-home))))

(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :hook (emacs-startup-hook . gcmh-mode))


;;; REMOTE

(use-package ange-ftp :custom (ange-ftp-netrc-filename "~/.authinfo.gpg"))

(use-package tramp
  :custom
  (tramp-persistency-file-name
   (expand-file-name "emacs/tramp/connection-history" (xdg-cache-home)))
  (tramp-default-method "ssh")
  (tramp-histfile-override t)
  (tramp-completion-reread-directory-timeout nil)

  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/.local/bin"))

(use-package sudo-edit
  :ensure t
  :hook
  (after-init-hook . sudo-edit-indicator-mode)
  (shell-mode-hook . sudo-edit-set-header))


;;; MAN

(use-package man
  :custom (Man-notify-method 'aggressive)
  :bind (:map help-map ("M-m" . man)))

(use-package apropos :custom (apropos-sort-by-scores t))

(use-package finder
  :bind (:map help-map ("M-c" . finder-commentary))

  :config
  (define-advice finder-exit (:override () with-package)
    (interactive)
    (if (string-match-p (rx "*Finder" (? "-package") "*") (buffer-name))
        (quit-window t)
      (dolist (buf '("*Finder*" "*Finder-package*"))
        (when (get-buffer buf)
          (kill-buffer buf))))))


;;; DIRED

(use-package dired
  :commands dired-get-marked-files

  :hook
  (dired-mode-hook          . dired-hide-details-mode)
  (dired-before-readin-hook . dired-setup-switches)

  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alDF --si --group-directories-first")
  (dired-ls-F-marks-symlinks t)

  :bind
  (:map dired-mode-map
        ("* &" . dired-flag-garbage-files)
        ("* d" . dired-flag-files-regexp)
        ("* g" . dired-mark-files-containing-regexp))

  :config
  (defun dired-setup-switches ()
    (pcase (file-remote-p default-directory 'method)
      ((or "ftp" "sftp")
       (setq-local dired-actual-switches "-al"))
      ("adb"
       (setq-local dired-actual-switches "-alDF"))))

  (define-advice dired-copy-filename-as-kill (:override (&optional arg) newline)
    (interactive "P")
    (let ((string
           (or (dired-get-subdir)
               (mapconcat #'identity
                          (if arg
                              (cond ((zerop (prefix-numeric-value arg))
                                     (dired-get-marked-files))
                                    ((consp arg)
                                     (dired-get-marked-files t))
                                    (t
                                     (dired-get-marked-files
                                      'no-dir (prefix-numeric-value arg))))
                            (dired-get-marked-files 'no-dir))
                          "\n"))))
      (unless (string= string "")
        (if (eq last-command 'kill-region)
            (kill-append string nil)
          (kill-new string))
        (message "%s" string)))))

(use-package dired-x
  :demand t
  :after dired
  :hook (dired-mode-hook . dired-omit-mode)

  :bind
  (:map ctl-x-map ("C-j" . dired-jump))
  (:map dired-mode-map
        ("* i" . dired-mark-images)
        ("* v" . dired-mark-videos))

  :custom
  (dired-guess-shell-alist-user
   `((,(rx (ext "csv" "doc" "docx" "xlsx" "xls" "odt" "ods" "odp" "ppt" "pptx"))
      "setsid -f libreoffice * >/dev/null 2>&1"
      "libreoffice --invisible --headless --convert-to pdf * &"
      "libreoffice --invisible --headless --convert-to epub * &"
      "libreoffice --invisible --headless --convert-to csv * &")

     (,(rx (ext "jpeg" "jpg" "gif" "png" "bmp" "tif" "thm" "nef" "jfif" "webp" "xpm"))
      "setsid -f sxiv * >/dev/null 2>&1"
      "setsid -f gimp * >/dev/null 2>&1")

     (,(rx (ext "eps"))
      "setsid -f inkscape * >/dev/null 2>&1")

     (,(rx (ext "ai"))
      "setsid -f inkscape * >/dev/null 2>&1"
      "setsid -f gimp * >/dev/null 2>&1")

     (,(rx (ext "fb2"))
      "ebook-convert ? .epub &")

     (,(rx (ext "pdf"))
      "setsid -f zathura * >/dev/null 2>&1"
      "setsid -f libreoffice * >/dev/null 2>&1"
      "setsid -f gimp * >/dev/null 2>&1")

     (,(rx (ext "epub" "djvu"))
      "setsid -f zathura * >/dev/null 2>&1")

     (,(rx (ext "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp"
                "vob" "wmv" "aiff" "wav"))
      "setsid -f mpv --force-window=yes * >/dev/null 2>&1"
      "video_duration * | format_duration"
      "video_duration * | awk '{s+=$1}END{print s}' | format_duration"
      "compress_video * &"
      "strip_video * &"
      "mpv -vo=drm")

     (,(rx (ext "cue"))
      "setsid -f mpv --force-window=yes * >/dev/null 2>&1")

     (,(rx (ext "rar"))
      "temp=\"$(echo `?` | rev | cut -d. -f 2- | rev)\"; mkdir -p \"${temp}\"; unrar x ? \"${temp}\"")))

  :config
  (defun dired-get-marker-char ()
    (list
     (pcase current-prefix-arg
       ('(4) ?\s)
       ('(16)
        (let* ((dflt (char-to-string dired-marker-char))
               (input (read-string
                       (format
                        "Marker character to use (default %s): " dflt)
                       nil nil dflt)))
          (aref input 0)))
       (_ dired-marker-char))))

  (defun dired-mark-images (&optional marker-char)
    (interactive (dired-get-marker-char))
    (dired-mark-extension
     '("jpeg" "jpg" "gif" "png" "bmp" "tif" "thm" "nef" "jfif" "webp" "xpm")
     marker-char))

  (defun dired-mark-videos (&optional marker-char)
    (interactive (dired-get-marker-char))
    (dired-mark-extension
     '("flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp"
       "vob" "wmv" "aiff" "wav")
     marker-char)))

(use-package dired-aux
  :demand t
  :after dired
  :bind (:map dired-mode-map ("b" . dired-stat))
  :custom (dired-create-destination-dirs 'ask)

  :config
  (add-to-list
   'dired-compress-file-suffixes
   `(,(rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -"))

  (defun dired-stat ()
    (interactive)
    (dired-do-shell-command "stat" current-prefix-arg
                            (dired-get-marked-files t current-prefix-arg))))

(use-package wdired
  :after dired
  ;; does not work as expected
  :hook (wdired-mode-hook . disable-image-dired)
  :config (defun disable-image-dired () (image-dired-minor-mode -1)))

(use-package image-dired
  :after dired
  :hook (dired-mode-hook . image-dired-minor-mode)

  :custom
  (image-dired-external-viewer "sxiv")
  (image-dired-db-file
   (expand-file-name "emacs/image-dired/db" (xdg-cache-home)))
  (image-dired-dir
   (expand-file-name "emacs/image-dired/thumbnails/" (xdg-cache-home)))
  (image-dired-gallery-dir
   (expand-file-name "emacs/image-dired/gallery/" (xdg-cache-home)))
  (image-dired-temp-image-file
   (expand-file-name "emacs/image-dired/temp" (xdg-cache-home)))
  (image-dired-temp-rotate-image-file
   (expand-file-name "emacs/image-dired/rotate_temp" (xdg-cache-home))))

(use-package dired-rsync
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("r" . dired-rsync)))

(use-package dired-git-info
  :ensure t
  :after dired
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package dired-async
  :ensure async
  :after dired
  :diminish dired-async-mode
  :init (dired-async-mode))


;;;; FIND

(use-package find-dired
  :custom (find-ls-option '("-print0 | xargs -0 ls -ldb --quoting-style=literal" . "-ldb")))

(use-package fd-dired
  :ensure t
  :bind (:map search-map ("f d" . fd-dired))
  :custom (fd-dired-ls-option '("| xargs -0 ls -ldb --quoting-style=literal" . "-ldb")))

(use-package locate
  :custom (locate-make-command-line 'locate-make-ignore-case-command-line)
  :bind (:map search-map ("f l" . locate))

  :config
  (defun locate-make-ignore-case-command-line (search-string)
    (list locate-command "-i" search-string)))


;;; EDITING

(use-package simple
  :hook
  (before-save-hook . delete-trailing-whitespace)
  (after-init-hook  . size-indication-mode)
  (after-init-hook  . column-number-mode)

  :bind
  ("C-h"   . backward-delete-char-untabify)
  ("M-K"   . kill-whole-line)
  ("C-M-S-k" . copy-whole-line)
  ("M-SPC" . just-one-space-fast)
  ("M-\\"  . delete-indentation)
  ("M-c"   . capitalize-dwim)
  ("M-l"   . downcase-dwim)
  ("M-u"   . upcase-dwim)
  ([remap move-beginning-of-line] . back-to-indentation-or-beginning)
  ([remap newline] . newline-and-indent)
  (:map ctl-x-map
        ("K"   . kill-current-buffer)
        ("C-r" . overwrite-mode))
  (:map mode-specific-map ("o P" . list-processes))

  :custom
  (completion-show-help nil)
  (shift-select-mode nil)
  (kill-do-not-save-duplicates t)
  (kill-read-only-ok t)
  (async-shell-command-buffer 'new-buffer)
  (async-shell-command-display-buffer nil)

  :config
  (defun back-to-indentation-or-beginning ()
    (interactive)
    (when (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

  (defun just-one-space-fast (&optional n)
    (interactive "*p")
    (cycle-spacing n nil 'fast))

  (defun copy-whole-line ()
    (interactive)
    (save-excursion
      (kill-new (thing-at-point 'line)))))

(use-package register
  :commands save-window-configuration-to-w
  :custom (register-separator ?\n)

  :bind
  (:map ctl-x-r-map
        ("C-@"   . nil)
        ("C-SPC" . nil)
        ("g"     . nil)
        ("x"     . nil)
        ("v"     . view-register)
        ("L"     . list-registers)
        ("p"     . prepend-to-register)
        ("a"     . append-to-register))

  :config
  (set-register register-separator "\n")
  (defun save-window-configuration-to-w (&rest _ignore)
    (window-configuration-to-register ?w)))

(use-package subword
  :hook ((php-mode-hook rust-mode-hook java-mode-hook) . subword-mode))

(use-package multiple-cursors
  :ensure t

  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->"         . mc/mark-next-like-this)
  ("C-<"         . mc/mark-previous-like-this)
  (:map mode-specific-map ("C-<" . mc/mark-all-like-this))

  :custom
  (mc/always-run-for-all t)
  (mc/always-repeat-command t))

(use-package edit-indirect
  :ensure t
  :custom (edit-indirect-guess-mode-function #'edit-indirect-guess-mode)
  :bind (:map ctl-x-map ("E" . edit-indirect-region-or-at-point))

  :config
  (defun edit-indirect-region-or-at-point ()
    (interactive)
    (if (use-region-p)
        (call-interactively #'edit-indirect-region)
      (edit-indirect-region (point) (point) t)))

  (defun edit-indirect-guess-mode (buf _beg _end)
    (cl-case (buffer-local-value 'major-mode buf)
      ('sh-mode (awk-mode))
      ('php-mode
       (pcase (completing-read "Mode: " '("sql" "html") nil t)
         ("sql" (sql-mode))
         ("html" (html-mode))))
      (t (normal-mode)))))


;;;;; FORMATTING

(use-package whitespace
  :diminish whitespace-mode
  :hook (before-save-hook . whitespace-cleanup))

(use-package format-all :ensure t)


;;;;; INPUT METHOD

(use-package cyrillic-dvorak-im
  :quelpa
  (cyrillic-dvorak-im :repo "xFA25E/cyrillic-dvorak-im"
                      :fetcher github
                      :version original)

  :demand t)

(use-package reverse-im
  :ensure t
  :demand t
  :after cyrillic-dvorak-im
  :config (reverse-im-activate "cyrillic-dvorak"))


;;;;; PAIRS

(use-package elec-pair :hook (after-init-hook . electric-pair-mode))

(use-package smartparens
  :ensure t

  :bind
  ("C-M-u" . sp-backward-up-sexp)
  ("C-M-d" . sp-down-sexp)
  ("M-F"   . sp-forward-symbol)
  ("M-B"   . sp-backward-symbol)
  ("C-)"   . sp-forward-slurp-sexp)
  ("C-M-)" . sp-forward-barf-sexp)
  ("C-("   . sp-backward-slurp-sexp)
  ("C-M-(" . sp-backward-barf-sexp)
  ("C-M-t" . sp-transpose-sexp)
  ("C-M-k" . sp-kill-sexp)
  ;; ("C-k"   . sp-kill-hybrid-sexp)
  ("C-M-w" . sp-copy-sexp)
  ("M-d"   . sp-kill-word)
  ("C-w"   . sp-backward-kill-word-or-region)
  ("M-["   . sp-unwrap-sexp)
  ("M-]"   . sp-rewrap-sexp)

  :config
  (defun sp-backward-kill-word-or-region (&optional count)
    (interactive "p")
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (sp-backward-kill-word count)))

  (require 'smartparens-config))


;;;; CONF

(use-package ledger-mode
  :ensure t
  :custom (ledger-default-date-format "%Y-%m-%d"))

(use-package conf-mode
  :hook (after-save-hook . xresources-reload)

  :config
  (defun xresources-reload ()
    (interactive)
    (when (and (derived-mode-p 'conf-xdefaults-mode)
               (yes-or-no-p "Reload xresources?"))
      (let ((xres (expand-file-name "X11/xresources" (xdg-config-home))))
        (shell-command (format "xrdb -load %s" xres))))))

(use-package csv-mode :ensure t)

(use-package tex-mode
  :hook (tex-mode-hook . setup-tex-mode-ispell-parser)

  :config
  (defun setup-tex-mode-ispell-parser ()
    (setq-local ispell-parser 'tex)))


;;;;; WEB

(use-package css-mode :bind (:map css-mode-map ("C-c m" . css-lookup-symbol)))

(use-package json-mode :ensure t)

(use-package apache-mode :ensure t)

(use-package robots-txt-mode :ensure t)

(use-package restclient
  :ensure t
  :mode ((rx (ext "http")) . restclient-mode))


;;;;; GIT

(use-package gitconfig-mode :ensure t)

(use-package gitignore-mode :ensure t)


;;;;; XML-LIKE

(use-package sgml-mode
  :custom (sgml-basic-offset 4)

  :bind
  (:map sgml-mode-map
        ("C-M-n" . sgml-skip-tag-forward)
        ("C-M-p" . sgml-skip-tag-backward)
        ("C-c C-r" . sgml-namify-char)))

(use-package nxml-mode :custom (nxml-child-indent 4))

(use-package emmet-mode
  :ensure t
  :diminish emmet-mode

  :hook
  ((nxml-mode-hook html-mode-hook mhtml-mode-hook web-mode-hook) . emmet-mode)

  :custom
  (emmet-preview-default t)
  (emmet-self-closing-tag-style ""))


;;;; PROG

(use-package nix-mode :ensure t)

(use-package cc-mode
  :custom (c-default-style '((java-mode . "java") (other . "awk"))))

(use-package rust-mode
  :ensure t
  :custom (rust-format-on-save t))


;;;;; SHELL

(use-package sh-script :custom (system-uses-terminfo nil))

(use-package executable
  :custom (executable-chmod 64)
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))


;;;;; SQL

(use-package sql-indent
  :ensure t
  :hook (sql-mode-hook . sqlind-minor-mode))

(use-package sqlup-mode
  :ensure t
  :hook sql-mode-hook)


;;;;; WEB

(use-package php-mode
  :ensure t

  :custom
  (php-mode-coding-style 'php)
  (php-manual-path (expand-file-name "php_docs/php-chunked-xhtml" (xdg-cache-home))))

(use-package web-mode
  :ensure t
  :mode (rx (ext "twig"))
  :custom (web-mode-markup-indent-offset 4))


;;;;; LSP

(use-package lsp-mode
  :ensure t

  :hook
  (rust-mode-hook . lsp)
  (lsp-mode-hook . lsp-enable-which-key-integration)

  :custom
  (lsp-session-file (expand-file-name "emacs/lsp/session" (xdg-cache-home)))
  (lsp-xml-server-work-dir (expand-file-name "emacs/lsp/xml" (xdg-cache-home))))

(use-package lsp-ui :ensure t)


;;;;; LISP

(use-package lisp-mode :config (put 'use-package #'lisp-indent-function 1))

(use-package lisp
  :commands kill-sexp
  :hook (after-save-hook . check-parens-in-prog-mode)
  :init (provide 'lisp)

  :config
  (defun check-parens-in-prog-mode ()
    (when (derived-mode-p 'prog-mode)
      (check-parens))))


;;;;;; ELITE LISP

(use-package elisp-mode
  :bind ("C-x C-S-e" . eval-and-replace)

  :custom
  (eval-expression-print-level t)
  (eval-expression-print-length t)

  :config
  (defun eval-and-replace ()
    (interactive)
    (kill-sexp -1)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))

(use-package ipretty
  :ensure t
  :bind ([remap eval-print-last-sexp] . ipretty-last-sexp))

(use-package pp
  :bind
  (:map emacs-lisp-mode-map
        ("C-c m" . pp-macroexpand-last-sexp)
        ("C-c M" . emacs-lisp-macroexpand))
  (:map lisp-interaction-mode-map
        ("C-c m" . pp-macroexpand-last-sexp)
        ("C-c M" . emacs-lisp-macroexpand)))


;;;;;; COMMON LISP (AKA BORSHCH)

(use-package sly
  :ensure t

  :custom
  (sly-default-lisp 'sbcl)
  (sly-lisp-implementations '((sbcl ("lisp-sbcl")) (ecl ("lisp-ecl"))))
  (sly-mrepl-history-file-name
   (expand-file-name "emacs/sly-mrepl-history" (xdg-cache-home))))

(use-package sly-quicklisp :ensure t)

(use-package sly-asdf
  :ensure t
  :after sly
  :config (add-to-list 'sly-contribs 'sly-asdf 'append))


;;;;;; SCHEME

(use-package scheme :custom (scheme-program-name "guile"))

(use-package geiser
  :ensure t
  :custom
  (geiser-repl-history-filename
   (expand-file-name "geiser/history" (xdg-cache-home))))


;;;;;; CLOJURE

(use-package clojure-mode :ensure t)

(use-package cider :ensure t)


;;; CORRECTNESS

(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args (list "--sug-mode=ultra")))

(use-package flycheck
  :ensure t
  :custom
  (flycheck-mode-line-prefix "FC")
  (flycheck-clang-pedantic-errors t)
  (flycheck-clang-pedantic t)
  (flycheck-gcc-pedantic-errors t)
  (flycheck-gcc-pedantic t)
  (flycheck-phpcs-standard "PSR12,PSR1,PSR2")
  :config
  (add-to-list 'flycheck-shellcheck-supported-shells 'dash))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :init (flycheck-checkbashisms-setup)
  :custom
  (flycheck-checkbashisms-newline t)
  (flycheck-checkbashisms-posix t))


;;; COMPLETION

(use-package bash-completion
  :ensure t
  :after shell
  :hook (shell-dynamic-complete-functions . bash-completion-dynamic-complete))


;;;; MINIBUFFER

(use-package insert-char-preview
  :ensure t
  :bind ([remap insert-char] . insert-char-preview))

(use-package eldoc :diminish eldoc-mode)

(use-package minibuffer
  :commands completing-read-in-region

  :bind
  (:map minibuffer-local-completion-map
        ("<return>" . minibuffer-force-complete-and-exit)
        ("RET" . minibuffer-force-complete-and-exit)
        ("C-j" . exit-minibuffer)
        ("SPC" . nil))

  :custom
  (completion-cycle-threshold 3)
  (read-file-name-completion-ignore-case t)
  (completion-category-defaults nil)
  (completion-pcm-complete-word-inserts-delimiters t)
  (completion-styles '(substring partial-completion flex))
  (completion-in-region-function 'completing-read-in-region)

  :config
  (defun completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
Use as a value for `completion-in-region-function'."
    (if (and (minibufferp) (not (string= (minibuffer-prompt) "Eval: ")))
        (completion--in-region start end collection predicate)
      (let* ((initial (buffer-substring-no-properties start end))
             (limit (car (completion-boundaries initial collection predicate "")))
             (all (completion-all-completions initial collection predicate (length initial)))
             (completion (cond ((atom all) nil)
                               ((and (consp all) (atom (cdr all)))
                                (concat (substring initial 0 limit) (car all)))
                               (t
                                (completing-read "Completion: " collection predicate nil initial)))))
        (if (null completion)
            (progn (message "No completion") nil)
          (delete-region start end)
          (insert completion)
          t)))))

(use-package orderless
  :ensure t
  :after minibuffer
  :custom
  (orderless-matching-styles
   '(orderless-literal orderless-flex orderless-prefixes orderless-regexp))
  :init (add-to-list 'completion-styles 'orderless))

(use-package minibuf-eldef
  :custom (minibuffer-eldef-shorten-default t)
  :hook (after-init-hook . minibuffer-electric-default-mode))

(use-package map-ynp
  :init (provide 'map-ynp)
  :custom (read-answer-short t))

(use-package completing-history
  :quelpa (completing-history :repo "oantolin/completing-history" :fetcher github)
  :bind ("M-H" . completing-history-insert-item))

(use-package icomplete
  :hook (after-init-hook . icomplete-mode)
  :bind
  (:map icomplete-minibuffer-map
        ("<tab>" . icomplete-force-complete)
        ("M-j"   . icomplete-fido-exit)
        ("<return>" . icomplete-force-complete-and-exit)
        ("RET" . icomplete-force-complete-and-exit)
        ("C-j" . exit-minibuffer)
        ("C-h" . icomplete-fido-backward-updir)
        ("C-n" . icomplete-forward-completions)
        ("C-p" . icomplete-backward-completions))
  :custom
  (icomplete-in-buffer t)
  (icomplete-tidy-shadowed-file-names t)
  (icomplete-separator (propertize " ⋅ " 'face 'shadow))
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil))

(use-package icomplete-vertical
  :ensure t
  :after icomplete
  :init (icomplete-vertical-mode))

(use-package consult
  :quelpa (consult :repo "minad/consult" :fetcher github :version original)
  :bind
  ("M-y" . consult-yank-replace)
  (:map goto-map ("o" . consult-outline)))


;;;; HIPPIE-EXP

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom (he-file-name-chars "-a-zA-Z0-9_/.,~^#$+={}"))

(use-package try-complete-file-name-with-env
  :quelpa
  (try-complete-file-name-with-env
   :repo "xFA25E/try-complete-file-name-with-env"
   :fetcher github
   :version original)

  :demand t
  :after hippie-exp)


;;; SEARCHING

(use-package isearch
  :bind (:map isearch-mode-map ("C-h" . isearch-delete-char))
  :config (define-key isearch-mode-map (kbd "C-?") isearch-help-map)

  :custom
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (search-whitespace-regexp ".*?"))

(use-package grep
  :config
  (add-to-list 'grep-files-aliases '("php" . "*.php *.phtml"))

  (define-advice grep-expand-template (:filter-return (cmd) cut)
    (concat cmd " | cut -c-500")))

(use-package wgrep
  :ensure t
  :custom (wgrep-auto-save-buffer t))

(use-package rg
  :ensure t
  :custom (rg-executable "rg")

  :bind
  (:map search-map
        :prefix-map rg-custom-map
        :prefix "r"
        ("r" . rg)
        ("." . rg-dwim)
        ("l" . rg-list-searches)
        ("t" . rg-literal)
        ("p" . rg-project)
        ("k" . rg-kill-saved-searches)
        ("s" . rg-save-search-as-name))
  (:map rg-mode-map
        ("C-n" . next-line)
        ("C-p" . previous-line)
        ("{" . rg-prev-file)
        ("M-{" . rg-prev-file)
        ("}" . rg-next-file)
        ("M-}" . rg-next-file)))


;;; JUMPING


;;;; ON BUFFER

(use-package avy
  :ensure t

  :bind
  ("M-z" . avy-goto-word-0)
  (:map goto-map
        ("M-g" . avy-goto-line)
        ("g"   . nil)
        ("n"   . nil)
        ("p"   . nil))

  :custom
  (avy-background t)
  (avy-goto-word-0-regexp (rx symbol-start (or (syntax word) (syntax symbol))))
  (avy-style 'words)
  (avy-keys (string-to-list "aoeuhtns")))

(use-package ace-link
  :ensure t
  :hook (after-init-hook . ace-link-setup-default)
  :bind (:map goto-map ("l" . ace-link)))


;;;; TO DEFINITION

(use-package dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package imenu
  :bind (:map goto-map ("i" . imenu))
  :custom
  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil)
  (imenu-space-replacement " ")
  (imenu-level-separator "/"))

(use-package flimenu
  :ensure t
  :after imenu
  :init (flimenu-global-mode))

(use-package imenu-anywhere
  :ensure t
  :bind (:map goto-map ("I" . imenu-anywhere)))

(use-package find-func
  :bind (:map search-map ("f b" . find-library))

  :custom
  (find-function-C-source-directory
   (expand-file-name "programs/emacs-27.1/src" (xdg-download-dir))))


;;; COMPILATION

(use-package compile
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)

  :bind (:map ctl-x-map ("c" . compile)))

;; Add support for cargo error --> file:line:col
(use-package cargo
  :ensure t
  :hook (rust-mode-hook . cargo-minor-mode)

  :custom
  (cargo-process--command-build "build --color never")
  (cargo-process--command-check "check --color never")
  (cargo-process--command-clippy "clippy --color never")
  (cargo-process--command-current-file-tests "test --color never")
  (cargo-process--command-current-test "test --color never")
  (cargo-process--command-rm "rm --color never")
  (cargo-process--command-run "run --color never")
  (cargo-process--command-test "test --color never"))


;;; REPL

(use-package comint
  :hook
  (kill-buffer-hook               . comint-write-input-ring)
  (kill-emacs-hook                . save-buffers-comint-input-ring)
  (comint-output-filter-functions . comint-strip-ctrl-m)
  (comint-output-filter-functions . comint-truncate-buffer)

  :custom
  (comint-input-ignoredups t)
  (comint-input-ring-size 10000)
  (comint-buffer-maximum-size 10240)

  :config
  (defun save-buffers-comint-input-ring ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf (comint-write-input-ring))))

  (defvar-local comint-history-filter-function nil)
  (define-advice comint-write-input-ring (:before (&rest _) filter-history)
    (let ((fn comint-history-filter-function))
      (when (and fn comint-input-ring (not (ring-empty-p comint-input-ring)))
        (thread-last comint-input-ring
          ring-elements
          (funcall fn)
          ring-convert-sequence-to-ring
          (setq-local comint-input-ring))))))

(use-package sql
  :hook (sql-interactive-mode-hook . sql-interactive-set-history)

  :custom
  (sql-mysql-options '("-A"))
  (sql-sqlite-options `("-column" "-header" "-cmd" "PRAGMA foreign_keys = ON;"))

  :config
  (defun sql-interactive-set-history ()
    (let ((file (expand-file-name
                 (format "emacs/sqli/%s_history" sql-interactive-product)
                 (xdg-cache-home))))
      (make-directory (file-name-directory file) t)
      (write-region "" nil file t)
      (setq sql-input-ring-file-name file))))


;;;; SHELL

(use-package shell
  :bind (:map shell-mode-map ("C-c M-d" . shell-change-directory))

  :custom
  (shell-prompt-pattern
   (rx line-start
       (one-or-more digit) " "
       alpha
       (zero-or-more (in ?- ?_ alpha digit)) " "))

  :hook (shell-mode-hook . shell-enable-comint-history)

  :config
  (defun shell-history-filter (elements)
    (cl-flet ((match-p
               (e)
               (string-match-p
                (rx bos
                    (or (and
                         (opt "sudo " (opt "-A "))
                         (or
                          "awk" "base16_theme" "bash" "bspc" "cat" "cd" "chmod" "chown"
                          "ckbatt" "command" "cp" "cut" "dash" "dd" "df" "dh" "du"
                          "ebook-convert" "echo" "em" "emacs" "env" "exit" "export" "fd" "feh"
                          "file" "find" "gawk" "gparted" "gpg" "grep" "gzip" "hash" "host"
                          "htop" "id" "ln" "locate" "ls" "man" "mbsync" "millisleep" "mkdir"
                          "mmpv" "mpop" "mpv" "mv" "notify-send" "pacman -Rsn" "pacman -S"
                          "ping" "pkill" "printf" "pwgen" "python" "quit" "read" "rg" "rimer"
                          "rm" "rmdir" "rofi" "runel" "setsid" "sh" "sleep" "stow" "strings"
                          "strip" "studies_" "sxiv" "tail" "time" "timer" "top" "touch" "tr"
                          "uname" "uptime" "watch" "wc" "which" "woof" "xbindkeys" "xclip" "xz"
                          "yay" "youtube-dl" "ytdl"))
                        eos))
                e)))
      (cl-delete-duplicates (cl-delete-if #'match-p elements) :test #'string-equal)))

  (defun shell-enable-comint-history ()
    (setq-local comint-input-ring-file-name
                (expand-file-name "emacs/comint/shell_history" (xdg-data-home)))
    (setq-local comint-history-filter-function #'shell-history-filter)
    (comint-read-input-ring 'silent))

  (defun shell-change-directory ()
    "Change directory in a shell, interactively."
    (interactive)
    (comint-show-maximum-output)
    (comint-delete-input)
    (let* ((read-dir (read-directory-name "Change directory: "))
           (dir (or (file-remote-p read-dir 'localname) read-dir)))
      (insert (concat "cd " (shell-quote-argument dir))))
    (comint-send-input)))

(use-package shell-pwd
  :quelpa (shell-pwd :repo "xFA25E/shell-pwd" :fetcher github :version original)
  :bind (:map mode-specific-map ("x s" . shell-pwd-switch-to-buffer))

  :config
  (cl-defun shell-pwd-switch-to-buffer (&optional (directory default-directory))
    (interactive
     (list (if current-prefix-arg
               (expand-file-name (read-directory-name "Default directory: "))
             default-directory)))
    (cl-flet ((shell-buffer-p (b) (eq (buffer-local-value 'major-mode b) 'shell-mode))
              (pwd-buffer () (shell-pwd-generate-buffer-name directory)))
      (let* ((buffer-name (generate-new-buffer-name (pwd-buffer)))
             (shell-buffers (mapcar #'buffer-name (cl-delete-if-not #'shell-buffer-p (buffer-list))))
             (name (completing-read "Shell buffer: " (cons buffer-name shell-buffers))))
        (if-let ((buffer (get-buffer name))) (pop-to-buffer buffer) (shell name))))
    (shell-pwd-enable)))


;;; TEMPLATES

(use-package autoinsert :hook (after-init-hook . auto-insert-mode))

(use-package skempo-mode
  :quelpa (skempo-mode :repo "xFA25E/skempo-mode" :fetcher github :version original)
  :hook ((emacs-lisp-mode-hook lisp-interaction-mode-hook) . skempo-mode)

  :bind
  (:map skempo-mode-map
        ("C-z" . skempo-mode-complete-tag-or-call-on-region)
        ("M-g M-e" . tempo-forward-mark)
        ("M-g M-a" . tempo-backward-mark))

  :config
  (defun skempo-mode-elisp-namespace ()
    (string-trim-right (buffer-name) (rx ".el" eos)))

  (defun skempo-mode-elisp-group ()
    (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))

  (skempo-mode-define-templates emacs-lisp-mode
    ("defvar" :tempo
     "(defvar " (skempo-mode-elisp-namespace) "-" p n>
     r> n>
     "\"" p "\")")

    ("defun" :tempo
     "(defun " (skempo-mode-elisp-namespace) "-" p " (" p ")" n>
     "\"" p "\"" n>
     r> ")")

    ("lambda" :tempo "(lambda (" p ") " n> r> ")")

    ("let" :tempo "(let ((" p "))" n> r> ")")

    ("defgroup" :tempo
     "(defgroup " (skempo-mode-elisp-group) " nil" n>
     "\"" p "\"" n>
     ":group " p "nil)")

    ("defcustom" :tempo
     "(defcustom " (skempo-mode-elisp-namespace) "-" p n>
     r> n>
     "\"" p "\"" n>
     ":type nil" n>
     ":group '" (skempo-mode-elisp-group) ")" n>)

    ("defface" :tempo
     "(defface " (skempo-mode-elisp-namespace) "-" p n>
     "'((t :inherit " p "nil))" n>
     "\"" p "\"" n>
     ":group '" (skempo-mode-elisp-group) ")")))


;;; APPLICATIONS

(use-package forms
  :hook (kill-buffer-hook . forms-kill-file-buffer)

  :config
  (defun forms-kill-file-buffer ()
    (when (and (derived-mode-p 'forms-mode) (buffer-live-p forms--file-buffer))
      (kill-buffer forms--file-buffer))))

(use-package vlf :ensure t)

(use-package sdcv
  :ensure t
  :bind (:map mode-specific-map ("o t" . sdcv-search-input))

  :config
  (define-advice sdcv-goto-sdcv (:after () fullscreen)
    (delete-other-windows)))

(use-package dictionary
  :ensure t
  :bind (:map mode-specific-map ("o T" . dictionary-search)))

(use-package ediff :hook (ediff-before-setup-hook . save-window-configuration-to-w))

(use-package net-utils
  :bind (:map mode-specific-map
              :prefix-map net-utils-prefix-map
              :prefix "n"
              ("a" . arp)
              ("d" . dig)
              ("h" . nslookup-host)
              ("i" . ifconfig)
              ("n" . netstat)
              ("p" . ping)
              ("p" . ping)
              ("r" . route)
              ("s" . smbclient)
              ("t" . traceroute)
              ("w" . iwconfig)))

(use-package readelf-mode
  :quelpa (readelf-mode :repo "sirikid/readelf-mode" :fetcher github :version original))

(use-package calendar :custom (calendar-week-start-day 1))

(use-package ibuffer
  :custom (ibuffer-default-sorting-mode 'major-mode)
  :bind (:map ctl-x-map ("C-S-b" . ibuffer-jump)))

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package proced
  :bind (:map mode-specific-map ("o p" . proced))
  :custom (proced-tree-flag t))

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file (expand-file-name "emacs/bookmarks" (xdg-data-home))))


;;;; XML

(use-package eww
  :custom
  (eww-browse-url-new-window-is-tab nil)
  (eww-search-prefix "https://ddg.co/lite/?q="))

(use-package xml
  :commands decode-sgml-entities encode-sgml-entities

  :config
  (defun decode-sgml-entities (beg end)
    (interactive "r")
    (save-excursion
      (narrow-to-region beg end)
      (goto-char beg)
      (xml-parse-string)
      (widen)))

  (defun encode-sgml-entities (beg end)
    (interactive "r")
    (let ((text (xml-escape-string (buffer-substring beg end))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))

(use-package htmlize :ensure t)


;;;; KEYS

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init-hook . which-key-mode))

(use-package free-keys :ensure t)


;;;; YO-HO

(use-package transmission
  :ensure t

  :bind
  (:map mode-specific-map ("o r" . transmission))
  (:map transmission-mode-map ("M" . transmission-move))

  :config
  (define-advice transmission (:after () fullscreen)
    (delete-other-windows)))

(use-package torrent-mode
  :quelpa (torrent-mode :repo "xFA25E/torrent-mode" :fetcher github :version original)
  :hook (after-init-hook . torrent-mode-setup))

(use-package mediainfo-mode
  :quelpa (mediainfo-mode :repo "xFA25E/mediainfo-mode" :fetcher github :version original)
  :hook (after-init-hook . mediainfo-mode-setup))


;;;; PROJECTILE

(use-package projectile
  :ensure t
  :bind-keymap ("M-m" . projectile-command-map)
  :hook (after-init-hook . projectile-mode)

  :custom
  (projectile-enable-caching t)
  (projectile-mode-line-prefix "")
  (projectile-cache-file (expand-file-name "emacs/projectile/cache" (xdg-cache-home)))
  (projectile-known-projects-file
   (expand-file-name "emacs/projectile/projects" (xdg-cache-home)))

  :config
  (define-advice projectile-default-mode-line (:filter-return (project-name) remove-empty)
    (when (not (string-equal project-name "[-]"))
      (concat " " project-name)))

  (define-advice delete-file-projectile-remove-from-cache
      (:override (filename &optional _trash) ftp-fix)
    (unless (string-equal (file-remote-p default-directory 'method) "ftp")
      (if (and projectile-enable-caching projectile-auto-update-cache (projectile-project-p))
          (let* ((project-root (projectile-project-root))
                 (true-filename (file-truename filename))
                 (relative-filename (file-relative-name true-filename project-root)))
            (if (projectile-file-cached-p relative-filename project-root)
                (projectile-purge-file-from-cache relative-filename)))))))


;;;; YTEL

(use-package ytel
  :ensure t
  :hook (ytel-mode-hook . toggle-truncate-lines)

  :custom
  (ytel-instances
   '("https://invidious.fdn.fr"
     "https://invidious.site"
     "https://invidious.kavin.rocks"
     "https://vid.encryptionin.space"
     "https://invidious.snopyta.org"
     "https://invidious.mservice.ru.com"
     "https://invidious.xyz"
     "https://vid.encryptionin.space"))
  (ytel-invidious-api-url (car ytel-instances))

  :bind
  (:map mode-specific-map ("o Y" . ytel))
  (:map ytel-mode-map
        ("c" . ytel-copy-link)
        ("v" . ytel-current-browse-url))

  :config
  (defun ytel-switch-instance ()
    (interactive)
    (setq ytel-invidious-api-url
          (completing-read "Instance: " ytel-instances)))

  (defun ytel-current-browse-url ()
    (interactive)
    (let* ((id (ytel-video-id (ytel-get-current-video)))
           (link (concat "https://www.youtube.com/watch?v=" id)))
      (browse-url link)))

  (defun ytel-copy-link ()
    (interactive)
    (let* ((video (ytel-get-current-video))
           (id (ytel-video-id video))
           (link (concat "https://www.youtube.com/watch?v=" id)))
      (kill-new link)
      (message "Copied %s" link)))

  (with-eval-after-load 'bruh
    (setf (alist-get 'ytel-mode bruh-mpvi-get-title-functions)
        (lambda () (ytel-video-title (ytel-get-current-video))))))

(use-package ytel-show
  ;; :load-path "~/Documents/projects/emacs-lisp/ytel-show/"
  :quelpa (ytel-show :repo "xFA25E/ytel-show" :fetcher github :version original)
  :after ytel
  :bind (:map ytel-mode-map ("RET" . ytel-show))

  :config
  (with-eval-after-load 'bruh
    (setf (alist-get 'ytel-show-mode bruh-mpvi-get-title-functions)
          #'ytel-show--current-video-id)
    (setf (alist-get 'ytel-show-comments-mode bruh-mpvi-get-title-functions)
          (lambda () ytel-show-comments--video-title))))


;;;; VERSION CONTROL

(use-package vc-hooks :custom (vc-handled-backends '(Git)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit)

  :custom
  (magit-credential-cache-daemon-socket
   (expand-file-name "git/credential/socket" (xdg-cache-home))))


;;;; RSS

(use-package newst-backend
  :hook (newsticker-new-item-functions . newsticker-add-thumbnail)

  :custom
  (newsticker-retrieval-interval 0)
  (newsticker-retrieval-method 'extern)
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old nil)
  (newsticker-dir (expand-file-name "emacs/newsticker" (xdg-cache-home)))
  (newsticker-url-list-defaults nil)
  (newsticker-url-list
   '(("Alt-Hype" "https://www.bitchute.com/feeds/rss/channel/thealthype/")
     ("American Renaissance" "https://www.bitchute.com/feeds/rss/channel/amrenaissance/")
     ("Atlanta Functional Programming" "https://www.youtube.com/feeds/videos.xml?channel_id=UCYg6qFXDE5SGT_YXhuJPU0A")
     ("Justus Walker" "https://www.youtube.com/feeds/videos.xml?user=senttosiberia")
     ("Luke Smith Blog" "https://lukesmith.xyz/rss.xml")
     ("Luke Smith PeerTube" "https://videos.lukesmith.xyz/feeds/videos.xml?accountId=3")
     ("Luke Smith YouTube" "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA")
     ("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
     ("Protesilaos Stavrou" "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g")
     ("TealDeer" "https://www.bitchute.com/feeds/rss/channel/tealdeer/")
     ("Tsoding" "https://www.youtube.com/feeds/videos.xml?channel_id=UCEbYhDd6c6vngsF5PQpFVWg")
     ("Мысли и методы" "http://feeds.soundcloud.com/users/soundcloud:users:259154388/sounds.rss")
     ("Простая Академия" "https://www.youtube.com/feeds/videos.xml?channel_id=UC8mmPf2oKdfE2pdjqctTWUw")
     ("Простые Мысли" "https://www.youtube.com/feeds/videos.xml?channel_id=UCZuRMfF5ZUHqYlKkvU12xvg")))

  :config
  (defun newsticker-add-thumbnail (_feedname item)
    (cl-flet ((d (thumb desc) (format "<img src=\"%s\"/><br/><pre>%s</pre>" thumb desc)))
      (pcase (newsticker--link item)
        ((rx "youtube.com")
         (let ((group (alist-get 'group (newsticker--extra item))))
           (setcar
            (nthcdr 1 item)
            (d (alist-get 'url (car (alist-get 'thumbnail group))) (cadr (alist-get 'description group))))))
        ((rx "bitchute.com")
         (let ((enclosure (alist-get 'enclosure (newsticker--extra item))))
           (setcar
            (nthcdr 1 item)
            (d (alist-get 'url (car enclosure)) (newsticker--desc item)))))
        ((rx "videos.lukesmith.xyz")
         (let ((thumbnail (alist-get 'thumbnail (newsticker--extra item))))
           (setcar
            (nthcdr 1 item)
            (d  (alist-get 'url (car thumbnail)) (newsticker--desc item)))))))))

(use-package newst-treeview
  :commands newsticker--treeview-get-selected-item
  :hook (newsticker-treeview-item-mode-hook . toggle-truncate-lines)

  :bind
  (:map mode-specific-map ("o n" . newsticker-show-news))
  (:map newsticker-treeview-mode-map
        ("r" . newsticker-treeview-show-duration)
        ("c" . newsticker-treeview-copy-link))

  :custom
  (newsticker-treeview-automatically-mark-displayed-items-as-old nil)
  (newsticker-treeview-treewindow-width 30)
  (newsticker-treeview-listwindow-height 6)
  (newsticker--treeview-list-sort-order 'sort-by-time-reverse)

  :config
  (defun newsticker-treeview-copy-link ()
    (interactive)
    (let ((link (newsticker--link (newsticker--treeview-get-selected-item))))
      (kill-new link)
      (message "Copied %s" link)))

  (defun newsticker-treeview-show-duration ()
    (interactive)
    (let* ((item (newsticker--treeview-get-selected-item))
           (title (newsticker--title item))
           (link (newsticker--link item))
           (duration-buffer "*youtube-duration*"))
      (message "\"%s\" duration: ..." title)
      (set-process-sentinel
       (start-process "youtube-duration" duration-buffer
                      "ytdl" "--no-color" "--get-duration" link)
       `(lambda (process _change)
          (when (eq 0 (process-exit-status process))
            (with-current-buffer ,duration-buffer
              (message "\"%s\" duration: %s"
                       ,title (string-trim (buffer-string)))
              (kill-buffer)))))))

  (with-eval-after-load 'bruh
    (setf (alist-get 'newsticker-treeview-mode bruh-mpvi-get-title-functions)
          (lambda () (newsticker--title (newsticker--treeview-get-selected-item))))))


;;;; MPD

(use-package mingus
  :ensure t

  :bind
  (:map mode-specific-map
        ("o s" . mingus)
        ("o S" . mingus-find-and-add-file))

  :custom
  (mingus-mode-line-separator "|")
  (mingus-mode-line-string-max 120)
  (mingus-mpd-config-file (expand-file-name "mpd/mpd.conf" (xdg-config-home)))
  (mingus-seek-amount 5)
  (mingus-use-mouse-p nil)

  :config
  (define-advice mingus-git-out (:override (&optional _x) kill)
    (interactive)
    (when (mingus-buffer-p) (kill-current-buffer)))

  (defun mingus-music-files ()
    (let* ((default-directory (xdg-music-dir))
           (exts (cdr (mapcan (lambda (e) `("-o" "-iname" ,(concat "*." e)))
                              '("flac" "m4a" "mp3" "ogg" "opus"))))
           (args `("." "(" ,@exts ")" "-type" "f" "-o" "-type" "d")))
      (with-temp-buffer
        (apply #'call-process "find" nil t nil args)
        (let (files)
          (goto-char (point-max))
          (beginning-of-line 0)
          (while (< (point-min) (point))
            (when (looking-at "\\./")
              (goto-char (match-end 0)))
            (push (buffer-substring (point) (line-end-position)) files)
            (beginning-of-line 0))
          files))))

  (defun mingus-find-and-add-file ()
    (interactive)
    (mingus-add-files
     ((lambda (f) (list (expand-file-name f (xdg-music-dir))))
      (completing-read "Add file to mpd: " (mingus-music-files) nil t)))
    (mpd-play mpd-inter-conn)
    (let ((buffer (get-buffer "*Mingus*")))
      (when (buffer-live-p (get-buffer buffer))
        (kill-buffer buffer)))))


;;;; E-READER

(use-package pdf-tools
  :ensure t
  :init (pdf-loader-install))

(use-package nov
  :ensure t
  :mode ((rx (ext "epub")) . nov-mode)

  :custom
  (nov-save-place-file (expand-file-name "emacs/nov-places" (xdg-cache-home))))

(use-package fb2-mode
  :quelpa (fb2-mode :repo "5k1m1/fb2-mode" :fetcher github :version original)
  :custom (fb2-replace-hard-space t))


;;; MAIL

(use-package message
  :commands message-send-mail-with-sendmail

  :custom
  (message-kill-buffer-on-exit t)
  (message-send-mail-function  #'message-send-mail-with-sendmail)
  (message-subject-re-regexp (rx bol (* blank)
                                 (* (or "R" "RE" "Re" "Ris")
                                    (* "[" (* digit) "]")
                                    (? " ") ":"
                                    (* blank)))))

(use-package sendmail
  :custom
  (sendmail-program "msmtp")
  (send-mail-function #'message-send-mail-with-sendmail))


;;;; MU4E

(use-package mu4e
  :hook (after-init-hook . (lambda () (mu4e t)))

  :bind
  (:map mode-specific-map ("o m" . mu4e))
  (:map mu4e-main-mode-map
        ("q" . quit-window)
        ("Q" . mu4e-quit))

  :custom
  (mu4e-headers-visible-lines 7)
  (mu4e-sent-folder "/SENT")
  (mu4e-drafts-folder "/DRAFTS")
  (mu4e-trash-folder "/TRASH")
  (mu4e-refile-folder "/ARCHIVE")
  (mu4e-view-show-images t)
  (mu4e-sent-messages-behavior 'sent)
  (mu4e-completing-read-function #'completing-read)
  (mu4e-change-filenames-when-moving t)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'always-ask)
  (mu4e-headers-date-format "%d %b %a %R")
  (mu4e-view-date-format "%a %d %b %Y %T")
  (mu4e-headers-time-format "%16R")
  (mu4e-view-show-addresses t)
  (mu4e-attachment-dir (expand-file-name (xdg-download-dir)))
  (mu4e-modeline-max-width 100)
  (mu4e-get-mail-command "mailsync -a")
  (mu4e-update-interval 300)
  (mu4e-maildir-shortcuts
   '(("/EXYS"    . ?e)
     ("/POLIMI"  . ?p)
     ("/SENT"    . ?s)
     ("/TRASH"   . ?t)
     ("/DRAFTS"  . ?d)
     ("/ARCHIVE" . ?a)))
  (mu4e-headers-fields
   '((:human-date . 16)
     (:flags      . 6)
     (:from       . 22)
     (:subject)))
  (mu4e-view-attachment-assoc
   (eval-when-compile
     (mapcan
      (lambda (args) (mapcar (lambda (ext) (cons ext (car args))) (cdr args)))
      '(("sxiv"        . ("jpeg" "jpg" "gif" "png" "bmp" "tif" "thm" "nef" "jfif" "webp"))
        ("zathura"     . ("pdf" "epub" "djvu"))
        ("libreoffice" . ("csv" "doc" "docx" "xlsx" "xls" "odt" "ods" "odp" "ppt" "pptx"))
        ("mpv"         . ("m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov"
                          "3gp" "vob"  "wmv" "aiff" "wav"))))))

  :config

  (load-file (expand-file-name "emacs/secrets/mu4e.el" (xdg-data-home)))
  (load-library "org-mu4e")

  (add-to-list 'mu4e-view-actions '("browser view" . mu4e-action-view-in-browser) t)

  (define-advice mu4e-action-view-in-browser
      (:around (oldfunc &rest args) check-parens-fix)
    (let ((prog-mode-hook nil)
          (browse-url-browser-function #'browse-url-firefox))
      (apply oldfunc args)))

  (define-advice mu4e-update-mail-and-index (:before (&rest _ignore) kill-update)
    (mu4e-kill-update-mail)))

(use-package mu4e-alert
  :ensure t

  :hook
  (after-init-hook . mu4e-alert-enable-notifications)
  (after-init-hook . mu4e-alert-enable-mode-line-display)

  :config
  (mu4e-alert-set-default-style 'libnotify))


;;; ORGANIZE

(use-package remember
  :commands remember-notes-maybe

  :bind (:map remember-notes-mode-map
              ("C-c C-c" . nil)
              ("C-c '"   . remember-notes-save-and-bury-buffer)
              ("C-c \""  . remember-notes-save-and-kill-terminal))

  :custom
  (remember-data-file (expand-file-name "emacs/notes" (xdg-data-home)))
  (initial-buffer-choice #'remember-notes-maybe)
  (remember-notes-initial-major-mode 'outline-mode)

  :config
  (defun remember-notes-maybe ()
    (let ((buffer (remember-notes)))
      (if (zerop (buffer-size buffer))
          (get-buffer-create "*scratch*")
        buffer)))

  (defun remember-notes-save-and-kill-terminal ()
    (interactive)
    (remember-notes-save-and-bury-buffer)
    (save-buffers-kill-terminal)))


;;;; ORG

(use-package org
  :ensure org-plus-contrib
  :bind (:map mode-specific-map ("G a a" . org-agenda))

  :custom
  (org-src-tab-acts-natively t)
  (org-startup-folded t)
  (org-agenda-files '("~/org/life.org"))
  (org-log-into-drawer t)
  (org-log-reschedule 'note)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-agenda-skip-additional-timestamps-same-entry nil)
  (org-refile-targets '((org-agenda-files :level . 1)))
  (org-id-locations-file
   (expand-file-name "emacs/org/id-locations" (xdg-cache-home)))

  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((calc       . t)
                                 (emacs-lisp . t)
                                 (sql        . t)
                                 (shell      . t))))

(use-package org-mime
  :ensure t
  :after message
  :bind
  (:map message-mode-map
        ("C-c M-o" . org-mime-htmlize)
        ("C-c M-e" . org-mime-edit-mail-in-org-mode)
        ("C-c M-t" . org-mime-revert-to-plain-text-mail))
  :config
  (define-advice org-mime-beautify-quoted (:filter-return (html) newlines)
    (let ((blockquote-count
           (save-match-data
             (with-temp-buffer
               (insert html)
               (goto-char (point-min))
               (how-many "blockquote" (point-min) (point-max))))))
      (if (/= 2 blockquote-count) html
        (replace-regexp-in-string
         "\n" "<br/>\n"
         (replace-regexp-in-string
          (rx (>= 3 "\n")) "\n\n"
          html)))))

  (define-advice org-mime-replace-images (:filter-args (args) fix-imgs)
    (cl-destructuring-bind (first . rest) args
      (cons (replace-regexp-in-string "src=\"file:///" "src=\"/" first) rest))))

(use-package ox-html
  :after org

  :custom
  (org-html-htmlize-output-type 'css)
  (org-html-htmlize-font-prefix "org-"))
