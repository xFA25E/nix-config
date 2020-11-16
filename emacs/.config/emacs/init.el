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
  :bind ("C-S-SPC" . insert-space-after-point)

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
  (resize-mini-windows t)
  (tab-width 4)
  (undo-limit 200000)
  (undo-outer-limit 20000000)
  (undo-strong-limit 300000)
  (use-dialog-box nil)
  (visible-bell nil)
  (x-gtk-use-system-tooltips nil)
  (x-stretch-cursor t)
  (fill-column 80)
  ;; (help-char (aref (kbd "C-l") 0))
  (kill-buffer-query-functions
   (remq #'process-kill-buffer-query-function kill-buffer-query-functions))
  (user-full-name "Valeriy Litkovskyy")
  (read-process-output-max (* 1024 1024))

  :config
  (setq-default line-spacing 0.2)

  (defun insert-space-after-point ()
    (interactive)
    (save-excursion (insert " "))))


;;;; FACES

(use-package faces
  :demand t
  :config
  (set-face-attribute 'default nil :family "Iosevka" :height 165)
  (set-face-attribute 'mode-line nil :family "DejaVu Sans" :height 125)
  (set-face-attribute 'mode-line-inactive nil :family "DejaVu Sans" :height 125)
  (set-face-attribute 'fixed-pitch-serif nil :family "DejaVu Serif")
  (set-face-attribute 'header-line nil :inverse-video nil :family "Iosevka"))

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
    ivy-occur-mode-hook
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
  :hook (emacs-lisp-mode-hook . form-feed-mode))


;;;;; THEMES

(use-package custom :demand t)

(use-package parchment-theme
  :ensure t
  :after custom
  :init (load-theme 'parchment t))


;;;;;; LEUVEN

(use-package faces
  :disabled
  :after compile
  :config
  (set-face-attribute 'compilation-info nil :foreground "deep sky blue")
  (set-face-attribute 'compilation-mode-line-exit nil :foreground "lawn green"))

(use-package faces
  :disabled
  :after mu4e
  :config
  (set-face-attribute 'mu4e-context-face nil :foreground "orange")
  (set-face-attribute 'mu4e-modeline-face nil :foreground "green"))

(use-package faces
  :disabled
  :after org
  (set-face-attribute 'org-list-dt nil :foreground "sky blue"))


;;;;; OUTLINE

(use-package outline
  :diminish outline-minor-mode
  :hook
  (emacs-lisp-mode-hook . outline-minor-mode))

(use-package outline
  :after imenu
  :config
  (defun outline-show-after-imenu-jump ()
    (when outline-minor-mode
      (outline-show-entry)))
  (add-hook 'imenu-after-jump-hook #'outline-show-after-imenu-jump))

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
  :hook (emacs-lisp-mode-hook . hs-minor-mode))

(use-package bicycle
  :ensure t
  :after hideshow

  :bind
  (:map hs-minor-mode-map
        ("<C-tab>" . bicycle-cycle)
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

(use-package frame :config (define-advice suspend-frame (:override ()) nil))


;;;; AUTH

(use-package auth-source
  :custom (auth-sources '("~/.authinfo.gpg" "~/.netrc" "~/.authinfo")))

(use-package auth-source-pass
  :custom
  (auth-source-pass-filename
   (or (getenv "PASSWORD_STORE_DIR") (expand-file-name "pass" (xdg-data-home)))))


;;;; URL

(use-package browse-url :custom (browse-url-secondary-browser-function #'browse-url-firefox))

(use-package bruh
  :after browse-url
  :quelpa (bruh :repo "a13/bruh" :fetcher github)

  :custom
  (bruh-default-browser #'eww-browse-url)
  (bruh-videos-browser-function #'bruh-mpvi-ytdli-or-browse)
  (browse-url-browser-function #'bruh-browse-url)

  :config
  (defvar-local bruh-mpvi-get-title-function (lambda () (read-from-minibuffer "Title: ")))
  (defun bruh-mpvi-ytdli-or-browse (url &rest rest)
    (cond
     ((yes-or-no-p (format "Mpv %s ?" url))
      (let ((process-environment (browse-url-process-environment)))
        (start-process (concat "mpv " url) nil "mpvi" url)))
     ((yes-or-no-p (format "Download %s ?" url))
      (let ((process-environment (browse-url-process-environment)))
        (start-process (concat "ytdl " url) nil "ytdli" url
                       (funcall bruh-mpvi-get-title-function))))
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
    (cl-labels ((trim-slashes (s) (string-trim-right s "/+"))
                (http-p (s) (string-match-p (rx bos (? "/") "http") s))
                (remote-or-exists-p (e) (or (file-remote-p e) (file-exists-p e))))
      (thread-last (cl-delete-duplicates
                    file-name-history
                    :test #'string-equal
                    :key #'trim-slashes)
        (cl-delete-if #'http-p)
        (cl-delete-if-not #'remote-or-exists-p)
        (mapcar #'substring-no-properties)
        (setq file-name-history)))))

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
  (shr-max-image-proportion 0.7)
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
  :bind (:map help-map ("M-m" . man))

  :config
  (set-face-attribute 'Man-overstrike nil :inherit 'font-lock-variable-name-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit 'font-lock-negation-char-face :underline t))

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
      "mediainfo"
      "mpv -vo=drm"
      "compress_video"
      "strip_video")

     (,(rx (ext "cue"))
      "setsid -f mpv --force-window=yes * >/dev/null 2>&1")

     (,(rx (ext "torrent"))
      "transmission-show"
      "transmission-remote --add")

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
  (defun dired-stat ()
    (interactive)
    (dired-do-shell-command "stat" current-prefix-arg
                            (dired-get-marked-files t current-prefix-arg))))

(use-package wdired
  ;; does not work as expected
  :hook (wdired-mode-hook . disable-image-dired)
  :config (defun disable-image-dired () (image-dired-minor-mode -1)))

(use-package image-dired
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
  :bind (:map dired-mode-map ("r" . dired-rsync)))

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package async
  :ensure t
  :after dired
  :diminish dired-async-mode
  :init (dired-async-mode))


;;;; FIND

(use-package find-dired
  :custom (find-ls-option '("-print0 | xargs -0 ls -ldb --quoting-style=literal" . "-ldb")))

(use-package fd-dired
  :ensure t
  :bind (:map search-map ("f D" . fd-dired))
  :custom (fd-dired-ls-option '("| xargs -0 ls -ldb --quoting-style=literal" . "-ldb"))

  :config
  (define-advice fd-dired (:filter-args (args) unquote)
    (let ((arg (second args)))
      (list (first args) (substring arg 1 (1- (length arg)))))))


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
        ("C-r" . overwrite-mode)
        ("m"   . nil))

  :custom
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
  ("C-k"   . sp-kill-hybrid-sexp)
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

(use-package lisp-mode :config (put 'use-package #'lisp-indent-function 1))

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

  :config (add-to-list 'flycheck-shellcheck-supported-shells 'dash))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :hook (after-init-hook . flycheck-checkbashisms-setup)

  :custom
  (flycheck-checkbashisms-newline t)
  (flycheck-checkbashisms-posix t))


;;; COMPLETION


;;;; MINIBUFFER

(use-package minibuffer :custom (read-file-name-completion-ignore-case t))

(use-package minibuf-eldef
  :custom (minibuffer-eldef-shorten-default t)
  :hook (after-init-hook . minibuffer-electric-default-mode))


(use-package eldoc :diminish eldoc-mode)

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


;;;; IVY

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands ivy-add-actions
  :hook (after-init-hook . ivy-mode)

  :custom
  (ivy-count-format "%d/%d ")
  (ivy-use-selectable-prompt t)
  (ivy-height 15))

(use-package ivy-xref
  :ensure t
  :custom (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :hook (after-init-hook . counsel-mode)
  :custom (counsel-fzf-dir-function #'counsel-directory)

  :bind
  ([remap tmm-menubar] . counsel-tmm)
  ([remap insert-char] . counsel-unicode-char)
  (:map counsel-mode-map ([remap apropos-command] . nil))
  (:map ctl-x-map ("C-f" . counsel-find-file))
  (:map search-map
        ("r" . counsel-rg)
        ("f d" . counsel-file-directory-jump)
        ("f b" . counsel-find-library)
        ("f l" . counsel-locate)
        ("f z" . counsel-fzf))
  (:map help-map
        ("A"   . counsel-apropos)
        ("F"   . counsel-faces)
        ("z e" . counsel-colors-emacs)
        ("z w" . counsel-colors-web))
  (:map goto-map
        ("i" . counsel-semantic-or-imenu)
        ("j" . counsel-find-symbol)
        ("l" . counsel-ace-link)
        ("m" . counsel-mark-ring)
        ("o" . counsel-outline))
  (:map mode-specific-map
        ;; SHELL
        ("x s" . counsel-switch-to-shell-buffer)
        ;; HISTORY
        ("h c" . counsel-command-history)
        ("h e" . counsel-esh-history)
        ("h m" . counsel-minibuffer-history)
        ("h s" . counsel-shell-history)
        ;; ORG
        ("G G"   . counsel-org-goto-all)
        ("G a h" . counsel-org-agenda-headlines)
        ("G a t" . counsel-org-tag-agenda)
        ("G c"   . counsel-org-capture)
        ("G e"   . counsel-org-entity)
        ("G f"   . counsel-org-file)
        ("G g"   . counsel-org-goto)
        ("G t"   . counsel-org-tag)
        ;; COMPLETION
        ("c c" . counsel-company)
        ("c s" . counsel-symbol)
        ;; OTHER
        ("o P" . counsel-list-processes)
        ("p"   . counsel-package)
        ("v"   . counsel-set-variable))

  :config
  (defun counsel-directory ()
    (or (counsel--git-root) default-directory))

  (defun counsel-file-directory-jump ()
    (interactive)
    (let ((find-program "fd") (counsel-file-jump-args (split-string "-t d -t f -c never")))
      (counsel-file-jump
       nil
       (or (when current-prefix-arg (counsel-read-directory-name "From directory: "))
           (counsel-directory)))))

  (defun kill-buffer-if-alive (buffer)
    (when (buffer-live-p (get-buffer buffer))
      (kill-buffer buffer)))

  (defun ivy-dired-jump-action (dir)
    (dired-jump nil (string-trim-right dir "/")))

  (ivy-add-actions
   'counsel-file-jump
   '(("j" ivy-dired-jump-action "dired jump")
     ("t" find-file-literally "literally")))

  (ivy-add-actions
   'counsel-find-file
   '(("j" ivy-dired-jump-action  "dired jump")
     ("J" find-file-other-window "other window")
     ("t" find-file-literally "literally")))

  (ivy-add-actions
   'counsel-switch-to-shell-buffer
   '(("k" kill-buffer-if-alive "kill buffer")))

  (define-advice counsel-switch-to-shell-buffer (:override () unique)
    (interactive)
    (let* ((default-directory (if current-prefix-arg
                                  (expand-file-name
                                   (counsel-read-directory-name
                                    "Default directory: "))
                                default-directory))
           (buffer-name (if (fboundp 'shell-pwd-generate-buffer-name)
                            (shell-pwd-generate-buffer-name default-directory)
                          "*shell*")))

      (ivy-read "Shell buffer: "
                (cons (generate-new-buffer-name buffer-name)
                      (counsel--buffers-with-mode 'shell-mode))
                :action #'counsel--switch-to-shell
                :caller #'counsel-switch-to-shell-buffer)

      (when (fboundp #'shell-pwd-enable)
        (shell-pwd-enable))))

  (define-advice counsel-set-variable (:around (c-set-var &rest args) prin-fix)
    (cl-letf (((symbol-function 'prin1-char) #'prin1-to-string))
      (if args
          (apply c-set-var args)
        (call-interactively c-set-var)))))


;;;; COMPANY

(use-package company
  :ensure t

  :bind
  (:map company-active-map
        ("M-o" . company-other-backend)
        ("M-h" . company-show-doc-buffer)
        ("M-w" . company-show-location)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-w" . backward-kill-word)
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort)
        ("C-h" . backward-delete-char-untabify))

  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence)))

(use-package company-try-hard
  :ensure t
  :after company
  :bind (:map company-mode-map ("M-Z" . company-try-hard)))

(use-package company-php
  :disabled
  :ensure t
  :demand t
  :after company php-mode
  :init (add-to-list 'company-backends #'company-ac-php-backend)
  :bind (:map php-mode-map ("M-." . ac-php-find-symbol-at-point))
  :custom (ac-php-tags-path (expand-file-name "emacs/ac-php" (xdg-cache-home)))
  :hook (php-mode-hook . ac-php-core-eldoc-setup))

(use-package company-shell
  :ensure t
  :after company

  :init
  (add-to-list 'company-backends #'company-shell)
  (add-to-list 'company-backends #'company-shell-env))

(use-package company-c-headers
  :ensure t
  :after company
  :init (add-to-list 'company-backends #'company-c-headers))

(use-package company-restclient
  :ensure t
  :after company
  :init (add-to-list 'company-backends #'company-restclient))


;;;; SHELL

(use-package bash-completion
  :ensure t
  :after shell
  :hook (shell-dynamic-complete-functions . bash-completion-dynamic-complete))


;;; SEARCHING

(use-package isearch
  :bind (:map isearch-mode-map ("C-h" . isearch-delete-char))
  :config (define-key isearch-mode-map (kbd "C-?") isearch-help-map)

  :custom
  (isearch-allow-scroll t)
  (isearch-lazy-count t))

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
  (:map search-map ("R" . rg))
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
  :hook (after-init-hook . ace-link-setup-default))


;;;; TO DEFINITION

(use-package dumb-jump
  :ensure t
  :custom (dumb-jump-selector 'ivy)
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package imenu-anywhere
  :ensure t
  :bind (:map goto-map ("I" . ivy-imenu-anywhere)))

(use-package find-func
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
  (cargo-process--command-add "add")
  (cargo-process--command-audit "audit -f")
  (cargo-process--command-bench "bench")
  (cargo-process--command-build "build --color never")
  (cargo-process--command-check "check --color never")
  (cargo-process--command-clean "clean")
  (cargo-process--command-clippy "clippy --color never")
  (cargo-process--command-current-file-tests "test --color never")
  (cargo-process--command-current-test "test --color never")
  (cargo-process--command-doc "doc")
  (cargo-process--command-doc-open "doc --open")
  (cargo-process--command-fmt "fmt")
  (cargo-process--command-init "init")
  (cargo-process--command-new "new")
  (cargo-process--command-outdated "outdated -R")
  (cargo-process--command-rm "rm --color never")
  (cargo-process--command-run "run --color never")
  (cargo-process--command-run-bin "run --bin")
  (cargo-process--command-run-example "run --example")
  (cargo-process--command-search "search")
  (cargo-process--command-test "test --color never")
  (cargo-process--command-update "update")
  (cargo-process--command-upgrade "upgrade"))


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
  (set-face-attribute 'comint-highlight-input nil :inherit 'diff-added)
  (set-face-attribute 'comint-highlight-prompt nil :inherit 'diff-hl-change)

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
  :commands shell-pwd-generate-buffer-name shell-pwd-shorten-directory)


;;; TEMPLATES

(use-package autoinsert :hook (find-file-hook . auto-insert))

(use-package yasnippet
  :ensure t
  :custom (yas-wrap-around-region t)

  :bind
  (:map yas-minor-mode-map
        ([(tab)] . nil)
        ("TAB"   . nil)
        ("<tab>" . nil)
        ("C-z"   . yas-expand))
  (:map yas-keymap
        ([(shift tab)] . nil)
        ([backtab]     . nil)
        ("S-TAB"       . nil)
        ("S-<tab>"     . nil)
        ([(tab)]       . nil)
        ("TAB"         . nil)
        ("<tab>"       . nil)
        ("C-S-z"       . yas-prev-field)
        ("C-z"         . yas-next-field-or-maybe-expand)))

(use-package yasnippet-snippets :ensure t)


;;; APPLICATIONS

(use-package vlf
  :ensure t
  :after counsel
  :init (ivy-add-actions 'counsel-find-file '(("l" vlf "view large file"))))

(use-package sdcv :ensure t)

(use-package ediff :hook (ediff-before-setup-hook . save-window-configuration-to-w))

(use-package activity-log
  :quelpa
  (activity-log :repo "xFA25E/activity-log" :fetcher github :version original))

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

(use-package elf-mode
  :quelpa (elf-mode :repo "sirikid/elf-mode" :fetcher github :version original))

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

(use-package auto-package-update
  :ensure t

  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-last-update-day-filename "last-package-update-day")
  (auto-package-update-prompt-before-update t)
  (auto-package-update-last-update-day-path
   (expand-file-name "emacs/last-package-update-day" (xdg-cache-home))))


;;;; XML

(use-package eww :custom (eww-search-prefix "https://ddg.co/lite/?q="))

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


;;;; PROJECTILE

(use-package projectile
  :ensure t
  :bind-keymap ("M-m" . projectile-command-map)

  :custom
  (projectile-completion-system 'ivy)
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

(use-package counsel-projectile
  :ensure t
  :hook (after-init-hook . counsel-projectile-mode))


;;;; YTEL

(use-package ytel
  :ensure t
  ;; :custom (ytel-invidious-api-url "https://invidious.snopyta.org")
  :custom (ytel-invidious-api-url "https://invidious.mservice.ru.com")
  :hook (ytel-mode-hook . toggle-truncate-lines)

  :bind
  (:map mode-specific-map ("o Y" . ytel))
  (:map ytel-mode-map
        ("c" . ytel-copy-link)
        ("t" . ytel-show-thumbnail)
        ("v" . ytel-current-browse-url)
        ("RET" . ytel-show))

  :config
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

  (defun ytel-show-thumbnail ()
    (interactive)
    (cl-flet ((pred (tm) (string-equal "maxresdefault" (alist-get 'quality tm))))
      (let ((method (concat "videos/" (ytel-video-id (ytel-get-current-video)))))
        (let-alist (ytel--API-call method '(("fields" "videoThumbnails")))
          (eww (alist-get 'url (cl-find-if #'pred .videoThumbnails))))))))

(use-package ytel
  :ensure t
  :after bruh
  :hook (ytel-mode-hook . ytel-title-function-setup)

  :config
  (defun ytel-title-function-setup ()
    (setq-local
     bruh-mpvi-get-title-function
     (lambda () (ytel-video-title (ytel-get-current-video))))))


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
     ("Justus Walker" "https://www.youtube.com/feeds/videos.xml?user=senttosiberia")
     ("Luke Smith" "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA")
     ("Luke Smith Blog" "https://lukesmith.xyz/rss.xml")
     ("Luke Smith PeerTube" "https://videos.lukesmith.xyz/feeds/videos.xml?accountId=3")
     ("Tsoding" "https://www.youtube.com/feeds/videos.xml?channel_id=UCEbYhDd6c6vngsF5PQpFVWg")
     ("Protesilaos Stavrou" "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g")
     ("Atlanta Functional Programming" "https://www.youtube.com/feeds/videos.xml?channel_id=UCYg6qFXDE5SGT_YXhuJPU0A")
     ("Мысли и методы" "http://feeds.soundcloud.com/users/soundcloud:users:259154388/sounds.rss")
     ("Простые Мысли" "https://www.youtube.com/feeds/videos.xml?channel_id=UCZuRMfF5ZUHqYlKkvU12xvg")
     ("Простая Академия" "https://www.youtube.com/feeds/videos.xml?channel_id=UC8mmPf2oKdfE2pdjqctTWUw")))

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
              (kill-buffer))))))))

(use-package newst-treeview
  :after bruh
  :hook (newsticker-treeview-mode-hook . newst-treeview-title-function-setup)
  :config
  (defun newst-treeview-title-function-setup ()
    (setq-local
     bruh-mpvi-get-title-function
     (lambda () (newsticker--title (newsticker--treeview-get-selected-item))))))


;;;; MPD

(use-package mingus
  :ensure t

  :bind
  (:map mode-specific-map ("o s" . mingus))

  :custom
  (mingus-mode-line-separator "|")
  (mingus-mode-line-string-max 120)
  (mingus-mpd-config-file (expand-file-name "mpd/mpd.conf" (xdg-config-home)))
  (mingus-seek-amount 5)
  (mingus-use-mouse-p nil)

  :config
  (define-advice mingus-git-out (:override (&optional _x) kill)
    (interactive)
    (when (mingus-buffer-p) (kill-current-buffer))))

(use-package mingus
  :ensure t
  :after counsel
  :bind (:map mode-specific-map ("o S" . mingus-find-and-add-file))

  :config
  (defun mingus-find-and-add-file ()
    (interactive)
    (counsel-require-program find-program)
    (let ((default-directory (xdg-music-dir)))
      (mingus-add-files
       (list
        (expand-file-name
         (ivy-read
          "Add file to mpd: "
          (counsel--find-return-list
           (split-string
            ". ( -iname *.flac -o -iname *.m4a -o -iname *.mp3 -o -iname *.ogg -o -iname *.opus ) -type f -o -type d"))
          :require-match t)
         (xdg-music-dir))))
      (mpd-play mpd-inter-conn)
      (kill-buffer-if-alive (get-buffer "*Mingus*")))))


;;;; E-READER

(use-package pdf-tools :ensure t)

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

  :bind
  (:map message-mode-map
        ("C-c M-o" . org-mime-htmlize)
        ("C-c M-e" . org-mime-edit-mail-in-org-mode)
        ("C-c M-t" . org-mime-revert-to-plain-text-mail))

  :config
  (define-advice org-mime-replace-images (:filter-args (args) fix-imgs)
    (cl-destructuring-bind (first . rest) args
      (cons (replace-regexp-in-string "src=\"file:///" "src=\"/" first) rest))))

(use-package ox-html
  :after org

  :custom
  (org-html-htmlize-output-type 'css)
  (org-html-htmlize-font-prefix "org-"))


;;; LOCAL VARIABLES

;; Local Variables:
;; lexical-binding: t
;; outline-minor-mode: t
;; eval: (cl-pushnew (quote emacs-lisp-checkdoc) flycheck-disabled-checkers)
;; End:
