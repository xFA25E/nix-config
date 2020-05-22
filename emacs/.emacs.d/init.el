;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); lexical-binding: t -*-

(require 'xdg)

(eval-and-compile
  (defvar nsm-settings-file (expand-file-name "emacs/network-security.data"
                                              (xdg-cache-home)))
  (defvar package-user-dir (expand-file-name "emacs/elpa" (xdg-cache-home)))
  (defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (defvar package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                             ("melpa" . "https://melpa.org/packages/")
                             ("org"   . "https://orgmode.org/elpa/")))

  (require 'package)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)

  (setq use-package-expand-minimally t
        use-package-enable-imenu-support t
        use-package-always-defer t
        use-package-hook-name-suffix nil)

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
    :custom (quelpa-use-package-inhibit-loading-quelpa t)))

(use-package gcmh
  :diminish gcmh-mode
  :ensure t
  :init (gcmh-mode 1))

(use-package emacs
  :bind ("C-S-SPC" . insert-space-after-point)

  :custom
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (enable-recursive-minibuffers t)
  (history-delete-duplicates t)
  (history-length 200)
  (hscroll-step 1)
  (indent-tabs-mode nil)
  (indicate-buffer-boundaries 'left)
  (indicate-empty-lines t)
  (next-screen-context-lines 10)
  (resize-mini-windows t)
  (tab-width 4)
  (truncate-lines t)
  (undo-limit 200000)
  (undo-outer-limit 20000000)
  (undo-strong-limit 300000)
  (use-dialog-box nil)
  (visible-bell nil)
  (x-gtk-use-system-tooltips nil)
  (x-stretch-cursor t)
  (fill-column 80)
  (help-char (aref (kbd "C-?") 0))
  (kill-buffer-query-functions
   (remq #'process-kill-buffer-query-function kill-buffer-query-functions))

  :config
  (setq-default line-spacing 0.2)

  (defun insert-space-after-point ()
    (interactive)
    (save-excursion (insert " "))))

(use-package minibuffer
  :commands read-file-name
  :custom (read-file-name-completion-ignore-case t)
  :bind (:map minibuffer-inactive-mode-map ("C-c M-h" . pcomplete-help)))

(use-package startup
  :init (provide 'startup)

  :custom
  (auto-save-list-file-prefix nil)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (auto-save-list-file-name
   (expand-file-name (format-time-string "emacs/auto-saves/list/%y-%m-%d~")
                     (xdg-cache-home))))

(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment "UTF-8"))

(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args (list "--sug-mode=ultra")))

(use-package paragraphs
  :init (provide 'paragraphs)
  :custom (sentence-end-double-space nil))

(use-package xdg
  :commands xdg-documents-dir xdg-download-dir

  :config
  (defun xdg-documents-dir ()
    (or (getenv "XDG_DOCUMENTS_DIR") "~/Documents"))

  (defun xdg-download-dir ()
    (or (getenv "XDG_DOWNLOAD_DIR") "~/Downloads")))

(use-package window
  :commands pop-to-buffer
  :init (provide 'window)

  :bind
  ("M-V"     . scroll-down-line)
  ("C-S-v"   . scroll-up-line)
  ("C-M-S-b" . previous-buffer)
  ("C-M-S-f" . next-buffer)
  ("M-Q"     . quit-window)
  (:map ctl-x-map ("C-b" . switch-to-buffer))

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Man" (0+ anything) "*" string-end)
                 . (display-buffer-reuse-mode-window
                    (inhibit-same-window . nil)
                    (mode . Man-mode)))))

(use-package subr
  :init (provide 'subr)

  :commands
  with-current-buffer
  shell-quote-argument
  error
  start-process
  add-to-list
  eval-after-load
  match-string
  replace-regexp-in-string
  string-prefix-p
  split-string
  butlast
  remove-hook

  :bind (:map mode-specific-map ("x t" . terminal-in-path))

  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (defun terminal-in-path (&optional path)
    "Opens an terminal at PATH. If no PATH is given, it uses
    the value of `default-directory'. PATH may be a tramp remote path."
    (interactive)
    (unless path (setq path default-directory))

    (let ((path     (replace-regexp-in-string (rx line-start "file:") "" path))
          (cd-str   "{ fn=%s; [ -d $fn ] && cd $fn || cd $(dirname $fn); }")
          (term-cmd nil))

      (if (tramp-tramp-file-p path)
          (let* ((tstruct   (tramp-dissect-file-name   path))
                 (tmethod   (tramp-file-name-method    tstruct))
                 (thost     (tramp-file-name-host      tstruct))
                 (tpath     (tramp-file-name-localname tstruct))
                 (tport     (tramp-file-name-port      tstruct))
                 (tuser     (tramp-file-name-user      tstruct)))

            (cond
             ((member tmethod '("scp" "ssh" "sftp"))
              (setq term-cmd (format "ssh %s %s%s -t '%s && bash'"
                                     (if tport (concat "-p " tport) "")
                                     (if tuser (concat tuser "@") "")
                                     thost
                                     (format cd-str tpath))))

             (t (error "not implemented for method %s" tmethod))))

        (setq term-cmd (format "%s && sh" (format cd-str path))))

      (start-process "terminal" nil "tm" term-cmd))))

(use-package subr-x :commands when-let)

(use-package tramp
  :commands
  tramp-dissect-file-name
  tramp-file-name-host
  tramp-file-name-localname
  tramp-file-name-method
  tramp-file-name-port
  tramp-file-name-user
  tramp-tramp-file-p

  :custom
  (tramp-persistency-file-name
   (expand-file-name "emacs/tramp/connection-history" (xdg-cache-home)))
  (tramp-default-method "ssh")
  (tramp-histfile-override t)
  (tramp-completion-reread-directory-timeout nil)

  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/.local/bin"))

(use-package apropos :custom (apropos-sort-by-scores t))

(use-package tool-bar :config (tool-bar-mode -1))

(use-package scroll-bar
  :custom
  (scroll-step 1)
  (scroll-margin 5)
  (scroll-conservatively 10000)

  :config (scroll-bar-mode -1))

(use-package menu-bar
  :bind ("<f10>" . menu-bar-mode)
  :config (menu-bar-mode -1))

(use-package tooltip :config (tooltip-mode -1))

(use-package faces
  :custom-face
  (default ((t (:inherit default :family "Iosevka"))))
  (mode-line ((t (:box (:line-width 1 :color nil :style nil)
                       :foreground "black" :background "white"
                       :font "DejaVu Sans"))))
  (mode-line-inactive ((t (:box (:line-width 1 :color "grey75" :style nil)
                                :weight light :inherit mode-line
                                :foreground "grey20" :background "grey90"))))
  (header-line ((t (:background "grey90" :foreground "grey20" :box nil))))
  (fixed-pitch-serif ((t (:inherit fixed-pitch-serif :font "DejaVu Serif")))))

(use-package paren
  :custom (show-paren-style 'parentheses)
  :hook (after-init-hook . show-paren-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package elec-pair :hook (after-init-hook . electric-pair-mode))

(use-package electric
  :hook (after-init-hook . electric-indent-mode-disable)
  :config (defun electric-indent-mode-disable () (electric-indent-mode -1)))

(use-package files
  :commands read-directory-name rename-this-file custom-backup-enable-predicate

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
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "emacs/auto-saves/" (xdg-cache-home)) t)))
  (backup-directory-alist
   `((".*" . ,(expand-file-name "emacs/backups" (xdg-data-home)))))

  :config
  (defun rename-this-file ()
    "Rename visiting file and it's buffer name."
    (interactive)
    (when-let ((filename (buffer-file-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (when (not (get-buffer new-name))
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))

  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

  (defun custom-backup-enable-predicate (name)
    (let ((regexp (rx (or (and string-start (or "/tmp/" "/dev/shm/"))
                          (and ".vcf" string-end)))))
      (or (not (string-match-p regexp name))
          (normal-backup-enable-predicate name)))))

(use-package novice :custom (disabled-command-function nil))

(use-package simple
  :hook
  (before-save-hook . delete-trailing-whitespace)
  (after-init-hook  . size-indication-mode)
  (after-init-hook  . column-number-mode)

  :bind
  ("C-h"   . backward-delete-char-untabify)
  ("C-x w" . mark-whole-buffer)
  ("M-K"   . kill-whole-line)
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
    (cycle-spacing n nil 'fast)))

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom (he-file-name-chars "-a-zA-Z0-9_/.,~^#$+={}"))

(use-package tex-mode
  :hook (tex-mode-hook . setup-tex-mode-ispell-parser)

  :config
  (defun setup-tex-mode-ispell-parser ()
    (setq-local ispell-parser 'tex)))

(use-package dired
  :commands dired-get-marked-files

  :hook
  (dired-mode-hook          . dired-hide-details-mode)
  (dired-before-readin-hook . dired-setup-switches)

  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alDF --si --group-directories-first")
  (dired-ls-F-marks-symlinks t)

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

(use-package dired-aux
  :demand t
  :after dired
  :commands dired-do-shell-command
  :bind (:map dired-mode-map ("b" . dired-stat))

  :config
  (add-to-list 'dired-compress-files-alist '("\\.tar\\'" . "tar -cf %o %i"))

  (defun dired-stat ()
    (interactive)
    (dired-do-shell-command "stat" current-prefix-arg
                            (dired-get-marked-files t current-prefix-arg))))

(use-package async
  :ensure t
  :after bytecomp
  :init (async-bytecomp-package-mode))

(use-package async
  :ensure t
  :after dired
  :diminish dired-async-mode
  :init (dired-async-mode))

(use-package dired-x
  :demand t
  :after dired
  :bind (:map ctl-x-map ("C-j" . dired-jump))

  :custom
  (dired-guess-shell-alist-user
   `((,(rx "." (or "csv" "doc" "docx" "xlsx" "xls" "odt" "ods" "odp" "ppt" "pptx") string-end)
      "setsid -f libreoffice * >/dev/null 2>&1"
      "libreoffice --invisible --headless --convert-to pdf * &"
      "libreoffice --invisible --headless --convert-to epub * &"
      "libreoffice --invisible --headless --convert-to csv * &")

     (,(rx "." (or "jpeg" "jpg" "gif" "png" "bmp" "tif" "thm" "nef" "jfif") string-end)
      "setsid -f sxiv * >/dev/null 2>&1"
      "setsid -f gimp * >/dev/null 2>&1")

     (,(rx ".fb2" string-end)
      "ebook-convert ? .epub &")

     (,(rx ".pdf" string-end)
      "setsid -f zathura * >/dev/null 2>&1"
      "setsid -f libreoffice * >/dev/null 2>&1")

     (,(rx "." (or "epub" "djvu") string-end)
      "setsid -f zathura * >/dev/null 2>&1")

     (,(rx "." (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi"
                   "mpg" "mov" "3gp" "vob" "wmv" "aiff")
           string-end)
      "setsid -f mpv --force-window=yes * >/dev/null 2>&1"
      "video_duration * | format_duration"
      "mediainfo"
      "mpv -vo=drm")

     (,(rx ".cue" string-end)
      "setsid -f mpv --force-window=yes * >/dev/null 2>&1")

     (,(rx ".torrent" string-end)
      "transmission-show"
      "transmission-remote --add")

     (,(rx ".rar" string-end)
      "temp=\"$(basename `?` .rar)\"; mkdir \"${temp}\"; unrar x ? \"${temp}\""))))

(use-package ibuffer
  :custom (ibuffer-default-sorting-mode 'major-mode)
  :bind (:map ctl-x-map ("C-S-b" . ibuffer-jump)))

(use-package cus-edit :custom (custom-file null-device))

(use-package cc-mode
  :custom (c-default-style '((java-mode . "java") (other . "awk"))))

(use-package compile
  :custom
  (compilation-always-kill   t)
  (compilation-scroll-output 'first-error)

  :bind (:map ctl-x-map ("c" . compile)))

(use-package sh-script
  :commands sh-show-shell
  :custom (system-uses-terminfo nil))

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main    t))

(use-package isearch
  :bind
  ([remap isearch-forward-regexp]  . isearch-forward)
  ([remap isearch-backward-regexp] . isearch-backward)
  (:map search-map ("p" . isearch-forward-symbol-at-point))
  (:map isearch-mode-map ("C-h" . isearch-delete-char)))

(use-package man
  :custom (Man-notify-method 'aggressive)
  :bind (:map help-map ("M" . man))

  :custom-face
  (Man-overstrike ((t (:inherit font-lock-variable-name-face :bold t))))
  (Man-underline  ((t (:inherit font-lock-negation-char-face :underline t)))))

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

(use-package wdired
  ;; does not work as expected
  :hook (wdired-mode-hook . disable-image-dired)
  :config (defun disable-image-dired () (image-dired-minor-mode -1)))

(use-package comint
  :functions
  write-region-with-filter
  comint-read-input-ring
  comint-show-maximum-output
  comint-delete-input
  comint-send-input

  :hook
  (kill-buffer-hook . comint-write-input-ring)
  (kill-emacs-hook  . save-buffers-comint-input-ring)
  (comint-output-filter-functions . comint-strip-ctrl-m)
  (comint-output-filter-functions . comint-truncate-buffer)

  :custom
  (comint-input-ignoredups t)
  (comint-input-ring-size 10000)
  (comint-buffer-maximum-size 10240)

  :init (defvar-local comint-history-filter-function (lambda (_file)))

  :custom-face
  (comint-highlight-input ((t (:inherit diff-added))))
  (comint-highlight-prompt ((t (:inherit diff-hl-change))))

  :config
  (defun save-buffers-comint-input-ring ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf (comint-write-input-ring))))

  (let ((original-write-region (symbol-function 'write-region)))
    (defun write-region-with-filter (oldfunc &rest args)
      (let ((filter-function comint-history-filter-function))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest largs)
                     (funcall filter-function (nth 2 largs))
                     (apply original-write-region (cons nil (cdr largs))))))
          (apply oldfunc args)))))

  (advice-add 'comint-write-input-ring :around #'write-region-with-filter))

(use-package shell
  :functions shell-history-filter

  :bind
  (:map shell-mode-map
        ("C-c M-d" . shell-change-directory)
        ("C-c M-h" . pcomplete-help))

  :custom
  (shell-prompt-pattern
   (rx line-start
       (one-or-more digit) " "
       alpha
       (zero-or-more (in ?- ?_ alpha digit)) " "))

  :hook (shell-mode-hook . shell-enable-comint-history)

  :config
  (defun shell-history-filter (file)
    (goto-char (point-min))
    (insert-file-contents file)
    (flush-lines
     (rx (or
          (and bol
               (opt "sudo " (opt "-A "))
               (or "aria2c" "awk" "base16_theme" "bash" "bspc" "cat" "cd"
                   "chmod" "chown" "ckbatt" "command" "cp" "cut" "dash" "dd"
                   "df" "dh" "du" "echo" "em" "emacs" "env" "exit" "export" "fd"
                   "feh" "file" "find" "gawk" "gparted" "gpg" "grep" "gzip"
                   "hash" "host" "htop" "id" "ln" "locate" "ls" "man" "mbsync"
                   "mkdir" "mmpv" "mpop" "mpv" "mpvi" "mv" "myoutube-dl"
                   "notify-send" "ping" "pkill" "printf" "pwgen" "python" "quit"
                   "read" "rg" "rimer" "rm" "rmdir" "rofi" "runel" "setsid" "sh"
                   "sleep" "stow" "strings" "strip" "sxiv" "time" "timer" "top"
                   "touch" "tr" "uname" "uptime" "watch" "wc" "which" "xclip"
                   "xz" "youtube-dl" "ytdl" "ytdla" "ytdlam" "ytdlay" "ytdli"
                   "ytdlp" "ytdlpa" "ytdlpay" "ytdlpy" "ytdly")
               eow)
          (not (any print space))))
     (point-min) (point-max))
    (delete-duplicate-lines (point-min) (point-max)))

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

(use-package autoinsert :hook (find-file-hook . auto-insert))

(use-package message
  :commands message-send-mail-with-sendmail

  :custom
  (message-kill-buffer-on-exit t)
  (message-send-mail-function  #'message-send-mail-with-sendmail)
  (message-subject-re-regexp (rx bol
                                 (* (any " " "\t"))
                                 (* (any "R" "r")
                                    (? (any "E" "e"))
                                    (* "[" (* digit) "]")
                                    (? " ") ":"
                                    (* (any " " "\t")))
                                 (* (any " " "\t")))))

(use-package sendmail
  :custom
  (sendmail-program "msmtp")
  (send-mail-function #'message-send-mail-with-sendmail))

(use-package mu4e
  :defines
  mu4e-maildir
  mu4e-view-actions
  mu4e-headers-actions
  mu4e-main-mode-map
  mu4e-headers-mode-map
  mu4e-view-mode-map

  :commands
  mu4e-kill-update-mail
  mu4e-update-mail-and-index
  mu4e-update-index
  mu4e-action-view-in-browser@check-parens-fix
  mu4e@with-index
  mu4e-update-mail-and-index@kill-update

  :hook (after-init-hook . start-mu4e)

  :bind
  (:map mode-specific-map ("o m" . mu4e))
  (:map mu4e-headers-mode-map ("C-c C-e" . mu4e-update-mail-and-index-exys))
  (:map mu4e-view-mode-map ("C-c C-e" . mu4e-update-mail-and-index-exys))

  (:map mu4e-main-mode-map
        ("q" . quit-window)
        ("C-c C-e" . mu4e-update-mail-and-index-exys))

  :custom
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
  (mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (mu4e-view-show-addresses t)
  (mu4e-attachment-dir (expand-file-name (xdg-download-dir)))
  (mu4e-modeline-max-width 100)
  (mu4e-get-mail-command "mailsync -a")
  (mu4e-update-interval 600)
  (mu4e-maildir
   (or (getenv "MAILDIR") (expand-file-name "mail" (xdg-data-home))))
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
     (mapcan (lambda (args)
               (mapcar (lambda (ext) (cons ext (car args)))
                       (cdr args)))
             '(("sxiv"        . ("png" "jpg" "gif" "jpeg" "bmp" "tif" "thm"))
               ("zathura"     . ("pdf" "epub" "djvu"))
               ("libreoffice" . ("doc" "docx" "xlsx" "xls" "odt" "ppt"))
               ("mpv"         . ("m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4"
                                 "avi" "mpg" "mov" "3gp" "vob"))))))

  :init
  (dolist (dir '("SENT" "DRAFTS" "TRASH" "ARCHIVE"))
    (make-directory (expand-file-name dir mu4e-maildir) t))

  :config
  (load-file (expand-file-name "emacs/secrets/mu4e.el" (xdg-data-home)))

  (add-to-list 'mu4e-view-actions
               '("browser view" . mu4e-action-view-in-browser)
               t)

  (define-advice mu4e-action-view-in-browser
      (:around (oldfunc &rest args) check-parens-fix)
    (let ((prog-mode-hook nil)
          (browse-url-browser-function #'browse-url-firefox))
      (apply oldfunc args)))

  (define-advice mu4e-update-mail-and-index
      (:before (&rest _ignore) kill-update)
    (mu4e-kill-update-mail))


  (defun mu4e-update-mail-and-index-exys ()
    (interactive)
    (let ((mu4e-get-mail-command "mailsync exys"))
      (mu4e-update-mail-and-index nil)))

  (defun start-mu4e () (mu4e t)))

(use-package proced :bind (:map mode-specific-map ("o p" . proced)))

(use-package executable
  :custom (executable-chmod 64)
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package minibuf-eldef
  :custom (minibuffer-eldef-shorten-default t)
  :hook (after-init-hook . minibuffer-electric-default-mode))

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

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file (expand-file-name "emacs/bookmarks" (xdg-data-home))))

(use-package uniquify :custom (uniquify-ignore-buffers-re   "^\\*"))

(use-package dired-hide-dotfiles
  :ensure t
  :bind (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

(use-package diredfl
  :ensure t
  :hook (dired-mode-hook . diredfl-mode)

  :custom-face
  (diredfl-compressed-file-suffix
   ((t (:inherit diredfl-compressed-file-suffix :foreground "orange")))))

(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map ("r" . dired-rsync)))

(use-package auto-package-update
  :ensure t

  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-last-update-day-filename "last-package-update-day")
  (auto-package-update-prompt-before-update t)
  (auto-package-update-last-update-day-path
   (expand-file-name "emacs/last-package-update-day" (xdg-cache-home))))

(use-package smali-mode
  :quelpa
  (smali-mode :repo "strazzere/Emacs-Smali" :fetcher github :version original)

  :mode (rx ".smali" string-end))

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
  :commands reverse-im-activate
  :config (reverse-im-activate "cyrillic-dvorak"))

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

(use-package ace-window
  :ensure t
  :commands aw-switch-to-window aw-flip-window
  :bind ("M-o" . ace-window)

  :custom
  (aw-keys (string-to-list "htnsaoeuid"))
  (aw-scope 'frame)
  (aw-background t)
  (aw-leading-char-style 'path)
  (aw-dispatch-always t)
  (aw-minibuffer-flag t)
  (aw-fair-aspect-ratio 3)
  (aw-dispatch-alist
   '((?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?r aw-flip-window)
     (?f aw-find-file-in-window "Find File In Window")
     (?F aw-find-file-other-window "Find File Other Window")
     (?b aw-switch-buffer-in-window "Select Buffer")
     (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?w aw-split-window-fair "Split Fair Window")
     (?k aw-delete-window "Delete Window")
     (?K delete-other-windows "Delete Other Windows")
     (?? aw-show-dispatch-help)))

  :custom-face
  (aw-leading-char-face
   ((t (:inherit aw-leading-char-face
                 :foreground "red" :weight bold :height 1.5))))

  :config
  (defun aw-find-file-in-window (window)
    "Find file in WINDOW."
    (aw-switch-to-window window)
    (call-interactively #'find-file))

  (defun aw-find-file-other-window (window)
    "Find file other WINDOW."
    (aw-switch-to-window window)
    (call-interactively #'find-file)
    (aw-flip-window)))

(use-package ace-link
  :ensure t
  :hook (after-init-hook . ace-link-setup-default))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands ivy-add-actions
  :hook (after-init-hook . ivy-mode)

  :custom
  (ivy-count-format "%d/%d ")
  (ivy-use-selectable-prompt t))

(use-package swiper
  :ensure t

  :bind
  ("C-s" . swiper-isearch)
  (:map search-map ("." . swiper-isearch-thing-at-point)))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :hook (after-init-hook . counsel-mode)
  :custom (counsel-fzf-dir-function #'counsel-fzf-dir-function-git-root)

  :functions
  get-grep-lines
  counsel-switch-to-shell-buffer@unique
  counsel-set-variable@prin-fix

  :commands
  counsel-git-grep-action
  counsel-read-directory-name
  counsel--buffers-with-mode
  counsel--switch-to-shell

  :bind
  ([remap tmm-menubar] . counsel-tmm)
  ([remap insert-char] . counsel-unicode-char)
  (:map counsel-mode-map ([remap apropos-command] . nil))
  (:map ctl-x-map ("C-f" . counsel-find-file))

  (:map search-map
        ("r" . counsel-rg)
        ("f f" . counsel-file-directory-jump)
        ("f d" . counsel-file-directory-jump-fd)
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
        ("c e" . counsel-el)
        ("c j" . counsel-clj)
        ("c l" . counsel-cl)
        ("c p" . counsel-jedi)
        ;; OTHER
        ("o P" . counsel-list-processes)
        ("p"   . counsel-package)
        ("v"   . counsel-set-variable))

  :config
  (defun counsel-fzf-dir-function-git-root ()
    (or (counsel--git-root) default-directory))

  (defun counsel-file-directory-jump ()
    (interactive)
    (let* ((find-program find-program)
           (counsel-file-jump-args
            (cl-flet ((entries (entries type f)
                               (list* (funcall f (car entries))
                                      (mapcan
                                       (lambda (e) (list "-o" type (funcall f e)))
                                       (cdr entries)))))
              (append '(".")
                      (when-let ((entries grep-find-ignored-directories))
                        (append '("-type" "d" "(" "-path")
                                (entries entries "-path" (lambda (d) (concat "*/" d)))
                                '(")" "-prune" "-o")))
                      (when-let ((entries grep-find-ignored-files))
                        (append '("!" "-type" "d" "(" "-name")
                                (entries entries "-name" #'identity)
                                '(")" "-prune" "-o")))
                      '("(" "-type" "f" "-o" "-type" "d" ")" "-print")))))
      (counsel-file-jump
       nil
       (or (when current-prefix-arg
             (counsel-read-directory-name "From directory: "))
           (counsel--git-root)
           default-directory))))

  (defun counsel-file-directory-jump-fd ()
    (interactive)
    (let ((find-program "fd")
          (counsel-file-jump-args (split-string "-t d -t f -c never")))
      (counsel-file-jump
       nil
       (or (when current-prefix-arg
             (counsel-read-directory-name "From directory: "))
           (counsel--git-root)
           default-directory))))

  (defun kill-buffer-if-alive (buffer)
    (when (buffer-live-p (get-buffer buffer))
      (kill-buffer buffer)))

  (defun ivy-dired-jump-action (dir)
    (dired-jump nil (string-trim-right dir "/")))

  (ivy-add-actions
   'counsel-file-jump
   '(("j" ivy-dired-jump-action "dired jump")))

  (ivy-add-actions
   'counsel-find-file
   '(("j" ivy-dired-jump-action  "dired jump")
     ("J" find-file-other-window "other window")))

  (ivy-add-actions
   'counsel-switch-to-shell-buffer
   '(("k" kill-buffer-if-alive "kill buffer")))

  (defun get-grep-lines (regex)
    (butlast
     (split-string
      (shell-command-to-string
       (format "grep -E -Hn --color=never -r -e %s"
               (shell-quote-argument regex)))
      "\n")))

  (defun counsel-recursive-grep ()
    (interactive)
    (ivy-read "Grep: "
              #'get-grep-lines
              :dynamic-collection t
              :action #'counsel-git-grep-action
              :caller 'counsel-dynamic-grep))

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

(use-package ivy-xref
  :ensure t
  :custom (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package mb-depth :hook (after-init-hook . minibuffer-depth-indicate-mode))

(use-package flycheck
  :ensure t
  :hook (after-init-hook . global-flycheck-mode)

  :custom
  (flycheck-mode-line-prefix "FC")
  (flycheck-global-modes '(not lisp-interaction-mode))
  (flycheck-shellcheck-supported-shells '(dash bash ksh88 sh))
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-clang-pedantic-errors t)
  (flycheck-clang-pedantic t)
  (flycheck-gcc-pedantic-errors t)
  (flycheck-gcc-pedantic t)
  (flycheck-phpcs-standard "PSR12,PSR1,PSR2")

  :config
  (setq flycheck-shellcheck-supported-shells '(dash bash ksh88 sh))

  (flycheck-define-command-checker 'sh-dash
    "A Dash Shell syntax checker."
    :command '("dash" "-n")
    :standard-input t
    :error-patterns '((error
                       line-start (one-or-more (not (any ":"))) ": " line ": "
                       (message)))
    :modes 'sh-mode
    :predicate (lambda () (eq sh-shell 'dash))
    :next-checkers '((warning . sh-shellcheck)))

  (add-to-list 'flycheck-checkers 'sh-dash))

(use-package ipretty
  :ensure t
  :bind ([remap eval-print-last-sexp] . ipretty-last-sexp))

(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init-hook . global-company-mode)

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
  (company-transformers '(company-sort-by-occurrence))
  (company-global-modes '(not org-mode
                              Info-mode
                              help-mode
                              Custom-mode
                              epa-key-list-mode
                              shell-mode)))

(use-package company-c-headers
  :ensure t
  :after company
  :init (add-to-list 'company-backends #'company-c-headers))

(use-package company-shell
  :ensure t
  :after company

  :init
  (add-to-list 'company-backends #'company-shell)
  (add-to-list 'company-backends #'company-shell-env))

(use-package select
  :custom
  (selection-coding-system 'utf-8)
  (select-enable-clipboard t))

(use-package org
  :ensure org-plus-contrib
  :commands add-book-to-library
  :bind (:map mode-specific-map ("G a a" . org-agenda))

  :custom
  (org-src-tab-acts-natively t)
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
                                 (shell      . t)))

  (defun add-book-to-library (file directory)
    (interactive
     (let ((lib-dir (expand-file-name "library/" (xdg-documents-dir))))
       (list (read-file-name "Book: " lib-dir nil t)
             (read-directory-name "Directory: " lib-dir nil t))))

    (string-match (rx "/" (group (one-or-more (not (any "/"))))
                      "." (or "pdf" "djvu" "fb2" "epub") string-end)
                  file)
    (let ((newfile (expand-file-name (substring (match-string 0 file) 1)
                                     directory))
          (book (match-string 1 file))
          (tags (upcase
                 (replace-regexp-in-string
                  (rx "/") ":"
                  (replace-regexp-in-string
                   (rx (or space "-")) "_"
                   (substring
                    directory
                    (length
                     (expand-file-name "library" (xdg-documents-dir)))))))))
      (find-file (expand-file-name "library/library_want.org" (xdg-documents-dir)))
      (goto-char (point-max))
      (insert
       (format "* WANT %s %s \n  :PROPERTIES:\n  :FILE:      [[file:%s]]\n  :END:\n"
               book tags (string-remove-prefix
                          (expand-file-name "library/" (xdg-documents-dir))
                          newfile)))
      (save-buffer)
      (rename-file file newfile))))

(use-package org-bullets
  :ensure t
  :hook (org-mode-hook . org-bullets-mode))

(use-package ox-html
  :after org

  :custom
  (org-html-htmlize-output-type 'css)
  (org-html-htmlize-font-prefix "org-"))

(use-package htmlize
  :ensure t
  :commands htmlize-region-save-screenshot)

(use-package css-mode :bind (:map css-mode-map ("C-c m" . css-lookup-symbol)))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook (css-mode-hook . rainbow-mode))

(use-package clipmon
  :ensure t
  :hook (after-init-hook . clipmon-mode))

(use-package cider :ensure t)

(use-package clojure-mode :ensure t)

(use-package eldoc
  :diminish eldoc-mode
  :hook (after-init-hook . global-eldoc-mode))

(use-package sudo-edit
  :ensure t
  :hook
  (after-init-hook . sudo-edit-indicator-mode)
  (shell-mode-hook . sudo-edit-set-header))

(use-package vlf
  :ensure t
  :after counsel
  :init (ivy-add-actions 'counsel-find-file '(("l" vlf "view large file"))))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init-hook . which-key-mode))

(use-package wttrin
  :ensure t

  :custom
  (wttrin-default-cities '("Vertemate, Italy" "Como, Italy"))
  (wttrin-default-accept-language '("Accept-Language" . "en-US")))

(use-package rust-mode
  :ensure t
  :custom (rust-format-on-save t))

;; Add support for cargo error --> file:line:col
(use-package cargo
  :ensure t
  :hook (rust-mode-hook . cargo-minor-mode)

  :custom
  (cargo-process--enable-rust-backtrace t)
  (cargo-process--command-flags "--color never"))

(use-package lsp-mode
  :ensure t
  :hook (rust-mode-hook . lsp)

  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-folding nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-prefer-flymake nil)
  (lsp-session-file (expand-file-name "emacs/lsp/session" (xdg-cache-home)))

  :bind
  (:map lsp-mode-map
        :prefix-map lsp-prefix-map
        :prefix "C-c l"
        ("." . lsp-execute-code-action)
        ("N" . lsp-rename)
        ("d" . lsp-describe-thing-at-point)
        ("g" . lsp-find-definition)
        ("i" . lsp-find-implementation)
        ("m" . lsp-format-buffer)
        ("r" . lsp-find-references)
        ("t" . lsp-find-type-definition)
        ("z" . lsp-workspace-restart)))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (:map lsp-prefix-map ("e" . lsp-ui-flycheck-list))

  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package company-lsp
  :ensure t
  :after company
  :init (add-to-list 'company-backends #'company-lsp))

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

(use-package jdecomp
  :ensure t
  :mode ((rx ".class" string-end) . jdecomp-mode)
  :custom (jdecomp-decompiler-paths '((cfr . "/usr/share/cfr/cfr.jar"))))

(use-package subword
  :diminish subword-mode
  :hook ((php-mode-hook rust-mode-hook java-mode-hook) . subword-mode))

(use-package php-mode
  :ensure t
  :commands run-php

  :custom
  (php-manual-path
   (expand-file-name "php_docs/php-chunked-xhtml" (xdg-cache-home)))

  :config
  (defun run-php ()
    (interactive)
    (pop-to-buffer (make-comint-in-buffer "php" "*php*" "php" nil "-a"))))

(use-package company-php
  :ensure t
  :demand t
  :after company php-mode
  :custom (ac-php-tags-path (expand-file-name "emacs/ac-php" (xdg-cache-home)))
  :init (add-to-list 'company-backends #'company-ac-php-backend))

(use-package php-eldoc
  :ensure t
  :hook (php-mode-hook . php-eldoc-enable))

(use-package lua-mode :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom (yas-wrap-around-region t)
  :hook (after-init-hook . yas-global-mode)

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

(use-package lisp
  :hook (after-save-hook . check-parens-in-prog-mode)
  :init (provide 'lisp)

  :config
  (defun check-parens-in-prog-mode ()
    (when (derived-mode-p 'prog-mode)
      (check-parens))))

(use-package youtube-dl
  :quelpa
  (youtube-dl :repo "skeeto/youtube-dl-emacs" :fetcher github :version original)

  :bind (:map mode-specific-map ("o y" . youtube-dl-list))

  :custom
  (youtube-dl-arguments nil)
  (youtube-dl-program "ytdly"))

(use-package transmission
  :ensure t

  :bind
  (:map mode-specific-map
        ("o r" . transmission))
  (:map transmission-mode-map
        ("M" . transmission-move)))

(use-package dumb-jump
  :ensure t
  :custom (dumb-jump-selector 'ivy)

  :bind
  ("C-M-g" . dumb-jump-go)
  ("C-M-m" . dumb-jump-back)
  ("C-x 4 C-M-g" . dumb-jump-go-other-window))

(use-package free-keys :ensure t)

(use-package eww :custom (eww-search-prefix "https://ddg.co/lite/?q="))

(use-package multitran
  :ensure t
  :custom (multitran-languages '("Russian" . "English"))
  :bind (:map mode-specific-map ("o t" . multitran)))

(use-package restclient
  :ensure t
  :mode ((rx ".http" string-end) . restclient-mode))

(use-package company-restclient
  :ensure t
  :after company
  :init (add-to-list 'company-backends #'company-restclient))

(use-package calendar :custom (calendar-week-start-day 1))

(use-package csv-mode :ensure t)

(use-package ansi-color
  :commands ansi-color-apply-on-region ansi-color-apply ansi-color-filter-apply

  :hook
  (shell-mode-hook         . ansi-color-for-comint-mode-on)
  (compilation-filter-hook . colorize-compilation)

  :config
  (defun colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point)))))

(use-package grep
  :commands grep-read-regexp grep-read-files grep-expand-template@cut
  :bind (:map search-map ("G" . rgrep))

  :config
  (add-to-list 'grep-files-aliases '("php" . "*.php *.phtml"))

  (define-advice grep-expand-template (:filter-return (cmd) cut)
    (concat cmd " | cut -c-500")))

(use-package nix-mode :ensure t)

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :custom (wgrep-auto-save-buffer t))

(use-package mingus
  :ensure t

  :commands
  mingus-playlistp
  mingus-get-absolute-filename
  mingus-dired-file@dired-jump

  :bind
  (:map mode-specific-map ("o s" . mingus))

  (:map dired-mode-map
        ("SPC"   . mingus-dired-add)
        ("S-SPC" . mingus-dired-add-and-play))

  :custom
  (mingus-mode-line-separator  "|")
  (mingus-mode-line-string-max 100)
  (mingus-mpd-config-file (expand-file-name "mpd/mpd.conf" (xdg-config-home)))
  (mingus-seek-amount 5)
  (mingus-use-mouse-p nil)

  :config
  (define-advice mingus-dired-file (:override () dired-jump)
    (interactive)
    (dired-jump nil (if (mingus-playlistp)
                        mingus-mpd-playlist-dir
                      (mingus-get-absolute-filename))))

  (define-advice mingus-git-out (:override (&optional x) kill)
    (interactive)
    (when (mingus-buffer-p)
      (kill-current-buffer))))

(use-package ede/base
  :custom
  (ede-project-placeholder-cache-file
   (expand-file-name "emacs/ede/projects.el" (xdg-cache-home))))

(use-package gamegrid
  :custom (gamegrid-user-score-file-directory
           (expand-file-name "emacs/games/" (xdg-cache-home))))

(use-package arduino-mode :ensure t)

(use-package json-mode :ensure t)

(use-package autorevert :custom (auto-revert-remote-files t))

(use-package time :custom (display-time-24hr-format t))

(use-package url-util
  :commands url-get-url-at-point encode-url-entities decode-url-entities

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

(use-package url
  :custom
  (url-configuration-directory
   (expand-file-name "emacs/url/" (xdg-cache-home)))

  :config
  (defun insert-image-from-url (&optional url)
    (interactive)
    (unless url (setq url (url-get-url-at-point)))
    (unless url
      (error "Couldn't find URL."))
    (let ((buffer (url-retrieve-synchronously url)))
      (unwind-protect
          (let ((data (with-current-buffer buffer
                        (goto-char (point-min))
                        (search-forward "\n\n")
                        (buffer-substring (point) (point-max)))))
            (insert-image (create-image data nil t)))
        (kill-buffer buffer)))))

(use-package prog-mode :hook (after-init-hook . global-prettify-symbols-mode))

(use-package elf-mode
  :quelpa (elf-mode :repo "abo-abo/elf-mode" :fetcher github :version original)
  :hook (after-init-hook . elf-setup-default))

(use-package savehist
  :custom (savehist-file (expand-file-name "emacs/savehist" (xdg-data-home)))

  :hook
  (after-init-hook    . savehist-mode)
  (savehist-save-hook . savehist-filter-file-name-history)

  :config
  (defun savehist-filter-file-name-history ()
    (cl-delete-if-not (lambda (e) (or (tramp-tramp-file-p e) (file-exists-p e)))
                      file-name-history)
    (cl-delete-duplicates file-name-history
                          :test (lambda (a b) (string-equal
                                          (string-trim-left a "/")
                                          (string-trim-left b "/"))))))
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

(use-package scheme :custom (scheme-program-name "guile"))

(use-package geiser :ensure t)

(use-package sxhkd-mode
  :quelpa (sxhkd-mode :repo "xFA25E/sxhkd-mode" :fetcher github :version original)
  :mode (rx "sxhkdrc" string-end))

(use-package conf-mode
  :hook (after-save-hook . xresources-reload)

  :config
  (defun xresources-reload ()
    (interactive)
    (when (and (derived-mode-p 'conf-xdefaults-mode)
               (yes-or-no-p "Reload xresources?"))
      (let ((xres (expand-file-name "X11/xresources" (xdg-config-home))))
        (shell-command (format "xrdb -load %s; runel remote reload" xres))))))

(use-package mwheel :config (mouse-wheel-mode -1))

(use-package remember
  :commands remember-notes-maybe

  :bind
  (:map remember-notes-mode-map
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

(use-package help :bind-keymap ("C-x h" . help-map))

(use-package edit-indirect
  :ensure t
  :functions edit-indirect-guess-mode
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
       (let ((mode (completing-read "Mode: " '("sql" "html") nil t)))
         (cond ((string-equal mode "sql")
                (sql-mode))
               ((string-equal mode "html")
                (html-mode)))))
      (t (normal-mode)))))

(use-package web-beautify :ensure t)

(use-package emmet-mode
  :ensure t
  :diminish emmet-mode

  :hook
  (nxml-mode-hook  . emmet-mode)
  (html-mode-hook  . emmet-mode)
  (mhtml-mode-hook . emmet-mode)
  (web-mode-hook   . emmet-mode)

  :custom
  (emmet-preview-default t)
  (emmet-self-closing-tag-style ""))

(use-package nov
  :ensure t
  :mode ((rx ".epub" string-end) . nov-mode)

  :custom
  (nov-save-place-file
   (expand-file-name "emacs/nov-places" (xdg-cache-home))))

(use-package byte-compile
  :hook (after-save-hook . byte-recompile-current-file)

  :config
  (defun byte-recompile-current-file ()
    (interactive)
    (when (derived-mode-p 'emacs-lisp-mode)
      (byte-recompile-file (buffer-file-name)))))

(use-package fb2-mode
  :quelpa (fb2-mode :repo "5k1m1/fb2-mode" :fetcher github :version original)
  :custom (fb2-replace-hard-space t))

(use-package sql
  :commands sql-send-region
  :custom (sql-mysql-options '("-A")))

(use-package sql-indent
  :ensure t
  :hook (sql-mode-hook . sqlind-minor-mode))

(use-package sqlup-mode
  :ensure t
  :hook sql-mode-hook sql-interactive-mode-hook)

(use-package find-func
  :custom
  (find-function-C-source-directory
   (expand-file-name "programs/emacs-26.3/src" (xdg-download-dir))))

(use-package find-dired
  :commands find-dired-grep-ignore
  :bind (:map search-map ("f F" . find-dired-grep-ignore))
  :custom (find-ls-option '("| xargs -0 ls -labdi --si" . "-labdi --si"))

  :config
  (defun find-dired-grep-ignore (dir args)
    (interactive (list (read-directory-name "Run find in directory: " nil "" t)
                       (read-string "Run find (with args): ")))

    (find-dired
     dir
     (cl-flet ((entries (entries type f)
                        (mapconcat
                         (lambda (e) (shell-quote-argument (funcall f e)))
                         (cdr entries)
                         (concat " -o " type " "))))
       (concat
        (when-let ((entries grep-find-ignored-directories))
          (concat "-type d \\( -path "
                  (entries entries "-path" (lambda (d) (concat "*/" d)))
                  " \\) -prune -o "))
        (when-let ((entries grep-find-ignored-files))
          (concat "\\! -type d \\( -name "
                  (entries entries "-name" #'identity)
                  " \\) -prune -o "))

        "\\( "
        (if (and args (not (string-empty-p args)))
            args
          "-name \\* -o -name .\\[\\!.\\]\\* -o -name ..\\?\\*")
        " \\) -print0")))))

(use-package fd-dired
  :ensure t
  :bind (:map search-map ("f D" . fd-dired))
  :custom (fd-dired-ls-option '("| xargs -0 ls -labdi --si" . "-labdi --si")))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :hook (after-init-hook . flycheck-checkbashisms-setup)

  :custom
  (flycheck-checkbashisms-newline t)
  (flycheck-checkbashisms-posix t))

(use-package so-long
  :quelpa
  (so-long
   :url
   "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/so-long.el"
   :fetcher url
   :version original)

  :hook (after-init-hook . global-so-long-mode))

(use-package lisp-mode :config (put 'use-package #'lisp-indent-function 1))

(use-package shell-pwd
  :quelpa (shell-pwd :repo "xFA25E/shell-pwd" :fetcher github :version original)
  :commands shell-pwd-generate-buffer-name shell-pwd-shorten-directory)

(use-package shell-synopsis
  :quelpa
  (shell-synopsis :repo "xFA25E/shell-synopsis"
                  :fetcher github
                  :version original)

  :hook (shell-mode-hook . shell-synopsis-setup))

(use-package imenu-anywhere
  :ensure t
  :bind (:map goto-map ("I" . ivy-imenu-anywhere)))

(use-package whitespace
  :diminish whitespace-mode
  :custom (whitespace-style '(face lines-tail))

  :hook
  (before-save-hook . whitespace-cleanup)
  (prog-mode-hook . whitespace-mode))

(use-package make-mode :hook (makefile-mode-hook . indent-tabs-mode))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :bind (:map aggressive-indent-mode-map ("C-c C-q" . nil))

  :hook
  (c-mode-hook          . aggressive-indent-mode)
  (emacs-lisp-mode-hook . aggressive-indent-mode)
  (lisp-mode-hook       . aggressive-indent-mode)
  (scheme-mode-hook     . aggressive-indent-mode)
  (clojure-mode-hook    . aggressive-indent-mode)
  (sgml-mode-hook       . aggressive-indent-mode)
  (rust-mode-hook       . aggressive-indent-mode)
  (sh-mode-hook         . aggressive-indent-mode))

(use-package pcomplete-declare
  :quelpa
  (pcomplete-declare :repo "xFA25E/pcomplete-declare"
    :fetcher github
    :version original))

(use-package activity-log
  :quelpa
  (activity-log :repo "xFA25E/activity-log" :fetcher github :version original))

(use-package pp
  :bind
  (:map emacs-lisp-mode-map
        ("C-c m" . pp-macroexpand-last-sexp)
        ("C-c M" . emacs-lisp-macroexpand))
  (:map lisp-interaction-mode-map
        ("C-c m" . pp-macroexpand-last-sexp)
        ("C-c M" . emacs-lisp-macroexpand)))

(use-package sly
  :ensure t
  :custom (inferior-lisp-program "sbcl"))

(use-package sly-quicklisp :ensure t)

(use-package sly-asdf
  :ensure t
  :after sly
  :config (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sgml-mode
  :custom (sgml-basic-offset 2)

  :bind
  (:map sgml-mode-map
        ("C-M-n" . sgml-skip-tag-forward)
        ("C-M-p" . sgml-skip-tag-backward)
        ("C-c C-r" . sgml-namify-char)))

(use-package rg
  :ensure t
  :custom (rg-executable "rg")

  :bind
  (:map search-map ("R" . rg-menu))
  (:map rg-mode-map
        ("C-n" . next-line)
        ("C-p" . previous-line)
        ("{" . rg-prev-file)
        ("M-{" . rg-prev-file)
        ("}" . rg-next-file)
        ("M-}" . rg-next-file)))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :custom (hl-paren-colors '("red"))

  :hook
  (lisp-mode-hook       . highlight-parentheses-mode)
  (emacs-lisp-mode-hook . highlight-parentheses-mode)
  (scheme-mode-hook     . highlight-parentheses-mode))

(use-package lisp-extra-font-lock
  :ensure t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook) . lisp-extra-font-lock-mode))

(use-package request
  :custom
  (request-storage-directory
   (expand-file-name "emacs/request/" (xdg-cache-home))))

(use-package shr-tag-pre-highlight
  :ensure t
  :after shr

  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package nxml-mode :custom (nxml-child-indent 2))

(use-package web-mode
  :ensure t
  :custom (web-mode-markup-indent-offset 2))

(use-package ange-ftp :custom (ange-ftp-netrc-filename "~/.authinfo.gpg"))

(use-package plantuml-mode
  :ensure t
  :mode (rx ".puml" string-end)
  :hook (completion-at-point-functions . plantuml-completion-at-point)

  :custom
  (plantuml-jar-path "/opt/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 4)

  :bind
  (:map plantuml-mode-map
        ("C-c C-p" . plantuml-complete-symbol)
        ("C-c C-o" . plantuml-set-output-type))

  :config
  (defun plantuml-completion-at-point ()
    "Function used for `completion-at-point-functions' in `plantuml-mode'."
    (when (derived-mode-p 'plantuml-mode)
      (let ((completion-ignore-case t) ; Not working for company-capf.
            (bounds (bounds-of-thing-at-point 'symbol))
            (keywords plantuml-kwdList))
        (when (and bounds keywords)
          (list (car bounds)
                (cdr bounds)
                keywords
                :exclusve 'no
                :company-docsig #'identity))))))

(use-package hl-line
  :hook
  (dired-mode-hook         . hl-line-mode)
  (csv-mode-hook           . hl-line-mode)
  (grep-mode-hook          . hl-line-mode)
  (ivy-occur-mode-hook     . hl-line-mode)
  (mingus-browse-hook      . hl-line-mode)
  (mingus-playlist-hooks   . hl-line-mode)
  (transmission-mode       . hl-line-mode)
  (transmission-files-mode . hl-line-mode)
  (transmission-peers-mode . hl-line-mode))

(use-package try-complete-file-name-with-env
  :quelpa
  (try-complete-file-name-with-env
   :repo "xFA25E/try-complete-file-name-with-env"
   :fetcher github
   :version original)

  :demand t
  :after hippie-exp)

(use-package bash-completion
  :ensure t

  :init
  (dolist (func '(pcomplete/xargs
                  pcomplete/find
                  pcomplete/bzip2
                  pcomplete/chgrp
                  pcomplete/chown
                  pcomplete/gdb
                  pcomplete/kill
                  pcomplete/gzip
                  pcomplete/make
                  pcomplete/mount
                  pcomplete/rm
                  pcomplete/rmdir
                  pcomplete/time
                  pcomplete/umount
                  pcomplete/which))
    (fmakunbound func)))

(use-package bash-completion
  :ensure t
  :after shell

  :init
  (when (not (memq 'bash-completion-dynamic-complete
                   shell-dynamic-complete-functions))
    (let ((rest (memq 'pcomplete-completions-at-point
                      shell-dynamic-complete-functions)))
      (setcdr rest (cons 'bash-completion-dynamic-complete (cdr rest))))))

(use-package frame :config (define-advice suspend-frame (:override ()) nil))

(use-package org-mime :ensure t)

(use-package vc-hooks :custom (vc-handled-backends nil))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit)

  :custom
  (magit-credential-cache-daemon-socket
   (expand-file-name "git/credential/socket" (xdg-cache-home))))

(use-package transient
  :custom
  (transient-history-file
   (expand-file-name "emacs/transient/history.el" (xdg-cache-home)))
  (transient-levels-file
   (expand-file-name "emacs/transient/levels.el" (xdg-cache-home)))
  (transient-values-file
   (expand-file-name "emacs/transient/values.el" (xdg-cache-home))))

(use-package apache-mode :ensure t)

(use-package robots-txt-mode :ensure t)

(use-package company-try-hard
  :ensure t
  :after company
  :bind (:map company-mode-map ("M-Z" . company-try-hard)))

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package diff-hl
  :ensure t

  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (prog-mode-hook          . diff-hl-mode)
  (org-mode-hook           . diff-hl-mode)
  (dired-mode-hook         . diff-hl-dired-mode))

(use-package gitconfig-mode :ensure t)

(use-package gitignore-mode :ensure t)

(use-package misc :bind ("C-M-z" . zap-up-to-char))

(use-package shr
  :custom
  (shr-external-browser #'browse-url-firefox)
  (shr-max-image-proportion 0.7)
  (shr-width (current-fill-column)))

(use-package finder :bind (:map help-map ("M-c" . finder-commentary)))

(use-package xml
  :commands
  xml-parse-string
  xml-escape-string
  decode-sgml-entities
  encode-sgml-entities

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

(use-package ediff
  :hook (ediff-before-setup-hook . save-window-configuration-to-w))

(use-package saveplace
  :hook (after-init-hook . save-place-mode)

  :custom
  (save-place-file (expand-file-name "emacs/saveplace" (xdg-data-home)))
  (save-place-forget-unreadable-files t))

(use-package smartparens
  :ensure t

  :bind
  ("C-M-u"   . sp-backward-up-sexp)
  ("C-M-d"   . sp-down-sexp)
  ("M-F"     . sp-forward-symbol)
  ("M-B"     . sp-backward-symbol)
  ("C-)"     . sp-forward-slurp-sexp)
  ("C-M-)"   . sp-forward-barf-sexp)
  ("C-("     . sp-backward-slurp-sexp)
  ("C-M-("   . sp-backward-barf-sexp)
  ("C-M-t"   . sp-transpose-sexp)
  ("C-M-k"   . sp-kill-sexp)
  ("C-k"     . sp-kill-hybrid-sexp)
  ("C-M-w"   . sp-copy-sexp)
  ("C-w"     . sp-backward-kill-word-or-region)
  ("M-["     . sp-unwrap-sexp)
  ("M-]"     . sp-rewrap-sexp)

  :config
  (defun sp-backward-kill-word-or-region (&optional count)
    (interactive "p")
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (sp-backward-kill-word count)))

  (require 'smartparens-config))

(use-package newst-backend
  :hook (newsticker-new-item-functions . newsticker-add-youtube-description)

  :custom
  (newsticker-retrieval-interval 0)
  (newsticker-retrieval-method 'extern)
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old nil)
  (newsticker-dir (expand-file-name "emacs/newsticker" (xdg-cache-home)))
  (newsticker-url-list-defaults nil)
  (newsticker-url-list
   '(("Mastering Emacs" "http://www.masteringemacs.org/feed")
     ("Rust Lang" "https://blog.rust-lang.org/feed.xml")
     ("WargarW" "https://queryfeed.net/tw?q=%40WargarW")
     ("Marie Cachet" "https://www.youtube.com/feeds/videos.xml?channel_id=UCLM6JlQkQ9tltBL9Bipci9g")
     ("Justus Walker" "https://www.youtube.com/feeds/videos.xml?user=senttosiberia")
     ("Luke Smith Blog" "https://lukesmith.xyz/rss.xml")
     ("Tsoding" "https://www.youtube.com/feeds/videos.xml?channel_id=UCEbYhDd6c6vngsF5PQpFVWg")
     ("Protesilaos Stavrou" "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g")
     ("young scrolls" "https://www.youtube.com/feeds/videos.xml?channel_id=UCXzmcdy3BVCnLqwOx2JERMg")
     ("uebermarginal" "https://www.youtube.com/feeds/videos.xml?channel_id=UCJ10M7ftQN7ylM6NaPiEB6w")
     ("Atlanta Functional Programming" "https://www.youtube.com/feeds/videos.xml?channel_id=UCYg6qFXDE5SGT_YXhuJPU0A")
     ("Борис Цацулин" "https://www.youtube.com/feeds/videos.xml?user=CaveMansTech")
     ("Ben Eater" "https://www.youtube.com/feeds/videos.xml?user=eaterbc")
     ("itpedia" "https://www.youtube.com/feeds/videos.xml?user=itpediachannel")
     ("Алексей Шевцов" "https://www.youtube.com/feeds/videos.xml?channel_id=UCM7-8EfoIv0T9cCI4FhHbKQ")
     ("Izzy Laif" "https://www.youtube.com/feeds/videos.xml?user=EasyyyLife")
     ("ТОПЛЕС" "https://www.youtube.com/feeds/videos.xml?user=toplesofficial")
     ("Kurzgesagt" "https://www.youtube.com/feeds/videos.xml?user=Kurzgesagt")
     ("FlynnFlyTaggart" "https://www.youtube.com/feeds/videos.xml?user=FlynnFlyTaggart")
     ("Простые Мысли" "https://www.youtube.com/feeds/videos.xml?channel_id=UCZuRMfF5ZUHqYlKkvU12xvg")
     ("EmacsCast" "https://www.youtube.com/feeds/videos.xml?channel_id=UCEfFUaIkjbI06PhALdcXNVA")
     ("Primitive Technology" "https://www.youtube.com/feeds/videos.xml?channel_id=UCAL3JXZSzSm8AlZyD3nQdBA")
     ("Epic History" "https://www.youtube.com/feeds/videos.xml?channel_id=UCvPXiKxH-eH9xq-80vpgmKQ")
     ("Luke Smith" "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA")
     ("PewDiePie" "https://www.youtube.com/feeds/videos.xml?user=PewDiePie")))

  :config
  (defun newsticker-add-youtube-description (_feedname item)
    "Parse youtube description by `ITEM'."
    (when (and (not (newsticker--desc item))
               (string-match-p (rx "youtube.com") (newsticker--link item)))
      (let* ((group (alist-get 'group (newsticker--extra item)))
             (thumbnail (alist-get 'url (car (alist-get 'thumbnail group))))
             (description (cadr (alist-get 'description group))))
        (setcar (nthcdr 1 item)
                (format "<img src=\"%s\"/><br/><pre>%s</pre>"
                        thumbnail
                        (with-temp-buffer
                          (insert description)
                          (fill-region (point-min) (point-max))
                          (buffer-string))))))))

(use-package newst-treeview
  :bind
  (:map mode-specific-map ("o n" . newsticker-show-news))
  (:map newsticker-treeview-mode-map
        ("d" . newsticker-treeview-youtube-dl)
        ("r" . newsticker-treeview-show-duration)
        ("m" . newsticker-treeview-play-in-mpvi)
        ("c" . newsticker-treeview-copy-link)
        ("B" . newsticker-treeview-open-in-browser))

  :custom
  (newsticker-treeview-automatically-mark-displayed-items-as-old nil)
  (newsticker-treeview-treewindow-width 40)
  (newsticker--treeview-list-sort-order 'sort-by-time-reverse)

  :config
  (defun newsticker-treeview-open-in-browser ()
    (interactive)
    (select-window (newsticker--treeview-item-window) t)
    (eww (newsticker--link (newsticker--treeview-get-selected-item))))

  (defun newsticker-treeview-copy-link ()
    (interactive)
    (let ((link (newsticker--link (newsticker--treeview-get-selected-item))))
      (kill-new link)
      (message "Copied %s" link)))

  (defun newsticker-treeview-play-in-mpvi ()
    (interactive)
    (let* ((item (newsticker--treeview-get-selected-item))
           (title (newsticker--title item))
           (link (newsticker--link item)))
      (message "Starting \"%s\" in mpvi" title)
      (start-process "mpvi" nil "mpvi" link)))

  (defun newsticker-treeview-youtube-dl ()
    (interactive)
    (let* ((item (newsticker--treeview-get-selected-item))
           (title (newsticker--title item))
           (link (newsticker--link item)))
      (if (youtube-dl link :title title)
          (message "Downloading \"%s\"" title)
        (message "Not youtube item"))))

  (defun newsticker-treeview-show-duration ()
    (interactive)
    (let* ((item (newsticker--treeview-get-selected-item))
           (title (newsticker--title item))
           (link (newsticker--link item))
           (duration-buffer "*youtube-duration*"))
      (message "\"%s\" duration: ..." title)
      (set-process-sentinel
       (start-process "youtube-duration" duration-buffer
                      youtube-dl-program "--no-color" "--get-duration" link)
       `(lambda (process _change)
          (when (eq 0 (process-exit-status process))
            (with-current-buffer ,duration-buffer
              (message "\"%s\" duration: %s"
                       ,title (string-trim (buffer-string)))
              (kill-buffer))))))))

(use-package bruh
  :after browse-url
  :quelpa (bruh :repo "a13/bruh" :fetcher github)

  :custom
  (bruh-default-browser #'eww-browse-url)
  (browse-url-browser-function #'bruh-browse-url))

(use-package projectile
  :ensure t
  :bind-keymap ("M-m" . projectile-command-map)

  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-mode-line-prefix "")
  (projectile-cache-file
   (expand-file-name "emacs/projectile/cache" (xdg-cache-home)))
  (projectile-known-projects-file
   (expand-file-name "emacs/projectile/projects" (xdg-cache-home)))

  :config
  (define-advice projectile-default-mode-line
      (:filter-return (project-name) remove-empty)
    (if (string-equal project-name "[-]") "" (concat " " project-name))))

(use-package counsel-projectile
  :ensure t
  :hook (after-init-hook . counsel-projectile-mode))

(use-package lsp-java
  :ensure t
  :hook (java-mode-hook . lsp)

  :custom
  (lsp-java-workspace-dir
   (expand-file-name "emacs/workspace/" (xdg-cache-home)))
  (lsp-java-server-install-dir
   (expand-file-name "emacs/eclipse.jdt.ls/server/" (xdg-cache-home))))


(use-package sdcv :ensure t)

;; end
