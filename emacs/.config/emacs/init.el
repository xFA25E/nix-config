;; -*- lexical-binding: t; -*-

;;; PACKAGE INIT

(defvar package-user-dir
  (eval-when-compile
    (expand-file-name
     "emacs/elpa"
     (let ((env (getenv "XDG_CACHE_HOME")))
       (if (or (null env) (not (file-name-absolute-p env)))
           (expand-file-name "~/.cache")
         env)))))

(defvar nsm-settings-file
  (eval-when-compile
    (expand-file-name
     "emacs/network-security.data"
     (let ((env (getenv "XDG_CACHE_HOME")))
       (if (or (null env) (not (file-name-absolute-p env)))
           (expand-file-name "~/.cache")
         env)))))

(defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(defvar package-archives
  '(("gnu"   . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org"   . "https://orgmode.org/elpa/")))

(defvar leaf-expand-minimally t)
(defvar leaf-key-bindlist nil)

(package-initialize)
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))


;;; UTILS

(leaf xdg
  :commands xdg-download-dir xdg-music-dir xdg-data-home xdg-cache-home
  :config
  (defun xdg-download-dir () (xdg--dir-home "XDG_DOWNLOAD_DIR" "~/Downloads"))
  (defun xdg-music-dir () (xdg--dir-home "XDG_MUSIC_DIR" "~/Music")))

(leaf diminish :package t)

(leaf quelpa
  :package t
  :custom
  `(quelpa-build-dir . ,(expand-file-name "emacs/quelpa/build" (xdg-cache-home)))
  `(quelpa-dir . ,(expand-file-name "emacs/quelpa" (xdg-cache-home)))
  '(quelpa-update-melpa-p . nil))

(leaf rx :config (rx-define ext (&rest exts) (and "." (or exts) string-end)))

(leaf subr
  :commands
  add-to-list
  alist-get
  derived-mode-p
  error
  replace-regexp-in-string
  shell-quote-argument
  start-process
  with-current-buffer
  :preface (provide 'subr)
  :advice
  (:override yes-or-no-p y-or-n-p)
  (:override suspend-frame ignore))

(leaf subr-x :commands thread-last)

(leaf bindings :preface (provide 'bindings))


;;; SETTINGS

(leaf emacs
  :setq-default '(truncate-lines . t)
  :custom
  '(create-lockfiles . nil)
  '(cursor-in-non-selected-windows . nil)
  '(enable-recursive-minibuffers . t)
  '(history-delete-duplicates . t)
  '(history-length . 1000)
  '(hscroll-step . 1)
  '(indent-tabs-mode . nil)
  '(indicate-buffer-boundaries . 'left)
  '(indicate-empty-lines . t)
  '(next-screen-context-lines . 10)
  '(tab-width . 4)
  '(undo-limit . 200000)
  '(undo-outer-limit . 20000000)
  '(undo-strong-limit . 300000)
  '(use-dialog-box . nil)
  '(visible-bell . nil)
  '(x-gtk-use-system-tooltips . nil)
  '(x-stretch-cursor . t)
  '(fill-column . 80)
  '(scroll-step . 1)
  '(scroll-conservatively . 10000)
  `(kill-buffer-query-functions
    . ,(remq #'process-kill-buffer-query-function kill-buffer-query-functions))
  '(user-full-name . "Valeriy Litkovskyy")
  `(read-process-output-max . ,(* 1024 1024))
  '(completion-ignore-case . t)
  '(read-buffer-completion-ignore-case . t))


;;;; FACES

(leaf mb-depth :hook (after-init-hook . minibuffer-depth-indicate-mode))

(leaf so-long :hook (after-init-hook . global-so-long-mode))

(leaf rainbow-mode :package t)

(leaf paren
  :custom '(show-paren-style . 'parentheses)
  :hook (after-init-hook . show-paren-mode))

(leaf hl-line
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

(leaf diff-hl
  :package t
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (dired-mode-hook . diff-hl-dired-mode)
  ((prog-mode-hook org-mode-hook) . diff-hl-mode))

(leaf ansi-color
  :hook
  (shell-mode-hook . ansi-color-for-comint-mode-on)
  (compilation-filter-hook . colorize-compilation)
  :config
  (defun colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point)))))

(leaf form-feed
  :package t
  :hook ((emacs-lisp-mode-hook scheme-mode-hook lisp-mode-hook) . form-feed-mode)
  :config (diminish 'form-feed-mode))


;;;;; THEMES

(leaf custom
  ;; :init (load-theme 'leuven t)
  :commands load-theme custom-theme-enabled-p)

(leaf acme-theme
  ;; :init (load-theme 'acme t)
  :package t)

(leaf modus-operandi-theme
  :init (load-theme 'modus-operandi t)
  :package t)

(leaf faces
  :bind (help-map :package help ("M-f" . list-faces-display))

  :config
  (set-face-attribute 'default nil :family "Iosevka" :height 170)
  (set-face-attribute 'mode-line nil :family "DejaVu Sans" :height 110)
  (set-face-attribute 'mode-line-inactive nil :family "DejaVu Sans" :height 110)
  (set-face-attribute 'fixed-pitch-serif nil :family "DejaVu Serif")
  (set-face-attribute 'header-line nil :inverse-video nil :family "Iosevka")

  (with-eval-after-load 'man
    (set-face-attribute 'Man-overstrike nil :inherit 'font-lock-variable-name-face :bold t)
    (set-face-attribute 'Man-underline nil :inherit 'font-lock-negation-char-face :underline t))

  (with-eval-after-load 'telega
    (set-face-attribute 'telega-webpage-fixed nil :family "Terminus")
    (set-face-attribute 'telega-entity-type-pre nil :family "Terminus")
    (set-face-attribute 'telega-entity-type-code nil :family "Terminus"))

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

(leaf outline
  :hook (emacs-lisp-mode-hook . outline-minor-mode)
  :config
  (diminish 'outline-minor-mode)

  (defun outline-show-after-jump ()
    (when outline-minor-mode
      (outline-show-entry)))

  (with-eval-after-load 'xref
    (add-hook 'xref-after-jump-hook #'outline-show-after-jump))

  (with-eval-after-load 'imenu
    (add-hook 'imenu-after-jump-hook #'outline-show-after-jump)))

(leaf outline-minor-faces
  :package t
  :after outline
  :hook (outline-minor-mode-hook . outline-minor-faces-add-font-lock-keywords))

(leaf hideshow
  :hook (emacs-lisp-mode-hook . hs-minor-mode)
  :bind (hs-minor-mode-map ("<C-M-tab>" . hs-toggle-hiding))
  :config (diminish 'hs-minor-mode))

(leaf bicycle
  :package t
  :bind
  (outline-minor-mode-map
   :package outline
   ("<C-tab>" . bicycle-cycle)
   ("<backtab>" . bicycle-cycle-global))
  (hs-minor-mode-map
   :package hideshow
   ("<C-tab>" . bicycle-cycle)
   ("<backtab>" . bicycle-cycle-global)
   ("<C-M-tab>" . hs-toggle-hiding)))


;;;; AUTH

(leaf auth-source
  :custom '(auth-sources . '("~/.authinfo.gpg" "~/.netrc" "~/.authinfo")))

(leaf auth-source-pass
  :custom
  `(auth-source-pass-filename
    . ,(or (getenv "PASSWORD_STORE_DIR") (expand-file-name "pass" (xdg-data-home)))))


;;;; URL

(leaf browse-url
  :custom '(browse-url-secondary-browser-function . #'browse-url-firefox)
  :bind (ctl-x-map :package subr ("B" . browse-url)))

(leaf bruh
  :defvar bruh-videos-re
  :preface
  (unless (package-installed-p 'bruh)
    (quelpa '(bruh :repo "a13/bruh" :fetcher github)))
  :after browse-url
  :custom
  '(bruh-images-browser-function . 'browse-url-find-file)
  '(bruh-default-browser . 'eww-browse-url)
  '(bruh-videos-browser-function . 'bruh-mpvi-ytdli-or-browse)
  '(browse-url-browser-function . 'bruh-browse-url)

  :defer-config
  (defun browse-url-find-file (url &optional _)
    (find-file url))
  (defun bruh-mpvi-ytdli-or-browse (url &rest rest)
    (let ((actions '("mpv" "ytdl" "default"))
          (youtube-id
           (save-match-data
             (string-match
              (rx (or (and "youtube.com/watch?" (*? any) "v=") "youtu.be/")
                  (group (= 11 (any "-_A-Za-z0-9"))))
              url)
             (match-string 1 url))))
      (when youtube-id
        (push "ytel" actions))
      (pcase (completing-read (format "Bruh %s: " url) actions nil t)
        ("ytel"
         (ytel-show (vector youtube-id)))
        ("mpv"
         (let ((process-environment (browse-url-process-environment)))
           (start-process (concat "mpv " url) nil "mpvi" url)))
        ("ytdl"
         (let ((process-environment (browse-url-process-environment)))
           (start-process (concat "ytdl " url) nil "ytdli" url)))
        ("default"
         (apply bruh-default-browser url rest)))))

  (dolist (re `(,(rx bos "http" (? "s") "://" (? "www.") "bitchute.com/" (or "embed" "video" "channel"))
                ,(rx bos "http" (? "s") "://videos.lukesmith.xyz/" (or "static/webseed" "videos/watch"))
                ,(rx  "http" (? "s") "://m.youtube.com")))
    (add-to-list 'bruh-videos-re re)))

(leaf url
  :custom
  `(url-configuration-directory . ,(expand-file-name "emacs/url/" (xdg-cache-home))))

(leaf url-handlers :hook (after-init-hook . url-handler-mode))

(leaf url-util
  :commands url-encode-entities url-decode-entities
  :config
  (defun url-decode-entities (beg end)
    (interactive "r")
    (let ((text (url-unhex-string (buffer-substring beg end))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text))))

  (defun url-encode-entities (beg end)
    (interactive "r")
    (let ((text (url-encode-url (buffer-substring beg end))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))


;;;; CACHE

(leaf savehist
  :custom `(savehist-file . ,(expand-file-name "emacs/savehist" (xdg-cache-home)))
  :hook
  (after-init-hook    . savehist-mode)
  (savehist-save-hook . savehist-filter-file-name-history)
  :config
  (defun savehist-filter-file-name-history ()
    (setq
     file-name-history
     (cl-delete-if-not
      (lambda (file-name)
        (and (not (string-empty-p file-name))
             (or (file-remote-p file-name)
                 (string-match-p (rx bos "http") file-name)
                 (file-exists-p file-name))))
      (cl-delete-duplicates
       (mapcar
        (lambda (s) (string-trim-right (expand-file-name s) (rx (+ "/"))))
        file-name-history)
       :test #'string-equal)))))

(leaf saveplace
  :hook (after-init-hook . save-place-mode)
  :custom
  `(save-place-file . ,(expand-file-name "emacs/saveplace" (xdg-cache-home)))
  '(save-place-forget-unreadable-files . t)
  '(save-place-limit . 1000)
  :config
  (setq save-place-skip-check-regexp
        (rx (or (regexp save-place-skip-check-regexp)
                (and bos "http")))))


;;;; FILES

(leaf autorevert
  :custom
  '(auto-revert-remote-files . t)
  '(auto-revert-avoid-polling . t))

(leaf files
  :commands read-directory-name custom-backup-enable-predicate

  :bind
  ("M-~" . nil)
  ("C-S-x C-S-c" . save-buffers-kill-emacs)
  (ctl-x-map :package subr ("R" . revert-buffer-no-confirm))

  :custom
  '(backup-by-copying . t)
  '(delete-old-versions . t)
  '(kept-new-versions . 10)
  '(kept-old-versions . 2)
  '(require-final-newline . nil)
  '(version-control . t)
  '(backup-enable-predicate . #'custom-backup-enable-predicate)
  ;; '(remote-file-name-inhibit-cache . nil) ; "Speed up tramp, caution!"
  `(auto-save-file-name-transforms
    . ',`((,(rx (* any)) ,(expand-file-name "emacs/auto-saves/" (xdg-cache-home)) t)))
  `(backup-directory-alist
    . ',`((,(rx (* any)) . ,(expand-file-name "emacs/backups" (xdg-data-home)))))

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

(leaf shr
  :defvar shr-external-rendering-functions
  :custom
  '(shr-use-fonts . nil)
  '(shr-use-colors . nil)
  '(shr-max-image-proportion . 0.7)
  '(shr-image-animate . nil)
  `(shr-width . ,(current-fill-column)))

(leaf shr-tag-pre-highlight
  :package t
  :after shr
  :commands shr-tag-pre-highlight
  :leaf-defer nil
  :config (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))


;;;; CLIPBOARD

(leaf select
  :custom
  '(selection-coding-system . 'utf-8)
  '(select-enable-clipboard . t))

(leaf clipmon
  :package t
  :hook (after-init-hook . clipmon-mode))


;;;; OTHER

(leaf startup
  :preface (provide 'startup)
  :custom
  `(auto-save-list-file-prefix
    . ,(expand-file-name (format-time-string "emacs/auto-saves/list/%y-%m-%d-")
                         (xdg-cache-home)))
  '(inhibit-startup-echo-area-message . t)
  '(inhibit-startup-screen . t)
  '(initial-scratch-message . nil))

(leaf window
  :preface (provide 'window)
  :bind
  ("M-V" . scroll-down-line)
  ("C-S-v" . scroll-up-line)
  ("C-M-S-b" . previous-buffer)
  ("C-M-S-f" . next-buffer)
  ("M-Q" . quit-window)
  (ctl-x-map :package subr ("C-b" . switch-to-buffer)))

(leaf time :custom '(display-time-24hr-format . t))

(leaf novice :custom '(disabled-command-function . nil))

(leaf cus-edit :custom '(custom-file . null-device))

(leaf uniquify :custom `(uniquify-ignore-buffers-re . ,(rx bol "*")))

(leaf mule
  :defer-config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment "UTF-8"))

(leaf gamegrid
  :custom
  `(gamegrid-user-score-file-directory
    . ,(expand-file-name "emacs/games/" (xdg-cache-home))))

(leaf ede/base
  :custom
  `(ede-project-placeholder-cache-file
    . ,(expand-file-name "emacs/ede/projects.el" (xdg-cache-home))))

(leaf async
  :package t
  :after bytecomp
  :config (async-bytecomp-package-mode))

(leaf bytecomp
  :defun byte-recompile-current-file-setup
  :hook (emacs-lisp-mode-hook . byte-recompile-current-file-setup)
  :config
  (defun byte-recompile-current-file-setup ()
    (add-hook 'after-save-hook 'byte-recompile-current-file nil t))

  (defun byte-recompile-current-file ()
    (interactive)
    (byte-recompile-file (buffer-file-name))))

(leaf transient
  :custom
  `(transient-history-file
    . ,(expand-file-name "emacs/transient/history.el" (xdg-cache-home)))
  `(transient-levels-file
    . ,(expand-file-name "emacs/transient/levels.el" (xdg-cache-home)))
  `(transient-values-file
    . ,(expand-file-name "emacs/transient/values.el" (xdg-cache-home))))

(leaf gcmh
  :package t
  :hook (emacs-startup-hook . gcmh-mode)
  :config (diminish 'gcmh-mode))


;;; REMOTE

(leaf ange-ftp :custom '(ange-ftp-netrc-filename . "~/.authinfo.gpg"))

(leaf tramp
  :custom
  `(tramp-persistency-file-name
    . ,(expand-file-name "emacs/tramp/connection-history" (xdg-cache-home)))
  '(tramp-default-method . "ssh")
  '(tramp-histfile-override . t)
  '(tramp-completion-reread-directory-timeout . nil)
  :defer-config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "~/.local/bin"))

(leaf sudo-edit
  :package t
  :hook
  (after-init-hook . sudo-edit-indicator-mode)
  (shell-mode-hook . sudo-edit-set-header))


;;; MAN

(leaf man
  :custom '(Man-notify-method . 'aggressive)
  :bind (help-map :package help ("M-m" . man)))

(leaf apropos :custom '(apropos-sort-by-scores . t))

(leaf finder
  :defun finder-exit-with-package
  :advice (:override finder-exit finder-exit-with-package)
  :bind (help-map :package help ("M-c" . finder-commentary))
  :config
  (defun finder-exit-with-package ()
    (interactive)
    (if (string-match-p (rx "*Finder" (? "-package") "*") (buffer-name))
        (quit-window t)
      (dolist (buf '("*Finder*" "*Finder-package*"))
        (when (get-buffer buf)
          (kill-buffer buf))))))


;;; DIRED

(leaf dired
  :defun dired-copy-filename-as-kill-join-newline
  :commands dired-get-marked-files
  :advice (:override dired-copy-filename-as-kill dired-copy-filename-as-kill-join-newline)

  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-before-readin-hook . dired-setup-switches)

  :custom
  '(dired-dwim-target . t)
  '(dired-listing-switches . "-alDF --si --group-directories-first")
  '(dired-ls-F-marks-symlinks . t)

  :bind
  (dired-mode-map
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

  (defun dired-copy-filename-as-kill-join-newline (&optional arg)
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

(leaf dired-x
  :defun dired-get-marker-char dired-mark-extension
  :after dired
  :hook (dired-mode-hook . dired-omit-mode)

  :bind
  (ctl-x-map :package subr ("C-j" . dired-jump))
  (dired-mode-map
   :package dired
   ("* i" . dired-mark-images)
   ("* v" . dired-mark-videos))

  :custom
  `(dired-guess-shell-alist-user
    . ',(list
         (list (rx (ext "csv" "doc" "docx" "xlsx" "xls" "odt" "ods" "odp" "ppt" "pptx"))
               "setsid -f libreoffice * >/dev/null 2>&1"
               "libreoffice --invisible --headless --convert-to pdf * &")

         (list (rx (ext "jpeg" "jpg" "gif" "png" "bmp" "tif" "thm" "nef" "jfif" "webp" "xpm"))
               "setsid -f sxiv * >/dev/null 2>&1"
               "setsid -f gimp * >/dev/null 2>&1")

         (list (rx (ext "ai" "eps"))
               "setsid -f inkscape * >/dev/null 2>&1"
               "setsid -f gimp * >/dev/null 2>&1")

         (list (rx (ext "fb2" "djvu"))
               "ebook-convert ? .epub &")

         (list (rx (ext "pdf"))
               "setsid -f libreoffice * >/dev/null 2>&1"
               "setsid -f gimp * >/dev/null 2>&1")

         (list (rx (ext "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp"
                        "vob" "wmv" "aiff" "wav"))
               "setsid -f mpv --force-window=yes --no-terminal * >/dev/null 2>&1"
               "video_duration * | format_duration"
               "video_duration * | awk '{s+=$1}END{print s}' | format_duration"
               "compress_video * &"
               "strip_video * &"
               "mpv -vo=drm")

         (list (rx (ext "cue"))
               "setsid -f mpv --force-window=yes --no-terminal * >/dev/null 2>&1")

         (list (rx (ext "rar"))
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

(leaf dired-aux
  :after dired
  :bind (dired-mode-map :package dired ("b" . dired-stat))
  :custom '(dired-create-destination-dirs . 'ask)
  :config
  (add-to-list
   'dired-compress-file-suffixes
   `(,(rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -"))

  (defun dired-stat ()
    (interactive)
    (dired-do-shell-command "stat" current-prefix-arg
                            (dired-get-marked-files t current-prefix-arg))))

(leaf wdired
  :after dired
  ;; does not work as expected
  :hook (wdired-mode-hook . disable-image-dired)
  :config (defun disable-image-dired () (image-dired-minor-mode -1)))

(leaf image-dired
  :after dired
  :hook (dired-mode-hook . image-dired-minor-mode)
  :custom
  '(image-dired-external-viewer . "sxiv")
  `(image-dired-db-file
    . ,(expand-file-name "emacs/image-dired/db" (xdg-cache-home)))
  `(image-dired-dir
    . ,(expand-file-name "emacs/image-dired/thumbnails/" (xdg-cache-home)))
  `(image-dired-gallery-dir
    . ,(expand-file-name "emacs/image-dired/gallery/" (xdg-cache-home)))
  `(image-dired-temp-image-file
    . ,(expand-file-name "emacs/image-dired/temp" (xdg-cache-home)))
  `(image-dired-temp-rotate-image-file
    . ,(expand-file-name "emacs/image-dired/rotate_temp" (xdg-cache-home))))

(leaf dired-rsync
  :after dired
  :package t
  :bind (dired-mode-map :package dired ("r" . dired-rsync)))

(leaf dired-git-info
  :package t
  :after dired
  :bind (dired-mode-map :package dired (")" . dired-git-info-mode)))

(leaf dired-async
  :package async
  :after dired
  :config
  (dired-async-mode)
  (diminish 'dired-async-mode))


;;;; FIND

(leaf find-dired
  :bind (search-map :package bindings ("f f" . find-dired))
  :custom '(find-ls-option . '("-print0 | sort -z | xargs -0 ls -ldF --si --quoting-style=literal" . "-ldhF")))

(leaf fd-dired
  :package t
  :custom '(fd-dired-ls-option . '("| sort -z | xargs -0 ls -ldF --si --quoting-style=literal" . "-ldhF"))
  :bind (search-map :package bindings ("f d" . fd-dired) ("f D" . fd-dired-list-searches)))

(leaf locate
  :defvar locate-command
  :custom '(locate-make-command-line . 'locate-make-ignore-case-command-line)
  :bind (search-map :package bindings ("f l" . locate))
  :config
  (defun locate-make-ignore-case-command-line (search-string)
    (list locate-command "-i" search-string)))


;;; EDITING

(leaf simple
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
  ([remap newline] . newline-and-indent)
  (ctl-x-map
   :package subr
   ("K"   . kill-current-buffer)
   ("C-r" . overwrite-mode))
  (mode-specific-map :package bindings ("o P" . list-processes))

  :custom
  '(completion-show-help . nil)
  '(shift-select-mode . nil)
  '(kill-do-not-save-duplicates . t)
  '(kill-read-only-ok . t)
  '(async-shell-command-buffer . 'new-buffer)
  '(async-shell-command-display-buffer . nil)

  :config
  (defun just-one-space-fast (&optional n)
    (interactive "*p")
    (cycle-spacing n nil 'fast))

  (defun copy-whole-line ()
    (interactive)
    (save-excursion
      (kill-new (thing-at-point 'line)))))

(leaf register
  :commands save-window-configuration-to-w
  :custom '(register-separator . ?\n)
  :bind
  (ctl-x-r-map
   :package bindings
   ("C-@" . nil)
   ("C-SPC" . nil)
   ("g" . nil)
   ("x" . nil)
   ("v" . view-register)
   ("L" . list-registers)
   ("p" . prepend-to-register)
   ("a" . append-to-register))
  :config
  (set-register register-separator "\n")
  (defun save-window-configuration-to-w (&rest _ignore)
    (window-configuration-to-register ?w)))

(leaf subword :hook ((php-mode-hook rust-mode-hook java-mode-hook) . subword-mode))

(leaf edit-indirect
  :defun edit-indirect-guess-mode
  :package t
  :custom '(edit-indirect-guess-mode-function . #'edit-indirect-guess-mode)
  :bind (ctl-x-map :package subr ("E" . edit-indirect-region-or-at-point))
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


(leaf paragraphs :bind ("C-M-S-t" . transpose-paragraphs))

;;;; FORMATTING

(leaf whitespace :hook (before-save-hook . whitespace-cleanup))

(leaf format-all :package t)


;;;; INPUT METHOD

(leaf cyrillic-dvorak-im
  :preface
  (unless (package-installed-p 'cyrillic-dvorak-im)
    (quelpa '(cyrillic-dvorak-im :repo "xFA25E/cyrillic-dvorak-im" :fetcher github)))
  :require t)

(leaf reverse-im
  :package t
  :after cyrillic-dvorak-im
  :require t
  :config (reverse-im-activate "cyrillic-dvorak"))


;;;; PAIRS

(leaf elec-pair :hook (after-init-hook . electric-pair-mode))

(leaf smartparens
  :defun sp-kill-region sp-backward-kill-word
  :package t
  ;; :preface
  ;; (unless (package-installed-p 'smartparens)
  ;;   (quelpa '(smartparens :url "https://git.sr.ht/~sokolov/granpa" :fetcher git)))

  :bind
  ("C-M-u" . sp-backward-up-sexp)
  ("C-M-d" . sp-down-sexp)
  ("M-F" . sp-forward-symbol)
  ("M-B" . sp-backward-symbol)
  ("C-)" . sp-forward-slurp-sexp)
  ("C-M-)" . sp-forward-barf-sexp)
  ("C-(" . sp-backward-slurp-sexp)
  ("C-M-(" . sp-backward-barf-sexp)
  ("C-M-t" . sp-transpose-sexp)
  ("C-M-k" . sp-kill-sexp)
  ("C-M-w" . sp-copy-sexp)
  ("M-d" . sp-kill-word)
  ("C-w" . sp-backward-kill-word-or-region)
  ("M-[" . sp-unwrap-sexp)
  ("M-]" . sp-rewrap-sexp)

  :config
  (defun sp-backward-kill-word-or-region (&optional count)
    (interactive "p")
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (sp-backward-kill-word count)))

  (require 'smartparens-config))


;;;; CONF

(leaf ledger-mode
  :package t
  :custom '(ledger-default-date-format . "%Y-%m-%d"))

(leaf conf-mode
  :hook (conf-xdefaults-mode-hook . xresources-reload-setup)
  :config
  (defun xresources-reload-setup ()
    (add-hook 'after-save-hook 'xresources-reload nil t))

  (defun xresources-reload ()
    (interactive)
    (when (yes-or-no-p "Reload xresources?")
      (let ((xres (expand-file-name "X11/xresources" (xdg-config-home))))
        (shell-command (format "xrdb -load %s" xres))))))

(leaf csv-mode :package t)

(leaf tex-mode
  :defvar ispell-parser
  :hook (tex-mode-hook . setup-tex-mode-ispell-parser)
  :config
  (defun setup-tex-mode-ispell-parser ()
    (setq-local ispell-parser 'tex)))


;;;;; WEB

(leaf css-mode :defvar css-mode-map :bind (css-mode-map ("C-c m" . css-lookup-symbol)))

(leaf json-mode :package t)

(leaf apache-mode :package t)

(leaf robots-txt-mode :package t)

(leaf restclient :package t)


;;;;; GIT

(leaf gitconfig-mode :package t)

(leaf gitignore-mode :package t)


;;;;; XML-LIKE

(leaf sgml-mode
  :defvar sgml-mode-map
  :custom '(sgml-basic-offset . 4)
  :bind
  (sgml-mode-map
   ("C-M-n" . sgml-skip-tag-forward)
   ("C-M-p" . sgml-skip-tag-backward)
   ("C-c C-r" . sgml-namify-char)))

(leaf nxml-mode :custom '(nxml-child-indent . 4))

(leaf emmet-mode
  :package t
  :hook ((nxml-mode-hook html-mode-hook mhtml-mode-hook web-mode-hook) . emmet-mode)
  :custom
  '(emmet-preview-default . t)
  '(emmet-self-closing-tag-style . "")
  :config (diminish 'emmet-mode))


;;;; PROG

(leaf nix-mode :package t)

(leaf cc-mode :custom '(c-default-style . '((java-mode . "java") (other . "awk"))))

(leaf rust-mode
  :package t
  :custom '(rust-format-on-save . t))


;;;;; SHELL

(leaf sh-script :custom '(system-uses-terminfo . nil))

(leaf executable
  :custom '(executable-chmod . 64)
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))


;;;;; SQL

(leaf sql-indent
  :package t
  :hook (sql-mode-hook . sqlind-minor-mode))

(leaf sqlup-mode
  :package t
  :hook sql-mode-hook)


;;;;; WEB

(leaf php-mode
  :package t
  :custom '(php-mode-coding-style . 'php))

(leaf web-mode
  :package t
  :mode "\\.twig\\'"
  :custom '(web-mode-markup-indent-offset . 4))


;;;;; LSP

(leaf eglot
  :package t
  :custom
  '(eglot-autoshutdown . t)
  '(eglot-confirm-server-initiated-edits . nil)
  '(eglot-sync-connect . nil))


;;;;; LISP

(leaf lisp
  :preface (provide 'lisp)
  :commands kill-sexp
  :hook (prog-mode-hook . check-parens-setup)
  :config
  (defun check-parens-setup ()
    (add-hook 'after-save-hook 'check-parens nil t)))


;;;;;; ELITE LISP

(leaf elisp-mode
  :bind ("C-x C-S-e" . eval-and-replace)
  :custom
  '(eval-expression-print-level . t)
  '(eval-expression-print-length . t)
  :config
  (defun eval-and-replace ()
    (interactive)
    (kill-sexp -1)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))

(leaf ipretty
  :package t
  :bind ([remap eval-print-last-sexp] . ipretty-last-sexp))

(leaf pp
  :bind
  (emacs-lisp-mode-map
   :package elisp-mode
   ("C-c m" . pp-macroexpand-last-sexp)
   ("C-c M" . emacs-lisp-macroexpand))
  (lisp-interaction-mode-map
   :package elisp-mode
   ("C-c m" . pp-macroexpand-last-sexp)
   ("C-c M" . emacs-lisp-macroexpand)))


;;;;;; COMMON LISP (AKA BORSHCH)

(leaf inf-lisp :custom '(inferior-lisp-program . "sbcl"))

(leaf sly
  :package t
  :custom
  '(sly-default-lisp . 'sbcl)
  '(sly-lisp-implementations . '((sbcl ("sbcl"))))
  `(sly-mrepl-history-file-name
    . ,(expand-file-name "emacs/sly-mrepl-history" (xdg-cache-home))))

(leaf sly-quicklisp :package t)

(leaf sly-asdf
  :package t
  :after sly
  :config (add-to-list 'sly-contribs 'sly-asdf 'append))


;;;;;; SCHEME

(leaf scheme :custom '(scheme-program-name . "guile"))

(leaf geiser
  :package t
  :custom
  `(geiser-repl-history-filename
    . ,(expand-file-name "geiser/history" (xdg-cache-home))))


;;;;;; CLOJURE

(leaf clojure-mode :package t)

(leaf cider :package t)


;;; CORRECTNESS

(leaf ispell
  :custom
  '(ispell-program-name . "aspell")
  '(ispell-extra-args . '("--sug-mode=ultra")))

(leaf flymake :custom '(flymake-no-changes-timeout . nil))

(leaf flycheck
  :defvar flycheck-shellcheck-supported-shells
  :package t
  :custom
  '(flycheck-mode-line-prefix . "FC")
  '(flycheck-clang-pedantic-errors . t)
  '(flycheck-clang-pedantic . t)
  '(flycheck-gcc-pedantic-errors . t)
  '(flycheck-gcc-pedantic . t)
  '(flycheck-phpcs-standard . "PSR12,PSR1,PSR2")
  '(flycheck-check-syntax-automatically . '(save mode-enabled))
  :defer-config
  (add-to-list 'flycheck-shellcheck-supported-shells 'dash))

(leaf flycheck-checkbashisms
  :package t
  :after flycheck
  :custom
  '(flycheck-checkbashisms-newline . t)
  '(flycheck-checkbashisms-posix . t)
  :init (flycheck-checkbashisms-setup))


;;; COMPLETION

(leaf bash-completion
  :package t
  :after shell
  :hook (shell-dynamic-complete-functions . bash-completion-dynamic-complete))


;;;; MINIBUFFER

(leaf minibuffer
  :bind (completion-in-region-mode-map ("M-v" . switch-to-completions))
  :custom
  '(completion-styles . '(substring partial-completion))
  '(completion-category-overrides . '((bookmark (styles basic))))
  '(read-file-name-completion-ignore-case . t)
  '(completion-pcm-complete-word-inserts-delimiters . t))

(leaf consult
  :package t
  :bind
  ("M-y" . consult-yank-replace)
  ("M-X" . consult-mode-command)
  ("M-H" . consult-history)
  (kmacro-keymap :package kmacro ("c" . consult-kmacro))
  (ctl-x-map :package subr ("F" . consult-file-externally))
  (project-prefix-map :package project ("i" . consult-project-imenu))
  (goto-map
   :package bindings
   ("o" . consult-outline)
   ("i" . consult-imenu)
   ("E" . consult-error)))

(leaf marginalia
  :package t
  :hook (after-init-hook . marginalia-mode)
  :custom '(marginalia-annotators . '(marginalia-annotators-heavy marginalia-annotators-light))
  :advice (:before-until marginalia-annotate-file marginalia-candidate-file-remote-p)
  :config
  (defun marginalia-candidate-file-remote-p (cand)
    (file-remote-p (marginalia--full-candidate cand))))

(leaf embark
  :package t
  :custom '(embark-occur-initial-view-alist . '((t . zebra)))
  :bind
  ("C-," . embark-act)
  ;; (embark-occur-mode-map ("," . embark-act))
  (completion-list-mode-map :package simple ("," . embark-act))
  (minibuffer-local-completion-map
   :package minibuffer
   ("C-," . embark-act)
   ("C-." . embark-act-noexit)))

(leaf orderless
  :package t
  :custom
  `(orderless-component-separator . ,(rx (+ space)))
  '(orderless-matching-styles
    . '(orderless-literal orderless-prefixes orderless-regexp))
  '(completion-styles . '(orderless partial-completion))
  :bind (minibuffer-local-completion-map :package minibuffer ("SPC" . nil)))

(leaf eldoc :defer-config (diminish 'eldoc-mode))

(leaf minibuf-eldef
  :custom '(minibuffer-eldef-shorten-default . t)
  :hook (after-init-hook . minibuffer-electric-default-mode))

(leaf map-ynp
  :preface (provide 'map-ynp)
  :custom '(read-answer-short . t))

(leaf insert-char-preview
  :package t
  :bind ([remap insert-char] . insert-char-preview))


;;;; HIPPIE-EXP

(leaf hippie-exp
  :defvar he-search-string he-tried-table he-expand-list
  :defun
  try-complete-file-name-with-env try-complete-file-name-partially-with-env
  he-init-string he-file-name-beg he-string-member he-reset-string
  he-concat-directory-file-name he-substitute-string
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom '(he-file-name-chars . "-a-zA-Z0-9_/.,~^#$+={}")
  :advice
  (:override try-complete-file-name try-complete-file-name-with-env)
  (:override try-complete-file-name-partially try-complete-file-name-partially-with-env)

  :config
  (defun try-complete-file-name-with-env (old)
    (unless old
      (he-init-string (he-file-name-beg) (point))
      (let ((name-part (file-name-nondirectory he-search-string))
            (dir-part (substitute-in-file-name
                       (expand-file-name (or (file-name-directory he-search-string) "")))))
        (unless (he-string-member name-part he-tried-table)
          (setq he-tried-table (cons name-part he-tried-table)))
        (if (and (not (equal he-search-string "")) (file-directory-p dir-part))
            (setq he-expand-list (sort (file-name-all-completions name-part dir-part) 'string-lessp))
          (setq he-expand-list ()))))

    (while (and he-expand-list (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if he-expand-list
        (let ((filename (he-concat-directory-file-name
                         (file-name-directory he-search-string)
                         (car he-expand-list))))
          (he-substitute-string filename)
          (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
          (setq he-expand-list (cdr he-expand-list))
          t)
      (when old (he-reset-string))
      nil))

  (defun try-complete-file-name-partially-with-env (old)
    (let ((expansion ()))
      (unless old
        (he-init-string (he-file-name-beg) (point))
        (let ((name-part (file-name-nondirectory he-search-string))
              (dir-part (substitute-in-file-name
                         (expand-file-name (or (file-name-directory he-search-string) "")))))
          (when (and (not (equal he-search-string "")) (file-directory-p dir-part))
            (setq expansion (file-name-completion name-part dir-part)))
          (when (or (eq expansion t) (string= expansion name-part) (he-string-member expansion he-tried-table))
            (setq expansion ()))))

      (if expansion
          (let ((filename (he-concat-directory-file-name (file-name-directory he-search-string) expansion)))
            (he-substitute-string filename)
            (setq he-tried-table (cons expansion (cdr he-tried-table)))
            t)
        (when old (he-reset-string))
        nil))))


;;; SEARCHING

(leaf isearch
  :bind (isearch-mode-map ("C-h" . isearch-delete-char))
  :custom
  '(isearch-allow-scroll . t)
  '(isearch-lazy-count . t)
  '(search-whitespace-regexp . ".*?")
  :config
  (define-key isearch-mode-map (kbd "C-?") isearch-help-map))

(leaf grep
  :defun grep-expand-template-add-cut
  :defvar grep-files-aliases
  :advice (:filter-return grep-expand-template grep-expand-template-add-cut)
  :bind (search-map :package bindings ("g" . rgrep))
  :config
  (add-to-list 'grep-files-aliases '("php" . "*.php *.phtml"))
  (defun grep-expand-template-add-cut (cmd)
    (concat cmd " | cut -c-500")))

(leaf wgrep
  :package t
  :custom '(wgrep-auto-save-buffer . t))

(leaf rg
  :defvar rg-mode-map
  :package t
  :custom '(rg-executable . "rg")

  :bind
  (search-map
   :package bindings
   ("r r" . rg)
   ("r ." . rg-dwim)
   ("r l" . rg-list-searches)
   ("r t" . rg-literal)
   ("r p" . rg-project)
   ("r k" . rg-kill-saved-searches)
   ("r s" . rg-save-search-as-name))
  (rg-mode-map
   ("C-n" . next-line)
   ("C-p" . previous-line)
   ("{" . rg-prev-file)
   ("M-{" . rg-prev-file)
   ("}" . rg-next-file)
   ("M-}" . rg-next-file)))


;;; JUMPING


;;;; ON BUFFER

(leaf avy
  :package t
  :bind
  ("M-z" . avy-goto-word-0)
  (goto-map
   :package bindings
   ("M-g" . avy-goto-line)
   ("g"   . nil)
   ("n"   . nil)
   ("p"   . nil))
  :custom
  '(avy-background . t)
  `(avy-goto-word-0-regexp . ,(rx symbol-start (or (syntax word) (syntax symbol))))
  '(avy-style . 'words)
  `(avy-keys . ',(string-to-list "aoeuhtns")))

(leaf ace-link
  :package t
  :hook (after-init-hook . ace-link-setup-default)
  :bind (goto-map :package bindings ("l" . ace-link)))


;;;; TO DEFINITION

(leaf dumb-jump
  :package t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(leaf imenu
  :custom
  '(imenu-auto-rescan . t)
  '(imenu-use-popup-menu . nil)
  '(imenu-space-replacement . " ")
  '(imenu-level-separator . "/"))

(leaf find-func
  :bind (search-map :package bindings ("f b" . find-library))
  :custom
  `(find-function-C-source-directory
    . ,(expand-file-name "programs/emacs-27.1/src" (xdg-download-dir))))


;;; COMPILATION

(leaf compile
  :custom
  '(compilation-always-kill . t)
  '(compilation-scroll-output . 'first-error)
  :bind (ctl-x-map :package subr ("c" . compile)))

;; Add support for cargo error --> file:line:col
(leaf cargo
  :package t
  :hook (rust-mode-hook . cargo-minor-mode)
  :custom
  '(cargo-process--command-build . "build --color never")
  '(cargo-process--command-check . "check --color never")
  '(cargo-process--command-clippy . "clippy --color never")
  '(cargo-process--command-current-file-tests . "test --color never")
  '(cargo-process--command-current-test . "test --color never")
  '(cargo-process--command-rm . "rm --color never")
  '(cargo-process--command-run . "run --color never")
  '(cargo-process--command-test . "test --color never"))


;;; REPL

(leaf comint
  :preface (defvar-local comint-history-filter-function nil)
  :defun comint-filter-input-ring
  :advice (:before comint-write-input-ring comint-filter-input-ring)

  :hook
  (kill-buffer-hook . comint-write-input-ring)
  (kill-emacs-hook . save-buffers-comint-input-ring)
  (comint-output-filter-functions . comint-strip-ctrl-m)
  (comint-output-filter-functions . comint-truncate-buffer)

  :custom
  '(comint-input-ignoredups . t)
  '(comint-input-ring-size . 10000)
  '(comint-buffer-maximum-size . 10240)

  :config
  (defun save-buffers-comint-input-ring ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf (comint-write-input-ring))))

  (defun comint-filter-input-ring (&rest _)
    (let ((fn comint-history-filter-function))
      (when (and fn comint-input-ring (not (ring-empty-p comint-input-ring)))
        (thread-last comint-input-ring
          ring-elements
          (funcall fn)
          ring-convert-sequence-to-ring
          (setq-local comint-input-ring))))))

(leaf sql
  :defvar sql-interactive-product sql-input-ring-file-name
  :hook (sql-interactive-mode-hook . sql-interactive-set-history)
  :custom
  '(sql-mysql-options . '("-A"))
  '(sql-sqlite-options . '("-column" "-header" "-cmd" "PRAGMA foreign_keys = ON;"))
  :config
  (defun sql-interactive-set-history ()
    (let ((file (expand-file-name
                 (format "emacs/sqli/%s_history" sql-interactive-product)
                 (xdg-cache-home))))
      (make-directory (file-name-directory file) t)
      (write-region "" nil file t)
      (setq sql-input-ring-file-name file))))


;;;; SHELL

(leaf shell
  :bind (shell-mode-map ("C-c M-d" . shell-change-directory))

  :custom
  `(shell-prompt-pattern
    . ,(rx line-start
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
                         (or "awk" "bash" "cat" "cd" "chmod" "chown" "command"
                             "cp" "cut" "dash" "dd" "df" "dh" "du" "ebook-convert"
                             "echo" "em" "emacs" "env" "exit" "export" "fd" "feh"
                             "file" "find" "gawk" "gparted" "grep" "gzip"
                             "hash" "host" "htop" "id" "ln" "locate" "ls" "man"
                             "mbsync" "millisleep" "mkdir" "mpop" "mpv" "mv"
                             "notify-send" "pacman -Rsn" "pacman -S" "ping" "pkill"
                             "printf" "pwgen" "python" "quit" "read" "rg" "rimer"
                             "rm" "rmdir" "rofi" "setsid" "sh" "sleep" "stow"
                             "strings" "strip" "studies_" "sxiv" "tail" "time"
                             "timer" "top" "touch" "tr" "uname" "uptime" "watch"
                             "wc" "which" "woof" "xclip" "xz" "yay" "youtube-dl"
                             "ytdl"))
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
      (insert (concat "cd " (shell-quote-argument (expand-file-name dir)))))
    (comint-send-input)))

(leaf shell-pwd
  :defun shell-pwd-generate-buffer-name
  :preface
  (unless (package-installed-p 'shell-pwd)
    (quelpa '(shell-pwd :repo "xFA25E/shell-pwd" :fetcher github)))
  :bind (mode-specific-map :package bindings ("x s" . shell-pwd-switch-to-buffer))
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

(leaf autoinsert :hook (after-init-hook . auto-insert-mode))

(leaf skempo-mode
  :preface
  (unless (package-installed-p 'skempo-mode)
    (quelpa '(skempo-mode :repo "xFA25E/skempo-mode" :fetcher github)))
  :hook ((emacs-lisp-mode-hook lisp-mode-hook) . skempo-mode)

  :bind
  (skempo-mode-map
   ("C-z" . skempo-mode-complete-tag-or-call-on-region)
   ("M-g M-e" . tempo-forward-mark)
   ("M-g M-a" . tempo-backward-mark))

  :config
  (defun skempo-mode-elisp-namespace ()
    (string-trim-right (buffer-name) (rx ".el" eos)))

  (defun skempo-mode-elisp-group ()
    (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))

  (skempo-mode-define-templates lisp-mode
    ("defvar" :tempo "(defvar " p n> r> n> "\"" p "\")")
    ("defun" :tempo "(defun " p " (" p ")" n> "\"" p "\"" n> r> ")")
    ("lambda" :tempo "(lambda (" p ") " n> r> ")")
    ("let" :tempo "(let ((" p "))" n> r> ")"))

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

(leaf vlf :package t)

(leaf ediff
  :custom '(ediff-window-setup-function . 'ediff-setup-windows-plain)
  :hook (ediff-before-setup-hook . save-window-configuration-to-w))

(leaf net-utils
  :bind
  (mode-specific-map
   :package bindings
   ("n a" . arp)
   ("n d" . dig)
   ("n h" . nslookup-host)
   ("n i" . ifconfig)
   ("n n" . netstat)
   ("n p" . ping)
   ("n p" . ping)
   ("n r" . route)
   ("n s" . smbclient)
   ("n t" . traceroute)
   ("n w" . iwconfig)))

(leaf readelf-mode
  :preface
  (unless (package-installed-p 'readelf-mode)
    (quelpa '(readelf-mode :repo "sirikid/readelf-mode" :fetcher github))))

(leaf calendar :custom '(calendar-week-start-day . 1))

(leaf ibuffer
  :custom '(ibuffer-default-sorting-mode . 'major-mode)
  :bind (ctl-x-map :package subr ("C-S-b" . ibuffer-jump)))

(leaf gdb-mi
  :custom
  '(gdb-many-windows . t)
  '(gdb-show-main . t))

(leaf proced
  :bind (mode-specific-map :package bindings ("o p" . proced))
  :custom '(proced-tree-flag . t))

(leaf bookmark
  :custom
  '(bookmark-save-flag . 1)
  `(bookmark-default-file . ,(expand-file-name "emacs/bookmarks" (xdg-data-home))))

(leaf telega
  :package t
  :bind (mode-specific-map :package bindings ("o l t" . telega))
  :hook (telega-load-hook . telega-notifications-mode)
  :custom
  `(telega-server-libs-prefix . ,(expand-file-name "~/.nix-profile"))
  `(telega-directory . ,(expand-file-name "emacs/telega" (xdg-cache-home)))
  '(telega-root-fill-column . 100)
  '(telega-chat-fill-column . 100)
  '(telega-webpage-fill-column . 100)
  :config (define-key mode-specific-map (kbd "o l") telega-prefix-map))

(leaf magit
  :package t
  :custom
  `(magit-credential-cache-daemon-socket
    . ,(expand-file-name "git/credential/socket" (xdg-cache-home))))

(leaf project
  :custom
  `(project-list-file
    . ,(expand-file-name "emacs/project.list" (xdg-cache-home))))

(leaf mediainfo-mode
  :custom
  '(mediainfo-mode-open-method
    . '("setsid" "-f" "mpv" "--force-window=yes" "--no-terminal" file-name))
  :preface
  (unless (package-installed-p 'mediainfo-mode)
    (quelpa '(mediainfo-mode :repo "xFA25E/mediainfo-mode" :fetcher github)))
  (add-to-list
   'auto-mode-alist
   `(,(rx (ext "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav"))
     . mediainfo-mode))
  (add-to-list
   'file-name-handler-alist
   `(,(rx (ext "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav"))
     . mediainfo-mode--file-handler)))


;;;; DICTIONARY

(leaf sdcv
  :package t
  :bind (mode-specific-map :package bindings ("o t" . sdcv-search-input)))

(leaf dictionary
  :package t
  :bind (mode-specific-map :package bindings ("o T" . dictionary-search)))


;;;; XML

(leaf eww
  :custom
  `(eww-bookmarks-directory . ,(expand-file-name "emacs" (xdg-data-home)))
  '(eww-browse-url-new-window-is-tab . nil)
  '(eww-search-prefix . "https://ddg.co/lite/?q="))

(leaf xml
  :defun xml-parse-string xml-escape-string
  :commands sgml-decode-entities sgml-encode-entities
  :config
  (defun sgml-decode-entities (beg end)
    (interactive "r")
    (save-excursion
      (narrow-to-region beg end)
      (goto-char beg)
      (xml-parse-string)
      (widen)))

  (defun sgml-encode-entities (beg end)
    (interactive "r")
    (let ((text (xml-escape-string (buffer-substring beg end))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert text)))))

(leaf htmlize :package t)


;;;; KEYS

(leaf which-key
  :package t
  :hook (after-init-hook . which-key-mode)
  :config (diminish 'which-key-mode))

(leaf free-keys :package t)


;;;; YO-HO

(leaf transmission
  :defvar transmission-mode-map
  :package t
  :bind
  (mode-specific-map :package bindings ("o r" . transmission))
  (transmission-mode-map ("M" . transmission-move)))

(leaf torrent-mode
  :preface
  (unless (package-installed-p 'torrent-mode)
    (quelpa '(torrent-mode :repo "xFA25E/torrent-mode" :fetcher github)))
  :mode "\\.torrent\\'")


;;;; YTEL

(leaf ytel
  :defvar ytel-mode-map
  :defun ytel-video-id ytel-get-current-video ytel-video-title
  :package t

  :custom
  '(ytel-instances
    . '("https://invidious.fdn.fr"
        "https://invidious.site"
        "https://invidious.kavin.rocks"
        "https://vid.encryptionin.space"
        "https://invidious.snopyta.org"
        "https://invidious.mservice.ru.com"
        "https://invidious.xyz"
        "https://vid.encryptionin.space"))
  '(ytel-invidious-api-url . "https://invidious.fdn.fr")

  :bind
  (mode-specific-map :package bindings ("o Y" . ytel))
  (ytel-mode-map
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
      (message "Copied %s" link))))

(leaf ytel-show
  :defvar ytel-show-comments--video-title
  :defun ytel-show--current-video-id
  :preface
  (unless (package-installed-p 'ytel-show)
    (quelpa '(ytel-show :repo "xFA25E/ytel-show" :fetcher github)))
  :after ytel
  :bind (ytel-mode-map :package ytel ("RET" . ytel-show)))


;;;; RSS

(leaf newst-backend
  :defun newsticker--link newsticker--extra newsticker--desc newsticker--title
  :hook (newsticker-new-item-functions . newsticker-add-thumbnail)

  :custom
  '(newsticker-retrieval-interval . 0)
  '(newsticker-retrieval-method . 'extern)
  '(newsticker-automatically-mark-items-as-old . nil)
  '(newsticker-automatically-mark-visited-items-as-old . nil)
  `(newsticker-dir . ,(expand-file-name "emacs/newsticker" (xdg-cache-home)))
  '(newsticker-url-list-defaults . nil)
  '(newsticker-url-list
    . '(("Alt-Hype" "https://www.bitchute.com/feeds/rss/channel/thealthype/")
        ("Sleepy Saxon" "https://www.youtube.com/feeds/videos.xml?channel_id=UCVyzFlPnWjqrgVljH8QUiCQ")
        ("Sean Last" "https://www.youtube.com/feeds/videos.xml?user=spawktalk")
        ("Knight's Move" "https://www.youtube.com/feeds/videos.xml?channel_id=UC63HcOlghFQ3pcursLUp3NQ")
        ("American Renaissance" "https://www.bitchute.com/feeds/rss/channel/amrenaissance/")
        ("TealDeer" "https://www.bitchute.com/feeds/rss/channel/tealdeer/")
        ("Luke Smith Blog" "https://lukesmith.xyz/rss.xml")
        ("Luke Smith PeerTube" "https://videos.lukesmith.xyz/feeds/videos.xml?accountId=3")
        ("Простая Академия" "https://www.youtube.com/feeds/videos.xml?channel_id=UC8mmPf2oKdfE2pdjqctTWUw")
        ("Простые Мысли" "https://www.youtube.com/feeds/videos.xml?channel_id=UCZuRMfF5ZUHqYlKkvU12xvg")
        ("Planet Emacslife" "https://planet.emacslife.com/atom.xml")))

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

(leaf newst-treeview
  :defvar newsticker-treeview-mode-map
  :defun newsticker--treeview-get-selected-item

  :bind
  (mode-specific-map :package bindings ("o n" . newsticker-show-news))
  (newsticker-treeview-mode-map
   ("r" . newsticker-treeview-show-duration)
   ("c" . newsticker-treeview-copy-link))

  :custom
  '(newsticker-treeview-automatically-mark-displayed-items-as-old . nil)
  '(newsticker-treeview-treewindow-width . 30)
  '(newsticker-treeview-listwindow-height . 6)
  '(newsticker--treeview-list-sort-order . 'sort-by-time-reverse)

  :config
  (defun newsticker-treeview-copy-link ()
    (interactive)
    (let ((link (newsticker--link (newsticker--treeview-get-selected-item))))
      (kill-new link)
      (message "Copied %s" link))))


;;;; MPD

(leaf mingus
  :defvar mpd-inter-conn
  :defun mingus-buffer-p mingus-git-out-and-kill mingus-add-files mingus-music-files
  :advice (:override mingus-git-out mingus-git-out-and-kill)
  :package t

  :bind
  (mode-specific-map
   :package bindings
   ("o s" . mingus)
   ("o S" . mingus-find-and-add-file))

  :custom
  '(mingus-mode-line-separator . "|")
  '(mingus-mode-line-string-max . 120)
  `(mingus-mpd-config-file . ,(expand-file-name "mpd/mpd.conf" (xdg-config-home)))
  '(mingus-seek-amount . 5)
  '(mingus-use-mouse-p . nil)

  :config
  (defun mingus-git-out-and-kill (&optional _)
    (interactive)
    (when (mingus-buffer-p)
      (kill-current-buffer)))

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
     (list (expand-file-name
            (completing-read "Add file to mpd: " (mingus-music-files) nil t)
            (xdg-music-dir))))
    (mpd-play mpd-inter-conn)
    (let ((buffer (get-buffer "*Mingus*")))
      (when (buffer-live-p (get-buffer buffer))
        (kill-buffer buffer)))))


;;;; E-READER

(leaf pdf-tools
  :package t
  :init (pdf-loader-install))

(leaf nov
  :package t
  :mode "\\.epub\\'"
  :custom `(nov-save-place-file . ,(expand-file-name "emacs/nov-places" (xdg-cache-home))))

(leaf fb2-mode
  :preface
  (unless (package-installed-p 'fb2-mode)
    (quelpa '(fb2-mode :repo "5k1m1/fb2-mode" :fetcher github)))
  :custom '(fb2-replace-hard-space . t))


;;; MAIL

(leaf message
  :defvar message-mode-map
  :commands message-send-mail-with-sendmail
  :custom
  '(message-kill-buffer-on-exit . t)
  '(message-send-mail-function . #'message-send-mail-with-sendmail)
  `(message-subject-re-regexp . ,(rx bol (* blank)
                                     (* (or "R" "RE" "Re" "Ris")
                                        (* "[" (* digit) "]")
                                        (? " ") ":"
                                        (* blank)))))

(leaf sendmail
  :custom
  '(sendmail-program . "msmtp")
  '(send-mail-function . #'message-send-mail-with-sendmail))


;;;; MU4E

(leaf mu4e
  :defun mu4e-action-view-in-browser-check-parens-fix mu4e-main-mode-map mu4e-view-actions
  :defvar mu4e-main-mode-map mu4e-view-actions
  :hook (after-init-hook . mu4e~start)

  :bind
  (mode-specific-map :package bindings ("o m" . mu4e))
  (mu4e-main-mode-map
   ("q" . quit-window)
   ("Q" . mu4e-quit))

  :custom
  '(mu4e-headers-visible-lines . 7)
  '(mu4e-sent-folder . "/SENT")
  '(mu4e-drafts-folder . "/DRAFTS")
  '(mu4e-trash-folder . "/TRASH")
  '(mu4e-refile-folder . "/ARCHIVE")
  '(mu4e-view-show-images . t)
  '(mu4e-sent-messages-behavior . 'sent)
  '(mu4e-change-filenames-when-moving . t)
  '(mu4e-context-policy . 'pick-first)
  '(mu4e-compose-context-policy . 'always-ask)
  '(mu4e-headers-date-format . "%d %b %a %R")
  '(mu4e-view-date-format . "%a %d %b %Y %T")
  '(mu4e-headers-time-format . "%16R")
  '(mu4e-view-show-addresses . t)
  `(mu4e-attachment-dir . ,(expand-file-name (xdg-download-dir)))
  '(mu4e-modeline-max-width . 100)
  '(mu4e-get-mail-command . "mailsync polimi")
  '(mu4e-update-interval . 300)
  '(mu4e-maildir-shortcuts
    . '(("/EXYS"    . ?e)
        ("/POLIMI"  . ?p)
        ("/SENT"    . ?s)
        ("/TRASH"   . ?t)
        ("/DRAFTS"  . ?d)
        ("/ARCHIVE" . ?a)))
  '(mu4e-headers-fields
    . '((:human-date . 16)
        (:flags      . 6)
        (:from       . 22)
        (:subject)))
  `(mu4e-view-attachment-assoc
    . ',(eval-when-compile
          (mapcan
           (lambda (args) (mapcar (lambda (ext) (cons ext (car args))) (cdr args)))
           '(("sxiv"        . ("jpeg" "jpg" "gif" "png" "bmp" "tif" "thm" "nef" "jfif" "webp"))
             ("libreoffice" . ("csv" "doc" "docx" "xlsx" "xls" "odt" "ods" "odp" "ppt" "pptx"))
             ("mpv"         . ("m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov"
                               "3gp" "vob"  "wmv" "aiff" "wav"))))))

  :config
  (load-file (expand-file-name "emacs/secrets/mu4e.el" (xdg-data-home)))
  (load-library "org-mu4e")
  (add-to-list 'mu4e-view-actions '("browser view" . mu4e-action-view-in-browser) t))

(leaf mu4e-alert
  :package t
  :after mu4e
  :config
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify))


;;; ORG

(leaf org
  :package org-plus-contrib
  :bind
  (mode-specific-map
   :package bindings
   ("G a" . org-agenda)
   ("G c" . org-capture))

  :custom
  '(org-src-tab-acts-natively . t)
  '(org-startup-folded . t)
  '(org-agenda-files . '("~/org/life.org"))
  '(org-log-into-drawer . t)
  '(org-log-reschedule . 'note)
  '(org-refile-use-outline-path . 'file)
  '(org-refile-allow-creating-parent-nodes . 'confirm)
  '(org-agenda-skip-additional-timestamps-same-entry . nil)
  '(org-refile-targets . '((org-agenda-files :level . 1)))
  `(org-id-locations-file
    . ,(expand-file-name "emacs/org/id-locations" (xdg-cache-home)))
  '(org-capture-templates
    . '(("r" "Remember" entry (file+headline "~/org/life.org" "Remember")
         "* TODO %?\n  SCHEDULED: %t\n")))

  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((calc       . t)
                                 (emacs-lisp . t)
                                 (sql        . t)
                                 (shell      . t))))

(leaf org-mime
  :defvar
  org-mime--saved-temp-window-config org-mime-src--beg-marker
  org-mime-src--end-marker org-mime-src--overlay org-mime-src--hint

  :defun
  org-mime-beautify-quoted-add-newlines org-mime-replace-images-fix-cids-and-path
  org-mime-mail-body-begin org-mime-mail-signature-begin
  org-mime-src--make-source-overlay org-mime-src-mode
  org-mime-edit-mail-in-org-mode-up-to-signature
  org-switch-to-buffer-other-window

  :advice
  (:filter-return org-mime-beautify-quoted org-mime-beautify-quoted-add-newlines)
  (:filter-args org-mime-replace-images org-mime-replace-images-fix-cids-and-path)
  (:override org-mime-edit-mail-in-org-mode org-mime-edit-mail-in-org-mode-up-to-signature)

  :package t
  :bind
  (message-mode-map
   :package message
   ("C-c M-o" . org-mime-htmlize)
   ("C-c M-e" . org-mime-edit-mail-in-org-mode)
   ("C-c M-t" . org-mime-revert-to-plain-text-mail))
  :config
  (defun org-mime-beautify-quoted-add-newlines (html)
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

  (defun org-mime-replace-images-fix-cids-and-path (args)
    (cl-destructuring-bind (first . rest) args
      (cons (replace-regexp-in-string "src=\"file:///" "src=\"/" first) rest)))

  (defun org-mime-edit-mail-in-org-mode-up-to-signature ()
    (interactive)
    ;; see `org-src--edit-element'
    (cond
     ((eq major-mode 'org-mode)
      (message "This command is not for `org-mode'."))
     (t
      (setq org-mime--saved-temp-window-config (current-window-configuration))
      (let* ((beg (copy-marker (org-mime-mail-body-begin)))
             (end (copy-marker (or (org-mime-mail-signature-begin)
                                   (point-max))))
             (bufname "OrgMimeMailBody")
             (buffer (generate-new-buffer bufname))
             (overlay (org-mime-src--make-source-overlay beg end))
             (text (buffer-substring-no-properties beg end)))

        (setq org-mime-src--beg-marker beg)
        (setq org-mime-src--end-marker end)
        ;; don't use local-variable because only user can't edit multiple emails
        ;; or multiple embedded org code in one mail
        (setq org-mime-src--overlay overlay)

        (save-excursion
          (delete-other-windows)
          (org-switch-to-buffer-other-window buffer)
          (erase-buffer)
          (insert org-mime-src--hint)
          (insert text)
          (goto-char (point-min))
          (org-mode)
          (org-mime-src-mode)))))))

(leaf ox-html
  :after org
  :custom
  '(org-html-htmlize-output-type . 'css)
  '(org-html-htmlize-font-prefix . "org-"))

(provide 'init)

;;; Local variables:
;;; eval: (require 'leaf)
;;; End:
