;;; -*- lexical-binding: t; eval: (progn (require (quote leaf)) (setq imenu-generic-expression lisp-imenu-generic-expression) (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t)); -*-

;; add to custom: theme, faces, keys (manual talked about it)
;; browse-url-custom to external file
;; maybe use svin's hack to disable gc during init
;; see if you can find in dired-extra selection of mime files
;; large chunks of code put in external files (maybe not packages)
;; maybe rx-ext should be eval-when-compile

(setf completion-ignore-case t
      disabled-command-function nil
      system-uses-terminfo nil)

(setenv "PAGER" "cat")

(defvar message-mode-map)
(defvar fb2-replace-hard-space t)
(defvar gamegrid-user-score-file-directory "/home/val/.cache/emacs/games/")
(defvar leaf-key-bindlist nil)
(defvar sly-lisp-implementations
  '((sbcl ("sbcl"))
    (ecl ("ecl"))
    (ccl ("ccl"))
    (clisp ("clisp"))))

(defun save-window-configuration-to-w (&rest _ignore)
  (window-configuration-to-register ?w))

(leaf rx :config (rx-define ext (&rest exts) (and "." (or exts) string-end)))

(leaf subr
  :commands
  add-to-list
  alist-get
  derived-mode-p
  error
  replace-regexp-in-string
  shell-quote-argument
  with-current-buffer
  :preface (provide 'subr)
  :advice
  (:override yes-or-no-p y-or-n-p)
  (:override suspend-frame ignore))

(leaf subr-x :commands thread-last)

(leaf bindings :preface (provide 'bindings))

;;; SETTINGS

;;;; FACES

(leaf hl-line
  :hook
  ((csv-mode-hook
    grep-mode-hook
    mingus-browse-hook
    mingus-playlist-hooks
    tar-mode-hook
    transmission-files-mode-hook
    transmission-mode-hook
    transmission-peers-mode-hook)
   . hl-line-mode))

(leaf diff-hl
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (org-mode-hook . diff-hl-mode))

(leaf ansi-color
  :hook (compilation-filter-hook . colorize-compilation)
  :config
  (defun colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point)))))

;;;;; THEMES

(leaf custom
  ;; :config (load-theme 'leuven t)
  ;; :config (load-theme 'acme t)
  ;; :config (load-theme 'modus-operandi t)
  :commands load-theme custom-theme-enabled-p)

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
  :commands outline-show-after-jump
  :config
  (defun outline-show-after-jump ()
    (when outline-minor-mode
      (outline-show-entry))))

(leaf outline-minor-faces :hook (outline-minor-mode-hook . outline-minor-faces-add-font-lock-keywords))

(leaf hideshow :bind (hs-minor-mode-map ("<C-M-tab>" . hs-toggle-hiding)))

(leaf bicycle
  :bind
  (outline-minor-mode-map
   :package outline
   ("<C-tab>" . bicycle-cycle)
   ("<backtab>" . bicycle-cycle-global))
  (hs-minor-mode-map
   :package hideshow
   ("<C-tab>" . bicycle-cycle)
   ("<backtab>" . bicycle-cycle-global)))

;;;; URL

(leaf browse-url
  :bind (ctl-x-map :package subr ("B" . browse-url))
  :config
  (setq browse-url-invidious-instances
        '(;; "invidious.ethibox.fr"
          ;; "invidious.site"
          ;; "invidious.fdn.fr"
          "invidious.tube" "invidious.zapashcanon.fr" "tube.connect.cafe"
          "vid.puffyan.us" "invidious.048596.xyz" "invidious.himiko.cloud"
          "invidious.zee.li" "invidious.namazso.eu" "yewtu.be" "ytprivate.com"
          "vid.mint.lgbt" "inv.skyn3t.in" "invidious.kavin.rocks"
          "au.ytprivate.com"))

  (defun browse-url-maybe-change-host-to-youtube (url)
    (let* ((url-object (url-generic-parse-url url))
           (url-host (url-host url-object)))
      (when (member url-host browse-url-invidious-instances)
        (setf (url-host url-object) "www.youtube.com"
              url (url-recreate-url url-object))))
    url)

  (defun browse-url-youtube-url-p (url)
    (string-match-p
     (rx bos (or (and (? (or "m." "www.")) "youtube.com") "youtu.be") eos)
     (or (url-host (url-generic-parse-url url)) "")))

  (defun browse-url-select-invidious-instance (url)
    (ido-completing-read (concat "Invidious instance for " url ": ")
                         browse-url-invidious-instances
                         nil t))

  (defun browse-url-read-char (prompt choices url)
    (cl-loop with prompt = (concat prompt " " url)
             for choice = (read-char prompt)
             until (memq choice choices)
             finally return choice))

  (defun browse-url-custom-browser (url &rest args)
    (let ((prompt (concat "[c]hromium [q]utebrowser [f]irefox [e]ww"))
          (choices '(?c ?q ?f ?e)))
      (apply
       (cl-case (browse-url-read-char prompt choices url)
         (?c #'browse-url-chromium)
         (?q #'browse-url-generic)
         (?f #'browse-url-firefox)
         (?e #'eww-browse-url))
       url args)))

  (defun browse-url-mpv (url &rest _args)
    (let ((url (browse-url-maybe-change-host-to-youtube url)))
      (call-process "setsid" nil 0 nil "-f" "mpvi" url)))

  (defun browse-url-ytdl (url &rest _args)
    (let ((url (browse-url-maybe-change-host-to-youtube url)))
      (call-process "ytdli" nil 0 nil url)))

  (defun browse-url-invidious (url &rest args)
    (let ((instance (browse-url-select-invidious-instance url))
          (url-object (url-generic-parse-url url)))
      (when (string-equal "youtu.be" (url-host url-object))
        (let* ((video-id (substring (car (url-path-and-query url-object)) 1 12))
               (query (url-build-query-string `(("v" ,video-id)))))
          (setf (url-filename url-object) (concat "/watch?" query))))
      (setf (url-host url-object) instance)
      (apply #'eww-browse-url (url-recreate-url url-object) args)))

  (defun browse-url-custom-media (url &rest args)
    (let ((prompt "[y]tdl [m]pv [b]rowser [c]omments")
          (choices '(?y ?m ?b ?c)))
      (when (browse-url-youtube-url-p url)
        (setq prompt (concat prompt " [i]nvidious")
              choices (cons ?i choices)))
      (apply (cl-case (browse-url-read-char prompt choices url)
               (?m #'browse-url-mpv)
               (?b #'browse-url-custom-browser)
               (?y #'browse-url-ytdl)
               (?i #'browse-url-invidious)
               (?c (lambda (url &rest _) (youtube-comments url))))
             url args)))

  (defun browse-url-custom (url &rest args)
    (let* ((media-extensions (rx (ext "flac" "m4a" "mp3" "ogg" "opus" "webm"
                                      "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob"
                                      "wmv" "aiff" "wav" "ogv" "flv")))
           (media-domains (rx (or "youtube.com" "youtu.be" "bitchute.com"
                                  "videos.lukesmith.xyz" "twitch.tv")))
           (url-object (url-generic-parse-url url))
           (url-type (url-type url-object))
           (url-host (url-host url-object))
           (url-path (car (url-path-and-query url-object))))
      (cond
       ((and (member url-type '(nil "file"))
             (string-match-p media-extensions url-path))
        (apply #'browse-url-mpv url args))
       ((and (member url-type '("https" "http"))
             (or (string-match-p media-extensions url-path)
                 (string-match-p media-domains url-host)
                 (member url-host browse-url-invidious-instances)))
        (apply #'browse-url-custom-media url args))
       (t
        (apply #'browse-url-custom-browser url args))))))

;;;; CACHE

(leaf savehist
  :commands savehist-filter-file-name-history
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


;;;; FILES

(leaf files
  :commands read-directory-name
  :bind (ctl-x-map :package subr ("R" . revert-buffer)))

;;;; SHR

(leaf shr-tag-pre-highlight
  :after shr
  :commands shr-tag-pre-highlight
  :leaf-defer nil
  :defvar shr-external-rendering-functions
  :config (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

;;;; OTHER

(leaf window
  :preface (provide 'window)
  :bind
  ("M-V" . scroll-down-line)
  ("C-S-v" . scroll-up-line)
  ("C-M-S-b" . previous-buffer)
  ("C-M-S-f" . next-buffer)
  ("M-Q" . quit-window))

(leaf mule
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment "UTF-8"))

(leaf async
  :after bytecomp
  :config (async-bytecomp-package-mode))

(leaf server :hook (after-init-hook . server-start))

;;; MAN

(leaf man :bind (help-map :package help ("M-m" . man)))

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
  :defvar dired-mode-map dired-actual-switches
  :defun dired-get-subdir
  :commands dired-get-marked-files
  :advice (:override dired-copy-filename-as-kill dired-copy-filename-as-kill-join-newline)

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
  :defun dired-get-marker-char
  :defvar dired-marker-char
  :commands dired-omit-mode dired-mark-extension

  :bind
  (ctl-x-map :package subr ("C-j" . dired-jump))
  (dired-mode-map
   :package dired
   ("* i" . dired-mark-images)
   ("* v" . dired-mark-videos))

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
       "vob" "wmv" "aiff" "wav" "ogv" "flv")
     marker-char)))

(leaf dired-aux
  :defvar dired-compress-file-suffixes
  :defer-config
  (add-to-list
   'dired-compress-file-suffixes
   `(,(rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

(leaf wdired
  :after dired
  :hook (wdired-mode-hook . image-dired-disable)
  :advice (:after wdired-finish-edit image-dired-enable)
  :config
  (defun image-dired-disable () (image-dired-minor-mode -1))
  (defun image-dired-enable (_ &rest) (image-dired-minor-mode 1)))

(leaf dired-rsync :bind (dired-mode-map :package dired ("r" . dired-rsync)))

(leaf dired-async
  :after dired
  :config (dired-async-mode))

;;;; FIND

(leaf find-dired :bind (search-map :package bindings ("f f" . find-dired)))

(leaf fd-dired :bind (search-map :package bindings ("f d" . fd-dired) ("f D" . fd-dired-list-searches)))

(leaf locate :bind (search-map :package bindings ("f l" . locate)))

;;; EDITING

(leaf simple
  :bind
  ("C-h"   . backward-delete-char-untabify)
  ("M-K"   . kill-whole-line)
  ("M-\\"  . delete-indentation)
  ("M-c"   . capitalize-dwim)
  ("M-l"   . downcase-dwim)
  ("M-u"   . upcase-dwim)
  ([remap newline] . newline-and-indent)
  (ctl-x-map
   :package subr
   ("K"   . kill-current-buffer)
   ("C-r" . overwrite-mode)
   ("M-t" . toggle-truncate-lines))
  (mode-specific-map :package bindings ("o P" . list-processes)))

(leaf register
  :bind
  (ctl-x-r-map
   :package bindings
   ("v" . view-register)
   ("L" . list-registers)
   ("p" . prepend-to-register)
   ("a" . append-to-register)))

(leaf subword :hook ((rust-mode-hook nix-mode-hook) . subword-mode))

(leaf edit-indirect :bind (ctl-x-map :package subr ("E" . edit-indirect-region)))

(leaf paragraphs :bind ("C-M-S-t" . transpose-paragraphs))

;;;; INPUT METHOD

(leaf cyrillic-dvorak-im :require t)

(leaf reverse-im
  :after cyrillic-dvorak-im
  :require t
  :config (reverse-im-activate "cyrillic-dvorak"))

;;;; PAIRS

(leaf smartparens
  :defun sp-kill-region sp-backward-kill-word

  :hook
  ((minibuffer-inactive-mode-hook nix-mode-hook org-mode-hook rust-mode-hook) . smartparens-mode)
  (smartparens-mode-hook . show-smartparens-mode)

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

(leaf tex-mode
  :defvar ispell-parser
  :hook (tex-mode-hook . (lambda () (setq-local ispell-parser 'tex))))

(leaf css-mode :defvar css-mode-map :bind (css-mode-map ("C-c m" . css-lookup-symbol)))

;;;;; XML-LIKE

(leaf sgml-mode
  :defvar sgml-mode-map
  :bind
  (sgml-mode-map
   ("C-M-n" . sgml-skip-tag-forward)
   ("C-M-p" . sgml-skip-tag-backward)
   ("C-c C-r" . sgml-namify-char)))

(leaf emmet-mode :hook ((nxml-mode-hook mhtml-mode-hook web-mode-hook) . emmet-mode))

;;;; PROG

(leaf web-mode :mode "\\.twig\\'")

;;;;; ELITE LISP

(leaf ipretty :bind ([remap eval-print-last-sexp] . ipretty-last-sexp))

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

;;; CORRECTNESS

(leaf flycheck-checkbashisms
  :after flycheck
  :config (flycheck-checkbashisms-setup))

;;; COMPLETION

;;;; MINIBUFFER

(leaf minibuffer
  :bind
  (completion-in-region-mode-map ("M-v" . switch-to-completions))
  (minibuffer-local-must-match-map ("C-j" . minibuffer-force-complete-and-exit)))

(leaf consult
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

(leaf orderless :bind (minibuffer-local-completion-map :package minibuffer ("SPC" . nil)))

(leaf insert-char-preview :bind ([remap insert-char] . insert-char-preview))


;;;; HIPPIE-EXP

(leaf hippie-exp
  :preface (defvar he-file-name-chars "-a-zA-Z0-9_/.,~^#$+={}")
  :defvar he-search-string he-tried-table he-expand-list
  :defun
  try-complete-file-name-with-env try-complete-file-name-partially-with-env
  he-init-string he-file-name-beg he-string-member he-reset-string
  he-concat-directory-file-name he-substitute-string
  :bind ([remap dabbrev-expand] . hippie-expand)

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
  :config (define-key isearch-mode-map (kbd "C-?") isearch-help-map))

(leaf grep
  :advice (:filter-return grep-expand-template grep-expand-template-add-cut)
  :bind (search-map :package bindings ("g" . rgrep))
  :config
  (defun grep-expand-template-add-cut (cmd)
    (concat cmd " | cut -c-500")))

(leaf rg
  :defvar rg-mode-map
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
  :bind
  ("M-z" . avy-goto-word-0)
  (goto-map :package bindings ("M-g" . avy-goto-line)))

(leaf ace-link
  :hook (after-init-hook . ace-link-setup-default)
  :bind (goto-map :package bindings ("l" . ace-link)))

;;;; TO DEFINITION

(leaf dumb-jump :hook (xref-backend-functions . dumb-jump-xref-activate))

(leaf find-func :bind (search-map :package bindings ("f b" . find-library)))

;;; COMPILATION

(leaf compile :bind (ctl-x-map :package subr ("c" . compile)))

;; Add support for cargo error --> file:line:col
(leaf cargo :hook (rust-mode-hook . cargo-minor-mode))

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

;;;; SHELL

(leaf shell
  :bind (shell-mode-map ("C-c M-d" . shell-change-directory))
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
                             "echo" "emacs" "env" "exit" "export" "fd" "feh"
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
    (setq-local comint-input-ring-file-name "/home/val/.local/share/emacs/comint/shell_history"
                comint-history-filter-function #'shell-history-filter)
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

(leaf shell-pwd :bind (mode-specific-map :package bindings ("x S" . shell-pwd-shell)))

;;; TEMPLATES

(leaf skempo
  :hook (nix-mode-hook . skempo-mode)

  :bind
  (skempo-mode-map
   ("C-z" . skempo-complete-tag-or-call-on-region)
   ("M-g M-e" . skempo-forward-mark)
   ("M-g M-a" . skempo-backward-mark))

  :config
  (skempo-advice-mode)

  (defun skempo-elisp-namespace ()
    (string-trim-right (buffer-name) (rx ".el" eos)))

  (defun skempo-elisp-group ()
    (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))

  (defun skempo-nix-hash ()
    (make-string 52 ?1))

  (skempo-define-tempo (vd :tag t :mode php-mode)
    "echo '<pre>'; var_dump(" p "); echo '</pre>';")

  (skempo-define-tempo (github :tag t :mode nix-mode)
    "fetchFromGitHub {" n>
    "owner = \"" p "\";" n>
    "repo = \"" p "\";" n>
    "rev = \"" p "\";" n>
    "sha256 = \"" p (skempo-nix-hash) "\";" n>
    "}" p >)

  (skempo-define-tempo (url :tag t :mode nix-mode)
    "fetchurl {" n>
    "url = \"" p "\";" n>
    "sha256 = \"" p (skempo-nix-hash) "\";" n>
    "}" p >)

  (skempo-define-tempo (zip :tag t :mode nix-mode)
    "fetchzip {" n>
    "url = \"" p "\";" n>
    "sha256 = \"" p (skempo-nix-hash) "\";" n>
    "}" p >)

  (skempo-define-tempo (git :tag t :mode nix-mode)
    "fetchGit {" n>
    "url = \"" p "\";" n>
    "rev = \"" p "\";" n>
    "}" p >)

  (skempo-define-tempo (lambda :tag t :mode (emacs-lisp-mode lisp-mode))
    "(lambda (" p ") " n> r> ")")

  (skempo-define-tempo (let :tag t :mode (emacs-lisp-mode lisp-mode))
    "(let ((" p "))" n> r> ")")

  (skempo-define-tempo (defvar :tag t :mode lisp-mode)
    "(defvar " p n> r> n> "\"" p "\")")

  (skempo-define-tempo (defun :tag t :mode lisp-mode)
    "(defun " p " (" p ")" n> "\"" p "\"" n> r> ")")

  (skempo-define-tempo (defvar :tag t :mode emacs-lisp-mode)
    "(defvar " (skempo-elisp-namespace) "-" p n>
    r> n>
    "\"" p "\")")

  (skempo-define-tempo (defun :tag t :mode emacs-lisp-mode)
    "(defun " (skempo-elisp-namespace) "-" p " (" p ")" n>
    "\"" p "\"" n>
    r> ")")

  (skempo-define-tempo (defgroup :tag t :mode emacs-lisp-mode)
    "(defgroup " (skempo-elisp-group) " nil" n>
    "\"" p "\"" n>
    ":group " p "nil)")

  (skempo-define-tempo (defcustom :tag t :mode emacs-lisp-mode)
    "(defcustom " (skempo-elisp-namespace) "-" p n>
    r> n>
    "\"" p "\"" n>
    ":type nil" n>
    ":group '" (skempo-elisp-group) ")" n>)

  (skempo-define-tempo (defface :tag t :mode emacs-lisp-mode)
    "(defface " (skempo-elisp-namespace) "-" p n>
    "'((t :inherit " p "nil))" n>
    "\"" p "\"" n>
    ":group '" (skempo-elisp-group) ")"))


;;; APPLICATIONS

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

(leaf ibuffer
  :defvar ibuffer-use-header-line
  :defun ibuffer-auto-mode ibuffer-clear-filter-groups
  :bind
  (ctl-x-map :package subr ("C-b" . ibuffer-jump))
  (mode-specific-map :package bindings ("x s" . shell-list-buffers))
  :config
  (defun shell-list-buffers ()
    (interactive)
    (let ((buffer-name "*Shell buffers*"))
      (ibuffer t buffer-name `((mode . shell-mode)))
      (with-current-buffer buffer-name
        (ibuffer-auto-mode)
        (set (make-local-variable 'ibuffer-use-header-line) nil)
        (ibuffer-clear-filter-groups)))))

(leaf magit
  :bind
  (ctl-x-map :package subr ("p m" . magit-project-status))
  (project-prefix-map :package project ("m" . magit-project-status)))

(leaf mediainfo-mode
  :commands mediainfo-mode--file-handler
  :preface
  (add-to-list
   'auto-mode-alist
   `(,(rx (ext "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav" "ogv" "flv"))
     . mediainfo-mode))
  (add-to-list
   'file-name-handler-alist
   `(,(rx (ext "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav" "ogv" "flv"
               "FLAC" "M4A" "MP3" "OGG" "OPUS" "WEBM" "MKV" "MP4" "AVI" "MPG" "MOV" "3GP" "VOB" "WMV" "AIFF" "WAV" "OGV" "FLV"))
     . mediainfo-mode--file-handler)))

(leaf youtube-comments :commands youtube-comments)

;;;; PROCESSES

(leaf pueue :commands pueue)

(leaf proced :bind (mode-specific-map :package bindings ("o p" . proced)))

(leaf neato-graph-bar :bind (mode-specific-map :package bindings ("o b" . neato-graph-bar)))

;;;; DICTIONARY

(leaf sdcv
  :bind (mode-specific-map :package bindings ("o t" . sdcv-search-input))
  :advice (:filter-return sdcv-search-with-dictionary-args sdcv-args-force-utf)
  :config
  (defun sdcv-args-force-utf (args)
    (cl-list* "--utf8-output" "--utf8-input" args)))

;;;; XML

(leaf eww
  :defvar eww-data eww-mode-map
  :defun eww-current-url
  :hook (eww-mode-hook . eww-restore-browse-url-browser-function)

  :bind
  (mode-specific-map :package bindings ("o y" . eww-invidous-search))
  (eww-mode-map ("V" . eww-browse-url-current))

  :config
  (defun eww-restore-browse-url-browser-function ()
    (kill-local-variable 'browse-url-browser-function))

  (defun eww-browse-url-current ()
    (interactive)
    (when-let ((url (eww-current-url)))
      (browse-url url)))

  (defun eww-invidous-search (search instance &optional arg)
    (interactive (let ((search (read-string "Search term: ")))
                   (list search
                         (browse-url-select-invidious-instance search)
                         current-prefix-arg)))
    (let ((url (concat "https://" instance "/search?" (url-build-query-string
                                                       `(("q" ,search))))))
      (eww url arg))))

;;;; YO-HO

(leaf transmission
  :defvar transmission-mode-map
  :bind
  (mode-specific-map :package bindings ("o r" . transmission))
  (transmission-mode-map ("M" . transmission-move)))

(leaf torrent-mode :mode "\\.torrent\\'")

;;;; RSS

(leaf newst-backend
  :defun newsticker--link newsticker--extra newsticker--desc newsticker--title
  :hook (newsticker-new-item-functions . newsticker-add-thumbnail)
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
  :preface (defvar newsticker--treeview-list-sort-order 'sort-by-time-reverse)
  :defun newsticker--treeview-get-selected-item
  :defvar newsticker-treeview-mode-map

  :bind
  (mode-specific-map :package bindings ("o n" . newsticker-show-news))
  (newsticker-treeview-mode-map ("w" . newsticker-treeview-copy-link))

  :config
  (defun newsticker-treeview-copy-link ()
    (interactive)
    (let ((link (newsticker--link (newsticker--treeview-get-selected-item))))
      (kill-new link)
      (message "Copied %s" link))))

;;;; MPD

(leaf mingus
  :preface (defvar mingus-music-directory "/home/val/Music")
  :defvar mpd-inter-conn mingus-mpd-playlist-dir

  :defun
  mingus-buffer-p mingus-git-out-and-kill mingus-add-files
  mingus-music-files mingus-directoryp
  mingus-get-absolute-filename mingus-playlistp

  :advice
  (:override mingus-git-out mingus-git-out-and-kill)
  (:override mingus-dired-file mingus-dired-jump-file)
  (:override mingus-dired-add mingus-dired-add-trim)

  :bind
  (mode-specific-map
   :package bindings
   ("o s" . mingus)
   ("o S" . mingus-find-and-add-file))
  (dired-mode-map
   :package dired
   ("SPC" . mingus-dired-add)
   ("S-SPC" . mingus-dired-add-and-play))

  :config
  (defun mingus-dired-jump-file ()
    "Open dired with parent dir of song at point."
    (interactive)
    (cond
     ((mingus-directoryp)
      (dired (concat mingus-music-directory (mingus-get-absolute-filename))))
     ((mingus-playlistp)
      (dired mingus-mpd-playlist-dir))
     (t
      (dired-jump nil (concat mingus-music-directory (mingus-get-absolute-filename))))))

  (defun mingus-dired-add-trim ()
    (interactive)
    (mingus-add-files (mapcar
                       (lambda (f)
                         (string-trim-left f (regexp-quote mingus-music-directory)))
                       (dired-get-marked-files))))

  (defun mingus-git-out-and-kill (&optional _)
    (interactive)
    (when (mingus-buffer-p)
      (kill-current-buffer)))

  (defvar mingus-music-files nil)
  (defun mingus-music-files ()
    (let* ((default-directory mingus-music-directory)
           (exts (cdr (mapcan (lambda (e) `("-o" "-iname" ,(concat "*." e)))
                              '("flac" "m4a" "mp3" "ogg" "opus"))))
           (args `("." "(" ,@exts ")" "-type" "f" "-o" "-type" "d")))
      (mapcar (lambda (m) (substring m 1))
              (cdr (apply #'process-lines "find" args)))))

  (defun mingus-find-and-add-file (&optional updatep)
    (interactive "P")
    (when (or (not mingus-music-files) updatep)
      (setf mingus-music-files (mingus-music-files)))

    (mingus-add-files
     (list (completing-read "Add file to mpd: " mingus-music-files nil t)))
    (mpd-play mpd-inter-conn)
    (let ((buffer (get-buffer "*Mingus*")))
      (when (buffer-live-p (get-buffer buffer))
        (kill-buffer buffer)))))

;;;; E-READER

(leaf pdf-tools :config (pdf-loader-install))

(leaf nov :mode "\\.epub\\'")

;;; MU4E

(leaf mu4e
  :defun mu4e-action-view-in-browser make-mu4e-context mu4e-message-field
  :defvar mu4e-main-mode-map mu4e-view-actions mu4e-contexts

  :preface
  (eval-when-compile
    (defmacro mu4e-make-folder-fn (folder)
      `(defun ,(intern (concat "mu4e-" folder "-folder-by-msg")) (msg)
         (when-let ((maildir (and msg (mu4e-message-field msg :maildir))))
           (save-match-data
             (string-match (rx bos "/" (group (+ (not "/")))) maildir)
             (concat "/" (match-string 1 maildir) ,(concat "/" folder)))))))

  (mu4e-make-folder-fn "trash")
  (mu4e-make-folder-fn "archive")
  (mu4e-make-folder-fn "sent")
  (mu4e-make-folder-fn "drafts")

  :bind (mode-specific-map :package bindings ("o m" . mu4e))

  :config
  (load-library "org-mu4e")
  (setq
   mu4e-contexts
   (list (make-mu4e-context
          :name "polimi"
          :vars '((mu4e-sent-folder . "/polimi/sent")
                  (mu4e-drafts-folder . "/polimi/drafts")
                  (mu4e-sent-messages-behavior . delete)
                  (user-mail-address . "valeriy.litkovskyy@mail.polimi.it")
                  (message-sendmail-extra-arguments . ("-a" "polimi"))
                  (mu4e-compose-signature . "Cordiali saluti,\nLitkovskyy Valeriy"))))))

;;; ORG

(leaf org
  :bind
  (mode-specific-map
   :package bindings
   ("G a" . org-agenda)
   ("G c" . org-capture)))

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

;;; Custom

(load (setf custom-file "/home/val/.config/nixpkgs/emacs/custom.el") t)
(set-register register-separator "\n")
