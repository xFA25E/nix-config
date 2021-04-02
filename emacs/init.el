;;; -*- lexical-binding: t; eval: (progn (require (quote leaf)) (setq imenu-generic-expression lisp-imenu-generic-expression) (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t)); -*-

;; add to custom: theme, faces, keys (manual talked about it)
;; see hooks and add-to-lists that are autoloaded and put them separately

;;; SETTINGS

(defvar leaf-key-bindlist nil)
(defvar fb2-replace-hard-space t)
(defvar gamegrid-user-score-file-directory "/home/val/.cache/emacs/games/")
(defvar sly-lisp-implementations
  '((sbcl ("sbcl"))
    (ecl ("ecl"))
    (ccl ("ccl"))
    (clisp ("clisp"))))

(setq completion-ignore-case t
      disabled-command-function nil
      custom-file "/home/val/.config/nixpkgs/emacs/custom.el")

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'bindings)
(provide 'subr)

;;;; FACES

(leaf hl-line
  :hook
  ((dired-mode-hook
    csv-mode-hook
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
  (dired-mode-hook . diff-hl-dired-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (org-mode-hook . diff-hl-mode))

(leaf ansi-color
  :defvar compilation-filter-start
  :hook (compilation-filter-hook . colorize-compilation)
  :config
  (defun colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

;;;;; THEMES

(leaf custom :commands load-theme custom-theme-enabled-p)

(leaf faces
  :bind (help-map :package help ("M-f" . list-faces-display))

  :config
  ;; (load-theme 'leuven t)
  ;; (load-theme 'acme t)
  (load-theme 'modus-operandi t)

  (set-face-attribute 'default nil :family "Iosevka" :height 170)
  (set-face-attribute 'mode-line nil :family "DejaVu Sans" :height 110)
  (set-face-attribute 'mode-line-inactive nil :family "DejaVu Sans" :height 110)
  (set-face-attribute 'fixed-pitch-serif nil :family "DejaVu Serif")
  (set-face-attribute 'header-line nil :inverse-video nil :family "Iosevka")

  (unless (custom-theme-enabled-p 'modus-operandi)
    (with-eval-after-load 'man
      (set-face-attribute 'Man-overstrike nil :inherit 'font-lock-variable-name-face :bold t)
      (set-face-attribute 'Man-underline nil :inherit 'font-lock-negation-char-face :underline t)))

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
  :defvar outline-minor-mode
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
  :bind
  (ctl-x-map :package subr ("B" . browse-url))
  (mode-specific-map :package bindings ("o y" . browse-url-youtube-search))
  :config (load "/home/val/.config/emacs/browse-url-multi.el"))

;;;; CACHE

(leaf savehist
  :defer-config
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

(leaf files :bind (ctl-x-map :package subr ("R" . revert-buffer)))

;;;; OTHER

(leaf window
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
  :init (async-bytecomp-package-mode))

;;; MAN

(leaf man :bind (help-map :package help ("M-m" . man)))

(leaf finder
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
  :defvar dired-mode-map
  :bind
  (dired-mode-map
   ("* &" . dired-flag-garbage-files)
   ("* d" . dired-flag-files-regexp)
   ("* g" . dired-mark-files-containing-regexp)))

(leaf dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :bind (ctl-x-map :package subr ("C-j" . dired-jump)))

(leaf dired-aux
  :defvar dired-compress-file-suffixes
  :defer-config
  (add-to-list
   'dired-compress-file-suffixes
   `(,(rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

(leaf dired-rsync :bind (dired-mode-map :package dired ("r" . dired-rsync)))

(leaf dired-async :after dired :config (dired-async-mode))

;;;; FIND

(leaf find-dired :bind (search-map :package bindings ("f f" . find-dired)))

(leaf fd-dired :bind (search-map :package bindings ("f d" . fd-dired) ("f D" . fd-dired-list-searches)))

(leaf locate :bind (search-map :package bindings ("f l" . locate)))

;;; EDITING

(leaf simple
  :bind
  ("C-h"  . backward-delete-char-untabify)
  ("M-K"  . kill-whole-line)
  ("M-\\" . delete-indentation)
  ("M-c"  . capitalize-dwim)
  ("M-l"  . downcase-dwim)
  ("M-u"  . upcase-dwim)
  ("C-w"  . kill-region-dwim)
  ([remap newline] . newline-and-indent)
  (ctl-x-map
   :package subr
   ("K"   . kill-current-buffer)
   ("C-r" . overwrite-mode)
   ("M-t" . toggle-truncate-lines))
  (mode-specific-map :package bindings ("o P" . list-processes))
  :config
  (defun kill-region-dwim (&optional count)
    (interactive "p")
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word count))))

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
  ("C-M-w" . sp-copy-sexp)
  ("M-[" . sp-unwrap-sexp)
  ("M-]" . sp-rewrap-sexp)
  ("C-)" . sp-forward-slurp-sexp)
  ("C-M-)" . sp-forward-barf-sexp)
  ("C-(" . sp-backward-slurp-sexp)
  ("C-M-(" . sp-backward-barf-sexp)
  (smartparens-mode-map
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("M-d" . sp-kill-word)
   ("C-w" . sp-kill-region-dwim))

  :config
  (defun sp-kill-region-dwim (&optional count)
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
  :after flycheck sh-mode
  :init (flycheck-checkbashisms-setup))

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
  :bind ("M-/" . hippie-expand)
  :config (load "/home/val/.config/emacs/hippie-exp-fixes.el"))

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

(leaf ace-link :bind (goto-map :package bindings ("l" . ace-link)))

;;;; TO DEFINITION

(leaf dumb-jump :hook (xref-backend-functions . dumb-jump-xref-activate))

(leaf find-func :bind (search-map :package bindings ("f b" . find-library)))

;;; COMPILATION

(leaf compile :bind (ctl-x-map :package subr ("c" . compile)))

(leaf cargo :hook (rust-mode-hook . cargo-minor-mode))

;;; REPL

(leaf comint
  :hook
  (kill-buffer-hook . comint-write-input-ring)
  (kill-emacs-hook . comint-write-buffers-input-ring)
  (comint-output-filter-functions . comint-strip-ctrl-m)
  (comint-output-filter-functions . comint-truncate-buffer)
  :config
  (defun comint-write-buffers-input-ring ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf (comint-write-input-ring)))))

;;;; SHELL

(leaf shell
  :bind (shell-mode-map ("C-c M-d" . shell-extra-change-directory))
  :config (load "/home/val/.config/emacs/shell-extra.el"))

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
  (load "/home/val/.config/emacs/skempo-templates.el"))

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
   (cons (rx "." (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav" "ogv" "flv") eos)
         'mediainfo-mode))
  (add-to-list
   'file-name-handler-alist
   (cons (rx "." (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav" "ogv" "flv"
                     "FLAC" "M4A" "MP3" "OGG" "OPUS" "WEBM" "MKV" "MP4" "AVI" "MPG" "MOV" "3GP" "VOB" "WMV" "AIFF" "WAV" "OGV" "FLV")
             eos)
         'mediainfo-mode--file-handler)))

(leaf youtube-comments :commands youtube-comments)

;;;; PROCESSES

(leaf pueue :commands pueue)

(leaf proced :bind (mode-specific-map :package bindings ("o p" . proced)))

;;;; DICTIONARY

(leaf sdcv
  :bind (mode-specific-map :package bindings ("o t" . sdcv-search-input))
  :advice (:filter-return sdcv-search-with-dictionary-args sdcv-args-force-utf)
  :config
  (defun sdcv-args-force-utf (args)
    (cl-list* "--utf8-output" "--utf8-input" args)))

;;;; XML

(leaf eww
  :defvar eww-mode-map
  :defun eww-current-url eww-links-at-point
  :bind (eww-mode-map ("V" . eww-browse-url-custom))
  :config
  (defun eww-browse-url-custom (&optional current)
    (interactive "P")
    (let ((browse-url-browser-function (default-value 'browse-url-browser-function))
          (url-at-point (car (eww-links-at-point))))
      (if (or current (not url-at-point))
          (browse-url (eww-current-url))
        (browse-url url-at-point)))))

;;;; YO-HO

(leaf transmission
  :defvar transmission-mode-map
  :bind
  (mode-specific-map :package bindings ("o r" . transmission))
  (transmission-mode-map ("M" . transmission-move)))

(leaf torrent-mode :mode "\\.torrent\\'")

;;;; RSS

(leaf newst-treeview
  :preface (defvar newsticker--treeview-list-sort-order 'sort-by-time-reverse)
  :defvar newsticker-treeview-mode-map
  :bind
  (mode-specific-map :package bindings ("o n" . newsticker-show-news))
  (newsticker-treeview-mode-map ("w" . newsticker-extra-treeview-copy-link))
  :config (load "/home/val/.config/emacs/newsticker-extra.el"))

;;;; MPD

(leaf mingus
  :bind
  (mode-specific-map
   :package bindings
   ("o s" . mingus)
   ("o S" . mingus-extra-find-and-add-file))
  (dired-mode-map
   :package dired
   ("SPC" . mingus-dired-add)
   ("S-SPC" . mingus-dired-add-and-play))
  :config (load "/home/val/.config/emacs/mingus-extra.el"))

;;;; E-READER

(leaf nov :mode "\\.epub\\'")

;;; MU4E

(leaf mu4e
  :defun make-mu4e-context
  :defvar mu4e-contexts
  :bind (mode-specific-map :package bindings ("o m" . mu4e))
  :config
  (load-library "org-mu4e")
  (setq
   mu4e-contexts
   (list (make-mu4e-context
          :name "polimi"
          :vars '((mu4e-trash-folder . "/polimi/trash")
                  (mu4e-refile-folder . "/polimi/archive")
                  (mu4e-sent-folder . "/polimi/sent")
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
  :defvar message-mode-map
  :bind
  (message-mode-map
   :package message
   ("C-c M-o" . org-mime-htmlize)
   ("C-c M-e" . org-mime-edit-mail-in-org-mode)
   ("C-c M-t" . org-mime-revert-to-plain-text-mail))
  :config (load "/home/val/.config/emacs/org-mime-fixes.el" t))

;;; After Init Lol

;; EBANIY ROT ETOGO DIRED-X
(add-hook 'dired-mode-hook 'sudo-edit-set-header)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setenv "PAGER" "cat")
(set-register register-separator "\n")
(ace-link-setup-default (eval-when-compile (kbd "M-o")))
(pdf-loader-install t t)
(server-start)

(load custom-file t)
