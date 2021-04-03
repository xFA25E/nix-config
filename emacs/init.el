;;; -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

;; add to custom: theme, faces, keys (manual talked about it)

;;; SETTINGS

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

(setenv "PAGER" "cat")

(server-start)

;;;; FACES

;; hl-line
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'csv-mode-hook 'hl-line-mode)
(add-hook 'grep-mode-hook 'hl-line-mode)
(add-hook 'mingus-browse-hook 'hl-line-mode)
(add-hook 'mingus-playlist-hooks 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'transmission-files-mode-hook 'hl-line-mode)
(add-hook 'transmission-mode-hook 'hl-line-mode)
(add-hook 'transmission-peers-mode-hook 'hl-line-mode)

;; diff-hl
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'org-mode-hook 'diff-hl-mode)

;; ansi-color
(with-eval-after-load 'compile
  (defun colorize-compilation ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'colorize-compilation))

;;;;; THEMES

(define-key help-map "\M-f" 'list-faces-display)

(load-theme 'modus-operandi t)

(set-face-attribute 'default nil :family "Iosevka" :height 170)
(set-face-attribute 'mode-line nil :family "DejaVu Sans" :height 110)
(set-face-attribute 'mode-line-inactive nil :family "DejaVu Sans" :height 110)

;; outline-minor-faces
(add-hook 'outline-minor-mode-hook 'outline-minor-faces-add-font-lock-keywords)

;; bicycle
(with-eval-after-load 'outline
  (define-key outline-minor-mode-map [C-tab] 'bicycle-cycle)
  (define-key outline-minor-mode-map [backtab] 'bicycle-cycle-global))
(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map [C-M-tab] 'hs-toggle-hiding)
  (define-key hs-minor-mode-map [C-tab] 'bicycle-cycle)
  (define-key hs-minor-mode-map [backtab] 'bicycle-cycle-global))

;;;; BROWSE-URL

(autoload 'browse-url-multi "/home/val/.config/emacs/browse-url-multi.el" nil t)
(autoload 'browse-url-youtube-search "/home/val/.config/emacs/browse-url-multi.el" nil t)
(define-key ctl-x-map "B" 'browse-url)
(define-key mode-specific-map "oy" 'browse-url-youtube-search)

;;;; SAVEHIST

(with-eval-after-load 'savehist
  (defun savehist-filter-file-name-history ()
    (let* ((trim-slashes (lambda (s) (string-trim-right (expand-file-name s) (rx (+ "/")))))
           (trimmed (mapcar trim-slashes file-name-history))
           (no-dups (cl-delete-duplicates trimmed :test #'string-equal))
           (remote-or-exist (lambda (file-name)
                              (and (not (string-empty-p file-name))
                                   (or (file-remote-p file-name)
                                       (string-match-p (rx bos "http") file-name)
                                       (file-exists-p file-name))))))
      (setq file-name-history (cl-delete-if-not remote-or-exist no-dups)))))

;;;; FILES

(define-key ctl-x-map "R" 'revert-buffer)

;;;; OTHER

;; window
(define-key global-map "\M-V" 'scroll-down-line)
(define-key global-map [?\C-\S-v] 'scroll-up-line)
(define-key global-map [?\C-\M-\S-b] 'previous-buffer)
(define-key global-map [?\C-\M-\S-f] 'next-buffer)
(define-key global-map "\M-Q" 'quit-window)

;; mule
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; bytecomp-async
(with-eval-after-load 'bytecomp
  (async-bytecomp-package-mode))

;;; MAN

(define-key help-map "\M-m" 'man)

;; finder
(define-key help-map "\M-c" 'finder-commentary)
(with-eval-after-load 'finder
  (defun finder-exit-with-package ()
    (interactive)
    (if (string-match-p (rx "*Finder" (opt "-package") "*") (buffer-name))
        (quit-window t)
      (when (get-buffer "*Finder-package*") (kill-buffer "*Finder-package*"))
      (when (get-buffer "*Finder*") (kill-buffer "*Finder*"))))
  (advice-add 'finder-exit :override 'finder-exit-with-package))

;;; DIRED

;; dired
(defvar dired-mode-map)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(with-eval-after-load 'dired
  (define-key dired-mode-map "*&" 'dired-flag-garbage-files)
  (define-key dired-mode-map "*d" 'dired-flag-files-regexp)
  (define-key dired-mode-map "*g" 'dired-mark-files-containing-regexp)
  (define-key dired-mode-map "r" 'dired-rsync)
  (dired-async-mode))

;; dired-aux
(defvar dired-compress-file-suffixes)
(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               (list (rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

;; dired-x
(autoload 'dired-omit-mode "dired-x" nil t)
(autoload 'dired-jump "dired-x" nil t)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(define-key ctl-x-map "\C-j" 'dired-jump)

;;;; FIND

;; find-dired
(define-key search-map "ff" 'find-dired)

;; fd-dired
(autoload 'fd-dired-list-searches "fd-dired" nil t)
(define-key search-map "fd" 'fd-dired)
(define-key search-map "fD" 'fd-dired-list-searches)

;; locate
(define-key search-map "fl" 'locate)

;;; EDITING

;; simple
(defun kill-region-dwim (&optional count)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word count)))

(define-key global-map "\C-h" 'backward-delete-char-untabify)
(define-key global-map "\M-K" 'kill-whole-line)
(define-key global-map "\M-\\" 'delete-indentation)
(define-key global-map "\M-c" 'capitalize-dwim)
(define-key global-map "\M-l" 'downcase-dwim)
(define-key global-map "\M-u" 'upcase-dwim)
(define-key global-map "\C-w" 'kill-region-dwim)
(define-key global-map [remap newline] 'newline-and-indent)
(define-key ctl-x-map "K" 'kill-current-buffer)
(define-key ctl-x-map "\C-r" 'overwrite-mode)
(define-key ctl-x-map "\M-t" 'toggle-truncate-lines)
(define-key mode-specific-map "oP" 'list-processes)

;; register
(define-key ctl-x-r-map "v" 'view-register)
(define-key ctl-x-r-map "L" 'list-registers)
(define-key ctl-x-r-map "p" 'prepend-to-register)
(define-key ctl-x-r-map "a" 'append-to-register)
(set-register register-separator "\n")

;; subword
(add-hook 'rust-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)

;; edit-indirect
(define-key ctl-x-map "E" 'edit-indirect-region)

;; paragraphs
(define-key global-map [?\C-\M-\S-t] 'transpose-paragraphs)

;;;; INPUT METHOD

(require 'cyrillic-dvorak-im)
(require 'reverse-im)
(reverse-im-activate "cyrillic-dvorak")

;;;; PAIRS

;; smartparens
(add-hook 'minibuffer-inactive-mode-hook 'smartparens-mode)
(add-hook 'nix-mode-hook 'smartparens-mode)
(add-hook 'org-mode-hook 'smartparens-mode)
(add-hook 'rust-mode-hook 'smartparens-mode)
(add-hook 'smartparens-mode-hook 'show-smartparens-mode)

(defun sp-kill-region-dwim (&optional count)
  (interactive "p")
  (if (use-region-p)
      (sp-kill-region (region-beginning) (region-end))
    (sp-backward-kill-word count)))

(autoload 'sp-backward-barf-sexp "smartparens" nil t)
(autoload 'sp-backward-slurp-sexp "smartparens" nil t)
(autoload 'sp-copy-sexp "smartparens" nil t)
(autoload 'sp-forward-barf-sexp "smartparens" nil t)
(autoload 'sp-forward-slurp-sexp "smartparens" nil t)
(autoload 'sp-rewrap-sexp "smartparens" nil t)
(autoload 'sp-unwrap-sexp "smartparens" nil t)
(define-key global-map (eval-when-compile (kbd "C-(")) 'sp-backward-slurp-sexp)
(define-key global-map (eval-when-compile (kbd "C-)")) 'sp-forward-slurp-sexp)
(define-key global-map (eval-when-compile (kbd "C-M-(")) 'sp-backward-barf-sexp)
(define-key global-map (eval-when-compile (kbd "C-M-)")) 'sp-forward-barf-sexp)
(define-key global-map "\C-\M-w" 'sp-copy-sexp)
(define-key global-map "\M-[" 'sp-unwrap-sexp)
(define-key global-map "\M-]" 'sp-rewrap-sexp)

(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map "\C-\M-u" 'sp-backward-up-sexp)
  (define-key smartparens-mode-map "\C-\M-d" 'sp-down-sexp)
  (define-key smartparens-mode-map "\C-\M-t" 'sp-transpose-sexp)
  (define-key smartparens-mode-map "\C-\M-k" 'sp-kill-sexp)
  (define-key smartparens-mode-map "\M-d" 'sp-kill-word)
  (define-key smartparens-mode-map "\C-w" 'sp-kill-region-dwim)
  (require 'smartparens-config))

;;;; CONF

;; tex-mode
(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

;; css-mode
(defvar css-mode-map)
(with-eval-after-load 'css-mode
  (define-key css-mode-map "\C-cm" 'css-lookup-symbol))

;;;;; XML-LIKE

;; sgml-mode
(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char))

;; emmet-mode
(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;;; PROG

;; web-mode
(add-to-list 'auto-mode-alist (cons (rx ".twig" eos) 'web-mode))

;;;;; EMACS LISP

;; ipretty
(define-key global-map [remap eval-print-last-sexp] 'ipretty-last-sexp)

;; pp
(define-key emacs-lisp-mode-map "\C-cm" 'pp-macroexpand-last-sexp)
(define-key emacs-lisp-mode-map "\C-cM" 'emacs-lisp-macroexpand)
(define-key lisp-interaction-mode-map "\C-cm" 'pp-macroexpand-last-sexp)
(define-key lisp-interaction-mode-map "\C-cM" 'emacs-lisp-macroexpand)

;;; CORRECTNESS

;; flycheck-checkbashisms
(with-eval-after-load 'flycheck
  (with-eval-after-load 'sh-mode
    (flycheck-checkbashisms-setup)))

;;; COMPLETION

;;;; MINIBUFFER

;; minibuffer
(define-key completion-in-region-mode-map "\M-v" 'switch-to-completions)
(define-key minibuffer-local-must-match-map "\C-j" 'minibuffer-force-complete-and-exit)

;; consult
(define-key global-map "\M-y" 'consult-yank-replace)
(define-key global-map "\M-X" 'consult-mode-command)
(define-key global-map "\M-H" 'consult-history)
(define-key ctl-x-map "F" 'consult-file-externally)
(define-key goto-map "o" 'consult-outline)
(define-key goto-map "i" 'consult-imenu)
(define-key goto-map "E" 'consult-error)
(with-eval-after-load 'kmacro
  (define-key kmacro-keymap "c" 'consult-kmacro))
(with-eval-after-load 'project
  (define-key project-prefix-map "i" 'consult-project-imenu))

;; orderless
(define-key minibuffer-local-completion-map " " nil)

;; insert-char-preview
(define-key global-map [remap insert-char] 'insert-char-preview)

;;;; HIPPIE-EXP

(define-key global-map "\M-/" 'hippie-expand)
(with-eval-after-load 'hippie-exp
  (load "/home/val/.config/emacs/hippie-exp-fixes.el"))

;;; SEARCHING

;; isearch
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key isearch-mode-map "\C-?" isearch-help-map)

;; grep
(define-key search-map "g" 'rgrep)
(with-eval-after-load 'grep
  (defun grep-expand-template-add-cut (cmd)
    (concat cmd " | cut -c-500"))
  (advice-add 'grep-expand-template :filter-return 'grep-expand-template-add-cut))

;; rg
(defvar rg-mode-map)
(define-key search-map "rr" 'rg)
(define-key search-map "r." 'rg-dwim)
(define-key search-map "rl" 'rg-list-searches)
(define-key search-map "rt" 'rg-literal)
(define-key search-map "rp" 'rg-project)
(define-key search-map "rk" 'rg-kill-saved-searches)
(define-key search-map "rs" 'rg-save-search-as-name)
(with-eval-after-load 'rg
  (define-key rg-mode-map "\C-n" 'next-line)
  (define-key rg-mode-map "\C-p" 'previous-line)
  (define-key rg-mode-map "{" 'rg-prev-file)
  (define-key rg-mode-map "\M-{" 'rg-prev-file)
  (define-key rg-mode-map "}" 'rg-next-file)
  (define-key rg-mode-map "\M-}" 'rg-next-file))

;;; JUMPING

;;;; ON BUFFER

;; avy
(define-key global-map "\M-z" 'avy-goto-word-0)
(define-key goto-map "\M-g" 'avy-goto-line)

;; ace-link
(define-key goto-map "l" 'ace-link)

;;;; TO DEFINITION

;; dumb-jump
(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;; find-func
(define-key search-map "fb" 'find-library)

;;; COMPILATION

;; compile
(define-key ctl-x-map "c" 'compile)

;; cargo
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; REPL

;; comint
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;;;; SHELL

(defvar shell-mode-map)
(with-eval-after-load 'shell
  (load "/home/val/.config/emacs/shell-extra.el")
  (define-key shell-mode-map "\C-c\M-d" 'shell-extra-change-directory))

(autoload 'shell-pwd-shell "shell-pwd" nil t)
(define-key mode-specific-map "xS" 'shell-pwd-shell)

;;; TEMPLATES

(add-hook 'find-file-hook 'auto-insert)

;;;; SKEMPO

(autoload 'skempo-mode "skempo" nil t)
(add-hook 'nix-mode-hook 'skempo-mode)
(with-eval-after-load 'skempo
  (define-key skempo-mode-map "\C-z" 'skempo-complete-tag-or-call-on-region)
  (define-key skempo-mode-map "\M-g\M-e" 'skempo-forward-mark)
  (define-key skempo-mode-map "\M-g\M-a" 'skempo-backward-mark)
  (skempo-advice-mode)

  (with-eval-after-load 'elisp-mode
    (load "/home/val/.config/emacs/skempo-emacs-lisp.el"))

  (with-eval-after-load 'sly
    (load "/home/val/.config/emacs/skempo-lisp.el"))

  (with-eval-after-load 'nix-mode
    (load "/home/val/.config/emacs/skempo-nix.el"))

  (with-eval-after-load 'php-mode
    (load "/home/val/.config/emacs/skempo-php.el")))

;;; APPLICATIONS

;; sudo-edit
(with-eval-after-load 'sudo-edit
  (add-hook 'dired-mode-hook 'sudo-edit-set-header)
  (add-hook 'shell-mode-hook 'sudo-edit-set-header)
  (sudo-edit-indicator-mode t))

;; net-utils
(autoload 'smbclient "net-utils" nil t)
(define-key mode-specific-map "na" 'arp)
(define-key mode-specific-map "nd" 'dig)
(define-key mode-specific-map "nh" 'nslookup-host)
(define-key mode-specific-map "ni" 'ifconfig)
(define-key mode-specific-map "nn" 'netstat)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "nr" 'route)
(define-key mode-specific-map "ns" 'smbclient)
(define-key mode-specific-map "nt" 'traceroute)
(define-key mode-specific-map "nw" 'iwconfig)

;; ibuffer
(define-key ctl-x-map "\C-b" 'ibuffer-jump)
(defvar ibuffer-use-header-line)
(declare-function ibuffer-auto-mode "ibuffer")
(declare-function ibuffer-clear-filter-groups "ibuffer")
(defun shell-list-buffers ()
  (interactive)
  (let ((buffer-name "*Shell buffers*"))
    (ibuffer t buffer-name `((mode . shell-mode)))
    (with-current-buffer buffer-name
      (ibuffer-auto-mode)
      (set (make-local-variable 'ibuffer-use-header-line) nil)
      (ibuffer-clear-filter-groups))))
(define-key mode-specific-map "xs" 'shell-list-buffers)

;; magit
(define-key ctl-x-map "pm" 'magit-project-status)
(with-eval-after-load 'project
  (define-key project-prefix-map "m" 'magit-project-status))

;; mediainfo-mode
(autoload 'mediainfo-mode--file-handler "mediainfo-mode" nil t)
(add-to-list 'auto-mode-alist
             (cons (rx "." (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav" "ogv" "flv") eos)
                   'mediainfo-mode))
(add-to-list 'file-name-handler-alist
             (cons (rx "." (or "flac" "m4a" "mp3" "ogg" "opus" "webm" "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob" "wmv" "aiff" "wav" "ogv" "flv"
                               "FLAC" "M4A" "MP3" "OGG" "OPUS" "WEBM" "MKV" "MP4" "AVI" "MPG" "MOV" "3GP" "VOB" "WMV" "AIFF" "WAV" "OGV" "FLV")
                       eos)
                   'mediainfo-mode--file-handler))

;; youtube-comments
(autoload 'youtube-comments "youtube-comments" nil t)

;;;; PROCESSES

;; pueue
(autoload 'pueue "pueue" nil t)

;; proced
(define-key mode-specific-map "op" 'proced)

;;;; DICTIONARY

;; sdcv
(define-key mode-specific-map "ot" 'sdcv-search-input)
(with-eval-after-load 'sdcv
  (defun sdcv-args-force-utf (args)
    (cl-list* "--utf8-output" "--utf8-input" args))
  (advice-add 'sdcv-search-with-dictionary-args :filter-return 'sdcv-args-force-utf))

;;;; XML

;; eww
(declare-function eww-links-at-point "eww")
(declare-function eww-current-url "eww")
(defvar eww-mode-map)
(with-eval-after-load 'eww
  (defun eww-browse-url-custom (&optional current)
    (interactive "P")
    (let ((browse-url-browser-function (default-value 'browse-url-browser-function))
          (url-at-point (car (eww-links-at-point))))
      (if (or current (not url-at-point))
          (browse-url (eww-current-url))
        (browse-url url-at-point))))
  (define-key eww-mode-map "V" 'eww-browse-url-custom))

;;;; YO-HO

;; transmission
(defvar transmission-mode-map)
(define-key mode-specific-map "or" 'transmission)
(with-eval-after-load 'transmission
  (define-key transmission-mode-map "M" 'transmission-move))

;; torrent-mode
(autoload 'torrent-mode "torrent-mode" nil t)
(add-to-list 'auto-mode-alist (cons (rx ".torrent" eos) 'torrent-mode))

;;;; RSS

;; newst-treeview
(defvar newsticker-treeview-mode-map)
(defvar newsticker--treeview-list-sort-order 'sort-by-time-reverse)
(define-key mode-specific-map "on" 'newsticker-show-news)
(with-eval-after-load 'newst-treeview
  (load "/home/val/.config/emacs/newsticker-extra.el")
  (define-key newsticker-treeview-mode-map "w" 'newsticker-extra-treeview-copy-link))

;;;; MPD

;; mingus
(autoload 'mingus-extra-find-and-add-file "/home/val/.config/emacs/mingus-extra.el" nil t)
(autoload 'mingus-dired-add "mingus" nil t)
(autoload 'mingus-dired-add-and-play "mingus" nil t)
(define-key mode-specific-map "os" 'mingus)
(define-key mode-specific-map "oS" 'mingus-extra-find-and-add-file)
(with-eval-after-load 'dired
  (define-key dired-mode-map " " 'mingus-dired-add)
  (define-key dired-mode-map (eval-when-compile (kbd "S-SPC")) 'mingus-dired-add-and-play))

;;;; E-READER

;; pdf-tools
(pdf-loader-install t t)

;; nov
(add-to-list 'auto-mode-alist (cons (rx ".epub" eos) 'nov-mode))

;;; MU4E

(defvar mu4e-contexts)
(declare-function make-mu4e-context "mu4e-context")
(define-key mode-specific-map "om" 'mu4e)
(with-eval-after-load 'mu4e
  (load-library "org-mu4e")
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "polimi"
          :vars
          '((mu4e-trash-folder . "/polimi/trash")
            (mu4e-refile-folder . "/polimi/archive")
            (mu4e-sent-folder . "/polimi/sent")
            (mu4e-drafts-folder . "/polimi/drafts")
            (mu4e-sent-messages-behavior . delete)
            (user-mail-address . "valeriy.litkovskyy@mail.polimi.it")
            (message-sendmail-extra-arguments "-a" "polimi")
            (mu4e-compose-signature . "Cordiali saluti,\nLitkovskyy Valeriy"))))))

;;; ORG

;; org
(define-key mode-specific-map "Ga" 'org-agenda)
(define-key mode-specific-map "Gc" 'org-capture)

;; org-mime
(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(defvar message-mode-map)
(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\M-o" 'org-mime-htmlize)
  (define-key message-mode-map "\C-c\M-e" 'org-mime-edit-mail-in-org-mode)
  (define-key message-mode-map "\C-c\M-t" 'org-mime-revert-to-plain-text-mail))
(with-eval-after-load 'org-mime
  (load "/home/val/.config/emacs/org-mime-fixes.el"))

;;; Custom

(load custom-file t)
