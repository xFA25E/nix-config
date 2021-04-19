;;; -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

;; project fixes
(push (cl-find "project" load-path :test 'string-match) load-path)
(autoload 'project--process-file-region "/home/val/.config/emacs/project-fixes.el")

;;; SETTINGS

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

;;;;; HL-LINE

(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'csv-mode-hook 'hl-line-mode)
(add-hook 'grep-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'transmission-files-mode-hook 'hl-line-mode)
(add-hook 'transmission-mode-hook 'hl-line-mode)
(add-hook 'transmission-peers-mode-hook 'hl-line-mode)
(add-hook 'mpc-songs-mode-hook 'hl-line-mode)
(add-hook 'mpc-tagbrowser-mode-hook 'hl-line-mode)

;;;;; ANSI-COLOR

(with-eval-after-load 'compile
  (defun colorize-compilation ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'colorize-compilation))

;;;;; THEMES

(define-key help-map "\M-f" 'list-faces-display)

(set-face-attribute 'default nil :height 150)
(set-face-attribute 'mode-line nil :height 105 :background "white smoke" :box nil)
(set-face-attribute 'mode-line-inactive nil :height 105 :background "dark gray")
(set-face-attribute 'header-line nil :height 150)

;;;;; OUTLINE

(with-eval-after-load 'outline
  (defun outline-show-after-jump ()
    (when outline-minor-mode (outline-show-entry))))

;;;;; OUTLINE-MINOR-FACES

(add-hook 'outline-minor-mode-hook 'outline-minor-faces-add-font-lock-keywords)

;;;;; BICYCLE

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
    (let (result)
      (dolist (file-name file-name-history)
        (let ((f (string-trim-right (expand-file-name file-name) "/+")))
          (unless (string-empty-p f)
            (when (or (file-remote-p f)
                      (string-match-p "\\`http" f)
                      (file-exists-p f))
              (cl-pushnew f result :test #'string-equal)))))
      (setq file-name-history result))))

;;;; FILES

(define-key ctl-x-map "R" 'revert-buffer)

;;;; WINDOW

(define-key global-map "\M-V" 'scroll-down-line)
(define-key global-map [?\C-\S-v] 'scroll-up-line)
(define-key global-map [?\C-\M-\S-b] 'previous-buffer)
(define-key global-map [?\C-\M-\S-f] 'next-buffer)
(define-key global-map "\M-Q" 'quit-window)
(define-key global-map "\M-O" 'other-window)

;;;; TAB BAR

(define-key global-map "\C-xtt" 'toggle-tab-bar-mode-from-frame)

;;;; MULE

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-language-environment "UTF-8")

;;;; BYTECOMP-ASYNC

(with-eval-after-load 'bytecomp
  (async-bytecomp-package-mode))

;;; MAN

(define-key help-map "\M-m" 'man)

;;;; FINDER

(define-key help-map "\M-c" 'finder-commentary)
(with-eval-after-load 'finder
  (defun finder-exit-with-package ()
    (interactive)
    (if (string-match-p (rx "*Finder" (? "-package") "*") (buffer-name))
        (quit-window t)
      (when (get-buffer "*Finder-package*") (kill-buffer "*Finder-package*"))
      (when (get-buffer "*Finder*") (kill-buffer "*Finder*"))))
  (advice-add 'finder-exit :override 'finder-exit-with-package))

;;; DIRED

(defvar dired-mode-map)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(with-eval-after-load 'dired
  (define-key dired-mode-map "*&" 'dired-flag-garbage-files)
  (define-key dired-mode-map "*d" 'dired-flag-files-regexp)
  (define-key dired-mode-map "*g" 'dired-mark-files-containing-regexp)
  (define-key dired-mode-map "r" 'dired-rsync)
  (dired-async-mode))

;;;; DIRED-AUX

(defvar dired-compress-file-suffixes)
(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               (list (rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

;;;; DIRED-X

(autoload 'dired-omit-mode "dired-x" nil t)
(autoload 'dired-jump "dired-x" nil t)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(define-key ctl-x-map "\C-j" 'dired-jump)

;;;; FIND-DIRED

(define-key search-map "ff" 'find-dired)

;;;; FD-DIRED

(autoload 'fd-dired-list-searches "fd-dired" nil t)
(define-key search-map "fd" 'fd-dired)
(define-key search-map "fD" 'fd-dired-list-searches)

;;;; LOCATE

(define-key search-map "fl" 'locate)

;;; EDITING

;;;; SIMPLE

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

;;;; REGISTER

(define-key ctl-x-r-map "v" 'view-register)
(define-key ctl-x-r-map "L" 'list-registers)
(define-key ctl-x-r-map "p" 'prepend-to-register)
(define-key ctl-x-r-map "a" 'append-to-register)
(set-register register-separator "\n")

;;;; SUBWORD

(add-hook 'rust-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)

;;;; EDIT-INDIRECT

(define-key ctl-x-map "E" 'edit-indirect-region)

;;;; PARAGRAPHS

(define-key global-map [?\C-\M-\S-t] 'transpose-paragraphs)

;;;; INPUT METHOD

(require 'cyrillic-dvorak-im)
(require 'reverse-im)
(reverse-im-activate "cyrillic-dvorak")

;;;; SMARTPARENS

(add-hook 'minibuffer-inactive-mode-hook 'smartparens-mode)
(add-hook 'nix-mode-hook 'smartparens-mode)
(add-hook 'org-mode-hook 'smartparens-mode)
(add-hook 'rust-mode-hook 'smartparens-mode)
(add-hook 'js-mode-hook 'smartparens-mode)
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

(define-key smartparens-mode-map "\C-\M-u" 'sp-backward-up-sexp)
(define-key smartparens-mode-map "\C-\M-d" 'sp-down-sexp)
(define-key smartparens-mode-map "\C-\M-t" 'sp-transpose-sexp)
(define-key smartparens-mode-map "\C-\M-k" 'sp-kill-sexp)
(define-key smartparens-mode-map "\M-d" 'sp-kill-word)
(define-key smartparens-mode-map "\C-w" 'sp-kill-region-dwim)

(with-eval-after-load 'smartparens
  (require 'smartparens-config))

;;;; CONF

;;;;; TEX-MODE

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

;;;;; CSS-MODE

(defvar css-mode-map)
(with-eval-after-load 'css-mode
  (define-key css-mode-map "\C-cm" 'css-lookup-symbol))

;;;;; XML-LIKE

;;;;;; SGML-MODE

(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char))

;;;;;; EMMET-MODE

(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;;; PROG

;;;;; WEB-MODE

(add-to-list 'auto-mode-alist (cons (rx ".twig" eos) 'web-mode))

;;;;; EMACS LISP

;;;;;; IPRETTY

(define-key global-map [remap eval-print-last-sexp] 'ipretty-last-sexp)

;;;;;; PP

(define-key emacs-lisp-mode-map "\C-cm" 'pp-macroexpand-last-sexp)
(define-key emacs-lisp-mode-map "\C-cM" 'emacs-lisp-macroexpand)
(define-key lisp-interaction-mode-map "\C-cm" 'pp-macroexpand-last-sexp)
(define-key lisp-interaction-mode-map "\C-cM" 'emacs-lisp-macroexpand)

;;; CORRECTNESS

;;;; FLYMAKE

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-g\M-f" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-g\M-b" 'flymake-goto-prev-error))

;;; COMPLETION

;;;; MINIBUFFER

(define-key completion-in-region-mode-map "\M-v" 'switch-to-completions)
(define-key minibuffer-local-must-match-map "\C-j" 'minibuffer-force-complete-and-exit)

;;;;; CONSULT

(define-key global-map "\M-y" 'consult-yank-replace)
(define-key global-map "\M-X" 'consult-mode-command)
(define-key global-map "\M-H" 'consult-history)
(define-key ctl-x-map "F" 'consult-file-externally)
(define-key ctl-x-map "M" 'consult-minor-mode-menu)
(define-key goto-map "o" 'consult-outline)
(define-key goto-map "i" 'consult-imenu)
(define-key goto-map "E" 'consult-compile-error)
(define-key goto-map "!" 'consult-flymake)
(define-key project-prefix-map "i" 'consult-project-imenu)
(define-key kmacro-keymap "c" 'consult-kmacro)

;;;;; ORDERLESS

(define-key minibuffer-local-completion-map " " nil)

;;;;; INSERT-CHAR-PREVIEW

(define-key global-map [remap insert-char] 'insert-char-preview)

;;;; HIPPIE-EXP

(define-key global-map "\M-/" 'hippie-expand)
(with-eval-after-load 'hippie-exp
  (load "/home/val/.config/emacs/hippie-exp-fixes.el"))

;;; SEARCHING

;;;; ISEARCH

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key isearch-mode-map "\C-?" isearch-help-map)

;;;; GREP

(define-key search-map "g" 'rgrep)
(with-eval-after-load 'grep
  (defun grep-expand-template-add-cut (cmd)
    (concat cmd " | cut -c-500"))
  (advice-add 'grep-expand-template :filter-return 'grep-expand-template-add-cut))

;;;; RG

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

;;;;; AVY

(define-key global-map "\M-z" 'avy-goto-word-0)
(define-key goto-map "\M-g" 'avy-goto-line)

;;;;; ACE-LINK

(define-key goto-map "l" 'ace-link)

;;;; TO DEFINITION

;;;;; DUMB-JUMP

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;;;;; FIND-FUNC

(define-key search-map "fb" 'find-library)

;;; COMPILATION

;;;; COMPILE

(define-key ctl-x-map "c" 'compile)

;;;; CARGO

(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; COMINT

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;;;; SHELL

(defvar shell-mode-map)
(with-eval-after-load 'shell
  (load "/home/val/.config/emacs/shell-extra.el")
  (define-key shell-mode-map "\C-c\M-d" 'shell-extra-change-directory)
  (define-key mode-specific-map "xS" 'shell-extra-list-buffers))

(define-key mode-specific-map "xs" 'shell-pwd-shell)

;;; TEMPLATES

(add-hook 'find-file-hook 'auto-insert)

;;;; SKEMPO

(add-hook 'nix-mode-hook 'skempo-mode)
(with-eval-after-load 'skempo
  (define-key skempo-mode-map "\C-z" 'skempo-complete-tag-or-call-on-region)
  (define-key skempo-mode-map "\M-g\M-e" 'skempo-forward-mark)
  (define-key skempo-mode-map "\M-g\M-a" 'skempo-backward-mark)

  (with-eval-after-load 'elisp-mode
    (load "/home/val/.config/emacs/skempo-emacs-lisp.el"))

  (with-eval-after-load 'sly
    (load "/home/val/.config/emacs/skempo-lisp.el"))

  (with-eval-after-load 'nix-mode
    (load "/home/val/.config/emacs/skempo-nix.el"))

  (with-eval-after-load 'php-mode
    (load "/home/val/.config/emacs/skempo-php.el")))

;;; APPLICATIONS

;;;; SUDO-EDIT

(with-eval-after-load 'sudo-edit
  (add-hook 'dired-mode-hook 'sudo-edit-set-header)
  (add-hook 'shell-mode-hook 'sudo-edit-set-header)
  (sudo-edit-indicator-mode t))

;;;; NET-UTILS

(autoload 'smbclient "net-utils" nil t)
(define-key mode-specific-map "na" 'arp)
(define-key mode-specific-map "nd" 'dig)
(define-key mode-specific-map "nh" 'nslookup-host)
(define-key mode-specific-map "ni" 'ifconfig)
(define-key mode-specific-map "nn" 'netstat)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "nr" 'route)
(define-key mode-specific-map "ns" 'smbclient)
(define-key mode-specific-map "nt" 'traceroute)
(define-key mode-specific-map "nw" 'iwconfig)

;;;; IBUFFER

(define-key ctl-x-map "\C-b" 'ibuffer-jump)

;;;; MAGIT

(define-key ctl-x-map "pm" 'magit-project-status)
(define-key project-prefix-map "m" 'magit-project-status)

;;;; PROCESSES

(define-key mode-specific-map "op" 'proced)
(define-key mode-specific-map "ou" 'pueue)

;;;; SDCV

(define-key mode-specific-map "ot" 'sdcv-search-input)
(with-eval-after-load 'sdcv
  (defun sdcv-args-force-utf (args)
    (cl-list* "--utf8-output" "--utf8-input" args))
  (advice-add 'sdcv-search-with-dictionary-args :filter-return 'sdcv-args-force-utf))

;;;; EWW

(declare-function eww-links-at-point "eww")
(defvar eww-mode-map)
(with-eval-after-load 'eww
  (defun eww-browse-url-custom ()
    (interactive)
    (let ((browse-url-browser-function (default-value 'browse-url-browser-function)))
      (when-let ((url-at-point (car (eww-links-at-point))))
        (browse-url url-at-point))))
  (define-key eww-mode-map "V" 'eww-browse-url-custom))

;;;; TRANSMISSION

(defvar transmission-mode-map)
(define-key mode-specific-map "or" 'transmission)
(with-eval-after-load 'transmission
  (define-key transmission-mode-map "M" 'transmission-move))

;;;; NEWSTICKER

(defvar newsticker-treeview-mode-map)
(defvar newsticker--treeview-list-sort-order 'sort-by-time-reverse)
(define-key mode-specific-map "on" 'newsticker-show-news)
(with-eval-after-load 'newst-treeview
  (load "/home/val/.config/emacs/newsticker-extra.el")
  (define-key newsticker-treeview-mode-map "w" 'newsticker-extra-treeview-copy-link))

;;;; MPC

(define-key mode-specific-map "os" 'mpc)

(defvar mpc-mode-map)
(defvar mpc-songs-mode-map)
(with-eval-after-load 'mpc
  (define-key mpc-mode-map "p" 'mpc-playlist)
  (define-key mpc-mode-map "u" 'mpc-update)
  (define-key mpc-mode-map "a" 'mpc-playlist-add)
  (define-key mpc-mode-map "A" 'mpc-play)
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

;;;; E-READER

;;;;; PDF-TOOLS

(pdf-loader-install t t)

;;;;; NOV

(add-to-list 'auto-mode-alist (cons (rx ".epub" eos) 'nov-mode))

;;; MU4E

(defvar mu4e-contexts)
(declare-function make-mu4e-context "mu4e-context")
(autoload 'mu4e "mu4e" nil t)
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

(define-key mode-specific-map "Ga" 'org-agenda)
(define-key mode-specific-map "Gc" 'org-capture)

;;;; ORG-MIME

(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(defvar message-mode-map)
(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\M-o" 'org-mime-htmlize)
  (define-key message-mode-map "\C-c\M-e" 'org-mime-edit-mail-in-org-mode)
  (define-key message-mode-map "\C-c\M-t" 'org-mime-revert-to-plain-text-mail))
(with-eval-after-load 'org-mime
  (load "/home/val/.config/emacs/org-mime-fixes.el"))

;;; CUSTOM

(defvar cus-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'customize-apropos)
    (define-key map "f" #'customize-face)
    (define-key map "g" #'customize-group)
    (define-key map "m" #'customize-mode)
    (define-key map "r" #'customize-rogue)
    (define-key map "s" #'customize-saved)
    (define-key map "t" #'customize-themes)
    (define-key map "u" #'customize-unsaved)
    (define-key map "v" #'customize-option)
    map))
(define-key ctl-x-map "C" cus-edit-map)

(load custom-file t)
