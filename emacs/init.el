;;; -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

;;; ACE LINK

(define-key goto-map "l" 'ace-link)

;;; ANSI COLOR

(with-eval-after-load 'compile
  (defun colorize-compilation ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'colorize-compilation))

;;; AVY

(define-key global-map "\M-z" 'avy-goto-word-0)
(define-key goto-map "\M-g" 'avy-goto-line)

;;; AVY COMPLETION

(autoload 'avy-completion "/home/val/.config/emacs/avy-completion.el" nil t)
(define-key minibuffer-local-completion-map "\M-z" 'avy-completion)
(define-key completion-in-region-mode-map "\M-z" 'avy-completion)
(with-eval-after-load 'vcomplete (define-key vcomplete-command-map "\M-z" 'avy-completion))

;;; BROWSE URL

(autoload 'browse-url-multi "/home/val/.config/emacs/browse-url-multi.el" nil t)
(autoload 'browse-url-youtube-search "/home/val/.config/emacs/browse-url-multi.el" nil t)
(define-key ctl-x-map "B" 'browse-url)
(define-key mode-specific-map "oy" 'browse-url-youtube-search)

;;; BYTECOMP ASYNC

(with-eval-after-load 'bytecomp (async-bytecomp-package-mode))

;;; CARGO

(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; COMINT

(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;;; CONSULT

(define-key global-map "\M-H" 'consult-history)
(define-key goto-map "o" 'consult-outline)
(define-key goto-map "i" 'consult-imenu)
(define-key goto-map "E" 'consult-compile-error)
(define-key goto-map "f" 'consult-flymake)
(define-key project-prefix-map "i" 'consult-project-imenu)
(define-key kmacro-keymap "c" 'consult-kmacro)

;;; CSS MODE

(defvar css-mode-map)
(with-eval-after-load 'css-mode (define-key css-mode-map "\C-cm" 'css-lookup-symbol))

;;; CUSTOM

(defvar cus-edit-map (make-sparse-keymap))
(define-key cus-edit-map "v" 'customize-option)
(define-key cus-edit-map "g" 'customize-group)
(define-key cus-edit-map "f" 'customize-face)
(define-key cus-edit-map "s" 'customize-saved)
(define-key cus-edit-map "u" 'customize-unsaved)
(define-key ctl-x-map "c" cus-edit-map)

;;; CYRILLIC DVORAK IM

(require 'cyrillic-dvorak-im)

;;; DIRED

(defvar dired-mode-map)
(with-eval-after-load 'dired
  (define-key dired-mode-map "r" 'dired-rsync)
  (dired-async-mode))

;;; DIRED AUX

(defvar dired-compress-file-suffixes)
(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               (list (rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

;;; DIRED X

(with-eval-after-load 'dired (require 'dired-x))

;;; DUMB JUMP

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;;; EDIT INDIRECT

(define-key ctl-x-map "E" 'edit-indirect-region)

;;; ELISP MODE

(define-key emacs-lisp-mode-map "\C-cM" 'emacs-lisp-macroexpand)
(define-key lisp-interaction-mode-map "\C-cM" 'emacs-lisp-macroexpand)

;;; EMACS

(setq completion-ignore-case t
      mode-line-compact 'long)

;;; EMMET MODE

(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'mhtml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;; ENV

(setenv "PAGER" "cat")

;;; EWW

(declare-function eww-links-at-point "eww")
(defvar eww-mode-map)
(with-eval-after-load 'eww
  (defun eww-browse-url-custom ()
    (interactive)
    (let ((browse-url-browser-function (default-value 'browse-url-browser-function)))
      (when-let ((url-at-point (car (eww-links-at-point))))
        (browse-url url-at-point))))
  (define-key eww-mode-map "V" 'eww-browse-url-custom))

;;; FACES

(define-key help-map "\M-f" 'list-faces-display)

;;; FD DIRED

(define-key search-map "fd" 'fd-dired)

;;; FIND DIRED

(define-key search-map "ff" 'find-dired)

;;; FIND FUNC

(define-key search-map "fb" 'find-library)

;;; FINDER

(define-key help-map "\M-c" 'finder-commentary)

;;; FLYMAKE

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-g\M-f" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-g\M-b" 'flymake-goto-prev-error))

;;; GREP

(define-key search-map "g" 'rgrep)
(with-eval-after-load 'grep
  (defun grep-expand-template-add-cut (cmd)
    (concat cmd " | cut -c-500"))
  (advice-add 'grep-expand-template :filter-return 'grep-expand-template-add-cut))

;;; HELP FNS

(define-key help-map "\M-k" 'describe-keymap)

;;; HIPPIE EXP

(define-key global-map "\C-_" 'hippie-expand)
(with-eval-after-load 'hippie-exp (load "/home/val/.config/emacs/hippie-exp-fixes.el"))

;;; HL LINE

(add-hook 'csv-mode-hook 'hl-line-mode)
(add-hook 'grep-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'transmission-files-mode-hook 'hl-line-mode)
(add-hook 'transmission-mode-hook 'hl-line-mode)
(add-hook 'transmission-peers-mode-hook 'hl-line-mode)
(add-hook 'mpc-mode-hook 'hl-line-mode)

;;; IPRETTY

(define-key global-map [remap eval-print-last-sexp] 'ipretty-last-sexp)

;;; ISEARCH

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key isearch-mode-map "\C-?" isearch-help-map)

;;; LOCATE

(define-key search-map "fl" 'locate)

;;; MAGIT

(define-key project-prefix-map "m" 'magit-project-status)

;;; MAN

(define-key help-map "\M-m" 'man)

;;; MINIBUFFER

(define-key completion-in-region-mode-map "\M-v" 'switch-to-completions)
(define-key minibuffer-local-must-match-map "\C-j" 'minibuffer-force-complete-and-exit)

;;; MPC

(define-key mode-specific-map "os" 'mpc)

;;;; MPC BINDINGS

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
  (define-key mpc-songs-mode-map [remap mpc-select] nil)
  (load "/home/val/.config/emacs/mpc-fixes.el"))

;;; MULE

(set-terminal-coding-system 'utf-8)

;;; MULE CMDS

(prefer-coding-system 'utf-8)

;;; MU4E

(autoload 'mu4e "mu4e" nil t)
(define-key mode-specific-map "om" 'mu4e)

(defvar mu4e-contexts)
(declare-function make-mu4e-context "mu4e-context")
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

;;; NATIVE COMPLETE

(with-eval-after-load 'comint (add-hook 'comint-dynamic-complete-functions 'native-complete-at-point))
(with-eval-after-load 'shell (native-complete-setup-bash))

;;; NET UTILS

(define-key mode-specific-map "nh" 'nslookup-host)
(define-key mode-specific-map "ni" 'ifconfig)
(define-key mode-specific-map "nn" 'netstat)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "nw" 'iwconfig)

;;; NEWSTICKER

(defvar newsticker-treeview-mode-map)
(define-key mode-specific-map "on" 'newsticker-show-news)
(with-eval-after-load 'newst-treeview
  (load "/home/val/.config/emacs/newsticker-extra.el")
  (define-key newsticker-treeview-mode-map "w" 'newsticker-extra-treeview-copy-link))

;;; NOV

(add-to-list 'auto-mode-alist (cons (rx ".epub" eos) 'nov-mode))

;;; NOVICE

(setq disabled-command-function nil)

;;; ORG AGENDA

(define-key mode-specific-map "Ga" 'org-agenda)

;;; ORG CAPTURE

(define-key mode-specific-map "Gc" 'org-capture)

;;; ORG MIME

(defvar message-mode-map)
(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\M-o" 'org-mime-htmlize)
  (define-key message-mode-map "\C-c\M-e" 'org-mime-edit-mail-in-org-mode)
  (define-key message-mode-map "\C-c\M-t" 'org-mime-revert-to-plain-text-mail))
(with-eval-after-load 'org-mime (load "/home/val/.config/emacs/org-mime-fixes.el"))

;;; OUTLINE

(with-eval-after-load 'outline
  (defun outline-show-after-jump ()
    (when outline-minor-mode (outline-show-entry))))

;;; OUTLINE MINOR FACES

(add-hook 'outline-minor-mode-hook 'outline-minor-faces-add-font-lock-keywords)

;;; PARAGRAPHS

(define-key global-map [?\C-\M-\S-t] 'transpose-paragraphs)

;;; PDF TOOLS

(pdf-loader-install t t)

;;; PP

(define-key emacs-lisp-mode-map "\C-cm" 'pp-macroexpand-last-sexp)
(define-key lisp-interaction-mode-map "\C-cm" 'pp-macroexpand-last-sexp)

;;; PROCED

(define-key mode-specific-map "op" 'proced)

;;; PUEUE

(define-key mode-specific-map "ou" 'pueue)

;;; REGISTER

(define-key ctl-x-r-map "v" 'view-register)
(define-key ctl-x-r-map "L" 'list-registers)
(define-key ctl-x-r-map "p" 'prepend-to-register)
(define-key ctl-x-r-map "a" 'append-to-register)

;;; REVERSE IM

(require 'reverse-im)
(reverse-im-activate "cyrillic-dvorak")

;;; RG

(define-key search-map "rr" 'rg)
(define-key search-map "rt" 'rg-literal)
(define-key search-map "rp" 'rg-project)

;;; SAVEHIST

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

;;; SDCV

(define-key mode-specific-map "ot" 'sdcv-search-input)
(with-eval-after-load 'sdcv
  (defun sdcv-args-force-utf (args)
    (cl-list* "--utf8-output" "--utf8-input" args))
  (advice-add 'sdcv-search-with-dictionary-args :filter-return 'sdcv-args-force-utf))

;;; SGML MODE

(defvar sgml-mode-map)
(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char))

;;; SHELL PWD

(define-key mode-specific-map "xs" 'shell-pwd-shell)
(define-key mode-specific-map "xS" 'shell-pwd-list-buffers)
(defvar shell-mode-map)
(with-eval-after-load 'shell
  (define-key shell-mode-map "\C-c\M-d" 'shell-pwd-change-directory))

;;; SIMPLE

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
(define-key ctl-x-map "K" 'kill-current-buffer)
(define-key mode-specific-map "oP" 'list-processes)

;;; SKEMPO

(add-hook 'nix-mode-hook 'skempo-mode)
(add-hook 'js-mode-hook 'skempo-mode)
(with-eval-after-load 'skempo
  (define-key skempo-mode-map "\C-z" 'skempo-complete-tag-or-call-on-region)
  (define-key skempo-mode-map "\M-g\M-e" 'skempo-forward-mark)
  (define-key skempo-mode-map "\M-g\M-a" 'skempo-backward-mark)
  (with-eval-after-load 'elisp-mode (load "/home/val/.config/emacs/skempo-emacs-lisp.el"))
  (with-eval-after-load 'sly (load "/home/val/.config/emacs/skempo-lisp.el"))
  (with-eval-after-load 'nix-mode (load "/home/val/.config/emacs/skempo-nix.el"))
  (with-eval-after-load 'php-mode (load "/home/val/.config/emacs/skempo-php.el"))
  (with-eval-after-load 'js (load "/home/val/.config/emacs/skempo-js.el")))

;;; SMARTPARENS

(with-eval-after-load 'smartparens (require 'smartparens-config))

;;;; SMARTPARENS HOOKS

(add-hook 'minibuffer-setup-hook 'smartparens-mode)
(add-hook 'nix-mode-hook 'smartparens-mode)
(add-hook 'rust-mode-hook 'smartparens-mode)
(add-hook 'js-mode-hook 'smartparens-mode)
(add-hook 'restclient-mode-hook 'smartparens-mode)
(add-hook 'smartparens-mode-hook 'show-smartparens-mode)

;;;; SMARTPARENS DEFS

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

;;;; SMARTPARENS BINDINGS

(define-key global-map [?\C-\(] 'sp-backward-slurp-sexp)
(define-key global-map [?\C-\)] 'sp-forward-slurp-sexp)
(define-key global-map [?\C-\M-\(] 'sp-backward-barf-sexp)
(define-key global-map [?\C-\M-\)] 'sp-forward-barf-sexp)
(define-key global-map "\C-\M-w" 'sp-copy-sexp)
(define-key global-map "\M-[" 'sp-unwrap-sexp)
(define-key global-map "\M-]" 'sp-rewrap-sexp)

(define-key smartparens-mode-map "\C-\M-u" 'sp-backward-up-sexp)
(define-key smartparens-mode-map "\C-\M-d" 'sp-down-sexp)
(define-key smartparens-mode-map "\C-\M-t" 'sp-transpose-sexp)
(define-key smartparens-mode-map "\C-\M-k" 'sp-kill-sexp)
(define-key smartparens-mode-map "\M-d" 'sp-kill-word)
(define-key smartparens-mode-map "\C-w" 'sp-kill-region-dwim)

;;; SUBWORD

(add-hook 'rust-mode-hook 'subword-mode)
(add-hook 'nix-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)

;;; TEX MODE

(defvar ispell-parser)
(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

;;; TRANSMISSION

(defvar transmission-mode-map)
(define-key mode-specific-map "or" 'transmission)
(with-eval-after-load 'transmission (define-key transmission-mode-map "M" 'transmission-move))

;;; VCOMPLETE

(with-eval-after-load 'vcomplete
  (define-key vcomplete-command-map [?\C-n] nil)
  (define-key vcomplete-command-map [?\C-p] nil)
  (define-key vcomplete-command-map [?\C-\M-m] nil)
  (define-key vcomplete-command-map "\M-v" 'switch-to-completions))

;;; WEB MODE

(add-to-list 'auto-mode-alist (cons (rx ".twig" eos) 'web-mode))

;;; WINDOW

(define-key global-map "\M-V" 'scroll-down-line)
(define-key global-map [?\C-\S-v] 'scroll-up-line)
(define-key global-map [?\C-\M-\S-b] 'previous-buffer)
(define-key global-map [?\C-\M-\S-f] 'next-buffer)
(define-key global-map "\M-Q" 'quit-window)
(define-key global-map "\M-o" 'other-window)

;;; LOAD CUSTOM

(load "/home/val/.config/nixpkgs/emacs/custom.el" nil nil t)
