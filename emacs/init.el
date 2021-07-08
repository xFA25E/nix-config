;;; -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

;;; ANSI COLOR

(with-eval-after-load 'compile
  (defun colorize-compilation ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'colorize-compilation))

;;; AVY

(define-key global-map "\M-z" 'avy-goto-word-0)
(define-key goto-map "\M-g" 'avy-goto-line)

;;; BROWSE URL

(define-key ctl-x-map "B" 'browse-url)
(define-key mode-specific-map "oy" 'browse-url-multi-youtube-search)

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
(define-key goto-map "F" 'consult-flymake)
(define-key project-prefix-map "i" 'consult-project-imenu)
(define-key kmacro-keymap "c" 'consult-kmacro)

;;; CSS MODE

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

(with-eval-after-load 'dired
  (define-key dired-mode-map "r" 'dired-rsync)
  (define-key dired-mode-map "\M-+"'dired-create-empty-file)
  (dired-async-mode))

;;; DIRED AUX

(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               (list (rx ".tar.bz2" eos) "" "bunzip2 -dc %i | tar -xf -")))

;;; DIRED X

(with-eval-after-load 'dired (require 'dired-x))

;;; DUMB JUMP

(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;;; EBDB

(with-eval-after-load 'ebdb-com
  (define-key ebdb-mode-map "\C-cm" 'ebdb-complete-push-mail-and-quit-window)
  (define-key ebdb-mode-map "\C-cM" 'ebdb-complete-push-mail))

(with-eval-after-load 'org-agenda
  (unless (bound-and-true-p ebdb-db-list)
    (ebdb-load)))

(with-eval-after-load 'mu4e-view
  (require 'ebdb-mu4e))

(with-eval-after-load 'message
  (require 'ebdb-message)
  (define-key message-mode-map "\C-ce" 'ebdb-complete))

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

(with-eval-after-load 'eww
  (defun eww-browse-url-custom ()
    (interactive)
    (let ((browse-url-browser-function (default-value 'browse-url-browser-function)))
      (when-let ((url-at-point (car (eww-links-at-point))))
        (browse-url url-at-point))))
  (define-key eww-mode-map "V" 'eww-browse-url-custom))

;;; FACES

(define-key help-map "\M-f" 'list-faces-display)

;;; FIND DIRED

(define-key search-map "n" 'find-name-dired)
(define-key search-map "N" 'find-dired)

;;; FIND FUNC

(define-key ctl-x-map "L" 'find-library)
(define-key ctl-x-map "F" 'find-function)
(define-key ctl-x-map "K" 'find-function-on-key)
(define-key ctl-x-map "V" 'find-variable)

;;; FINDER

(define-key help-map "\M-c" 'finder-commentary)

;;; FLYMAKE

(with-eval-after-load 'flymake
  (define-key flymake-mode-map "\M-g\M-f" 'flymake-goto-next-error)
  (define-key flymake-mode-map "\M-g\M-b" 'flymake-goto-prev-error))

;;; GREP

(define-key search-map "g" 'rgrep)
(with-eval-after-load 'grep
  (define-advice grep-expand-template (:filter-return (cmd) add-cut)
    (concat cmd " | cut -c-500")))

;;; HELP FNS

(define-key help-map "\M-k" 'describe-keymap)

;;; HIPPIE EXP

(define-key global-map "\C-_" 'hippie-expand)

;;; HL LINE

(add-hook 'csv-mode-hook 'hl-line-mode)
(add-hook 'grep-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'transmission-files-mode-hook 'hl-line-mode)
(add-hook 'transmission-mode-hook 'hl-line-mode)
(add-hook 'transmission-peers-mode-hook 'hl-line-mode)
(add-hook 'mpc-mode-hook 'hl-line-mode)

;;; IPRETTY

(define-key lisp-interaction-mode-map "\C-j" 'ipretty-last-sexp)

;;; ISEARCH

(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key isearch-mode-map "\C-?" isearch-help-map)

;;; LINK HINT

(define-key goto-map "\M-l" 'link-hint-open-link)
(define-key goto-map "\M-L" 'link-hint-copy-link)
(with-eval-after-load 'link-hint
  (cl-pushnew 'rg-mode (get 'link-hint-compilation-link :vars)))

;;; LOCATE

(define-key search-map "l" 'locate)

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
  (define-key mpc-songs-mode-map [remap mpc-select] nil))

;;; MU4E

(autoload 'mu4e "mu4e" nil t)
(autoload 'mu4e~compose-mail "mu4e-compose")
(define-mail-user-agent 'mu4e-user-agent 'mu4e~compose-mail 'message-send-and-exit 'message-kill-buffer 'message-send-hook)
(define-key mode-specific-map "om" 'mu4e)

(with-eval-after-load 'mu4e-context
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

;;; NET UTILS

(define-key mode-specific-map "nh" 'nslookup-host)
(define-key mode-specific-map "ni" 'ifconfig)
(define-key mode-specific-map "nn" 'netstat)
(define-key mode-specific-map "np" 'ping)
(define-key mode-specific-map "nw" 'iwconfig)

;;; NEWSTICKER

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

(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
(with-eval-after-load 'message
  (define-key message-mode-map "\C-c\M-o" 'org-mime-htmlize)
  (define-key message-mode-map "\C-c\M-e" 'org-mime-edit-mail-in-org-mode)
  (define-key message-mode-map "\C-c\M-t" 'org-mime-revert-to-plain-text-mail))

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

(with-eval-after-load 'cyrillic-dvorak-im
  (require 'reverse-im)
  (reverse-im-activate "cyrillic-dvorak"))

;;; RG

(define-key search-map "r" 'rg-menu)

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
  (define-advice sdcv-search-with-dictionary-args (:filter-return (args) utf)
    (cl-list* "--utf8-output" "--utf8-input" args)))

;;; SGML MODE

(with-eval-after-load 'sgml-mode
  (define-key sgml-mode-map "\C-\M-n" 'sgml-skip-tag-forward)
  (define-key sgml-mode-map "\C-\M-p" 'sgml-skip-tag-backward)
  (define-key sgml-mode-map "\C-c\C-r" 'sgml-namify-char))

;;; SHELL PWD

(define-key mode-specific-map "xs" 'shell-pwd-shell)
(define-key mode-specific-map "xS" 'shell-pwd-list-buffers)
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
(define-key mode-specific-map "oP" 'list-processes)

;;; SKEMPO

(add-hook 'nix-mode-hook 'skempo-mode)
(add-hook 'js-mode-hook 'skempo-mode)
(with-eval-after-load 'skempo
  (define-key skempo-mode-map "\C-z" 'skempo-complete-tag-or-call-on-region)
  (define-key skempo-mode-map "\M-g\M-e" 'skempo-forward-mark)
  (define-key skempo-mode-map "\M-g\M-a" 'skempo-backward-mark)
  (load "/home/val/.config/emacs/skempo-templates.el"))

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

(add-hook 'tex-mode-hook (lambda nil (setq-local ispell-parser 'tex)))

;;; TRANSMISSION

(define-key mode-specific-map "or" 'transmission)
(with-eval-after-load 'transmission (define-key transmission-mode-map "M" 'transmission-move))

;;; URL PARSE

(with-eval-after-load 'url-parse
  (define-advice url-generic-parse-url (:around (fn &rest args) save-match-data)
    (save-match-data (apply fn args))))

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
