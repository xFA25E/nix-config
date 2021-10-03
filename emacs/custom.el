(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(abbrev-file-name
   (expand-file-name "nixpkgs/emacs/abbrev_defs"
                     (xdg-config-home)))
 '(abbrev-suggest t)
 '(after-save-hook '(executable-make-buffer-file-executable-if-script-p))
 '(ange-ftp-netrc-filename "~/.authinfo.gpg")
 '(apropos-sort-by-scores t)
 '(async-shell-command-buffer 'new-buffer)
 '(auth-sources '("~/.authinfo.gpg" "~/.netrc" "~/.authinfo"))
 '(auto-insert-alist
   '((("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
      (replace-regexp-in-string "[^A-Z0-9]" "_"
                                (replace-regexp-in-string "\\+" "P"
                                                          (upcase
                                                           (file-name-nondirectory buffer-file-name))))
      "#ifndef " str n "#define " str "

" _ "

#endif")
     (("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program")
      nil "#include \""
      (let
          ((stem
            (file-name-sans-extension buffer-file-name))
           ret)
        (dolist
            (ext
             '("H" "h" "hh" "hpp" "hxx" "h++")
             ret)
          (when
              (file-exists-p
               (concat stem "." ext))
            (setq ret
                  (file-name-nondirectory
                   (concat stem "." ext))))))
      & 34 | -10)
     (("[Mm]akefile\\'" . "Makefile")
      . "makefile.inc")
     (html-mode lambda nil
                (sgml-tag "html"))
     (plain-tex-mode . "tex-insert.tex")
     (bibtex-mode . "tex-insert.tex")
     (latex-mode "options, RET: " "\\documentclass[" str & 93 | -1 123
                 (read-string "class: ")
                 "}
"
                 ("package, %s: " "\\usepackage["
                  (read-string "options, RET: ")
                  & 93 | -1 123 str "}
")
                 _ "
\\begin{document}
" _ "
\\end{document}")
     (("/bin/.*[^/]\\'" . "Shell-Script mode magic number")
      lambda nil
      (if
          (eq major-mode
              (default-value 'major-mode))
          (sh-mode)))
     (ada-mode . ada-header)
     (("\\.[1-9]\\'" . "Man page skeleton")
      "Short description: " ".\\\" Copyright (C), "
      (format-time-string "%Y")
      "  "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      "
.\\\" You may distribute this file under the terms of the GNU Free
.\\\" Documentation License.
.TH "
      (file-name-base
       (buffer-file-name))
      " "
      (file-name-extension
       (buffer-file-name))
      " "
      (format-time-string "%Y-%m-%d ")
      "
.SH NAME
"
      (file-name-base
       (buffer-file-name))
      " \\- " str "
.SH SYNOPSIS
.B "
      (file-name-base
       (buffer-file-name))
      "
" _ "
.SH DESCRIPTION
.SH OPTIONS
.SH FILES
.SH \"SEE ALSO\"
.SH BUGS
.SH AUTHOR
"
      (user-full-name)
      '(if
           (search-backward "&"
                            (line-beginning-position)
                            t)
           (replace-match
            (capitalize
             (user-login-name))
            t t))
      '(end-of-line 1)
      " <"
      (progn user-mail-address)
      ">
")
     (".dir-locals.el" nil ";;; Directory Local Variables
" ";;; For more information see (info \"(emacs) Directory Variables\")

" "(("
'(setq v1
       (let
           (modes)
         (mapatoms
          (lambda
            (mode)
            (let
                ((name
                  (symbol-name mode)))
              (when
                  (string-match "-mode$" name)
                (push name modes)))))
         (sort modes 'string<)))
(completing-read "Local variables for mode: " v1 nil t)
" . (("
(let
    ((all-variables
      (apropos-internal ".*"
                        (lambda
                          (symbol)
                          (and
                           (boundp symbol)
                           (get symbol 'variable-documentation))))))
  (completing-read "Variable to set: " all-variables))
" . "
(completing-read "Value to set it to: " nil)
"))))
")
     (("\\.el\\'" . "Emacs Lisp header")
      "Short description: " ";;; "
      (file-name-nondirectory
       (buffer-file-name))
      " --- " str
      (make-string
       (max 2
            (- 80
               (current-column)
               27))
       32)
      "-*- lexical-binding: t; -*-"
      '(setq lexical-binding t)
      "

;; Copyright (C) "
      (format-time-string "%Y")
      "  "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      "

;; Author: "
      (user-full-name)
      '(if
           (search-backward "&"
                            (line-beginning-position)
                            t)
           (replace-match
            (capitalize
             (user-login-name))
            t t))
      '(end-of-line 1)
      " <"
      (progn user-mail-address)
      ">
;; Keywords: "
      '(require 'finder)
      '(setq v1
             (mapcar
              (lambda
                (x)
                (list
                 (symbol-name
                  (car x))))
              finder-known-keywords)
             v2
             (mapconcat
              (lambda
                (x)
                (format "%12s:  %s"
                        (car x)
                        (cdr x)))
              finder-known-keywords "
"))
      ((let
           ((minibuffer-help-form v2))
         (completing-read "Keyword, C-h: " v1 nil t))
       str ", ")
      & -2 "

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; " _ "

;;; Code:



(provide '"
      (file-name-base
       (buffer-file-name))
      ")
;;; "
      (file-name-nondirectory
       (buffer-file-name))
      " ends here
")
     (("\\.texi\\(nfo\\)?\\'" . "Texinfo file skeleton")
      "Title: " "\\input texinfo   @c -*-texinfo-*-
@c %**start of header
@setfilename "
      (file-name-base
       (buffer-file-name))
      ".info
" "@settitle " str "
@c %**end of header
@copying
"
      (setq short-description
            (read-string "Short description: "))
      ".

" "Copyright @copyright{} "
      (format-time-string "%Y")
      "  "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      "

@quotation
Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
A copy of the license is included in the section entitled ``GNU
Free Documentation License''.

A copy of the license is also available from the Free Software
Foundation Web site at @url{https://www.gnu.org/licenses/fdl.html}.

@end quotation

The document was typeset with
@uref{https://www.gnu.org/software/texinfo/, GNU Texinfo}.

@end copying

@titlepage
@title " str "
@subtitle " short-description "
@author "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      " <"
      (progn user-mail-address)
      ">
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@c Output the table of the contents at the beginning.
@contents

@ifnottex
@node Top
@top " str "

@insertcopying
@end ifnottex

@c Generate the nodes for this menu with `C-c C-u C-m'.
@menu
@end menu

@c Update all node entries with `C-c C-u C-n'.
@c Insert new nodes with `C-c C-c n'.
@node Chapter One
@chapter Chapter One

" _ "

@node Copying This Manual
@appendix Copying This Manual

@menu
* GNU Free Documentation License::  License for copying this manual.
@end menu

@c Get fdl.texi from https://www.gnu.org/licenses/fdl.html
@include fdl.texi

@node Index
@unnumbered Index

@printindex cp

@bye

@c "
      (file-name-nondirectory
       (buffer-file-name))
      " ends here
")
     (lisp-mode .
                [(nil "(in-package :cl)
")
                 skeleton-template-lisp-defpackage])))
 '(auto-insert-mode t)
 '(auto-revert-avoid-polling t)
 '(auto-revert-mode-text " AR")
 '(auto-revert-remote-files t)
 '(auto-save-file-name-transforms
   (list
    (list ".*"
          (expand-file-name "emacs/auto-saves/"
                            (xdg-cache-home))
          t)))
 '(auto-save-list-file-prefix
   (expand-file-name "emacs/auto-saves-list/.saves-"
                     (xdg-cache-home)))
 '(avy-background t)
 '(avy-goto-word-0-regexp "\\_<\\(\\sw\\|\\s_\\)")
 '(avy-keys '(97 111 101 117 104 116 110 115))
 '(avy-style 'words)
 '(backup-by-copying t)
 '(backup-directory-alist
   (list
    (cons ".*"
          (expand-file-name "emacs/backups"
                            (xdg-data-home)))))
 '(before-save-hook '(whitespace-cleanup delete-trailing-whitespace))
 '(blink-cursor-mode nil)
 '(bookmark-default-file (expand-file-name "emacs/bookmarks" (xdg-data-home)))
 '(bookmark-fontify nil)
 '(bookmark-menu-confirm-deletion t)
 '(bookmark-save-flag 1)
 '(browse-url-browser-function 'browse-url-multi)
 '(browse-url-generic-program "brave-incognito")
 '(browse-url-multi-answers
   '(("brave" 98 "Open in brave" browse-url-generic)
     ("eww" 101 "Open in eww" eww-browse-url)
     ("ytdli" 121 "Download with ytdli" browse-url-multi-ytdli)
     ("mpvi" 109 "Open in mpvi" browse-url-multi-mpvi)
     ("invidious" 105 "Open as invidious url in eww" browse-url-multi-invidious)
     ("firefox" 102 "Open in firefox" browse-url-firefox)
     ("transmission" 116 "Add to transmission" browse-url-multi-transmission)))
 '(browse-url-multi-invidious-instances
   '("vid.puffyan.us" "yewtu.be" "ytprivate.com" "invidious.kavin.rocks"))
 '(browse-url-secondary-browser-function 'browse-url-multi)
 '(byte-count-to-string-function '(lambda (s) (file-size-human-readable s 'si)))
 '(c-default-style '((java-mode . "java") (other . "awk")))
 '(calendar-week-start-day 1)
 '(cargo-process--command-build "build --color never")
 '(cargo-process--command-check "check --color never")
 '(cargo-process--command-clippy "clippy --color never -Zunstable-options")
 '(cargo-process--command-current-file-tests "test --color never")
 '(cargo-process--command-current-test "test --color never")
 '(cargo-process--command-rm "rm --color never")
 '(cargo-process--command-run "run --color never")
 '(cargo-process--command-test "test --color never")
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10000)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 10000)
 '(comint-mode-hook '(smartparens-mode))
 '(comint-password-prompt-regexp
   (rx
    (or
     (regexp
      (eval
       (car
        (get 'comint-password-prompt-regexp 'standard-value))))
     (and
      (any "Pp")
      "assword " eos))))
 '(compilation-always-kill t)
 '(compilation-scroll-output t)
 '(completion-category-overrides '((bookmark (styles basic))))
 '(completion-cycle-threshold 2)
 '(completion-show-help nil)
 '(completion-styles '(partial-completion flex))
 '(completions-detailed t)
 '(completions-format 'one-column)
 '(completions-group t)
 '(create-lockfiles nil)
 '(custom-file
   (expand-file-name "nixpkgs/emacs/custom.el"
                     (xdg-config-home)))
 '(delete-old-versions t)
 '(diary-file "~/org/diary")
 '(dired-async-mode-lighter "")
 '(dired-create-destination-dirs 'ask)
 '(dired-dwim-target t)
 '(dired-guess-shell-alist-user
   (list
    (list
     (rx "."
         (or "csv" "doc" "docx" "odp" "ods" "odt" "ppt" "pptx" "xls" "xlsx")
         eos)
     "setsid -f libreoffice * >/dev/null 2>&1" "libreoffice --invisible --headless --convert-to pdf * &")
    (list
     (rx "."
         (or "bmp" "gif" "jfif" "jpeg" "jpg" "nef" "png" "thm" "tif" "webp" "xpm")
         eos)
     "setsid -f sxiv * >/dev/null 2>&1" "setsid -f gimp * >/dev/null 2>&1")
    (list
     (rx "."
         (or "ai" "eps")
         eos)
     "setsid -f inkscape * >/dev/null 2>&1" "setsid -f gimp * >/dev/null 2>&1")
    (list
     (rx "."
         (or "djvu" "fb2")
         eos)
     "ebook-convert ? .epub &")
    (list
     (rx ".pdf" eos)
     "setsid -f libreoffice * >/dev/null 2>&1" "setsid -f gimp * >/dev/null 2>&1")
    (list
     (rx "."
         (or "3gp" "aiff" "avi" "flac" "flv" "m4a" "mkv" "mov" "mp3" "mp4" "mpg" "ogg" "ogv" "opus" "vob" "wav" "webm" "wmv")
         eos)
     "setsid -f mpv --profile=gui * >/dev/null 2>&1" "video_duration * | format_duration" "video_duration * | awk '{s+=$1}END{print s}' | format_duration" "compress_video * &" "strip_video * &" "mediainfo" "mpv -vo=drm")
    (list
     (rx ".cue" eos)
     "setsid -f mpv --profile=gui * >/dev/null 2>&1")
    (list
     (rx ".rar" eos)
     "temp=\"$(echo `?` | rev | cut -d. -f 2- | rev)\"; mkdir -p \"${temp}\"; unrar x ? \"${temp}\"")
    (list
     (rx ".torrent" eos)
     "transmission-show")
    (list
     (rx ".epub" eos)
     "ebook-convert ? .mobi &")))
 '(dired-listing-switches "-lFAv --si --group-directories-first")
 '(dired-ls-F-marks-symlinks t)
 '(dired-mode-hook '(dired-hide-details-mode hl-line-mode))
 '(display-battery-mode t)
 '(display-time-default-load-average nil)
 '(display-time-mail-function
   '(lambda nil
      (let
          ((count
            (car
             (process-lines "notmuch" "count" "tag:unread"))))
        (unless
            (string= "0" count)
          count))))
 '(display-time-mode t)
 '(display-time-string-forms
   '((format-time-string "%a %b %e %H:%M" now)
     (if mail
         (concat " @" mail)
       "")))
 '(ebdb-complete-mail nil)
 '(ebdb-complete-mail-allow-cycling nil)
 '(ebdb-completion-display-record nil)
 '(ebdb-record-self "5ecfc8f5-f490-4745-8cf1-86b1964e4ab7")
 '(ebdb-sources (expand-file-name "emacs/ebdb" (xdg-data-home)))
 '(ebdb-user-mail-address-re 'self)
 '(ede-project-placeholder-cache-file (expand-file-name "emacs/ede/projects.el" (xdg-cache-home)))
 '(ediff-before-setup-hook
   '((lambda
       (&rest _)
       (window-configuration-to-register 119))))
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-sync-connect nil)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string "")
 '(emacs-lisp-mode-hook '(skempo-mode flymake-mode smartparens-mode abbrev-mode))
 '(enable-recursive-minibuffers t)
 '(eshell-directory-name (expand-file-name "emacs/eshell/" (xdg-cache-home)))
 '(eval-expression-print-length t)
 '(eval-expression-print-level t)
 '(eww-bookmarks-directory (expand-file-name "emacs/" (xdg-data-home)))
 '(eww-browse-url-new-window-is-tab nil)
 '(eww-search-prefix "https://ddg.co/lite/?q=")
 '(executable-chmod 64)
 '(fd-dired-ls-option
   '("| xargs -0 ls -ldF --si --quoting-style=literal" . "-ldhF"))
 '(fill-column 80)
 '(find-ls-option
   '("-print0 | xargs -0 ls -ldF --si --quoting-style=literal" . "-ldhF"))
 '(flymake-mode-line-format
   '(" " flymake-mode-line-exception flymake-mode-line-counters))
 '(flymake-no-changes-timeout nil)
 '(flyspell-default-dictionary "russian")
 '(flyspell-dictionaries-that-consider-dash-as-word-delimiter '("francais" "deutsch8" "norsk" "russian"))
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(global-so-long-mode t)
 '(grep-files-aliases
   '(("php" . "*.php *.phtml")
     ("all" . "* .[!.]* ..?*")
     ("el" . "*.el")
     ("ch" . "*.[ch]")
     ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]")))
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-all-abbrevs))
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(hscroll-step 1)
 '(html-mode-hook '(emmet-mode))
 '(ibuffer-default-sorting-mode 'major-mode)
 '(ibuffer-show-empty-filter-groups nil)
 '(image-dired-db-enabled t)
 '(image-dired-db-file (expand-file-name "emacs/image-dired-db" (xdg-data-home)))
 '(image-dired-dir
   (expand-file-name "emacs/image-dired/thumbnails/"
                     (xdg-cache-home)))
 '(image-dired-external-viewer "image-dired-external-viewer")
 '(image-dired-gallery-dir
   (expand-file-name "emacs/image-dired/gallery/"
                     (xdg-cache-home)))
 '(image-dired-temp-image-file
   (expand-file-name "emacs/image-dired/temp"
                     (xdg-cache-home)))
 '(image-dired-temp-rotate-image-file
   (expand-file-name "emacs/image-dired/rotate_temp"
                     (xdg-cache-home)))
 '(image-file-name-extensions
   '("mp4" "mkv" "png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg"))
 '(imenu-auto-rescan t)
 '(imenu-level-separator "/")
 '(imenu-space-replacement " ")
 '(imenu-use-popup-menu nil)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inferior-lisp-program "nix-shell -p sbcl --run sbcl")
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(isearch-lazy-count t)
 '(ispell-local-dictionary-alist
   '(("russian" "[А-Яа-яA-Za-z]" "[^А-Яа-яA-Za-z]" "[-]" nil
      ("-d" "ru_RU,en_US")
      "~tex" utf-8)
     ("english" "[A-Za-z]" "[^A-Za-z]" "[-']" nil nil "~tex" utf-8)))
 '(ispell-program-name "hunspell")
 '(java-mode-hook '(subword-mode))
 '(js-indent-level 2)
 '(kept-new-versions 10)
 '(kill-do-not-save-duplicates t)
 '(kill-read-only-ok t)
 '(leaf-expand-minimally t)
 '(ledger-default-date-format "%Y-%m-%d")
 '(link-hint-types
   '(link-hint-shr-url link-hint-org-link link-hint-markdown-link link-hint-help-link link-hint-info-link link-hint-package-link link-hint-package-keyword-link link-hint-package-install-link link-hint-epkg-button link-hint-compilation-link link-hint-nov-link link-hint-customize-widget link-hint-notmuch-hello link-hint-button link-hint-completion-list-candidate link-hint-text-url link-hint-file-link link-hint-org-agenda-item link-hint-xref-item link-hint-man-button link-hint-dired-filename))
 '(lisp-mode-hook
   '(skempo-mode smartparens-mode sly-editing-mode abbrev-mode))
 '(magit-credential-cache-daemon-socket (expand-file-name "git/credential/socket" (xdg-cache-home)))
 '(magit-define-global-key-bindings nil)
 '(mail-envelope-from 'header)
 '(mail-user-agent 'notmuch-user-agent)
 '(marginalia-annotator-registry
   '((command marginalia-annotate-command marginalia-annotate-binding builtin none)
     (customize-group marginalia-annotate-customize-group builtin none)
     (variable marginalia-annotate-variable builtin none)
     (function marginalia-annotate-function builtin none)
     (face marginalia-annotate-face builtin none)
     (color marginalia-annotate-color builtin none)
     (unicode-name marginalia-annotate-char builtin none)
     (minor-mode marginalia-annotate-minor-mode builtin none)
     (symbol marginalia-annotate-symbol builtin none)
     (environment-variable marginalia-annotate-environment-variable builtin none)
     (input-method marginalia-annotate-input-method builtin none)
     (coding-system marginalia-annotate-coding-system builtin none)
     (charset marginalia-annotate-charset builtin none)
     (package marginalia-annotate-package builtin none)
     (imenu marginalia-annotate-imenu builtin none)
     (bookmark marginalia-annotate-bookmark builtin none)
     (buffer marginalia-annotate-buffer builtin none)
     (consult-multi marginalia-annotate-consult-multi builtin none)))
 '(marginalia-mode t)
 '(max-mini-window-height 1.0)
 '(menu-bar-mode nil)
 '(message-directory "~/.mail/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function 'message-send-mail-with-sendmail)
 '(message-subject-re-regexp
   (rx bol
       (* blank)
       (*
        (or "R" "RE" "Re" "Ris")
        (* "["
           (* digit)
           "]")
        (32 " ")
        ":"
        (* blank))))
 '(minibuffer-beginning-of-buffer-movement t)
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-eldef-shorten-default t)
 '(minibuffer-electric-default-mode t)
 '(mpc-browser-tags '(file))
 '(mpc-data-directory (expand-file-name "emacs/mpc" (xdg-cache-home)))
 '(mpc-mpd-music-directory "~/Music")
 '(mpc-songs-format "%-6{Time} %{file}")
 '(newsticker-automatically-mark-items-as-old nil)
 '(newsticker-automatically-mark-visited-items-as-old nil)
 '(newsticker-dir (expand-file-name "emacs/newsticker" (xdg-cache-home)))
 '(newsticker-obsolete-item-max-age 31536000)
 '(newsticker-retrieval-interval 0)
 '(newsticker-retrieval-method 'extern)
 '(newsticker-treeview-automatically-mark-displayed-items-as-old nil)
 '(newsticker-treeview-listwindow-height 6)
 '(newsticker-treeview-treewindow-width 30)
 '(newsticker-url-list
   '(("The Alternative Hypothesis Bitchute" "https://www.bitchute.com/feeds/rss/channel/thealthype" nil nil nil)
     ("The Alternative Hypothesis Website" "http://thealternativehypothesis.org/index.php/feed" nil nil nil)
     ("American Renaissance" "https://www.bitchute.com/feeds/rss/channel/amrenaissance" nil nil nil)
     ("Mouthy Buddha" "https://www.bitchute.com/feeds/rss/channel/mouthybuddha" nil nil nil)
     ("TealDeer" "https://www.bitchute.com/feeds/rss/channel/tealdeer" nil nil nil)
     ("Простая Академия" "https://www.youtube.com/feeds/videos.xml?channel_id=UC8mmPf2oKdfE2pdjqctTWUw" nil nil nil)
     ("Простые Мысли" "https://www.youtube.com/feeds/videos.xml?channel_id=UCZuRMfF5ZUHqYlKkvU12xvg" nil nil nil)
     ("Простая Академия Сайт" "https://prosto.academy/feed/" nil nil nil)
     ("Luke Smith Blog" "https://lukesmith.xyz/rss.xml" nil nil nil)
     ("Luke Smith PeerTube" "https://lukesmith.xyz/peertube" nil nil nil)
     ("Protesilaos Stavrou" "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" nil nil nil)
     ("WhatifAltHist" "https://www.youtube.com/feeds/videos.xml?user=WhatifAltHist" nil nil nil)
     ("JFG Tonight" "https://www.bitchute.com/feeds/rss/channel/jfgtonight" nil nil nil)
     ("Sean Last" "https://www.youtube.com/feeds/videos.xml?channel_id=UCK1Uk2f36aglexxLkfOWnEQ" nil nil nil)
     ("Ideas And Data" "https://ideasanddata.wordpress.com/feed/" nil nil nil)
     ("Паучительные Истории" "https://www.youtube.com/feeds/videos.xml?channel_id=UC4rpWi42yPqTA0wnfx7MqOA" nil nil nil)
     ("КоверАраб" "https://www.youtube.com/feeds/videos.xml?channel_id=UCjulQNQQJmpYzI-BD1-s03w" nil nil nil)
     ("White Mage" "https://www.youtube.com/feeds/videos.xml?channel_id=UC1k29QaI7FKn5wki72Lyy7w" nil nil nil)
     ("Emil O. W. Kirkegaard" "https://www.youtube.com/feeds/videos.xml?channel_id=UCbT_SxIN5oMSb7bjYkE8FpQ" nil nil nil)))
 '(newsticker-url-list-defaults nil)
 '(next-screen-context-lines 10)
 '(notmuch-address-internal-completion '(received nil))
 '(notmuch-address-use-company nil)
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-archive-tags '("+archive" "-inbox" "-spam" "-trash" "-deleted"))
 '(notmuch-fcc-dirs '(("polimi" . "polimi/sent +sent +polimi")))
 '(notmuch-mua-cite-function 'message-cite-original-without-signature)
 '(notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
 '(notmuch-saved-searches
   '((:name "unread all" :query "tag:unread" :key "ua")
     (:name "unread polimi" :query "tag:unread and tag:polimi" :key "up")
     (:name "unread spam" :query "tag:unread and tag:spam" :key "us")
     (:name "inbox all" :query "tag:inbox" :key "ia")
     (:name "inbox polimi" :query "tag:polimi and tag:inbox" :key "ip")
     (:name "sent all" :query "tag:sent" :key "sa")
     (:name "sent polimi" :query "tag:polimi and tag:sent" :key "sp")
     (:name "archive all" :query "tag:archive" :key "aa")
     (:name "archive polimi" :query "tag:polimi and tag:archive" :key "ap")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "drafts" :query "tag:draft" :key "d")))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-all-multipart/alternative-parts t)
 '(notmuch-show-all-tags-list t)
 '(notmuch-show-empty-saved-searches t)
 '(notmuch-tagging-keys
   '(("a" notmuch-archive-tags "Archive")
     ("u" notmuch-show-mark-read-tags "Mark read")
     ("f"
      ("+flagged" "-trash" "-spam" "-deleted")
      "Flag")
     ("s"
      ("+spam" "-inbox" "-archive" "-trash" "-deleted")
      "Mark as spam")
     ("t"
      ("+trash" "-inbox" "-spam" "-archive" "-deleted")
      "Trash")
     ("d"
      ("+deleted")
      "Delete")
     ("i"
      ("+inbox" "-trash" "-spam" "-archive" "-deleted")
      "Inbox")))
 '(nov-save-place-file (expand-file-name "emacs/nov-places" (xdg-cache-home)))
 '(nov-text-width 80)
 '(nsm-settings-file
   (expand-file-name "emacs/network-security.data"
                     (xdg-cache-home)))
 '(nxml-child-indent 4)
 '(org-adapt-indentation nil)
 '(org-agenda-files '("~/org/study.org" "~/org/life.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-babel-load-languages
   '((calc . t)
     (emacs-lisp . t)
     (sql . t)
     (shell . t)
     (http . t)))
 '(org-capture-templates
   '(("r" "Remember" entry
      (file+headline "~/org/life.org" "Remember")
      "* TODO %?")))
 '(org-clock-display-default-range 'untilnow)
 '(org-duration-format 'h:mm)
 '(org-edit-src-content-indentation 0)
 '(org-habit-following-days 4)
 '(org-habit-graph-column 52)
 '(org-habit-preceding-days 30)
 '(org-html-htmlize-output-type 'css)
 '(org-id-locations-file (expand-file-name "emacs/org-id-locations" (xdg-data-home)))
 '(org-log-into-drawer t)
 '(org-log-reschedule 'note)
 '(org-mode-hook '(smartparens-mode))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m org-checklist))
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-targets '((org-agenda-files :level . 1)))
 '(org-refile-use-outline-path 'file)
 '(org-startup-folded t)
 '(org-tags-column 0)
 '(package-archives nil)
 '(php-mode-coding-style 'php)
 '(php-mode-hook '(skempo-mode smartparens-mode subword-mode))
 '(proced-tree-flag t)
 '(project-compilation-buffer-name-function 'project-prefixed-buffer-name)
 '(project-list-file (expand-file-name "emacs/project.list" (xdg-cache-home)))
 '(project-switch-commands
   '((magit-project-status "Magit" nil)
     (project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-dired "Dired" nil)
     (project-vc-dir "VC-Dir" nil)
     (project-eshell "Eshell" nil)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(register-separator 43)
 '(rust-format-on-save t)
 '(safe-local-variable-values
   '((org-confirm-babel-evaluate)
     (eval hl-line-mode t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (byte-recompile-file
              (buffer-file-name)))
           nil t)))
 '(save-place-file (expand-file-name "emacs/saveplace" (xdg-cache-home)))
 '(save-place-limit 1000)
 '(save-place-mode t)
 '(save-place-skip-check-regexp
   (rx
    (or
     (regexp
      (eval
       (car
        (get 'save-place-skip-check-regexp 'standard-value))))
     (and bos "http"))))
 '(savehist-file (expand-file-name "emacs/savehist" (xdg-cache-home)))
 '(savehist-mode t)
 '(savehist-save-hook '(savehist-filter-file-name-history))
 '(scheme-mode-hook '(smartparens-mode geiser-mode--maybe-activate))
 '(scheme-program-name "guile")
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(sdcv-env-lang "en_US.UTF-8")
 '(sdcv-translate-command
   '(program "--non-interactive" dictionary-data-dir dictionary-list word))
 '(search-whitespace-regexp ".*?")
 '(send-mail-function 'message-send-mail-with-sendmail)
 '(sendmail-program "msmtp")
 '(sgml-basic-offset 4)
 '(sh-mode-hook
   '(sh-electric-here-document-mode smartparens-mode flymake-shellcheck-load flymake-mode))
 '(shell-mode-hook '(shell-pwd-enable ansi-color-for-comint-mode-on))
 '(shift-select-mode nil)
 '(shr-max-image-proportion 0.7)
 '(shr-use-fonts nil)
 '(size-indication-mode t)
 '(skempo-always-create-abbrev t)
 '(skempo-always-create-tag t)
 '(skempo-completing-read t)
 '(skempo-delete-duplicate-marks t)
 '(skempo-mode-lighter "")
 '(skempo-skeleton-marks-support t)
 '(small-temporary-file-directory "/dev/shm/")
 '(sql-input-ring-file-name (expand-file-name "emacs/sql_history" (xdg-cache-home)))
 '(sql-interactive-mode-hook '(sql-indent-enable))
 '(sql-mode-hook '(sqlup-mode sql-indent-enable smartparens-mode))
 '(sql-sqlite-options '("-column" "-header" "-cmd" "PRAGMA foreign_keys = ON;"))
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-select-tab-modifiers '(super))
 '(tab-bar-show 1)
 '(tab-bar-tab-hints t)
 '(tab-width 4)
 '(text-mode-hook '(abbrev-mode text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(tramp-completion-reread-directory-timeout nil)
 '(tramp-debug-to-file t)
 '(tramp-default-method "ssh")
 '(tramp-persistency-file-name (expand-file-name "emacs/tramp" (xdg-cache-home)))
 '(tramp-remote-path
   '("/run/current-system/sw/bin" "~/.local/bin" tramp-own-remote-path tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
 '(transient-enable-popup-navigation t)
 '(transient-history-file
   (expand-file-name "emacs/transient/history.el"
                     (xdg-cache-home)))
 '(transient-levels-file
   (expand-file-name "emacs/transient/levels.el"
                     (xdg-cache-home)))
 '(transient-values-file
   (expand-file-name "emacs/transient/values.el"
                     (xdg-cache-home)))
 '(truncate-lines t)
 '(undo-limit 200000)
 '(undo-strong-limit 300000)
 '(uniquify-ignore-buffers-re "^\\*")
 '(url-configuration-directory (expand-file-name "emacs/url/" (xdg-cache-home)))
 '(url-handler-mode t)
 '(use-dialog-box nil)
 '(user-full-name "Valeriy Litkovskyy")
 '(vc-handled-backends '(Git))
 '(version-control t)
 '(web-mode-markup-indent-offset 2)
 '(wgrep-auto-save-buffer t)
 '(x-gtk-use-system-tooltips nil)
 '(x-stretch-cursor t)
 '(xref-after-jump-hook '(recenter xref-pulse-momentarily)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white smoke" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(header-line ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box nil :height 135))))
 '(mode-line ((t (:background "white smoke" :foreground "black" :box (:line-width 1 :color "grey75") :height 101))))
 '(mode-line-inactive ((t (:weight light :foreground "grey20" :background "dark gray" :inherit mode-line))))
 '(region ((t (:extend t :background "LemonChiffon2" :distant-foreground "gtk_selection_fg_color"))))
 '(tab-bar ((t (:inherit variable-pitch :background "black" :foreground "black" :height 101))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "white smoke" :box (:line-width 1 :color "white smoke")))))
 '(tab-bar-tab-inactive ((t (:box (:line-width 1 :color "black") :foreground "black" :background "dark grey" :inherit tab-bar-tab)))))
