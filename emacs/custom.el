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
 '(avy-goto-word-0-regexp "\\_<\\(?:\\sw\\|\\s_\\)")
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
     ("firefox" 102 "Open in firefox" browse-url-firefox)))
 '(browse-url-multi-invidious-instances
   '("vid.puffyan.us" "invidious.048596.xyz" "yewtu.be" "ytprivate.com" "invidious.kavin.rocks" "tube.connect.cafe"))
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
   "\\(^ *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:Auth\\|\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|PEM\\|Re\\(?:peat\\|type\\)\\|SUDO\\|UNIX\\|\\[sudo]\\|enter\\(?: \\(?:auth\\|\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) +.*\\)\\(?:\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|m\\(?:ot de passe\\|ật khẩu\\)\\|p\\(?:a\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|in\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\|Response\\)\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\\(?: [[:alpha:]]+ .+\\)?[[:blank:]]*[:：៖][[:blank:]]*\\'\\|[Pp]assword \\'")
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
   '(("\\.\\(?:csv\\|doc\\|docx\\|odp\\|ods\\|odt\\|ppt\\|pptx\\|xls\\|xlsx\\)\\'" "setsid -f libreoffice * >/dev/null 2>&1" "libreoffice --invisible --headless --convert-to pdf * &")
     ("\\.\\(?:bmp\\|gif\\|jfif\\|jpeg\\|jpg\\|nef\\|png\\|thm\\|tif\\|webp\\|xpm\\)\\'" "setsid -f sxiv * >/dev/null 2>&1" "setsid -f gimp * >/dev/null 2>&1")
     ("\\.\\(?:ai\\|eps\\)\\'" "setsid -f inkscape * >/dev/null 2>&1" "setsid -f gimp * >/dev/null 2>&1")
     ("\\.\\(?:djvu\\|fb2\\)\\'" "ebook-convert ? .epub &")
     ("\\.pdf\\'" "setsid -f libreoffice * >/dev/null 2>&1" "setsid -f gimp * >/dev/null 2>&1")
     ("\\.\\(?:3gp\\|aiff\\|avi\\|flac\\|flv\\|m4a\\|mkv\\|mov\\|mp3\\|mp4\\|mpg\\|ogg\\|ogv\\|opus\\|vob\\|wav\\|webm\\|wmv\\)\\'" "setsid -f mpv --profile=gui * >/dev/null 2>&1" "video_duration * | format_duration" "video_duration * | awk '{s+=$1}END{print s}' | format_duration" "compress_video * &" "strip_video * &" "mediainfo" "mpv -vo=drm")
     ("\\.cue\\'" "setsid -f mpv --profile=gui * >/dev/null 2>&1")
     ("\\.rar\\'" "temp=\"$(echo `?` | rev | cut -d. -f 2- | rev)\"; mkdir -p \"${temp}\"; unrar x ? \"${temp}\"")
     ("\\.torrent\\'" "transmission-show")
     ("\\.epub\\'" "ebook-convert ? .mobi &")))
 '(dired-listing-switches "-lF --si --group-directories-first")
 '(dired-ls-F-marks-symlinks t)
 '(dired-mode-hook '(dired-hide-details-mode hl-line-mode))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-mail-function
   (lambda nil
     (not
      (string= "0"
               (car
                (process-lines "notmuch" "count" "tag:unread"))))))
 '(display-time-mode t)
 '(display-time-string-forms
   '((if mail
         (concat " "
                 (propertize display-time-mail-string 'display
                             `(when
                                  (and display-time-use-mail-icon
                                       (display-graphic-p))
                                ,@display-time-mail-icon ,@(if
                                                               (and display-time-mail-face
                                                                    (memq
                                                                     (plist-get
                                                                      (cdr display-time-mail-icon)
                                                                      :type)
                                                                     '(pbm xbm)))
                                                               (let
                                                                   ((bg
                                                                     (face-attribute display-time-mail-face :background)))
                                                                 (if
                                                                     (stringp bg)
                                                                     (list :background bg)))))
                             'face display-time-mail-face 'help-echo "You have new mail; mouse-1: Read mail" 'mouse-face 'mode-line-highlight 'local-map
                             (make-mode-line-mouse-map 'mouse-1 read-mail-command)))
       "")
     (propertize
      (format-time-string
       (or display-time-format
           (if display-time-24hr-format " %H:%M" "%-I:%M%p"))
       now)
      'help-echo
      (format-time-string "%a %b %e, %Y" now))
     (if
         (and
          (not display-time-format)
          display-time-day-and-date)
         (format-time-string " %a %b %e" now)
       "")))
 '(display-time-use-mail-icon t)
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
 '(emacs-lisp-mode-hook
   '(skempo-mode outline-minor-mode flymake-mode smartparens-mode abbrev-mode))
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
 '(image-dired-db-file (expand-file-name "emacs/image-dired/db" (xdg-cache-home)))
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
 '(imenu-after-jump-hook '(outline-show-after-jump))
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
   '(link-hint-shr-url link-hint-org-link link-hint-markdown-link link-hint-help-link link-hint-info-link link-hint-package-link link-hint-package-keyword-link link-hint-package-install-link link-hint-epkg-button link-hint-compilation-link link-hint-nov-link link-hint-customize-widget link-hint-notmuch-hello link-hint-button link-hint-text-url link-hint-completion-list-candidate link-hint-file-link link-hint-org-agenda-item link-hint-xref-item link-hint-man-button link-hint-dired-filename))
 '(lisp-mode-hook
   '(skempo-mode smartparens-mode sly-editing-mode abbrev-mode))
 '(magit-credential-cache-daemon-socket (expand-file-name "git/credential/socket" (xdg-cache-home)))
 '(magit-define-global-key-bindings nil)
 '(mail-user-agent 'notmuch-user-agent)
 '(marginalia-mode t)
 '(max-mini-window-height 1.0)
 '(menu-bar-mode nil)
 '(message-directory "~/.mail/")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function 'message-send-mail-with-sendmail)
 '(message-subject-re-regexp
   "^[[:blank:]]*\\(?:\\(?:R\\(?:is\\|[Ee]\\)?\\)\\(?:\\[[[:digit:]]*]\\)* ?:[[:blank:]]*\\)*")
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
     ("Luke Smith Blog" "https://lukesmith.xyz/rss.xml" nil nil nil)
     ("Luke Smith PeerTube" "https://lukesmith.xyz/peertube" nil nil nil)
     ("Protesilaos Stavrou" "https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" nil nil nil)
     ("VComplete Dev" "https://git.sr.ht/~dsemy/vcomplete/log/master/rss.xml" nil nil nil)))
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
   '((:name "todo" :query "tag:todo" :key "t")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "spam" :query "tag:unread and tag:spam" :key "s")
     (:name "polimi inbox" :query "tag:polimi and tag:inbox" :key "pp")
     (:name "polimi sent" :query "tag:polimi and tag:sent" :key "ps")
     (:name "polimi archive" :query "tag:polimi and tag:archive" :key "pa")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "flagged" :query "tag:flagged" :key "f")))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-all-multipart/alternative-parts t)
 '(notmuch-show-all-tags-list t)
 '(notmuch-show-empty-saved-searches t)
 '(notmuch-tagging-keys
   '(("a" notmuch-archive-tags "Archive")
     ("u" notmuch-show-mark-read-tags "Mark read")
     ("f"
      ("+flagged")
      "Flag")
     ("s"
      ("+spam" "-inbox" "-archive" "-trash" "-deleted")
      "Mark as spam")
     ("t"
      ("+todo" "-trash" "-spam" "-deleted")
      "Todo")
     ("d"
      ("+trash" "-inbox" "-spam" "-archive" "-deleted")
      "Trash")
     ("D"
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
 '(org-agenda-files '("~/org/life.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-babel-load-languages '((calc . t) (emacs-lisp . t) (sql . t) (shell . t)))
 '(org-capture-templates
   '(("r" "Remember" entry
      (file+headline "~/org/life.org" "Remember")
      "* TODO %?")))
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
 '(outline-minor-mode-cycle t)
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
 '(read-mail-command '(lambda nil (interactive) (notmuch-search "tag:unread")))
 '(register-separator 43)
 '(rust-format-on-save t)
 '(safe-local-variable-values
   '((eval hl-line-mode t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (byte-recompile-file
              (buffer-file-name)))
           nil t)))
 '(save-place-file (expand-file-name "emacs/saveplace" (xdg-cache-home)))
 '(save-place-limit 1000)
 '(save-place-mode t)
 '(save-place-skip-check-regexp
   "\\`/\\(?:cdrom\\|floppy\\|mnt\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)\\|\\`http")
 '(savehist-file (expand-file-name "emacs/savehist" (xdg-cache-home)))
 '(savehist-mode t)
 '(savehist-save-hook '(savehist-filter-file-name-history))
 '(scheme-mode-hook '(smartparens-mode geiser-mode--maybe-activate))
 '(scheme-program-name "guile")
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(search-whitespace-regexp ".*?")
 '(send-mail-function 'message-send-mail-with-sendmail)
 '(sendmail-program "msmtp")
 '(sgml-basic-offset 4)
 '(sh-mode-hook
   '(sh-electric-here-document-mode smartparens-mode flymake-shellcheck-load flymake-mode))
 '(shell-mode-hook '(ansi-color-for-comint-mode-on))
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
 '(tab-bar-new-tab-to 'rightmost)
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
 '(vcomplete-mode t)
 '(version-control t)
 '(web-mode-markup-indent-offset 2)
 '(wgrep-auto-save-buffer t)
 '(x-gtk-use-system-tooltips nil)
 '(x-stretch-cursor t)
 '(xref-after-jump-hook '(outline-show-after-jump recenter xref-pulse-momentarily)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(header-line ((t (:height 150 :box nil :foreground "grey20" :background "grey90" :inherit mode-line))))
 '(mode-line ((t (:background "white smoke" :foreground "black" :box nil :height 105))))
 '(mode-line-inactive ((t (:height 105 :weight light :box (:line-width (1 . -1) :color "grey75") :foreground "grey20" :background "dark gray" :inherit mode-line))))
 '(tab-bar ((t (:height 105 :foreground "black" :background "black" :inherit variable-pitch))))
 '(tab-bar-tab ((t (:box (:line-width (1 . 1) :color "white") :background "white" :inherit tab-bar))))
 '(tab-bar-tab-inactive ((t (:box (:line-width (1 . 1) :color "black") :foreground "black" :background "dark grey" :inherit tab-bar-tab)))))
