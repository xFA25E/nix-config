;;; pcmpl-args-extra.el --- pcmpl-arg-extra          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords:

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

;; should work on remote too
;; grep/git should be more elegant

;;; Code:

(require 'subr-x)
(require 'cl-seq)
(require 'pcmpl-args)

;;;; GIT FIX

(defun pcmpl-args-git-commands ()
  (pcmpl-args-cached 'git-commands t
    (with-temp-buffer
      (pcmpl-args-process-file "git" "help" "-a")
      (goto-char (point-min))
      (let ((cmds (copy-sequence pcmpl-args-git-commands)))
        (while (re-search-forward (rx bol (+ " ") (group (+ (not space)))
                                      (+ space) (group (*? any)) eol)
                                  nil t)
          (let ((cmd (match-string 1))
                (help (match-string 2)))
            (unless (assoc cmd cmds)
              (push (list cmd help) cmds))))
        (setq cmds (cl-sort cmds #'string< :key #'car))
        (pcmpl-args-completion-table-with-annotations
         cmds `(metadata (category . git-command)))))))

;;;; PASS

(defvar pcmpl-args-pass-subcommands
  (pcmpl-args-completion-table-with-annotations
   '(("cp" "Copy password or directory")
     ("edit" "Insert a new password or edit an existing password")
     ("find" "List names of passwords inside the tree that match patterns")
     ("generate" "Generate new password")
     ("git" "Execute git commands")
     ("grep" "Searches inside each decrypted password file")
     ("help" "Show usage message")
     ("init" "Initialize new password storage")
     ("insert" "Insert a new password into the password store")
     ("ls" "List names of passwords")
     ("mv" "Move password or directory")
     ("rm" "Remove password or directory")
     ("show" "Decrypt and print a password")
     ("version" "Show version information"))))

(defun pcmpl-args-pass-prefix ()
  (let ((directory (or (getenv "PASSWORD_STORE_DIR")
                       (expand-file-name "~/.password-store"))))
    (concat directory "/")))

(defun pcmpl-args-pass-entry-from-path (prefix path)
  (thread-last path
    (string-remove-prefix prefix)
    (string-remove-suffix ".gpg")))

(defun pcmpl-args-pass-find (&rest find-args)
  (let* ((prefix (pcmpl-args-pass-prefix))
         (args `("find" "-L" ,prefix
                 "(" "-name" ".git*" "-o" "-name" ".gpg-id" ")" "-prune"
                 "-o" ,@find-args "-print"))
         (paths (cdr (apply #'process-lines args)))
         (entry-from-path (apply-partially 'pcmpl-args-pass-entry-from-path prefix)))
    (sort (mapcar entry-from-path paths) #'string<)))

(defun pcmpl-args-pass-keys (args)
  (thread-first
      (thread-last (process-lines "gpg2" "--list-secret-keys" "--with-colons")
        (mapcar (lambda (key) (elt (split-string key ":") 9)))
        (delete ""))
    (cl-delete-duplicates :test 'string=)
    (sort 'string<)
    (cl-set-difference (car (alist-get '* args)) :test #'string=)))

(defun pcmpl-args-pass-subcommand-specs (cmd)
  (pcase cmd
    ("edit"
     (list
      '((argument 0 (("PASSNAME" (:eval (pcmpl-args-pass-find "-type" "f"))))))))

    ("find"
     (list
      '((argument * (("PATTERN" none))))))

    ("generate"
     (list
      '((option "-n, --no-symbols" :help "Use only alphanumeric characters.")
        (option "-c, --clip" :help "Copy the password to the clipboard.")
        (option "-i, --in-place" :help "Only replace the first line of the password file.")
        (option "-f, --force" :help "Don't prompt before overwriting an existing password.")
        (argument 0 (("PASSNAME" (:eval (pcmpl-args-pass-find)))))
        (argument 1 (("PASSLENGTH" none))))))

    ("git"
     (list
      ;; Copied from pcomplete/git
      (append
       (pcmpl-args-git-extract-argspecs-from-help "")
       `((argument 0 (("GIT-COMMAND" nil))
                   :subparser
                   (lambda (arguments argspecs seen)
                     (let ((stub (pop arguments)))
                       (push (list :name 0
                                   :stub stub
                                   :values (plist-get (car seen) :values)
                                   :action `("GIT-COMMAND" (:eval (pcmpl-args-git-commands))))
                             seen)
                       (if (null arguments)
                           (list arguments argspecs seen)
                         (setq argspecs
                               (ignore-errors
                                 (pcmpl-args-git-extract-argspecs-from-help stub)))
                         (setq argspecs
                               (append
                                argspecs
                                (cond ((equal stub "help")
                                       `((argument * (("GIT-COMMAND"
                                                       (:eval (pcmpl-args-git-commands)))))))
                                      (t `((argument * (("FILE" t))))))))
                         (list arguments (pcmpl-args-make-argspecs argspecs) seen)))))))))

    ("grep"
     (list
      ;; Copied from pcomplete/grep
      (append
       (pcmpl-args-extract-argspecs-from-manpage "grep")
       '((option ("-e" "--regexp=") (("PATTERN" none))
                 :aliases (0)
                 :help "use PATTERN for matching")
         (argument 0 (("PATTERN" none)) :excludes ("-e" "--regexp="))))
      :hints
      '(("\\`\\(-d\\|--directories\\)=" ("read" "recurse" "skip"))
        ("\\`--binary-files=" ("binary" "text" "without-match"))
        ("\\`\\(-D\\|--devices\\)=" ("read" "skip"))
        ("\\`--colou?r=" ("yes" "no" "always" "never" "auto")))))

    ("init"
     (list
      '((option "-p, --path=SUBFOLDER" (("SUBFOLDER" (:eval (pcmpl-args-pass-find "-type" "d"))))
                :help "GPGIDs are assigned for that specific SUBFOLDER of the store.")
        (argument * (("GPGID" (:lambda pcmpl-args-pass-keys)))))))

    ("insert"
     (list
      '((option "-e, --echo" :help "Enable keyboard echo and don't confirm the password.")
        (option "-m, --multiline" :help "Read lines until EOF or Ctrl+D is reached.")
        (option "-f, --force" :help "Don't prompt before overwriting an existing password.")
        (argument 0 (("PASSNAME" (:eval (pcmpl-args-pass-find))))))))

    ("ls"
     (list
      '((argument 0 (("SUBFOLDER" (:eval (pcmpl-args-pass-find "-type" "d"))))))))

    ("rm"
     (list
      '((option "-r, --recursive" :help "Delete PASSNAME recursively if it is a directory.")
        (option "-f, --force" :help "Do not interactively prompt before removal.")
        (argument 0 (("PASSNAME" (:eval (pcmpl-args-pass-find))))))))

    ("show"
     (list
      '((option "-c[LINENUMBER], --clip[=LINENUMBER]" (("LINENUMBER" none))
                :help "Copy the first (or specified) line to the clipboard.")
        (option "-q[LINENUMBER], --qrcode[=LINENUMBER]" (("LINENUMBER" none))
                :help "Display a QR code of the first (or specified) line.")
        (argument 0 (("PASSNAME" (:eval (pcmpl-args-pass-find "-type" "f"))))))))

    ((or "cp" "mv")
     (list
      '((option "-f, --force" :help "Silently overwrite NEWPATH if it exists.")
        (argument 0 (("OLDPATH" (:eval (pcmpl-args-pass-find)))))
        (argument 1 (("NEWPATH" (:eval (pcmpl-args-pass-find))))))))))

(defun pcomplete/pass ()
  (pcmpl-args-pcomplete
   (pcmpl-args-make-argspecs
    '((argument 0 (("PASSSUBCOMMAND" nil))
                :subparser
                (lambda (arguments argspecs seen)
                  (let ((stub (pop arguments)))
                    (push (list :name 0
                                :stub stub
                                :values (plist-get (car seen) :values)
                                :action '("PASSSUBCOMMAND" (:eval pcmpl-args-pass-subcommands)))
                          seen)
                    (when arguments
                      (let ((specs (pcmpl-args-pass-subcommand-specs stub)))
                        (setq argspecs (apply 'pcmpl-args-make-argspecs specs))))
                    (list arguments argspecs seen))))))))



(provide 'pcmpl-args-extra)
;;; pcmpl-args-extra.el ends here
