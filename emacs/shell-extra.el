;;; shell-extra.el --- Shell extra features          -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
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

;; Command to change directory interactively.  Enable history and history
;; filters.

;;; Code:

(require 'shell)

(defgroup shell-extra nil
  "Extra shell features."
  :group 'shell)

(defcustom shell-extra-history-filename
  "/home/val/.local/share/bash_history"
  "Shell history file."
  :type 'file
  :group 'shell-extra)

(defun shell-extra-change-directory ()
  "Change directory in a shell, interactively."
  (interactive)
  (comint-show-maximum-output)
  (comint-delete-input)
  (let* ((read-dir (read-directory-name "Change directory: "))
         (dir (or (file-remote-p read-dir 'localname) read-dir)))
    (insert (concat "cd " (shell-quote-argument (expand-file-name dir)))))
  (comint-send-input))

(defun shell-extra-enable-history ()
  "Enable shell history.
Set `comint-input-ring-file-name' and load input ring."
  (setq-local comint-input-ring-file-name shell-extra-history-filename))

(provide 'shell-extra)
;;; shell-extra.el ends here
