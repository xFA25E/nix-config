;;; newsticker-extra.el --- Extra features for newsticker  -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

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

;; Custom parser for thumbnails.  Command to copy link to kill ring.

;;; Code:

(require 'rx)
(require 'xml)
(require 'newst-backend)
(require 'newst-treeview)

(cl-defun newsticker-extra-add-thumbnail (_feedname item)
  "Add thumbnail to `ITEM' if it's a media url."
  (let ((thumbnail nil) (description nil))

    (pcase (list (newsticker--link item) (newsticker--extra item))
      (`(,(rx "youtube.com") ,(map group))
       (setf description (cadr (alist-get 'description group))
             thumbnail (alist-get 'url (car (alist-get 'thumbnail group)))))

      (`(,(rx "bitchute.com") ,(map enclosure))
       (setf description (newsticker--desc item)
             thumbnail (alist-get 'url (car enclosure))))

      (`(,(rx "videos.lukesmith.xyz") ,(map thumbnail))
       (setf description (newsticker--desc item)
             thumbnail (alist-get 'url (car thumbnail))))

      (_ (cl-return nil)))

    (setf (nth 1 item) (format "<img src=\"%s\"/><br/><pre>%s</pre>" thumbnail
                               (xml-escape-string description)))))

(defun newsticker-extra-treeview-copy-link ()
  "Copy current item link to `kill-ring'."
  (interactive)
  (let ((link (newsticker--link (newsticker--treeview-get-selected-item))))
    (kill-new link)
    (message "Copied %s" link)))

(defun newsticker-handle-url (nt-link)
  "Newsticker button action on `NT-LINK'."
  (interactive (list (get-text-property (point) 'nt-link)))
  (pcase nt-link
    ((rx "videos.lukesmith.xyz" (*? any) ".torrent" eos)
     (let ((dir (expand-file-name "Luke Smith" (getenv "YTDL_DIR"))))
       (call-process "transmission-remote" nil 0 nil
                     "--no-start-paused" "--add" nt-link
                     "--download-dir" dir)))))

(provide 'newsticker-extra)
;;; newsticker-extra.el ends here
