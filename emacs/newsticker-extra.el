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
(require 'newst-backend)
(require 'newst-treeview)

(defun newsticker-extra-add-thumbnail (_feedname item)
  "Add thumbnail to `ITEM' if it's a media url."
  (cl-flet ((d (thumb desc) (format "<img src=\"%s\"/><br/><pre>%s</pre>" thumb desc)))
    (pcase (newsticker--link item)
      ((rx "youtube.com")
       (let ((group (alist-get 'group (newsticker--extra item))))
         (setf
          (nth 1 item)
          (d (alist-get 'url (car (alist-get 'thumbnail group))) (cadr (alist-get 'description group))))))
      ((rx "bitchute.com")
       (let ((enclosure (alist-get 'enclosure (newsticker--extra item))))
         (setf
          (nth 1 item)
          (d (alist-get 'url (car enclosure)) (newsticker--desc item)))))
      ((rx "videos.lukesmith.xyz")
       (let ((thumbnail (alist-get 'thumbnail (newsticker--extra item))))
         (setf
          (nth 1 item)
          (d  (alist-get 'url (car thumbnail)) (newsticker--desc item))))))))

(defun newsticker-extra-treeview-copy-link ()
  "Copy current item link to `kill-ring'."
  (interactive)
  (let ((link (newsticker--link (newsticker--treeview-get-selected-item))))
    (kill-new link)
    (message "Copied %s" link)))

(provide 'newsticker-extra)
;;; newsticker-extra.el ends here
