;;; newsticker-extra.el --- Extra features for newsticker  -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

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

      (_ (cl-return-from newsticker-extra-add-thumbnail)))

    (setf (nth 1 item) (format "<img src=\"%s\"/><br/><pre>%s</pre>" thumbnail
                               (xml-escape-string description)))))

(defun newsticker-extra-treeview-copy-link ()
  "Copy current item link to `kill-ring'."
  (interactive)
  (let ((link (newsticker--link (newsticker--treeview-get-selected-item))))
    (kill-new link)
    (message "Copied %s" link)))

(defun newsticker-handle-url ()
  "Newsticker button action on."
  (pcase (get-text-property (point) 'nt-link)
    ((and (rx "videos.lukesmith.xyz" (*? any) ".torrent" eos) nt-link)
     (let ((dir (expand-file-name "Luke Smith" (getenv "YTDL_DIR"))))
       (call-process "transmission-remote" nil 0 nil
                     "--no-start-paused" "--add" nt-link
                     "--download-dir" dir)))))

(provide 'newsticker-extra)
