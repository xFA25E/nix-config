;;; mingus-extra.el --- Extra features and fixes for Mingus  -*- lexical-binding: t; eval: (progn (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t)); -*-

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

;; Fix mingus dired-add and dired-file.  Kill mignus buffers on quit.  Add music
;; files with completion.

;;; Code:

(require 'mingus)

(defvar mingus-extra-music-files nil)

(defgroup mingus-extra nil
  "Mingus extra features and fixes."
  :group 'mingus)

(defcustom mingus-extra-music-directory
  "/home/val/Music"
  "Mpd music directory."
  :type 'directory
  :group 'mingus-extra)

(defun mingus-extra-dired-jump-file ()
  "Open dired with parent dir of song at point."
  (interactive)
  (cond
   ((mingus-directoryp)
    (dired (concat mingus-extra-music-directory (mingus-get-absolute-filename))))
   ((mingus-playlistp)
    (dired mingus-mpd-playlist-dir))
   (t
    (dired-jump nil (concat mingus-extra-music-directory (mingus-get-absolute-filename))))))

(defun mingus-extra-dired-add-trim ()
  "Add music files to mpd by trimming name with music dir."
  (interactive)
  (mingus-add-files
   (mapcar
    (lambda (f)
      (string-trim-left f (regexp-quote mingus-extra-music-directory)))
    (dired-get-marked-files))))

(defun mingus-extra-git-out-and-kill (&optional _)
  "On quit, kill mingus buffer."
  (interactive)
  (when (mingus-buffer-p)
    (kill-current-buffer)))

(defun mingus-extra-music-files ()
  "Get music files."
  (let* ((default-directory mingus-extra-music-directory)
         (exts (cdr (mapcan (lambda (e) `("-o" "-iname" ,(concat "*." e)))
                            '("flac" "m4a" "mp3" "ogg" "opus"))))
         (args `("." "(" ,@exts ")" "-type" "f" "-o" "-type" "d")))
    (mapcar (lambda (m) (substring m 1))
            (cdr (apply #'process-lines "find" args)))))

(defun mingus-extra-find-and-add-file (&optional updatep)
  "Add music files to mpd with completion.
If `UPDATEP' is non-nil, update cache."
  (interactive "P")
  (when (or (not mingus-extra-music-files) updatep)
    (setf mingus-extra-music-files (mingus-extra-music-files)))

  (mingus-add-files
   (list (completing-read "Add file to mpd: " mingus-extra-music-files nil t)))
  (mpd-play mpd-inter-conn)
  (let ((buffer (get-buffer "*Mingus*")))
    (when (buffer-live-p (get-buffer buffer))
      (kill-buffer buffer))))

(advice-add 'mingus-git-out :override 'mingus-extra-git-out-and-kill)
(advice-add 'mingus-dired-file :override 'mingus-extra-dired-jump-file)
(advice-add 'mingus-dired-add :override 'mingus-extra-dired-add-trim)

(provide 'mingus-extra)
;;; mingus-extra.el ends here
