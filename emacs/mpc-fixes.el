;;; mpc-fixes.el --- Fix lexical scope in mpc in 28 -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'cl-lib)
(require 'subr-x)
(require 'mpc)

(defun mpc-format (format-spec info &optional hscroll)
  "Format the INFO according to FORMAT-SPEC, inserting the result at point."
  (let* ((pos 0)
         (start (point))
         (col (if hscroll (- hscroll) 0))
         (insert (lambda (str)
                   (cond
                    ((>= col 0) (insert str))
                    (t (insert (substring str (min (length str) (- col))))))))
         (pred #'always))
    (while (string-match "%\\(?:%\\|\\(-\\)?\\([0-9]+\\)?{\\([[:alpha:]][[:alnum:]]*\\)\\(?:-\\([^}]+\\)\\)?}\\)" format-spec pos)
      (let ((pre-text (substring format-spec pos (match-beginning 0))))
        (funcall insert pre-text)
        (setq col (+ col (string-width pre-text))))
      (setq pos (match-end 0))
      (if (null (match-end 3))
          (progn
            (funcall insert "%")
            (setq col (+ col 1)))
        (let* ((size (match-string 2 format-spec))
               (tag (intern (match-string 3 format-spec)))
               (post (match-string 4 format-spec))
               (right-align (match-end 1))
               (text
                (if (eq info 'self) (symbol-name tag)
                  (pcase tag
                    ((or 'Time 'Duration)
                     (let ((time (cdr (or (assq 'time info) (assq 'Time info)))))
                       (setq pred #'ignore) ;Just assume it's never eq.
                       (when time
                         (mpc-secs-to-time (if (and (eq tag 'Duration)
                                                    (string-match ":" time))
                                               (substring time (match-end 0))
                                             time)))))
                    ('Cover
                     (let ((dir (file-name-directory (cdr (assq 'file info)))))
                       ;; (debug)
                       (setq pred
                             (let ((oldpred pred))
                               (lambda (info)
                                 (and (funcall oldpred info)
                                      (equal dir (file-name-directory
                                                  (cdr (assq 'file info))))))))
                       (if-let* ((covers '(".folder.png" "cover.jpg" "folder.jpg"))
                                 (cover (cl-loop for file in (directory-files (mpc-file-local-copy dir))
                                                 if (member (downcase file) covers)
                                                 return (concat dir file)))
                                 (file (with-demoted-errors "MPC: %s"
                                         (mpc-file-local-copy cover))))
                           (let (image)
                             (if (null size) (setq image (create-image file))
                               (let ((tempfile (make-temp-file "mpc" nil ".jpg")))
                                 (call-process "convert" nil nil nil
                                               "-scale" size file tempfile)
                                 (setq image (create-image tempfile))
                                 (mpc-tempfiles-add image tempfile)))
                             (setq size nil)
                             (propertize dir 'display image))
                         ;; Make sure we return something on which we can
                         ;; place the `mpc--uptodate-p' property, as
                         ;; a negative-cache.  We could also use
                         ;; a default cover.
                         (progn (setq size nil) " "))))
                    (_ (let ((val (cdr (assq tag info))))
                         ;; For Streaming URLs, there's no other info
                         ;; than the URL in `file'.  Pretend it's in `Title'.
                         (when (and (null val) (eq tag 'Title))
                           (setq val (cdr (assq 'file info))))
                         (setq pred
                               (let ((oldpred pred))
                                 (lambda (info)
                                   (and (funcall oldpred info)
                                        (equal val (cdr (assq ',tag info)))))))
                         (cond
                          ((not (and (eq tag 'Date) (stringp val))) val)
                          ;; For "date", only keep the year!
                          ((string-match "[0-9]\\{4\\}" val)
                           (match-string 0 val))
                          (t val)))))))
               (space (when size
                        (setq size (string-to-number size))
                        (propertize " " 'display
                                    (list 'space :align-to (+ col size)))))
               (textwidth (if text (string-width text) 0))
               (postwidth (if post (string-width post) 0)))
          (when text
            (let ((display
                   (if (and size
                            (> (+ postwidth textwidth) size))
                       (propertize
                        (truncate-string-to-width text size nil nil "…")
                        'help-echo text)
                     text)))
              (when (memq tag '(Artist Album Composer)) ;FIXME: wrong list.
                (setq display
                      (propertize display
                                  'mouse-face 'highlight
                                  'follow-link t
                                  'keymap `(keymap
                                            (mouse-2
                                             . ,(lambda ()
                                                  (interactive)
                                                  (mpc-constraints-push 'noerror)
                                                  (mpc-constraints-restore
                                                   ',(list (list tag text)))))))))
              (funcall insert
                       (concat (when size
                                 (propertize " " 'display
                                             (list 'space :align-to
                                                   (+ col
                                                      (if (and size right-align)
                                                          (- size postwidth textwidth)
                                                        0)))))
                               display post))))
          (if (null size) (setq col (+ col textwidth postwidth))
            (insert space)
            (setq col (+ col size))))))
    (put-text-property start (point) 'mpc--uptodate-p pred)))

(provide 'mpc-fixes)
