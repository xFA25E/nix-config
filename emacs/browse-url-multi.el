;;; browse-url-multi.el --- Browse url multi-browser function  -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'cl-macs)
(require 'url-parse)
(require 'browse-url)

(defgroup browse-url-multi nil
  "A multi-browser browse-url function."
  :group 'browse-url)

(defcustom browse-url-multi-invidious-instances
  '("invidio.us")
  "Invidious hosts."
  :type '(repeat string)
  :group 'browse-url-multi)

(defun browse-url-multi-maybe-change-host-to-youtube (url)
  "Swap `URL's host to youtube if invidious."
  (let* ((url-object (url-generic-parse-url url))
         (url-host (url-host url-object)))
    (when (member url-host browse-url-multi-invidious-instances)
      (setf (url-host url-object) "www.youtube.com"
            url (url-recreate-url url-object))))
  url)

(defun browse-url-multi-select-invidious-instance (url)
  "Select invidious instance for `URL' with completion."
  (ido-completing-read (concat "Invidious instance for " url ": ")
                       browse-url-multi-invidious-instances
                       nil t))

(defun browse-url-multi-browser (url &rest args)
  "Select browser for `URL' and `ARGS'."
  (let ((read-answer-short t)
        (prompt (format "Browser %s " url))
        (choices '(("brave" ?b "" browse-url-generic)
                   ("eww" ?e "" eww-browse-url))))
    (apply (nth 3 (assoc (read-answer prompt choices) choices)) url args)))

(defun browse-url-multi-mpv (url &rest _args)
  "Open `URL' in mpv."
  (let ((url (browse-url-multi-maybe-change-host-to-youtube url)))
    (call-process "setsid" nil 0 nil "-f" "mpvi" url)))

(defun browse-url-multi-ytdl (url &rest _args)
  "Open `URL' in youtube-dl."
  (let ((url (browse-url-multi-maybe-change-host-to-youtube url)))
    (call-process "ytdli" nil 0 nil url)))

(defun browse-url-multi-invidious (url &rest args)
  "Select action for invidious `URL' and `ARGS'."
  (let ((instance (browse-url-multi-select-invidious-instance url))
        (url-object (url-generic-parse-url url)))
    (when (string-equal "youtu.be" (url-host url-object))
      (let* ((video-id (substring (car (url-path-and-query url-object)) 1 12))
             (query (url-build-query-string `(("v" ,video-id)))))
        (setf (url-filename url-object) (concat "/watch?" query))))
    (setf (url-host url-object) instance)
    (apply #'eww-browse-url (url-recreate-url url-object) args)))

(defun browse-url-multi-media (url &rest args)
  "Select action for media `URL' and `ARGS'."
  (let ((read-answer-short t)
        (prompt (format "Media %s " url))
        (choices '(("ytdl" ?y nil browse-url-multi-ytdl)
                   ("mpv" ?m nil browse-url-multi-mpv)
                   ("browser" ?b nil browse-url-multi-browser)
                   ("invidious" ?i nil browse-url-multi-invidious))))
    (apply (nth 3 (assoc (read-answer prompt choices) choices)) url args)))

(defun browse-url-multi (url &rest args)
  "Select action for `URL' and `ARGS'."
  (let* ((media-extensions (rx "." (or "flac" "m4a" "mp3" "ogg" "opus" "webm"
                                       "mkv" "mp4" "avi" "mpg" "mov" "3gp" "vob"
                                       "wmv" "aiff" "wav" "ogv" "flv")
                               eos))
         (media-domains (rx (or "youtube.com" "youtu.be" "bitchute.com"
                                "videos.lukesmith.xyz" "twitch.tv")))
         (url-object (url-generic-parse-url url))
         (url-type (url-type url-object))
         (url-host (url-host url-object))
         (url-path (car (url-path-and-query url-object))))
    (cond
     ((and (member url-type '(nil "file"))
           (string-match-p media-extensions url-path))
      (apply #'browse-url-multi-mpv url args))
     ((and (member url-type '("https" "http"))
           (or (string-match-p media-extensions url-path)
               (string-match-p media-domains url-host)
               (member url-host browse-url-multi-invidious-instances)))
      (apply #'browse-url-multi-media url args))
     (t
      (apply #'browse-url-multi-browser url args)))))

(defun browse-url-youtube-search (search &optional arg)
  "Browse-url `SEARCH' query in youtube.
Use new buffer if `ARG' is non-nil."
  (interactive "SSearch term: \nP")
  (browse-url (concat "https://www.youtube.com/search?"
                      (url-build-query-string `(("q" ,search))))
              arg))

(provide 'browse-url-multi)
