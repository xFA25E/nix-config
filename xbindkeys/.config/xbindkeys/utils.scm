(define-module (utils)
  #:export (global-set-key global-set-keys main))

(use-modules (key-map) (permutate))

(define run-command (@@ (guile-user) run-command))
(define xbindkey-function (@@ (guile-user) xbindkey-function))
(define set-numlock! (@@ (guile-user) set-numlock!))

(define global-key-map (make-key-map))
(define current-key-map global-key-map)
(define current-key-path '())

(define (global-set-key key-chord definition)
  (define-key global-key-map key-chord
    (cond ((string? definition) (lambda () (run-command definition)))
          ((procedure? definition) definition)
          (else (error "Key definition can only be a string or a procedure")))))

(define-macro (global-set-keys keys cmds)
  (if (and (list? keys) (list? cmds))
      `(begin . ,(map (lambda (k c) `(global-set-key ,k ,c))
                      (permutate keys) (permutate cmds)))
      (error "Neither string or list in define-key")))

(define (notify-panel key-path)
  (run-command (string-append "sratus -u keyseq -v '" key-path "'")))

(define (current-run-key key)
  (let ((value (lookup-key current-key-map key)))
    (set! current-key-map
      (if (hash-table? value)
          value
          (begin (value) global-key-map))))

  (if (eq? current-key-map global-key-map)
      (unless (null? current-key-path)
        (set! current-key-path '())
        (notify-panel ""))
      (begin
        (set! current-key-path (cons key current-key-path))
        (notify-panel (string-join (reverse current-key-path) " ")))))

(define (bind-key key)
  (let ((run-key (lambda () (current-run-key key)))
        (ksym (kbd key)))
    (xbindkey-function ksym run-key)
    (xbindkey-function (cons "Mod2" ksym) run-key)))

(define (main)
  (set-numlock! #t)
  (for-each bind-key (delete! "s-g" (key-map-keys global-key-map)))
  (xbindkey-function
   (kbd "s-g")
   (lambda ()
     (set! current-key-map global-key-map)
     (unless (null? current-key-path)
       (set! current-key-path '())
       (notify-panel "")))))
