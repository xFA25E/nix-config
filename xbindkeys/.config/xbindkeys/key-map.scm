(define-module (key-map)
  #:export (make-key-map define-key lookup-key key-map-keys kbd))

(use-modules (ice-9 format)
             (srfi srfi-1))

(define make-key-map make-hash-table)

(define (define-key key-map key-chord procedure)
  (%define-key key-map (split-key-chord key-chord) procedure))

(define (lookup-key key-map key-chord)
  (%lookup-key key-map (split-key-chord key-chord)))

(define (key-map-keys key-map)
  (delete-duplicates!
   (apply
    append!
    (hash-map->list
     (lambda (key value)
       (cons key (if (hash-table? value) (key-map-keys value) '())))
     key-map))))

(define (kbd kstring)
  "Similar to emacs's kbd function, but it does not handle sequences."
  (define (kbd-modifier modifier)
    (cond ((string= modifier "s") "Mod4")
          ((string= modifier "S") "Shift")
          ((string= modifier "M") "Alt")
          ((string= modifier "C") "Control")
          (else (error (format #f "Unknown kdb string \"~A\"" modifier)))))

  (when (string-contains kstring " ")
    (error (format #f "String \"~A\" has spaces!" kstring)))

  (let loop ((s (string-split kstring #\-))
             (result '()))
    (if (null? (cdr s))
        (reverse! (cons (car s) result))
        (loop (cdr s) (cons (kbd-modifier (car s)) result)))))

(define (split-key-chord key-chord)
  (let ((key-path (string-split key-chord #\space)))
    (for-each kbd key-path)
    key-path))

(define (%define-key key-map key-path procedure)
  (let ((key (car key-path))
        (keys (cdr key-path)))
    (if (null? keys)
        (hash-set! key-map key procedure)
        (let ((ref (hash-ref key-map key)))
          (%define-key
           (cond ((hash-table? ref) ref)
                 ((not ref) (hash-set! key-map key (make-key-map)))
                 (else (error "Cant add key-map to single binding")))
           keys procedure)))))

(define (%lookup-key key-map key-path)
  (if (null? key-path)
      key-map
      (%lookup-key (hash-ref key-map (car key-path)) (cdr key-path))))
