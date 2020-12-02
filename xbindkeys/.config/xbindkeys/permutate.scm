(define-module (permutate)
  #:export (permutate))

(define (arg->string arg)
  (cond
   ((string? arg) arg)
   ((number? arg) (number->string arg))
   ((symbol? arg) (symbol->string arg))
   (else (error "Permutations must be one of: string, number, symbol"))))

(define (prepare-permutations lst)
  (cons (let ((op (car lst)))
          (unless (memq op '(and or))
            (error "List should start with and/or."))
          op)
        (map (lambda (arg)
               (if (list? arg)
                   (prepare-permutations arg)
                   (arg->string arg)))
             (cdr lst))))

(define (handle-and pfx lst)
  (if (null? lst)
      (list pfx)
      (let ((head (car lst)) (tail (cdr lst)))
        (if (list? head)
            (apply append! (map (lambda (p) (handle-and p tail))
                                (%permutate pfx head)))
            (handle-and (string-append pfx head) tail)))))


(define (handle-or pfx lst)
  (if (null? lst)
      (list pfx)
      (apply append!
             (map (lambda (e)
                    (if (list? e)
                        (%permutate pfx e)
                        (list (string-append pfx e))))
                  lst))))

(define (%permutate pfx lst)
  (if (null? lst)
      (list pfx)
      (case (car lst)
        ((and) (handle-and pfx (cdr lst)))
        ((or) (handle-or pfx (cdr lst))))))

(define (permutate lst)
  (if (null? (cdr lst))
      (list)
      (%permutate "" (prepare-permutations lst))))
