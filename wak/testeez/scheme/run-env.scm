(define-syntax only-dialects
  (syntax-rules ()
    ((_ dialect ...) '(dialect ...))))

(define-syntax all-dialects-except
  (syntax-rules ()
    ((_ dialect ...) '(dialect ...))))

; (define-syntax dialect
;   (lambda (form r compare)
;     (car (filter-map
;           (lambda (x) (and (eq? (car x) 'scheme48) (cadr x)))
;           (cdr form)))))

(define this-directory (make-parameter #f))

(define test-environment (make-parameter #f))