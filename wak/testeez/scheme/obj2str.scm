;; originally stolen from SLIB
(define (object->string obj)
  (cond ((symbol? obj) (symbol->string obj))
        ((number? obj) (number->string obj))
        (else
         (call-with-string-output-port
           (lambda (port)
             (write obj port))))))
