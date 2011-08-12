#!r6rs
(library (wak testeez utils)
  (export object->string format-exception)
  (import (rnrs base)
          (rnrs io ports)
	  (rnrs io simple)
          (rnrs conditions)
          (spells include))

  (define (format-exception e)
    (cond ((condition? e)
           (map object->string (simple-conditions e)))
          (else
           (list (object->string e)))))

  (include-file ((wak testeez scheme) obj2str)))
