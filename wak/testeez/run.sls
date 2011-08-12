#!r6rs

;;@ Utilities for running testcases.
(library (wak testeez run)
  (export run-tests
          run-tests-and-exit
          run-tests-in-directory
          run-tests-in-file
          main)
  (import (except (rnrs base) string-copy string->list string-for-each)
          (rnrs eval)
          (rnrs control)
          (rnrs io simple)
          (rnrs lists)
          (rnrs exceptions)
          (rnrs programs)
          (spells strings)
          (spells char-set)
          (spells misc)
          (spells parameter)
          (spells filesys)
          (spells pathname)
          (spells tracing)
          (spells include)
          (wak testeez)
          (wak testeez utils)
          (wak testeez run-env))

  (define system-loader (make-parameter (lambda (system) #f)))
  
  (include-file ((wak testeez scheme) run)))
