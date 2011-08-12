#!r6rs
(library (wak testeez run-env)
  (export only-dialects
          all-dialects-except
          this-directory
          test-environment)
  (import (rnrs base)
          (spells lists)
          (spells parameter)
          (spells pathname)
          (spells include))
  
  (include-file ((wak testeez scheme) run-env)))
