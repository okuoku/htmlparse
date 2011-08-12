#!r6rs
(library (wak testeez (0 2))
  (export testeez
          set-log-level!)
  (import (rnrs base)
          (rnrs io simple)
          (rnrs exceptions)
          (wak private include)
          (wak testeez utils))
  
  (include-file ((wak testeez scheme) testeez)))
