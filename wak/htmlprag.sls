#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (wak htmlprag (0 19))
  (export
   html->shtml
   html->sxml
   html->sxml-0nf
   html->sxml-1nf
   html->sxml-2nf
   make-html-tokenizer
   make-shtml-entity
   parse-html/tokenizer
   shtml->html
   shtml-comment-symbol
   shtml-decl-symbol
   shtml-empty-symbol
   shtml-end-symbol
   shtml-entity-symbol
   shtml-entity-value
   shtml-named-char-id
   shtml-numeric-char-id
   shtml-pi-symbol
   shtml-start-symbol
   shtml-text-symbol
   shtml-token-kind
   shtml-top-symbol
   tokenize-html
   write-shtml-as-html)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (wak private include)
    (srfi :6 basic-string-ports))

  (define-syntax begin0
    (syntax-rules ()
      ((_ form0 form1 ...)
       (let ((result form0))
         (begin form1 ... result)))))
  
  (include-file ("wak" "htmlprag") "htmlprag.scm")
)
