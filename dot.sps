(import (rnrs)
        (mosh pp)
        (shorten)
        (srfi :26)
        (srfi :48)
        (yuni util files))

(define (statesym sym)
  (string->symbol
    (list->string 
      (map (^e (if (char=? #\- e) #\_ e))
           (string->list
             (symbol->string sym))))))

(define src (file->sexp-list "out.scm"))

(define states ;; ((sym . "descr") ...)
  (map (^e (cons (statesym (caar e)) (caddr (car e)))) src))

(define (state? sym)
  (find (^e (eq? (car e) (statesym sym))) states))

(define (transit state)
  ;; generate transition list from a state
  ;; transition ::= (#f sym)
  ;;                (((char ...) sym) ...)
  (define rule? (find (^e (and (pair? e) (eq? (car e) 'rule))) state))
  (define (filt e) (if (null? e) #f e))
  (define (flat x)
    (fold-left (^[cur e] (if (pair? e)
                           (append cur e)
                           (cons e cur)))
               '() x))
  (define (collect x)
    (cond
      ((and (symbol? x) (state? x)) (list x))
      ((pair? x)
       (flat (filter filt (map collect x))))
      (else '())))

  (if rule?
    (map (^e (cons (car e) (collect (cdr e)))) (cdr rule?))
    (cons #f (collect state))))

(define (out p)
  (define (line str) (display str p) (newline p))
  (define (emit-char-edge name l)
    (define (format-charlist c*)
      (define (fmtc c)
        (cond
          ((string? c) c)
          ((char=? #\newline c) "Newline")
          ((char=? #\tab c) "Tab")
          ((char=? #\xa c) "LF")
          ((char=? #\xc c) "FF")
          ((char=? #\space c) "Space")
          ((char=? #\nul c)
           "NULL")
          ((char=? #\" c) "\\\"")
          ((char=? #\' c) "\\'")
          (else (string c))))
      (fold-left (^[cur e] (string-append cur " " e))
                 ""
                 (map (^e (fmtc e)) c*)))
    (let ((char-list (car l))
          ;; if state-list is null, then state-list := (name)
          (state-list (if (null? (cdr l)) (list name) (cdr l))))
      (for-each (^e (line (format "~a -> ~a [label = \"~a\"]"
                                  (statesym name)
                                  (statesym e)
                                  (format-charlist char-list))))
                state-list)))
  (define (emit-misc-edge name e)
    (line (format "~a -> ~a" 
                  (statesym name)
                  (statesym e))))
  (line "digraph Gp {")
  (line "rankdir = TB")
  ;; emit nodes
  (for-each (^e (line (format "~a [label=\"~a\"]" (car e) (cdr e))))
            states)
  ;; emit edges
  (for-each (^e (let ((statename (caar e))
                      (tr (transit e)))
                  (if (car tr)
                    (for-each (cut emit-char-edge statename <>) tr)
                    (for-each (cut emit-misc-edge statename <>)
                              (let ((d (cdr tr)))
                                (if (pair? d) d (list d)))))))
            src)
  (line "}"))

(when (file-exists? "graph.dot")
  (delete-file "graph.dot"))

(call-with-output-file "graph.dot" out)
