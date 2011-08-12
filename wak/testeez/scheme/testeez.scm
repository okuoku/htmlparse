;; testeez.scm -- test framework
;; arch-tag: 3043451E-085C-4C55-AC09-035F32BA4DD9

;;; @Package     Testeez
;;; @Subtitle    Lightweight Unit Test Mechanism for Scheme
;;; @HomePage    http://www.neilvandyke.org/testeez/
;;; @Author      Neil W. Van Dyke
;;; @AuthorEmail neil@@neilvandyke.org
;;; @Version     0.2
;;; @Date        2005-03-07

;;; @legal
;;; Copyright @copyright{} 2005 Neil W. Van Dyke.  This program is Free
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at your option) any
;;; later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU Lesser
;;; General Public License [LGPL] for details.  For other license options and
;;; consulting, contact the author.
;;; @end legal

;;; Code:

;;; @section Introduction

;;; Testeez is a simple test case mechanism for R5RS Scheme.  It was written to
;;; support regression test suites embedded in the author's portable
;;; one-file-per-library Scheme libraries.
;;;
;;; A series of Testeez tests is listed within the @code{testeez} syntax.  By
;;; following a simple convention, these tests can be disabled and the
;;; dependency on Testeez removed for production code.  For example, to use
;;; Testeez in a ``Foo'' library, one can first add a syntax wrapper around
;;; @code{testeez} like so:
;;;
;;; @lisp
;;; (define-syntax foo-internal:testeez
;;;   (syntax-rules ()
;;;     ((_ x ...)
;;;      ;; Note: Comment-out exactly one of the following two lines.
;;;      ;; (error "Tests disabled.")
;;;      (testeez x ...)
;;;      )))
;;; @end lisp
;;;
;;; Then, this wrapper @code{foo-internal:testeez} can be used in a procedure
;;; that executes the test suite of the ``Foo'' library:
;;;
;;; @lisp
;;; (define (foo-internal:test)
;;;   (foo-internal:testeez
;;;    "Foo Station"
;;;
;;;    (test/equal "Put two and two together" (+ 2 2) 4)
;;;
;;;    (test-define "Bar function" bar (lambda (x) (+ x 42)))
;;;
;;;    (test/equal "Bar scene" (bar 69) 0)
;;;
;;;    (test/eqv   "Full circle" (* (bar -21) 2) 42)
;;;
;;;    (test/eqv   "Multiple"
;;;                (values (+ 2 2) (string #\h #\i) (char-upcase #\p))
;;;                (values 4 "hi" #\P))))
;;; @end lisp
;;;
;;; When the tests are enabled and @code{(foo-internal:test)} is evaluated,
;;; output like the following (which looks prettier fontified in Emacs'
;;; @code{*scheme*} buffer) is printed:
;;;
;;; @smallexample
;;; ;;; BEGIN "Foo Station" TESTS
;;;
;;; ;; 1. Put two and two together
;;; (+ 2 2)
;;; ;; ==> 4
;;; ;; Passed.
;;;
;;; ;; DEFINE: Bar function
;;; (define bar (lambda (x) (+ x 42)))
;;;
;;; ;; 2. Bar scene
;;; (bar 69)
;;; ;; ==> 111
;;; ;; FAILED!  Expected:
;;; ;;     0
;;;
;;; ;; 3. Full circle
;;; (* (bar -21) 2)
;;; ;; ==> 42
;;; ;; Passed.
;;;
;;; ;; 4. Multiple
;;; (values (+ 2 2) (string #\h #\i) (char-upcase #\p))
;;; ;; ==> 4
;;; ;;     "hi"
;;; ;;     #\P
;;; ;; Passed.
;;;
;;; ;;; END "Foo Station" TESTS: some FAILED
;;; ;;;     (Total: 4  Passed: 3  Failed: 1)
;;; @end smallexample
;;;
;;; Future versions of Testeez will add additional features, such as custom
;;; predicates and handling of errors.

;;; @section Interface

;;; The interface consists of the @code{testeez} syntax.

(define (%testeez:make-data title) (vector title 0 0 0 ""))

(define (%testeez:data-title  o) (vector-ref o 0))
(define (%testeez:data-total  o) (vector-ref o 1))
(define (%testeez:data-passed o) (vector-ref o 2))
(define (%testeez:data-failed o) (vector-ref o 3))

(define (%testeez:set-data-title!  o x) (vector-set! o 0 x))
(define (%testeez:set-data-total!  o x) (vector-set! o 1 x))
(define (%testeez:set-data-passed! o x) (vector-set! o 2 x))
(define (%testeez:set-data-failed! o x) (vector-set! o 3 x))

;;; jao --->
(define (%testeez:data-msg o) (vector-ref o 4))
(define (%testeez:set-data-msg! o x) (vector-set! o 4 x))

(define *testeez:log-level* 1)
(define (%testeez:level->int level)
  (case level
    ((low) 1)
    ((medium) 2)
    ((high) 3)
    (else 3)))

(define (%testeez:log-level>= level)
  (>= *testeez:log-level* (%testeez:level->int level)))

(define-syntax %testeez:with-level
  (syntax-rules ()
    ((_ level form ...) (if (%testeez:log-level>= 'level)
                            (begin form ...)))))

;;@ Set the log level. @1 can be either @code{'low}, @code{'medium} or
;; @code{'high}. Only messages with a level above @1 will be ouput.
(define (set-log-level! level)
  (set! *testeez:log-level* (%testeez:level->int level)))

;;; jao <---

(define (%testeez:print-values-list first-prefix next-prefix val-list)
  (display first-prefix)
  (let loop ((val-list val-list))
    (write (car val-list))
    (newline)
    (if (not (null? (cdr val-list)))
        (begin (display next-prefix)
               (loop (cdr val-list))))))

(define (%testeez:print-result result-list)
  (%testeez:print-values-list ";; ==> "
                              ";;     "
                              result-list))

(define (%testeez:start-test data desc expr-quoted)
  (%testeez:set-data-total! data (+ 1 (%testeez:data-total data)))
  (%testeez:set-data-msg! data
                          (string-append
                           (string #\newline) ";; "
                           (number->string (%testeez:data-total data))
                           ". "
                           desc
                           (string #\newline)
                           (object->string expr-quoted)
                           (string #\newline))))

(define (%testeez:finish-test data pred pred-rest result-list expected-list)
  (let ((failed (lambda ()
                  (%testeez:set-data-failed! data
                                             (+ 1 (%testeez:data-failed data)))
                  (display (%testeez:data-msg data))
                  (%testeez:print-result result-list)
                  (display ";; FAILED!  Expected:")
                  (newline)
                  (%testeez:print-values-list ";;     "
                                              ";;     "
                                              expected-list))))
    (let loop ((pred          pred)
               (pred-rest     pred-rest)
               (result-list   result-list)
               (expected-list expected-list))
      (if (null? result-list)
          (if (null? expected-list)
              (begin (%testeez:set-data-passed!
                      data
                      (+ 1 (%testeez:data-passed data)))
                     (%testeez:with-level
                      high
                      (display (%testeez:data-msg data))
                      (display ";; Passed.")
                      (newline)))
              (failed))
          (if (null? expected-list)
              (failed)
              (if (pred (car result-list) (car expected-list))
                  (if (null? pred-rest)
                      (loop pred
                            pred-rest
                            (cdr result-list)
                            (cdr expected-list))
                      (loop (car pred-rest)
                            (cdr pred-rest)
                            (cdr result-list)
                            (cdr expected-list)))
                  (failed)))))))

(define (%testeez:finish-test/exception data exception)
  (%testeez:set-data-failed! data (+ 1 (%testeez:data-failed data)))
  (display (%testeez:data-msg data))
  (display ";; FAILED!  Exception:")
  (newline)
  (%testeez:print-values-list ";;     "
                              ";;     "
                              (map (lambda (line)
                                     (string-append ";;     " line))
                                   (format-exception exception))))

(define (%testeez:start-eval data desc expr-quoted)
  (%testeez:set-data-msg! data
                          (string-append (string #\newline) ";; EVAL: "
                                         desc
                                         (string #\newline)
                                         (object->string expr-quoted)
                                         (string #\newline))))

(define (%testeez:start-define data desc expr-quoted)
  (%testeez:set-data-msg! data
                          (string-append (string #\newline) ";; DEFINE: "
                                         desc
                                         (string #\newline)
                                         (object->string expr-quoted)
                                         (string #\newline))))

(define (%testeez:start-tests title)
  (%testeez:with-level medium
                       (newline)
                       (display ";;; BEGIN ")
                       (write title)
                       (display " TESTS")
                       (newline))
  (%testeez:make-data title))

(define (%testeez:finish-tests data)
  (let ((total  (%testeez:data-total  data))
        (passed (%testeez:data-passed data))
        (failed (%testeez:data-failed data)))
    ;; TODO: Check that total = passed + failed
    (if (or (> failed 0)
            (%testeez:log-level>= 'medium))
        (begin
          (newline)
          (display ";;; END ")
          (write (%testeez:data-title data))
          (display " TESTS: ")
          (display (cond ((zero? failed) "all PASSED")
                         ((zero? passed) "ALL FAILED")
                         (else           "some FAILED")))
          (newline)
          (display ";;;     (Total: ")
          (display total)
          (display "  Passed: ")
          (display passed)
          (display "  Failed: ")
          (display failed)
          (display ")")
          (newline)))))

;;; @defsyntax testeez title form ...
;;;
;;; The @code{testeez} syntax contains a short string @var{title} and one or
;;; more @var{forms}, of the following syntaxes, which are evaluated in order.
;;;
;;; @table @code
;;;
;;; @item (test/equal @var{desc} @var{expr} @var{expected})
;;; Execute a test case.  @var{desc} is a short title or description of the
;;; test case, @var{expr} is a Scheme expression, and @var{expected} is an
;;; expression for the expected value (or multiple values).  The test case
;;; passes iff each value of @var{expr} is @code{equal?} to the corresponding
;;; value of @var{expected}.
;;;
;;; @item (test/eq @var{desc} @var{expr} @var{expected})
;;; Like @code{test/equal}, except the equivalence predicate is @code{eq?}
;;; rather than @code{equal?}.
;;;
;;; @item (test/eqv @var{desc} @var{expr} @var{expected})
;;; Like @code{test/equal}, except the equivalence predicate is @code{eqv?}
;;; rather than @code{equal?}.
;;;
;;; @item (test-define @var{desc} @var{name} @var{val})
;;; Bind a variable.  @var{desc} is a short description string, @var{name} is
;;; the identifier, and @var{val} is the value expression.  The binding is
;;; visible to the remainder of the enclosing @code{testeez} syntax.
;;;
;;; @item (test-eval @var{desc} @var{expr})
;;; Evaluate an expression.
;;;
;;; @end table

;; TODO: Lose the "begin"s.

;; TODO: Expose the custom equivalence predicates, once we're sure we like
;; the syntax.  Should add generic predicates first.

(define-syntax %testeez:body
  (syntax-rules
      (test/eq test/equal test/eqv test/equiv test-true test-false test-eval test-define)

    ((_ DATA-VAR
        (%testeez:test/equiv DESC EXPR EXPECTED (PRED0 PRED1 ...))
        REST ...)
     ;; TODO: Maybe turn "(PRED0 PRED1 ...)" into a string so that
     ;; "%testeez:finish-test" can report the equivalence predicate(s) used.
     (begin (%testeez:start-test  DATA-VAR DESC (quote EXPR))
            (guard (e (#t (%testeez:finish-test/exception DATA-VAR e)))
              (let ((result-list   (call-with-values (lambda () EXPR)     list))
                    (expected-list (call-with-values (lambda () EXPECTED) list)))
                (%testeez:finish-test DATA-VAR
                                      PRED0
                                      (quasiquote ((unquote PRED1) ...))
                                      result-list
                                      expected-list)))
            (%testeez:body        DATA-VAR REST ...)))

    ((_ DATA-VAR (test/equiv DESC EXPR EXPECTED (PRED0 PRED1 ...)) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR EXPECTED (PRED0 PRED1 ...))
                    REST ...))

    ((_ DATA-VAR (test/eq DESC EXPR EXPECTED) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR EXPECTED (eq?))
                    REST ...))

    ((_ DATA-VAR (test/equal DESC EXPR EXPECTED) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR EXPECTED (equal?))
                    REST ...))

    ((_ DATA-VAR (test/eqv DESC EXPR EXPECTED) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR EXPECTED (eqv?))
                    REST ...))

    ;;; jao ----->
    ((_ DATA-VAR (test-true DESC EXPR) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR #t (eq?))
                    REST ...))

    ((_ DATA-VAR (test-false DESC EXPR) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR #f (eq?))
                    REST ...))
    ;;; jao <-----

    ((_ DATA-VAR (test-define DESC NAME VAL) REST ...)
     (begin (%testeez:start-define DATA-VAR
                                   DESC
                                   (list 'define
                                         (quote NAME)
                                         (quote VAL)))
            (let ()
              (define NAME VAL)
              (%testeez:with-level high (display (%testeez:data-msg DATA-VAR)))
              (%testeez:body DATA-VAR REST ...))))
    ((_ DATA-VAR (test-eval DESC EXPR) REST ...)
     (begin (%testeez:start-eval  DATA-VAR DESC (quote EXPR))
;; ----> jao
            (let ((result (call-with-values (lambda () EXPR) list)))
              (%testeez:with-level high (%testeez:print-result result)))
;; <---- jao
            (%testeez:body         DATA-VAR REST ...)))

    ((_ DATA-VAR) (if #f #f))))

(define-syntax testeez
  (syntax-rules (test/equal test-eval test-define test-true test-false)
    ((_ TITLE BODY ...)
     (let ((data (%testeez:start-tests TITLE)))
       (%testeez:body         data BODY ...)
       (%testeez:finish-tests data)))))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.2 --- 2005-03-07
;;; Multiple values are now supported.  @code{test/eq} and @code{test/eqv}
;;; have been added.  Minor formatting changes to test log output.
;;;
;;; @item Version 0.1 --- 2005-01-02
;;; Initial release.
;;;
;;; @end table

;;; @unnumberedsec References

;;; @table @asis
;;;
;;; @item [LGPL]
;;; Free Software Foundation, ``GNU Lesser General Public License,'' Version
;;; 2.1, 1999-02, 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.@*
;;; @uref{http://www.gnu.org/copyleft/lesser.html}
;;;
;;; @end table
