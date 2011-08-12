;;; @Package     HtmlPrag
;;; @Subtitle    Pragmatic Parsing and Emitting of HTML using SXML and SHTML
;;; @HomePage    http://www.neilvandyke.org/htmlprag/
;;; @Author      Neil Van Dyke
;;; @Version     0.19
;;; @Date        2009-11-08
;;; @PLaneT      neil/htmlprag:1:6

;; $Id: htmlprag.ss,v 1.412 2009/11/08 08:29:06 neilpair Exp $

;;; @legal
;;; Copyright @copyright{} 2003 -- 2009 Neil Van Dyke.  This program is
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 3 of the License (LGPL 3), or (at your option)
;;; any later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See
;;; @indicateurl{http://www.gnu.org/licenses/} for details.  For other licenses
;;; and consulting, please contact the author.
;;; @end legal

;;; @section Introduction

;;; HtmlPrag provides permissive HTML parsing and emitting capability to Scheme
;;; programs.  The parser is useful for software agent extraction of
;;; information from Web pages, for programmatically transforming HTML files,
;;; and for implementing interactive Web browsers.  HtmlPrag emits ``SHTML,''
;;; which is an encoding of HTML in
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/SXML.html, SXML}, so that
;;; conventional HTML may be processed with XML tools such as
;;; @uref{http://pair.com/lisovsky/query/sxpath/, SXPath}.  Like Oleg
;;; Kiselyov's @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#HTML-parser,
;;; SSAX-based HTML parser}, HtmlPrag provides a permissive tokenizer, but also
;;; attempts to recover structure.  HtmlPrag also includes procedures for
;;; encoding SHTML in HTML syntax.
;;;
;;; The HtmlPrag parsing behavior is permissive in that it accepts erroneous
;;; HTML, handling several classes of HTML syntax errors gracefully, without
;;; yielding a parse error.  This is crucial for parsing arbitrary real-world
;;; Web pages, since many pages actually contain syntax errors that would
;;; defeat a strict or validating parser.  HtmlPrag's handling of errors is
;;; intended to generally emulate popular Web browsers' interpretation of the
;;; structure of erroneous HTML.  We euphemistically term this kind of parse
;;; ``pragmatic.''
;;;
;;; HtmlPrag also has some support for XHTML, although XML namespace qualifiers
;;; are currently accepted but stripped from the resulting SHTML.  Note that
;;; valid XHTML input is of course better handled by a strict XML parser.
;;;
;;; HtmlPrag requires R5RS, SRFI-6, and SRFI-23.  This version of HtmlPrag is
;;; specific to PLT Scheme, due to a transition period in how portability is
;;; handled, but the exceedingly portable version 0.16 is available at:
;;; @uref{http://www.neilvandyke.org/htmlprag/htmlprag-0-16.scm}

(define (%gosc os)
  (begin0 (get-output-string os)
    (close-output-port os)))

;;; @section SHTML and SXML

;;; SHTML is a variant of SXML, with two minor but useful extensions:
;;;
;;; @itemize
;;;
;;; @item
;;; The SXML keyword symbols, such as @code{*TOP*}, are defined to be in all
;;; uppercase, regardless of the case-sensitivity of the reader of the hosting
;;; Scheme implementation in any context.  This avoids several pitfalls.
;;;
;;; @item
;;; Since not all character entity references used in HTML can be converted to
;;; Scheme characters in all R5RS Scheme implementations, nor represented in
;;; conventional text files or other common external text formats to which one
;;; might wish to write SHTML, SHTML adds a special @code{&} syntax for
;;; non-ASCII (or non-Extended-ASCII) characters.  The syntax is @code{(&
;;; @var{val})}, where @var{val} is a symbol or string naming with the symbolic
;;; name of the character, or an integer with the numeric value of the
;;; character.
;;;
;;; @end itemize

;;; @defvar  shtml-comment-symbol
;;; @defvarx shtml-decl-symbol
;;; @defvarx shtml-empty-symbol
;;; @defvarx shtml-end-symbol
;;; @defvarx shtml-entity-symbol
;;; @defvarx shtml-pi-symbol
;;; @defvarx shtml-start-symbol
;;; @defvarx shtml-text-symbol
;;; @defvarx shtml-top-symbol
;;;
;;; These variables are bound to the following case-sensitive symbols used in
;;; SHTML, respectively: @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*},
;;; @code{*END*}, @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*},
;;; and @code{*TOP*}.  These can be used in lieu of the literal symbols in
;;; programs read by a case-insensitive Scheme reader.

(define shtml-comment-symbol (string->symbol "*COMMENT*"))
(define shtml-decl-symbol    (string->symbol "*DECL*"))
(define shtml-empty-symbol   (string->symbol "*EMPTY*"))
(define shtml-end-symbol     (string->symbol "*END*"))
(define shtml-entity-symbol  (string->symbol "*ENTITY*"))
(define shtml-pi-symbol      (string->symbol "*PI*"))
(define shtml-start-symbol   (string->symbol "*START*"))
(define shtml-text-symbol    (string->symbol "*TEXT*"))
(define shtml-top-symbol     (string->symbol "*TOP*"))

;;; @defvar  shtml-named-char-id
;;; @defvarx shtml-numeric-char-id
;;;
;;; These variables are bound to the SHTML entity public identifier strings
;;; used in SHTML @code{*ENTITY*} named and numeric character entity
;;; references.

(define shtml-named-char-id   "shtml-named-char")
(define shtml-numeric-char-id "shtml-numeric-char")

;;; @defproc make-shtml-entity val
;;;
;;; Yields an SHTML character entity reference for @var{val}.  For example:
;;;
;;; @lisp
;;; (make-shtml-entity "rArr")                  @result{} (& rArr)
;;; (make-shtml-entity (string->symbol "rArr")) @result{} (& rArr)
;;; (make-shtml-entity 151)                     @result{} (& 151)
;;; @end lisp

(define (make-shtml-entity val)
  (list '& (cond ((symbol?  val) val)
                 ((integer? val) val)
                 ((string?  val) (string->symbol val))
                 (else (error 'make-shtml-entity
                              "invalid SHTML entity value: ~S"
                              val)))))

;; TODO:
;;
;; (define (shtml-entity? x)
;;   (and (shtml-entity-value entity) #t))

;;; @defproc shtml-entity-value obj
;;;
;;; Yields the value for the SHTML entity @var{obj}, or @code{#f} if @var{obj}
;;; is not a recognized entity.  Values of named entities are symbols, and
;;; values of numeric entities are numbers.  An error may raised if @var{obj}
;;; is an entity with system ID inconsistent with its public ID.  For example:
;;;
;;; @lisp
;;; (define (f s) (shtml-entity-value (cadr (html->shtml s))))
;;; (f "&nbsp;")  @result{} nbsp
;;; (f "&#2000;") @result{} 2000
;;; @end lisp

(define (shtml-entity-value entity)
  (cond ((not (pair? entity)) #f)
        ((null? (cdr entity)) #f)
        ((eqv? (car entity) '&)
         ;; TODO: Error-check for extraneous list members?
         (let ((val (cadr entity)))
           (cond ((symbol?  val) val)
                 ((integer? val) val)
                 ((string?  val) (string->symbol val))
                 (else           #f))))
        ((eqv? (car entity) shtml-entity-symbol)
         (if (null? (cddr entity))
             #f
             (let ((public-id (list-ref entity 1))
                   (system-id (list-ref entity 2)))
               ;; TODO: Error-check for extraneous list members?
               (cond ((equal? public-id shtml-named-char-id)
                      (string->symbol system-id))
                     ((equal? public-id shtml-numeric-char-id)
                      (string->number system-id))
                     (else #f)))))
        (else #f)))

;;; @section Tokenizing

;;; The tokenizer is used by the higher-level structural parser, but can also
;;; be called directly for debugging purposes or unusual applications.  Some of
;;; the list structure of tokens, such as for start tag tokens, is mutated and
;;; incorporated into the SHTML list structure emitted by the parser.

;;; @defproc make-html-tokenizer in normalized?
;;;
;;; Constructs an HTML tokenizer procedure on input port @var{in}.  If boolean
;;; @var{normalized?} is true, then tokens will be in a format conducive to use
;;; with a parser emitting normalized SXML.  Each call to the resulting
;;; procedure yields a successive token from the input.  When the tokens have
;;; been exhausted, the procedure returns the null list.  For example:
;;;
;;; @lisp
;;; (define input (open-input-string "<a href=\"foo\">bar</a>"))
;;; (define next  (make-html-tokenizer input #f))
;;; (next) @result{} (a (^ (href "foo")))
;;; (next) @result{} "bar"
;;; (next) @result{} (*END* a)
;;; (next) @result{} ()
;;; (next) @result{} ()
;;; @end lisp

(define make-html-tokenizer
  ;; TODO: Have the tokenizer replace contiguous whitespace within individual
  ;; text tokens with single space characters (except for when in `pre' and
  ;; verbatim elements).  The parser will introduce new contiguous whitespace
  ;; (e.g., when text tokens are concatenated, invalid end tags are removed,
  ;; whitespace is irrelevant between certain elements), but then the parser
  ;; only has to worry about the first and last character of each string.
  ;; Perhaps the text tokens should have both leading and trailing whitespace
  ;; stripped, and contain flags for whether or not leading and trailing
  ;; whitespace occurred.
  (letrec ((no-token '())

           ;; TODO: Maybe make these three variables options.

           (verbatim-to-eof-elems '(plaintext))

           (verbatim-pair-elems '(script server style xmp))

           (ws-chars (list #\space
                           (integer->char 9)
                           (integer->char 10)
                           (integer->char 11)
                           (integer->char 12)
                           (integer->char 13)))

           (gosc/string-or-false
            (lambda (os)
              (let ((s (%gosc os)))
                (if (string=? s "") #f s))))

           (gosc/symbol-or-false
            (lambda (os)
              (let ((s (gosc/string-or-false os)))
                (if s (string->symbol s) #f))))
           )
    (lambda (in normalized?)
      ;; TODO: Make a tokenizer option that causes XML namespace qualifiers to
      ;; be ignored.
      (letrec
          (
           ;; Port buffer with inexpensive unread of one character and slightly
           ;; more expensive pushback of second character to unread.  The
           ;; procedures themselves do no consing.  The tokenizer currently
           ;; needs two-symbol lookahead, due to ambiguous "/" while parsing
           ;; element and attribute names, which could be either empty-tag
           ;; syntax or XML qualified names.
           (c           #f)
           (next-c      #f)
           (c-consumed? #t)
           (read-c      (lambda ()
                          (if c-consumed?
                              (if next-c
                                  (begin (set! c      next-c)
                                         (set! next-c #f))
                                  (set! c (read-char in)))
                              (set! c-consumed? #t))))
           (unread-c    (lambda ()
                          (if c-consumed?
                              (set! c-consumed? #f)
                              ;; TODO: Procedure name in error message really
                              ;; isn't "make-html-tokenizer"...
                              (error 'make-html-tokenizer
                                     "already unread: ~S"
                                     c))))
           (push-c      (lambda (new-c)
                          (if c-consumed?
                              (begin (set! c           new-c)
                                     (set! c-consumed? #f))
                              (if next-c
                                  (error 'make-html-tokenizer
                                         "pushback full: ~S"
                                         c)
                                  (begin (set! next-c      c)
                                         (set! c           new-c)
                                         (set! c-consumed? #f))))))

           ;; TODO: These procedures are a temporary convenience for
           ;; enumerating the pertinent character classes, with an eye towards
           ;; removing redundant tests of character class.  These procedures
           ;; should be eliminated in a future version.
           (c-eof?      (lambda () (eof-object? c)))
           (c-amp?      (lambda () (eqv? c #\&)))
           (c-apos?     (lambda () (eqv? c #\')))
           (c-bang?     (lambda () (eqv? c #\!)))
           (c-colon?    (lambda () (eqv? c #\:)))
           (c-quot?     (lambda () (eqv? c #\")))
           (c-equals?   (lambda () (eqv? c #\=)))
           (c-gt?       (lambda () (eqv? c #\>)))
           (c-lsquare?  (lambda () (eqv? c #\[)))
           (c-lt?       (lambda () (eqv? c #\<)))
           (c-minus?    (lambda () (eqv? c #\-)))
           (c-pound?    (lambda () (eqv? c #\#)))
           (c-ques?     (lambda () (eqv? c #\?)))
           (c-semi?     (lambda () (eqv? c #\;)))
           (c-slash?    (lambda () (eqv? c #\/)))
           (c-splat?    (lambda () (eqv? c #\*)))
           (c-lf?       (lambda () (eqv? c #\newline)))
           (c-angle?    (lambda () (memv c '(#\< #\>))))
           (c-ws?       (lambda () (memv c ws-chars)))
           (c-alpha?    (lambda () (char-alphabetic? c)))
           (c-digit?    (lambda () (char-numeric? c)))
           (c-alphanum? (lambda () (or (c-alpha?) (c-digit?))))
           (c-hexlet?   (lambda () (memv c '(#\a #\b #\c #\d #\e #\f
                                             #\A #\B #\C #\D #\E #\F))))

           (skip-ws     (lambda () (read-c) (if (c-ws?) (skip-ws) (unread-c))))

           (if-read-chars
            (lambda (match-chars yes-thunk no-proc)
              (let loop ((chars       match-chars)
                         (match-count 0))
                (if (null? chars)
                    (yes-thunk)
                    (begin (read-c)
                           (if (eqv? c (car chars))
                               (begin (loop (cdr chars) (+ 1 match-count)))
                               (begin (unread-c)
                                      (no-proc match-chars match-count))))))))

           (write-chars-count
            (lambda (chars count port)
              (let loop ((chars chars)
                         (count count))
                (or (zero? count)
                    (begin (write-char (car chars) port)
                           (loop (cdr chars)
                                 (- count 1)))))))

           (make-start-token
            (if normalized?
                (lambda (name ns attrs)
                  (list name (cons '^ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list name)
                      (list name (cons '^ attrs))))))

           (make-empty-token
            (lambda (name ns attrs)
              (cons shtml-empty-symbol
                    (make-start-token name ns attrs))))

           (make-end-token
            (if normalized?
                (lambda (name ns attrs)
                  (list shtml-end-symbol
                        name
                        (cons '^ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list shtml-end-symbol name)
                      (list shtml-end-symbol
                            name
                            (cons '^ attrs))))))

           (make-comment-token
            (lambda (str) (list shtml-comment-symbol str)))

           (make-decl-token
            (lambda (parts) (cons shtml-decl-symbol parts)))

           (scan-qname
            ;; TODO: Make sure we don't accept local names that have "*", since
            ;; this can break SXML tools.  Have to validate this afterwards if
            ;; "verbatim-safe?".  Also check for "^" and maybe "^^".  Check
            ;; qname parsing code, especially for verbatim mode.  This is
            ;; important!
            (lambda (verbatim-safe?)
              ;; Note: If we accept some invalid local names, we only need two
              ;; symbols of lookahead to determine the end of a qname.
              (letrec ((os      #f)
                       (ns      '())
                       (vcolons 0)
                       (good-os (lambda ()
                                  (or os
                                      (begin (set! os (open-output-string))
                                             os)))))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((or (c-ws?) (c-splat?))
                         (if verbatim-safe?
                             (unread-c)
                             #f))
                        ((or (c-angle?) (c-equals?) (c-quot?) (c-apos?))
                         (unread-c))
                        ((c-colon?)
                         (or (null? ns)
                             (set! ns (cons ":" ns)))
                         (if os
                             (begin
                               (set! ns (cons (%gosc os)
                                              ns))
                               (set! os #f))
                             #f)
                         (loop))
                        ((c-slash?)
                         (read-c)
                         (cond ((or (c-eof?)
                                    (c-ws?)
                                    (c-equals?)
                                    (c-apos?)
                                    (c-quot?)
                                    (c-angle?)
                                    (c-splat?))
                                (unread-c)
                                (push-c #\/))
                               (else (write-char #\/ (good-os))
                                     (write-char c   os)
                                     (loop))))
                        (else (write-char c (good-os))
                              (loop))))
                (let ((ns    (if (null? ns)
                                 #f
                                 (apply string-append
                                        (reverse ns))))
                      (local (if os (%gosc os) #f)))
                  (if verbatim-safe?
                      ;; TODO: Make sure we don't have ambiguous ":" or drop
                      ;; any characters!
                      (cons ns local)
                      ;; Note: We represent "xml:" and "xmlns:" syntax as
                      ;; normal qnames, for lack of something better to do with
                      ;; them when we don't support XML namespaces.
                      ;;
                      ;; TODO: Local names are currently forced to lowercase,
                      ;; since HTML is usually case-insensitive.  If XML
                      ;; namespaces are used, we might wish to keep local names
                      ;; case-sensitive.
                      (if local
                          (if ns
                              (if (or (string=? ns "xml")
                                      (string=? ns "xmlns"))
                                  (string->symbol (string-append ns ":" local))
                                  (cons ns
                                        (string->symbol (string-downcase
                                                         local))))
                              (string->symbol (string-downcase local)))
                          (if ns
                              (string->symbol (string-downcase ns))
                              ;; TODO: Ensure in rest of code that returning #f
                              ;; as a name here is OK.
                              #f)))))))

           (scan-tag
            (lambda (start?)
              (skip-ws)
              (let ((tag-name   (scan-qname #f))
                    (tag-ns     #f)
                    (tag-attrs  #f)
                    (tag-empty? #f))
                ;; Scan element name.
                (if (pair? tag-name)
                    (begin (set! tag-ns   (car tag-name))
                           (set! tag-name (cdr tag-name)))
                    #f)
                ;; TODO: Ensure there's no case in which a #f tag-name isn't
                ;; compensated for later.
                ;;
                ;; Scan element attributes.
                (set! tag-attrs
                      (let scan-attr-list ()
                        (read-c)
                        (cond ((c-eof?)   '())
                              ((c-angle?) (unread-c) '())
                              ((c-slash?)
                               (set! tag-empty? #t)
                               (scan-attr-list))
                              ((c-alpha?)
                               (unread-c)
                               (let ((attr (scan-attr)))
                                 (cons attr (scan-attr-list))))
                              (else (scan-attr-list)))))
                ;; Find ">" or unnatural end.
                (let loop ()
                  (read-c)
                  (cond ((c-eof?)   no-token)
                        ((c-slash?) (set! tag-empty? #t) (loop))
                        ((c-gt?)    #f)
                        ((c-ws?)    (loop))
                        (else       (unread-c))))
                ;; Change the tokenizer mode if necessary.
                (cond ((not start?) #f)
                      (tag-empty?   #f)
                      ;; TODO: Maybe make one alist lookup here, instead of
                      ;; two.
                      ((memq tag-name verbatim-to-eof-elems)
                       (set! nexttok verbeof-nexttok))
                      ((memq tag-name verbatim-pair-elems)
                       (set! nexttok (make-verbpair-nexttok tag-name))))
                ;; Return a token object.
                (if start?
                    (if tag-empty?
                        (make-empty-token tag-name tag-ns tag-attrs)
                        (make-start-token tag-name tag-ns tag-attrs))
                    (make-end-token tag-name tag-ns tag-attrs)))))

           (scan-attr
            (lambda ()
              (let ((name (scan-qname #f))
                    (val  #f))
                (if (pair? name)
                    (set! name (cdr name))
                    #f)
                (let loop-equals-or-end ()
                  (read-c)
                  (cond ((c-eof?) no-token)
                        ((c-ws?)  (loop-equals-or-end))
                        ((c-equals?)
                         (let loop-quote-or-unquoted ()
                           (read-c)
                           (cond ((c-eof?) no-token)
                                 ((c-ws?) (loop-quote-or-unquoted))
                                 ((or (c-apos?) (c-quot?))
                                  (let ((term c))
                                    (set! val (open-output-string))
                                    (let loop-quoted-val ()
                                      (read-c)
                                      (cond ((c-eof?)      #f)
                                            ((eqv? c term) #f)
                                            (else (write-char c val)
                                                  (loop-quoted-val))))))
                                 ((c-angle?) (unread-c))
                                 (else
                                  (set! val (open-output-string))
                                  (write-char c val)
                                  (let loop-unquoted-val ()
                                    (read-c)
                                    (cond ((c-eof?)  no-token)
                                          ((c-apos?) #f)
                                          ((c-quot?) #f)
                                          ((or (c-ws?) (c-angle?)
                                               ;;(c-slash?)
                                               )
                                           (unread-c))
                                          ;; Note: We can treat a slash in an
                                          ;; unquoted attribute value as a
                                          ;; value constituent because the
                                          ;; slash is specially-handled only
                                          ;; for XHTML, and XHTML attribute
                                          ;; values must always be quoted.  We
                                          ;; could do lookahead for "/>", but
                                          ;; that wouldn't let us parse HTML
                                          ;; "<a href=/>" correctly, so this is
                                          ;; an easier and more correct way to
                                          ;; do things.
                                          (else (write-char c val)
                                                (loop-unquoted-val))))))))
                        (else (unread-c))))
                (if normalized?
                    (list name (if val
                                   (%gosc val)
                                   (symbol->string name)))
                    (if val
                        (list name (%gosc val))
                        (list name))))))

           (scan-comment
            ;; TODO: Rewrite this to use tail recursion rather than a state
            ;; variable.
            (lambda ()
              (let ((os    (open-output-string))
                    (state 'start-minus))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((c-minus?)
                         (set! state
                               (case state
                                 ((start-minus)            'start-minus-minus)
                                 ((start-minus-minus body) 'end-minus)
                                 ((end-minus)              'end-minus-minus)
                                 ((end-minus-minus) (write-char #\- os) state)
                                 (else (error '<make-html-tokenizer>
                                              "invalid state: ~S"
                                              state))))
                         (loop))
                        ((and (c-gt?) (eq? state 'end-minus-minus)) #f)
                        (else (case state
                                ((end-minus)       (write-char #\- os))
                                ((end-minus-minus) (display "--" os)))
                              (set! state 'body)
                              (write-char c os)
                              (loop))))
                (make-comment-token (%gosc os)))))

           (scan-possible-cdata
            (lambda ()
              ;; Read "<!" and current character is "[", so try to read the
              ;; rest of the CDATA start delimeter.
              (if-read-chars
               '(#\C #\D #\A #\T #\A #\[)
               (lambda ()
                 ;; Successfully read CDATA section start delimiter, so read
                 ;; the section.
                 (scan-cdata))
               (lambda (chars count)
                 ;; Did not read rest of CDATA section start delimiter, so
                 ;; return a string for what we did read.
                 (let ((os (open-output-string)))
                   (display "<![" os)
                   (write-chars-count chars count os)
                   (%gosc os))))))

           (scan-cdata
            (lambda ()
              (let ((os (open-output-string)))
                (let loop ()
                  (if-read-chars
                   '(#\] #\] #\>)
                   (lambda () (%gosc os))
                   (lambda (chars count)
                     (if (zero? count)
                         (if (eof-object? c)
                             (%gosc os)
                             (begin (write-char c os)
                                    (read-c)
                                    (loop)))
                         (begin (write-char #\] os)
                                (if (= count 2)
                                    (push-c #\])
                                    #f)
                                (loop)))))))))

           (scan-pi
            (lambda ()
              (skip-ws)
              (let ((name (open-output-string))
                    (val  (open-output-string)))
                (let scan-name ()
                  (read-c)
                  (cond ((c-eof?)   #f)
                        ((c-ws?)    #f)
                        ((c-alpha?) (write-char c name) (scan-name))
                        (else       (unread-c))))
                ;; TODO: Do we really want to emit #f for PI name?
                (set! name (gosc/symbol-or-false name))
                (let scan-val ()
                  (read-c)
                  (cond ((c-eof?)  #f)
                        ;; ((c-amp?) (display (scan-entity) val)
                        ;;           (scan-val))
                        ((c-ques?)
                         (read-c)
                         (cond ((c-eof?) (write-char #\? val))
                               ((c-gt?)  #f)
                               (else     (write-char #\? val)
                                         (unread-c)
                                         (scan-val))))
                        (else (write-char c val) (scan-val))))
                (list shtml-pi-symbol
                      name
                      (%gosc val)))))

           (scan-decl
            ;; TODO: Find if SXML includes declaration forms, and if so, use
            ;; whatever format SXML wants.
            ;;
            ;; TODO: Rewrite to eliminate state variables.
            (letrec
                ((scan-parts
                  (lambda ()
                    (let ((part       (open-output-string))
                          (nonsymbol? #f)
                          (state      'before)
                          (last?      #f))
                      (let loop ()
                        (read-c)
                        (cond ((c-eof?) #f)
                              ((c-ws?)
                               (case state
                                 ((before) (loop))
                                 ((quoted) (write-char c part) (loop))))
                              ((and (c-gt?) (not (eq? state 'quoted)))
                               (set! last? #t))
                              ((and (c-lt?) (not (eq? state 'quoted)))
                               (unread-c))
                              ((c-quot?)
                               (case state
                                 ((before)   (set! state 'quoted) (loop))
                                 ((unquoted) (unread-c))
                                 ((quoted)   #f)))
                              (else
                               (if (eq? state 'before)
                                   (set! state 'unquoted)
                                   #f)
                               (set! nonsymbol? (or nonsymbol?
                                                    (not (c-alphanum?))))
                               (write-char c part)
                               (loop))))
                      (set! part (%gosc part))
                      (if (string=? part "")
                          '()
                          (cons (if (or (eq? state 'quoted) nonsymbol?)
                                    part
                                    ;; TODO: Normalize case of things we make
                                    ;; into symbols here.
                                    (string->symbol part))
                                (if last?
                                    '()
                                    (scan-parts))))))))
              (lambda () (make-decl-token (scan-parts)))))

           (scan-entity
            (lambda ()
              (read-c)
              (cond ((c-eof?) "&")
                    ((c-alpha?)
                     ;; TODO: Do entity names have a maximum length?
                     (let ((name (open-output-string)))
                       (write-char c name)
                       (let loop ()
                         (read-c)
                         (cond ((c-eof?)   #f)
                               ((c-alpha?) (write-char c name) (loop))
                               ((c-semi?)  #f)
                               (else       (unread-c))))
                       (set! name (%gosc name))
                       ;; TODO: Make the entity map an option.
                       (let ((pair (assoc name '(("amp"  . "&")
                                                 ("apos" . "'")
                                                 ("gt"   . ">")
                                                 ("lt"   . "<")
                                                 ("quot" . "\"")))))
                         (if pair
                             (cdr pair)
                             (make-shtml-entity name)))))
                    ((c-pound?)
                     (let ((num  (open-output-string))
                           (hex? #f))
                       (read-c)
                       (cond ((c-eof?)            #f)
                             ((memv c '(#\x #\X)) (set! hex? #t) (read-c)))
                       (let loop ()
                         (cond ((c-eof?)  #f)
                               ((c-semi?) #f)
                               ((or (c-digit?) (and hex? (c-hexlet?)))
                                (write-char c num)
                                (read-c)
                                (loop))
                               (else (unread-c))))
                       (set! num (%gosc num))
                       (if (string=? num "")
                           "&#;"
                           (let ((n (string->number num (if hex? 16 10))))
                             (if (<= 32 n 126)
                                 ;; (and (<= 32 n 255) (not (= n 127)))
                                 (string (integer->char n))
                                 (make-shtml-entity n))))))
                    (else (unread-c) "&"))))

           (normal-nexttok
            (lambda ()
              (read-c)
              (cond ((c-eof?) no-token)
                    ((c-lt?)
                     (let loop ()
                       (read-c)
                       (cond ((c-eof?)   "<")
                             ;; ((c-ws?)    (loop))
                             ((c-slash?) (scan-tag #f))
                             ((c-ques?)  (scan-pi))
                             ((c-alpha?) (unread-c) (scan-tag #t))
                             ((c-bang?)
                              (read-c)
                              (if (c-lsquare?)
                                  (scan-possible-cdata)
                                  (let loop ()
                                    (cond ((c-eof?)   no-token)
                                          ((c-ws?)    (read-c) (loop))
                                          ((c-minus?) (scan-comment))
                                          (else       (unread-c)
                                                      (scan-decl))))))
                             (else (unread-c) "<"))))
                    ((c-gt?) ">")
                    (else (let ((os (open-output-string)))
                            (let loop ()
                              (cond ((c-eof?)   #f)
                                    ((c-angle?) (unread-c))
                                    ((c-amp?)
                                     (let ((entity (scan-entity)))
                                       (if (string? entity)
                                           (begin (display entity os)
                                                  (read-c)
                                                  (loop))
                                           (let ((saved-nexttok nexttok))
                                             (set! nexttok
                                                   (lambda ()
                                                     (set! nexttok
                                                           saved-nexttok)
                                                     entity))))))
                                    (else (write-char c os)
                                          (or (c-lf?)
                                              (begin (read-c) (loop))))))
                            (let ((text (%gosc os)))
                              (if (equal? text "")
                                  (nexttok)
                                  text)))))))

           (verbeof-nexttok
            (lambda ()
              (read-c)
              (if (c-eof?)
                  no-token
                  (let ((os (open-output-string)))
                    (let loop ()
                      (or (c-eof?)
                          (begin (write-char c os)
                                 (or (c-lf?)
                                     (begin (read-c) (loop))))))
                    (%gosc os)))))

           (make-verbpair-nexttok
            (lambda (elem-name)
              (lambda ()
                (let ((os (open-output-string)))
                  ;; Accumulate up to a newline-terminated line.
                  (let loop ()
                    (read-c)
                    (cond ((c-eof?)
                           ;; Got EOF in verbatim context, so set the normal
                           ;; nextok procedure, then fall out of loop.
                           (set! nexttok normal-nexttok))
                          ((c-lt?)
                           ;; Got "<" in verbatim context, so get next
                           ;; character.
                           (read-c)
                           (cond ((c-eof?)
                                  ;; Got "<" then EOF, so set to the normal
                                  ;; nexttok procedure, add the "<" to the
                                  ;; verbatim string, and fall out of loop.
                                  (set! nexttok normal-nexttok)
                                  (write-char #\< os))
                                 ((c-slash?)
                                  ;; Got "</", so...
                                  (read-c)
                                  (cond
                                   ((c-eof?)
                                    (display "</" os))
                                   ((c-alpha?)
                                    ;; Got "</" followed by alpha, so unread
                                    ;; the alpha, scan qname, compare...
                                    (unread-c)
                                    (let* ((vqname (scan-qname #t))
                                           (ns     (car vqname))
                                           (local  (cdr vqname)))
                                      ;; Note: We ignore XML namespace
                                      ;; qualifier for purposes of comparison.
                                      ;;
                                      ;; Note: We're interning strings here for
                                      ;; comparison when in theory there could
                                      ;; be many such unique interned strings
                                      ;; in a valid HTML document, although in
                                      ;; practice this should not be a problem.
                                      (if (and local
                                               (eqv? (string->symbol
                                                      (string-downcase local))
                                                     elem-name))
                                          ;; This is the terminator tag, so
                                          ;; scan to the end of it, set the
                                          ;; nexttok, and fall out of the loop.
                                          (begin
                                            (let scan-to-end ()
                                              (read-c)
                                              (cond ((c-eof?) #f)
                                                    ((c-gt?)  #f)
                                                    ((c-lt?)  (unread-c))
                                                    ((c-alpha?)
                                                     (unread-c)
                                                     ;; Note: This is an
                                                     ;; expensive way to skip
                                                     ;; over an attribute, but
                                                     ;; in practice more
                                                     ;; verbatim end tags will
                                                     ;; not have attributes.
                                                     (scan-attr)
                                                     (scan-to-end))
                                                    (else (scan-to-end))))
                                            (set! nexttok
                                                  (lambda ()
                                                    (set! nexttok
                                                          normal-nexttok)
                                                    (make-end-token
                                                     elem-name #f '()))))
                                          ;; This isn't the terminator tag, so
                                          ;; add to the verbatim string the
                                          ;; "</" and the characters of what we
                                          ;; were scanning as a qname, and
                                          ;; recurse in the loop.
                                          (begin
                                            (display "</" os)
                                            (if ns
                                                (begin (display ns os)
                                                       (display ":" os))
                                                #f)
                                            (if local
                                                (display local os)
                                                #f)
                                            (loop)))))
                                   (else
                                    ;; Got "</" and non-alpha, so unread new
                                    ;; character, add the "</" to verbatim
                                    ;; string, then loop.
                                    (unread-c)
                                    (display "</" os)
                                    (loop))))
                                 (else
                                  ;; Got "<" and non-slash, so unread the new
                                  ;; character, write the "<" to the verbatim
                                  ;; string, then loop.
                                  (unread-c)
                                  (write-char #\< os)
                                  (loop))))
                          (else
                           ;; Got non-"<" in verbatim context, so just add it
                           ;; to the buffer, then, if it's not a linefeed, fall
                           ;; out of the loop so that the token can be
                           ;; returned.
                           (write-char c os)
                           (or (c-lf?) (loop)))))
                  ;; Return the accumulated line string, if non-null, or call
                  ;; nexttok.
                  (or (gosc/string-or-false os) (nexttok))))))

           (nexttok #f))

        (set! nexttok normal-nexttok)
        (lambda () (nexttok))))))

;;; @defproc tokenize-html in normalized?
;;;
;;; Returns a list of tokens from input port @var{in}, normalizing according to
;;; boolean @var{normalized?}.  This is probably most useful as a debugging
;;; convenience.  For example:
;;;
;;; @lisp
;;; (tokenize-html (open-input-string "<a href=\"foo\">bar</a>") #f)
;;; @result{} ((a (^ (href "foo"))) "bar" (*END* a))
;;; @end lisp

(define (tokenize-html in normalized?)
  (let ((next-tok (make-html-tokenizer in normalized?)))
    (let loop ((tok (next-tok)))
      (if (null? tok)
          '()
          (cons tok (loop (next-tok)))))))

;;; @defproc shtml-token-kind token
;;;
;;; Returns a symbol indicating the kind of tokenizer @var{token}:
;;; @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*}, @code{*END*},
;;; @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*}.
;;; This is used by higher-level parsing code.  For example:
;;;
;;; @lisp
;;; (map shtml-token-kind
;;;      (tokenize-html (open-input-string "<a<b>><c</</c") #f))
;;; @result{} (*START* *START* *TEXT* *START* *END* *END*)
;;; @end lisp

(define (shtml-token-kind token)
  (cond ((string? token) shtml-text-symbol)
        ((list?   token)
         (let ((s (list-ref token 0)))
           (if (memq s `(,shtml-comment-symbol
                         ,shtml-decl-symbol
                         ,shtml-empty-symbol
                         ,shtml-end-symbol
                         ,shtml-entity-symbol
                         ,shtml-pi-symbol))
               s
               shtml-start-symbol)))
        (else (error 'shtml-token-kind
                     "unrecognized token kind: ~S"
                     token))))

;;; @section Parsing

;;; Most applications will call a parser procedure such as
;;; @code{html->shtml} rather than calling the tokenizer directly.

;; @defvar %empty-elements
;;
;; List of names of HTML element types that have no content, represented as a
;; list of symbols.  This is used internally by the parser and encoder.  The
;; effect of mutating this list is undefined.

;; TODO: Document exactly which elements these are, after we make the new
;; parameterized parser constructor.

(define %empty-elements
  '(& area base br frame hr img input isindex keygen link meta object param
      spacer wbr))

;;; @defproc parse-html/tokenizer tokenizer normalized?
;;;
;;; Emits a parse tree like @code{html->shtml} and related procedures, except
;;; using @var{tokenizer} as a source of tokens, rather than tokenizing from an
;;; input port.  This procedure is used internally, and generally should not be
;;; called directly.

(define parse-html/tokenizer
  ;; Note: This algorithm was originally written in 2001 (as part of the first
  ;; Scheme library the author ever wrote), and then on 2009-08-16 was revamped
  ;; to not use mutable pairs, for PLT 4 compatibility.  It could still use
  ;; some work to be more functional, but it works for now.
  (letrec ((empty-elements
            ;; TODO: Maybe make this an option.  This might also be an
            ;; acceptable way to parse old HTML that uses the `p' element as a
            ;; paragraph terminator.
            %empty-elements)
           (parent-constraints
            ;; TODO: Maybe make this an option.
            '((area     . (map))
              (body     . (html))
              (caption  . (table))
              (colgroup . (table))
              (dd       . (dl))
              (dt       . (dl))
              (frame    . (frameset))
              (head     . (html))
              (isindex  . (head))
              (li       . (dir menu ol ul))
              (meta     . (head))
              (noframes . (frameset))
              (option   . (select))
              (p        . (body li td th))
              (param    . (applet))
              (tbody    . (table))
              (td       . (tr))
              (th       . (tr))
              (thead    . (table))
              (title    . (head))
              (tr       . (table tbody thead))))
           (token-kinds-that-always-get-added
            `(,shtml-comment-symbol
              ,shtml-decl-symbol
              ,shtml-entity-symbol
              ,shtml-pi-symbol
              ,shtml-text-symbol))
           (start-tag-name (lambda (tag-token) (car tag-token)))
           (end-tag-name   (lambda (tag-token) (list-ref tag-token 1))))
    (lambda (tokenizer normalized?)
      (let ((begs (list (vector #f '()))))
        (letrec ((add-thing-as-child-of-current-beg
                  (lambda (tok)
                    (let ((beg (car begs)))
                      (vector-set! beg 1 (cons tok (vector-ref beg 1))))))

                 (beg->elem
                  (lambda (beg)
                    (let ((elem-name          (vector-ref beg 0))
                          (attrs-and-contents (reverse (vector-ref beg 1))))
                      (cons elem-name attrs-and-contents))))

                 (finish-current-beg-and-return-elem
                  (lambda ()
                    (let ((elem (beg->elem (car begs))))
                      (set! begs (cdr begs))
                      (or (null? begs)
                          (add-thing-as-child-of-current-beg elem))
                      elem)))

                 (finish-current-beg
                  (lambda ()
                    (finish-current-beg-and-return-elem)))

                 (finish-all-begs-and-return-top
                  (lambda ()
                    (let loop ()
                      (let ((elem (finish-current-beg-and-return-elem)))
                        (if (car elem)
                            (loop)
                            (cdr elem))))))

                 (finish-begs-up-to-and-including-name
                  (lambda (name)
                    (let loop-find-name ((find-begs begs)
                                         (depth     1))
                      (let ((beg-name (vector-ref (car find-begs) 0)))
                        (cond ((not beg-name)
                               ;; We reached the root without finding a
                               ;; matching beg, so simply discard it.
                               #f)
                              ((eqv? name beg-name)
                               ;; We found a match, so finish the begs up to
                               ;; depth.
                               (let loop-finish ((depth depth))
                                 (or (zero? depth)
                                     (begin
                                       (finish-current-beg)
                                       (loop-finish (- depth 1))))))
                              (else
                               ;; Didn't find a match yet, and there's still at
                               ;; least one more beg to look at, so recur.
                               (loop-find-name (cdr find-begs)
                                               (+ depth 1))))))))

                 (finish-begs-upto-but-not-including-names
                  (lambda (names)
                    (let loop-find-name ((find-begs begs)
                                         (depth     0))
                      (let ((beg-name (vector-ref (car find-begs) 0)))
                        (cond ((not beg-name)
                               ;; We reached the root without finding a
                               ;; matching beg, so simply discard it.
                               #f)
                              ((memq beg-name names)
                               ;; We found a match, so finish the begs up to
                               ;; depth.
                               (let loop-finish ((depth depth))
                                 (or (zero? depth)
                                     (begin
                                       (finish-current-beg)
                                       (loop-finish (- depth 1))))))
                              (else
                               ;; Didn't find a match yet, and there's still at
                               ;; least one more beg to look at, so recur.
                               (loop-find-name (cdr find-begs)
                                               (+ depth 1)))))))))

          (let loop ()
            (let ((tok (tokenizer)))
              (if (null? tok)
                  (finish-all-begs-and-return-top)
                  (let ((kind (shtml-token-kind tok)))
                    (cond ((memv kind token-kinds-that-always-get-added)
                           (add-thing-as-child-of-current-beg tok))
                          ((eqv? kind shtml-start-symbol)
                           (let* ((name (start-tag-name tok))
                                  (cell (assq name parent-constraints)))
                             (and cell
                                  (finish-begs-upto-but-not-including-names
                                   (cons 'div (cdr cell))))
                             (if (memq name empty-elements)
                                 (add-thing-as-child-of-current-beg tok)
                                 (set! begs (cons (vector (car tok)
                                                          (cdr tok))
                                                  begs)))))
                          ((eqv? kind shtml-empty-symbol)
                           ;; Empty tag token, so just add it to current
                           ;; beginning while stripping off leading `*EMPTY*'
                           ;; symbol so that the token becomes normal SXML
                           ;; element syntax.
                           (add-thing-as-child-of-current-beg (cdr tok)))
                          ((eqv? kind shtml-end-symbol)
                           (let ((name (end-tag-name tok)))
                             (if name
                                 ;; Try to finish to a start tag matching this
                                 ;; end tag.  If none, just drop the token,
                                 ;; though we used to add it to the current
                                 ;; beginning.
                                 (finish-begs-up-to-and-including-name
                                  name)
                                 ;; We have an anonymous end tag, so match it
                                 ;; with the most recent beginning.  If no
                                 ;; beginning to match, then just drop the
                                 ;; token, though we used to add it to the
                                 ;; current beginning.
                                 (and (vector-ref (car begs) 0)
                                      (finish-current-beg)))))
                          (else (error 'parse-html/tokenizer
                                       "unknown tag kind: ~S"
                                       kind)))
                    (loop))))))))))

;; TODO: Quote of message to a user:
;;
;; >I think this behavior is due to HtmlPrag's use in "parse-html/tokenizer"
;; >of its local "parent-constraints" variable.
;; >
;; >The following line of code from the variable binding expresses the
;; >constraint that any "p" element can have as immediate parent element
;; >only "body", "td", or "th":
;; >
;; >              (p        . (body td th))
;; >
;; >I think I know a good heuristic for dealing with unfamiliar but
;; >seemingly well-formed elements, like "page" in this case, but I'm afraid
;; >I don't have time to implement it right now.  (I am job-hunting right
;; >now, and there are many other coding things I need to do first.)
;; >
;; >Would adding "page" to the above line of the HtmlPrag source code work
;; >around the current problem, or do you need a better solution right now?

;; @defproc %parse-html input normalized? top?
;;
;; This procedure is now used internally by @code{html->shtml} and its
;; variants, and should not be used directly by programs.  The interface is
;; likely to change in future versions of HtmlPrag.

(define (%parse-html input normalized? top?)
  (let ((parse
         (lambda ()
           (parse-html/tokenizer
            (make-html-tokenizer
             (cond ((input-port? input) input)
                   ((string?     input) (open-input-string input))
                   (else (error '%parse-html
                                "invalid input type: ~S"
                                input)))
             normalized?)
            normalized?))))
    (if top?
        (cons shtml-top-symbol (parse))
        (parse))))

;;; @defproc  html->sxml-0nf input
;;; @defprocx html->sxml-1nf input
;;; @defprocx html->sxml-2nf input
;;; @defprocx html->sxml     input
;;; @defprocx html->shtml    input
;;;
;;; Permissively parse HTML from @var{input}, which is either an input port or
;;; a string, and emit an SHTML equivalent or approximation.  To borrow and
;;; slightly modify an example from Kiselyov's discussion of his HTML parser:
;;;
;;; @lisp
;;; (html->shtml
;;;  "<html><head><title></title><title>whatever</title></head><body>
;;; <a href=\"url\">link</a><p align=center><ul compact style=\"aa\">
;;; <p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened</i>
;;; still &lt; bold </b></body><P> But not done yet...")
;;; @result{}
;;; (*TOP* (html (head (title) (title "whatever"))
;;;              (body "\n"
;;;                    (a (^ (href "url")) "link")
;;;                    (p (^ (align "center"))
;;;                       (ul (^ (compact) (style "aa")) "\n"))
;;;                    (p "BLah"
;;;                       (*COMMENT* " comment <comment> ")
;;;                       " "
;;;                       (i " italic " (b " bold " (tt " ened")))
;;;                       "\n"
;;;                       "still < bold "))
;;;              (p " But not done yet...")))
;;; @end lisp
;;;
;;; Note that in the emitted SHTML the text token @code{"still < bold"} is
;;; @emph{not} inside the @code{b} element, which represents an unfortunate
;;; failure to emulate all the quirks-handling behavior of some popular Web
;;; browsers.
;;;
;;; The procedures @code{html->sxml-@var{n}nf} for @var{n} 0 through 2
;;; correspond to 0th through 2nd normal forms of SXML as specified in SXML,
;;; and indicate the minimal requirements of the emitted SXML.
;;;
;;; @code{html->sxml} and @code{html->shtml} are currently aliases for
;;; @code{html->sxml-0nf}, and can be used in scripts and interactively, when
;;; terseness is important and any normal form of SXML would suffice.

(define (html->sxml-0nf input) (%parse-html input #f #t))
(define (html->sxml-1nf input) (%parse-html input #f #t))
(define (html->sxml-2nf input) (%parse-html input #t #t))

(define html->sxml  html->sxml-0nf)
(define html->shtml html->sxml-0nf)

;;; @section Emitting HTML

;;; Two procedures encoding the SHTML representation as conventional HTML,
;;; @code{write-shtml-as-html} and @code{shtml->html}.  These are perhaps most
;;; useful for emitting the result of parsed and transformed input HTML.  They
;;; can also be used for emitting HTML from generated or handwritten SHTML.

(define (%write-shtml-as-html/fixed shtml out foreign-filter)
  (letrec
      ((write-shtml-text
        (lambda (str out)
          (let ((len (string-length str)))
            (let loop ((i 0))
              (if (< i len)
                  (begin (display (let ((c (string-ref str i)))
                                    (case c
                                      ;; ((#\") "&quot;")
                                      ((#\&) "&amp;")
                                      ((#\<) "&lt;")
                                      ((#\>) "&gt;")
                                      (else c)))
                                  out)
                         (loop (+ 1 i)))
                  #f)))))

       (do-thing
        (lambda (thing)
          (cond ((string? thing) (write-shtml-text thing out))
                ((list? thing)   (if (not (null? thing))
                                     (do-list-thing thing)
                                     #f))
                (else (do-thing (foreign-filter thing #f))))))
       (do-list-thing
        (lambda (thing)
          (let ((head (car thing)))
            (cond ((symbol? head)
                   ;; Head is a symbol, so...
                   (cond ((eq? head shtml-comment-symbol)
                          ;; TODO: Make sure the comment text doesn't contain a
                          ;; comment end sequence.
                          (display "<!-- " out)
                          (let ((text (car (cdr thing))))
                            (if (string? text)
                                ;; TODO: Enforce whitespace safety without
                                ;; padding unnecessarily.
                                ;;
                                ;; (let ((len (string-length text)))
                                ;; (if (= len 0)
                                ;; (display #\space out)
                                ;; (begin (if (not (eqv?
                                ;; (string-ref text 0)
                                ;; #\space))
                                (display text out)
                                (error 'write-shtml-as-html
                                       "invalid SHTML comment text: ~S"
                                       thing)))
                          (or (null? (cdr (cdr thing)))
                              (error 'write-shtml-as-html
                                     "invalid SHTML comment body: ~S"
                                     thing))
                          (display " -->" out))
                         ((eq? head shtml-decl-symbol)
                          (let ((head (car (cdr thing))))
                            (display "<!" out)
                            (display (symbol->string head) out)
                            (for-each
                             (lambda (n)
                               (cond ((symbol? n)
                                      (display #\space out)
                                      (display (symbol->string n) out))
                                     ((string? n)
                                      (display " \"" out)
                                      (%write-dquote-ampified n out)
                                      (display #\" out))
                                     (else (error 'write-shtml-as-html
                                                  "invalid SHTML decl: ~S"
                                                  thing))))
                             (cdr (cdr thing)))
                            (display #\> out)))
                         ((eq? head shtml-pi-symbol)
                          (display "<?" out)
                          (display (symbol->string (car (cdr thing))) out)
                          (display #\space out)
                          (display (car (cdr (cdr thing))) out)
                          ;; TODO: Error-check that no more rest of PI.
                          (display "?>" out))
                         ((eq? head shtml-top-symbol)
                          (for-each do-thing (cdr thing)))
                         ((eq? head shtml-empty-symbol)
                          #f)
                         ((eq? head '^)
                          (error 'write-shtml-as-html
                                 "illegal position of SHTML attributes: ~S"
                                 thing))
                         ((or (eq? head '&) (eq? head shtml-entity-symbol))
                          (let ((val (shtml-entity-value thing)))
                            (if val
                                (begin (write-char     #\& out)
                                       (and (integer? val)
                                            (write-char #\# out))
                                       (display        val out)
                                       (write-char     #\; out))
                                (error 'write-shtml-as-html
                                       "invalid SHTML entity reference: ~S"
                                       thing))))
                         ((memq head `(,shtml-end-symbol
                                       ,shtml-start-symbol
                                       ,shtml-text-symbol))
                          (error 'write-shtml-as-html
                                 "invalid SHTML symbol: ~S"
                                 head))
                         (else
                          (display #\< out)
                          (display head out)
                          (let* ((rest   (cdr thing)))
                            (or (null? rest)
                                (let ((second (car rest)))
                                  (and (list? second)
                                       (not (null? second))
                                       (eq? (car second)
                                            '^)
                                       (begin (for-each do-attr (cdr second))
                                              (set! rest (cdr rest))))))
                            (if (memq head
                                      %empty-elements)
                                ;; TODO: Error-check to make sure the element
                                ;; has no content other than attributes.  We
                                ;; have to test for cases like: (br (^) ()
                                ;; (()))
                                (display " />" out)
                                (begin (display #\> out)
                                       (for-each do-thing rest)
                                       (display "</" out)
                                       (display (symbol->string head) out)
                                       (display #\> out)))))))
                  ;; ((or (list? head) (string? head))
                  ;;
                  ;; Head is a list or string, which might occur as the result
                  ;; of an SXML transform, so we'll cope.
                  (else
                   ;; Head is not a symbol, which might occur as the result of
                   ;; an SXML transform, so we'll cope.
                   (for-each do-thing thing))
                  ;;(else
                  ;; ;; Head is NOT a symbol, list, or string, so error.
                  ;; (error 'write-shtml-as-html
                  ;;        "invalid SHTML list: ~S"
                  ;;        thing))
                  ))))
       (write-attr-val-dquoted
        (lambda (str out)
          (display #\" out)
          (display str out)
          (display #\" out)))
       (write-attr-val-squoted
        (lambda (str out)
          (display #\' out)
          (display str out)
          (display #\' out)))
       (write-attr-val-dquoted-and-amped
        (lambda (str out)
          (display #\" out)
          (%write-dquote-ampified str out)
          (display #\" out)))
       (write-attr-val
        (lambda (str out)
          (let ((len (string-length str)))
            (let find-dquote-and-squote ((i 0))
              (if (= i len)
                  (write-attr-val-dquoted str out)
                  (let ((c (string-ref str i)))
                    (cond ((eqv? c #\")
                           (let find-squote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-squoted str out)
                                 (if (eqv? (string-ref str i) #\')
                                     (write-attr-val-dquoted-and-amped str
                                                                       out)
                                     (find-squote (+ 1 i))))))
                          ((eqv? c #\')
                           (let find-dquote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-dquoted str out)
                                 (if (eqv? (string-ref str i) #\")
                                     (write-attr-val-dquoted-and-amped str
                                                                       out)
                                     (find-dquote (+ 1 i))))))
                          (else (find-dquote-and-squote (+ 1 i))))))))))

       (collect-and-write-attr-val
        ;; TODO: Take another look at this.
        (lambda (lst out)
          (let ((os #f))
            (let do-list ((lst lst))
              (for-each
               (lambda (thing)
                 (let do-thing ((thing thing))
                   (cond ((string? thing)
                          (or os (set! os (open-output-string)))
                          (display thing os))
                         ((list? thing)
                          (do-list thing))
                         ((eq? thing #t)
                          #f)
                         (else
                          (do-thing (foreign-filter thing #t))))))
               lst))
            (and os
                 (begin
                   (display #\= out)
                   (write-attr-val (%gosc os) out))))))

       (do-attr
        (lambda (attr)
          (or (list? attr)
              (error 'write-shtml-as-html
                     "invalid SHTML attribute: ~S"
                     attr))
          (or (null? attr)
              (let ((name (car attr)))
                (or (symbol? name)
                    (error 'write-shtml-as-html
                           "invalid name in SHTML attribute: ~S"
                           attr))
                (or (eq? name '^)
                    (begin
                      (display #\space out)
                      (display name    out)
                      (collect-and-write-attr-val (cdr attr) out)

                      )))))))
    (do-thing shtml)
    #f))

(define (%write-dquote-ampified str out)
  ;; TODO: If we emit "&quot;", we really should parse it, and HTML 4.01 says
  ;; we should, but anachronisms in HTML create the potential for nasty
  ;; mutilation of URI in attribute values.
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (begin (display (let ((c (string-ref str i)))
                            (if (eqv? c #\") "&quot;" c))
                          out)
                 (loop (+ 1 i)))
          #f))))

;;; @defproc write-shtml-as-html shtml [out [foreign-filter]]
;;;
;;; Writes a conventional HTML transliteration of the SHTML @var{shtml} to
;;; output port @var{out}.  If @var{out} is not specified, the default is the
;;; current output port.  HTML elements of types that are always empty are
;;; written using HTML4-compatible XHTML tag syntax.
;;;
;;; If @var{foreign-filter} is specified, it is a procedure of two argument
;;; that is applied to any non-SHTML (``foreign'') object encountered in
;;; @var{shtml}, and should yield SHTML.  The first argument is the object, and
;;; the second argument is a boolean for whether or not the object is part of
;;; an attribute value.
;;;
;;; No inter-tag whitespace or line breaks not explicit in @var{shtml} is
;;; emitted.  The @var{shtml} should normally include a newline at the end of
;;; the document.  For example:
;;;
;;; @lisp
;;; (write-shtml-as-html
;;;  '((html (head (title "My Title"))
;;;          (body (^ (bgcolor "white"))
;;;                (h1 "My Heading")
;;;                (p "This is a paragraph.")
;;;                (p "This is another paragraph.")))))
;;; @end lisp
;;;
;;; outputs:
;;;
;;; @example
;;; <html><head><title>My Title</title></head><body bgcolor="whi
;;; te"><h1>My Heading</h1><p>This is a paragraph.</p><p>This is
;;;  another paragraph.</p></body></html>
;;; @end example

(define write-shtml-as-html
  (letrec ((error-foreign-filter
            (lambda (foreign-object in-attribute-value?)
              (error 'write-shtml-as-html
                     (if in-attribute-value?
                         "unhandled foreign object in shtml attribute value: ~S"
                         "unhandled foreign object in shtml: ~S")
                     foreign-object))))
    (lambda (shtml . rest)
      (case (length rest)
        ((0) (%write-shtml-as-html/fixed shtml
                                         (current-output-port)
                                         error-foreign-filter))
        ((1) (%write-shtml-as-html/fixed shtml
                                         (car rest)
                                         error-foreign-filter))
        ((2) (%write-shtml-as-html/fixed shtml
                                         (car rest)
                                         (cadr rest)))
        (else (error 'write-shtml-as-html
                     "extraneous arguments: ~S"
                     (cddr rest)))))))

;;; @defproc shtml->html shtml
;;;
;;; Yields an HTML encoding of SHTML @var{shtml} as a string.  For example:
;;;
;;; @lisp
;;; (shtml->html
;;;  (html->shtml
;;;   "<P>This is<br<b<I>bold </foo>italic</ b > text.</p>"))
;;; @result{} "<p>This is<br /><b><i>bold italic</i></b> text.</p>"
;;; @end lisp
;;;
;;; Note that, since this procedure constructs a string, it should normally
;;; only be used when the HTML is relatively small.  When encoding HTML
;;; documents of conventional size and larger, @code{write-shtml-as-html} is
;;; much more efficient.

(define (shtml->html shtml)
  (let ((os (open-output-string)))
    (write-shtml-as-html shtml os)
    (%gosc os)))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.19 --- 2009-11-08 --- PLaneT @code{(1 6)}
;;; Whitespace after a @code{<} in a context that would otherwise start a tag
;;; is no longer considered the start of a tag.  This behavior is consistent
;;; with, e.g., Firefox 3, and we have found a major site relying on it.  Three
;;; regression tests were changed to match the new desired behavior.
;;;
;;; @item Version 0.18 --- 2009-11-07 --- PLaneT @code{(1 5)}
;;; The @code{p} element can be a child of the @code{li} element.
;;;
;;; @item Version 0.17 --- 2009-08-16 --- PLaneT @code{(1 4)}
;;; License is now LGPL3.  Converted to author's new Scheme management system.
;;; Revamped high-level parser to not use mutable pairs, for PLT Scheme 4.x
;;; compatibility.  Until the new portability mechanism is in place, the
;;; previous portable version of HtmlPrag is available at:
;;; @uref{http://www.neilvandyke.org/htmlprag/htmlprag-0-16.scm}
;;;
;;; @item Version 0.16 --- 2005-12-18
;;; Documentation fix.
;;;
;;; @item Version 0.15 --- 2005-12-18
;;; In the HTML parent element constraints that are used for structure
;;; recovery, @code{div} is now always permitted as a parent, as a stopgap
;;; measure until substantial time can be spent reworking the algorithm to
;;; better support @code{div} (bug reported by Corey Sweeney and Jepri).  Also
;;; no longer convert to Scheme character any HTML numeric character reference
;;; with value above 126, to avoid Unicode problem with PLT 299/300 (bug
;;; reported by Corey Sweeney).
;;;
;;; @item Version 0.14 --- 2005-06-16
;;; XML CDATA sections are now tokenized.  Thanks to Alejandro Forero Cuervo
;;; for suggesting this feature.  The deprecated procedures @code{sxml->html}
;;; and @code{write-sxml-html} have been removed.  Minor documentation changes.
;;;
;;; @item Version 0.13 --- 2005-02-23
;;; HtmlPrag now requires @code{syntax-rules}, and a reader that can read the
;;; at-sign character as a symbol.  SHTML now has a special @code{&} element
;;; for character entities, and it is emitted by the parser rather than the old
;;; @code{*ENTITY*} kludge.  @code{shtml-entity-value} supports both the new
;;; and the old character entity representations.  @code{shtml-entity-value}
;;; now yields @code{#f} on invalid SHTML entity, rather than raising an error.
;;; @code{write-shtml-as-html} now has a third argument, @code{foreign-filter}.
;;; @code{write-shtml-as-html} now emits SHTML @code{&} entity references.
;;; Changed @code{shtml-named-char-id} and @code{shtml-numeric-char-id}, as
;;; previously warned.  Testeez is now used for the test suite.  Test procedure
;;; is now the internal @code{%htmlprag:test}.  Documentation changes.
;;; Notably, much documentation about using HtmlPrag under various particular
;;; Scheme implementations has been removed.
;;;
;;; @item Version 0.12 --- 2004-07-12
;;; Forward-slash in an unquoted attribute value is now considered a value
;;; constituent rather than an unconsumed terminator of the value (thanks to
;;; Maurice Davis for reporting and a suggested fix).  @code{xml:} is now
;;; preserved as a namespace qualifier (thanks to Peter Barabas for
;;; reporting).  Output port term of @code{write-shtml-as-html} is now
;;; optional.  Began documenting loading for particular implementation-specific
;;; packagings.
;;;
;;; @item Version 0.11 --- 2004-05-13
;;; To reduce likely namespace collisions with SXML tools, and in anticipation
;;; of a forthcoming set of new features, introduced the concept of ``SHTML,''
;;; which will be elaborated upon in a future version of HtmlPrag.  Renamed
;;; @code{sxml-@var{x}-symbol} to @code{shtml-@var{x}-symbol},
;;; @code{sxml-html-@var{x}} to @code{shtml-@var{x}}, and
;;; @code{sxml-token-kind} to @code{shtml-token-kind}.  @code{html->shtml},
;;; @code{shtml->html}, and @code{write-shtml-as-html} have been added as
;;; names.  Considered deprecated but still defined (see the ``Deprecated''
;;; section of this documentation) are @code{sxml->html} and
;;; @code{write-sxml-html}.  The growing pains should now be all but over.
;;; Internally, @code{htmlprag-internal:error} introduced for Bigloo
;;; portability.  SISC returned to the test list; thanks to Scott G.  Miller
;;; for his help.  Fixed a new character @code{eq?}  bug, thanks to SISC.
;;;
;;; @item Version 0.10 --- 2004-05-11
;;; All public identifiers have been renamed to drop the ``@code{htmlprag:}''
;;; prefix.  The portability identifiers have been renamed to begin with an
;;; @code{htmlprag-internal:} prefix, are now considered strictly
;;; internal-use-only, and have otherwise been changed.  @code{parse-html} and
;;; @code{always-empty-html-elements} are no longer public.
;;; @code{test-htmlprag} now tests @code{html->sxml} rather than
;;; @code{parse-html}.  SISC temporarily removed from the test list, until an
;;; open source Java that works correctly is found.
;;;
;;; @item Version 0.9 --- 2004-05-07
;;; HTML encoding procedures added.  Added
;;; @code{htmlprag:sxml-html-entity-value}.  Upper-case @code{X} in hexadecimal
;;; character entities is now parsed, in addition to lower-case @code{x}.
;;; Added @code{htmlprag:always-empty-html-elements}.  Added additional
;;; portability bindings.  Added more test cases.
;;;
;;; @item Version 0.8 --- 2004-04-27
;;; Entity references (symbolic, decimal numeric, hexadecimal numeric) are now
;;; parsed into @code{*ENTITY*} SXML.  SXML symbols like @code{*TOP*} are now
;;; always upper-case, regardless of the Scheme implementation.  Identifiers
;;; such as @code{htmlprag:sxml-top-symbol} are bound to the upper-case
;;; symbols.  Procedures @code{htmlprag:html->sxml-0nf},
;;; @code{htmlprag:html->sxml-1nf}, and @code{htmlprag:html->sxml-2nf} have
;;; been added.  @code{htmlprag:html->sxml} now an alias for
;;; @code{htmlprag:html->sxml-0nf}.  @code{htmlprag:parse} has been refashioned
;;; as @code{htmlprag:parse-html} and should no longer be directly.  A number
;;; of identifiers have been renamed to be more appropriate when the
;;; @code{htmlprag:} prefix is dropped in some implementation-specific
;;; packagings of HtmlPrag: @code{htmlprag:make-tokenizer} to
;;; @code{htmlprag:make-html-tokenizer}, @code{htmlprag:parse/tokenizer} to
;;; @code{htmlprag:parse-html/tokenizer}, @code{htmlprag:html->token-list} to
;;; @code{htmlprag:tokenize-html}, @code{htmlprag:token-kind} to
;;; @code{htmlprag:sxml-token-kind}, and @code{htmlprag:test} to
;;; @code{htmlprag:test-htmlprag}.  Verbatim elements with empty-element tag
;;; syntax are handled correctly.  New versions of Bigloo and RScheme tested.
;;;
;;; @item Version 0.7 --- 2004-03-10
;;; Verbatim pair elements like @code{script} and @code{xmp} are now parsed
;;; correctly.  Two Scheme implementations have temporarily been dropped from
;;; regression testing: Kawa, due to a Java bytecode verifier error likely due
;;; to a Java installation problem on the test machine; and SXM 1.1, due to
;;; hitting a limit on the number of literals late in the test suite code.
;;; Tested newer versions of Bigloo, Chicken, Gauche, Guile, MIT Scheme, PLT
;;; MzScheme, RScheme, SISC, and STklos.  RScheme no longer requires the
;;; ``@code{(define get-output-string close-output-port)}'' workaround.
;;;
;;; @item Version 0.6 --- 2003-07-03
;;; Fixed uses of @code{eq?} in character comparisons, thanks to Scott G.
;;; Miller.  Added @code{htmlprag:html->normalized-sxml} and
;;; @code{htmlprag:html->nonnormalized-sxml}.  Started to add
;;; @code{close-output-port} to uses of output strings, then reverted due to
;;; bug in one of the supported dialects.  Tested newer versions of Bigloo,
;;; Gauche, PLT MzScheme, RScheme.
;;;
;;; @item Version 0.5 --- 2003-02-26
;;; Removed uses of @code{call-with-values}.  Re-ordered top-level definitions,
;;; for portability.  Now tests under Kawa 1.6.99, RScheme 0.7.3.2, Scheme 48
;;; 0.57, SISC 1.7.4, STklos 0.54, and SXM 1.1.
;;;
;;; @item Version 0.4 --- 2003-02-19
;;; Apostrophe-quoted element attribute values are now handled.  A bug that
;;; incorrectly assumed left-to-right term evaluation order has been fixed
;;; (thanks to MIT Scheme for confronting us with this).  Now also tests OK
;;; under Gauche 0.6.6 and MIT Scheme 7.7.1.  Portability improvement for
;;; implementations (e.g., RScheme 0.7.3.2.b6, Stalin 0.9) that cannot read the
;;; at-sign character as a symbol (although those implementations tend to
;;; present other portability issues, as yet unresolved).
;;;
;;; @item Version 0.3 --- 2003-02-05
;;; A test suite with 66 cases has been added, and necessary changes have been
;;; made for the suite to pass on five popular Scheme implementations.  XML
;;; processing instructions are now parsed.  Parent constraints have been added
;;; for @code{colgroup}, @code{tbody}, and @code{thead} elements.  Erroneous
;;; input, including invalid hexadecimal entity reference syntax and extraneous
;;; double quotes in element tags, is now parsed better.
;;; @code{htmlprag:token-kind} emits symbols more consistent with SXML.
;;;
;;; @item Version 0.2 --- 2003-02-02
;;; Portability improvements.
;;;
;;; @item Version 0.1 --- 2003-01-31
;;; Dusted off author's old Guile-specific code from April 2001, converted to
;;; emit SXML, mostly ported to R5RS and SRFI-6, added some XHTML support and
;;; documentation.  A little preliminary testing has been done, and the package
;;; is already useful for some applications, but this release should be
;;; considered a preview to invite comments.
;;;
;;; @end table
