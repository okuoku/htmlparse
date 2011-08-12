;; run.scm -- test drivers

;; Copyright (C) 2004, 2005, 2006-2008 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Thu Nov 04, 2004 13:24

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Comentary:

;; Utilities to lauch tests written using infrastructure.scm

;;; Code:

;;@ Run specified tests. The first argument is a list whose
;; elements can be either directories or single file names:
;; @code{run-tests-in-directory} or @code{run-tests-in-file} is
;; applied accordingly. If empty is provided, the current directory is
;; used. The second argument is an identifier of the scheme dialect in
;; use, which can appear in the @code{only-dialects} and
;; @code{all-dialects-except} marker forms in test files.

(define (run-tests tests dialect env)
  (for-each (lambda (f)
              (cond ((file-directory? f) (run-tests-in-directory f dialect env))
                    ((file-regular? f) (run-tests-in-file f dialect env))
                    (else
                     (display (list "skipping non-existing" f))
                     (newline))))
            tests))

(define (run-tests-and-exit tests dialect env)
  (run-tests tests dialect env)
  (exit 0))

;;@ Call @code{run-tests-in-file} for each file in the given
;; directory. If the directory contains a file named @file{tests.scm},
;; the list of files is read from it.
(define (run-tests-in-directory dir dialect env)
  (let ((listing (make-pathname #f dir (make-file "tests" "scm"))))
    (let ((files (if (file-regular? listing)
                     (map (lambda (x)
                            (pathname-with-file dir (cond ((string? x) x)
                                                          (else (car x)))))
                          (with-input-from-file (x->namestring listing) read))
                     (directory-fold dir cons '()))))
      (for-each (lambda (f)
                  (run-tests-in-file f dialect env))
                files))))

;;@ Load the given file if it contains at least one
;; @code{testeez} form. @code{dialect} is used to skip, if necessary,
;; test files containing @code{only-dialects} or
;; @code{all-dialects-except} forms.
(define run-tests-in-file
  (letrec ((find
            (lambda (l)
              (cond
               ((not (pair? l)) #f)
               ((pair? (car l)) (or (find (car l)) (find (cdr l))))
               (else (or (eq? 'testeez (car l)) (find (cdr l)))))))
           (cont?
            (lambda (l d)
              (cond ((eof-object? l) #f)
                    ((not (pair? l)) #t)
                    ((eq? (car l) 'only-dialects) (memq d (cdr l)))
                    ((eq? (car l) 'all-dialects-except) (not (memq d (cdr l))))
                    (else #t)))))
    (lambda (file dialect env)
      (newline)
      (let ((filename (x->namestring file)))
        (with-input-from-file filename
          (lambda ()
            (let loop ((in (read)))
              (cond ((find in)
                     (begin (display (list "Loading " filename "... "))
                            (newline)
                            (call-with-input-file filename
                              (lambda (port)
                                (let loop ((forms '()))
                                  (let ((form (read port)))
                                    (if (eof-object? form)
                                        (eval `(let () ,@(reverse forms)) env)
                                        (loop (cons form forms)))))))
                            (display (list "..." filename "done"))
                            (newline)))
                    ((cont? in dialect) (loop (read)))))))))))

(define package-name->import-spec
  (let ((all-but-dot (char-set-complement (char-set #\.))))
    (lambda (spec)
      (if (symbol? spec)
          (map string->symbol (string-tokenize (symbol->string spec) all-but-dot))
          spec))))

(define (construct-test-environment imports)
  (guard (c (#t (display "(Error constructing environment: ")
                (newline)
                (for-each (lambda (line)
                            (display line)
                            (newline))
                          (format-exception c))
                (display ")")
                (newline)
                #f))
    (apply environment
           (append
            '((except (rnrs base) error string-copy string-for-each string->list)
              (rnrs io simple)
              (testeez)
              (testeez run-env))
            imports))))

;; test spec grammar:
;;
;; <test spec> -> (<clause> ...)
;; <clause> -> (systems <system name> ...)
;;             (files (<file spec> <required library>) ...)
;; <code> -> <filename>
;;           (code <scheme expr> ...) <filename>
(define (eval-test-spec dialect pathname test-spec tests)
  (for-each
   (lambda (spec)
     (case (car spec)
       ((systems)
        (for-each (lambda (system) ((system-loader) system)) (cdr spec)))
       ((files)
        (for-each
         (lambda (clause)
           (cond ((or (null? tests) (member (car clause) tests))
                  (let* ((head (car clause))
                         (code-there? (and (list? head) (eq? 'code (car head))))
                         (code (if code-there? (cdr head) '()))
                         (fpath (if code-there? (cadr clause) head))
                         (pkgs (map package-name->import-spec ((if code-there? cddr cdr) clause)))
                         (env (construct-test-environment pkgs)))
                    (when env
                      (parameterize ((test-environment env))
                        (guard (c (#t (display (list "Uncaught exception during tests: " c))
                                      (newline)))
                          (unless (null? code)
                            (eval `(let () ,@code) env))
                          (run-tests
                           (list (pathname-with-file pathname fpath))
                           dialect
                           env))))))))
         (cdr spec)))))
   test-spec))

(define (main args)
  (for-each (lambda (tests-file)
              (call-with-input-file tests-file
                (lambda (port)
                  (let ((test-spec (read port))
                        (pathname (x->pathname tests-file)))
                    (parameterize
                        ((this-directory (directory-namestring pathname)))
                      (eval-test-spec (scheme-dialect) pathname test-spec '()))))))
            (cdr args)))

;;; run.scm ends here

