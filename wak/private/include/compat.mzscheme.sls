;;; compat.mzscheme.sls --- include compatibility for mzscheme

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the MIT/X11 license.

;; You should have received a copy of the MIT/X11 license along with
;; this program. If not, see
;; <http://www.opensource.org/licenses/mit-license.php>.

;;; Commentary:

;;; Code:
#!r6rs

(library (wak private include compat)
  (export stale-when
          read-annotated
          annotation?
          annotation-expression
          file-mtime
          merge-path
          library-search-paths)
  (import (rnrs base)
          (rnrs io simple)
          (prefix (scheme base) mz:)
          (scheme mpair))

(define-syntax stale-when
  (syntax-rules ()
    ((_ conditition body ...)
     (begin body ...))))

(define (read-annotated port)
  (read port))

(define (annotation? thing)
  #f)

(define (annotation-expression thing)
  thing)

(define (merge-path path origin)
  (mz:path->string (apply mz:build-path origin path)))

(define (file-mtime filename)
  (mz:file-or-directory-modify-seconds filename))

(define (library-search-paths)
  (list->mlist (mz:current-library-collection-paths)))

)
