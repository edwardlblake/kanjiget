#lang racket/base

;;;
;;;    KanjiGet
;;;    Copyright 2011-2012 Edward L. Blake
;;;
;;;    This source file is part of KanjiGet.
;;;    See LICENSE.txt
;;;

(require srfi/1)
(require srfi/4)
(require srfi/9)
(require srfi/34)
(require srfi/69)

;;;
;;; Mapping Racket-specific functions to generic function names.
;;;

(define (::path-split pt)
  (let-values (((a b _) (split-path pt)))
    (values (path->string a) (path->string b))))
(define ::directory? directory-exists?)
(define (::path-build . pts) (path->string (apply build-path pts)))
(define ::system-path find-system-path)
(define (::call-with-input-file/text f c) ( call-with-input-file f c #:mode  'text))
(define (::call-with-output-file/text/replace f c) ( call-with-output-file f c #:mode  'text #:exists  'truncate/replace))
(define ::file? file-exists?)
(define ::input-port-byte-position  file-position)
(define ::output-port-byte-position file-position)
(define ::force-output flush-output)
(define (::read-substring! a s e p) (read-string! a p s e))
(define (::call-with-output-file/text/keep f c) ( call-with-output-file f c #:mode  'text))
(define (::call-with-input-file/bin f c) ( call-with-input-file f c))
(define (::read-u8vector! u8v fi) (read-bytes! u8v fi))
(define (::sort lst ls ky) (sort lst ls #:key ky))
(define (::call-with-output-file/bin/keep f c) ( call-with-output-file f c))

;;;
;;; Include generic Scheme code.
;;;

(require mzlib/include)
(include "constants-app-labels-en.scm")
(include "constants-app-version.scm")
(include "constants-filenames.scm")
(include "preferences.scm")
(include "wiktionarydb.scm")
(include "wiktionarytemplates.scm")

;;;
;;; Platform specific access to FFI
;;;

(require (only-in ffi/unsafe get-ffi-obj _fun _pointer _int))

(define WINAPI_SetWindowPos
  (case (system-type)
    [[windows]
     (get-ffi-obj "SetWindowPos" "user32" (_fun _pointer _int _int _int _int _int _int -> _int)
                  (λ () (void)))]
    [else void]))
(define WINAPI_HWND_TOPMOST  -1)
(define WINAPI_HWND_NOTOPMOST -2)

(provide (all-defined-out))
