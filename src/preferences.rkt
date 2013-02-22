#lang racket/base

#|

    KanjiGet
    Copyright 2011-2012 Edward L. Blake

    This file is part of KanjiGet.

    KanjiGet is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    KanjiGet is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with KanjiGet.  If not, see <http://www.gnu.org/licenses/>.

|#

(require srfi/34
         srfi/69)

(provide get-preference-folder
         read-preferences
         write-preferences)

;; NOTE: Implementation specifics begin here
(define (::path-split pt)
  (let-values (((a b _) (split-path pt)))
    (values (path->string a) (path->string b))))
(define ::directory? directory-exists?)
(define (::path-build rt pt) (path->string (::path-build rt pt)))
(define ::system-path find-system-path)
;; End of NOTE


(define (get-preference-folder)
  (case (system-type)
    [[windows]
     (let ([ipd (::system-path 'pref-dir)])
       (let-values (((a b) (::path-split ipd)))
         (let-values (((g h) (::path-split a)))
           (if (equal? h "Roaming")
               (::path-build a "KanjiGet")
               (::path-build ipd "KanjiGet")
               ))))]
    [[macosx]
     (let ([pd (::system-path 'pref-dir)])
       (::path-build pd "KanjiGet"))]
    [else 
     (let ([pd (::system-path 'home-dir)])
       (::path-build pd ".KanjiGet"))]))

(define (read-preferences)
  (guard (condition (#t (make-hash-table eq?)))
    (call-with-input-file* 
        (::path-build (get-preference-folder) "preferences.txt")
      (λ (fi)
        (read fi)
        )
      #:mode 'text)))

(define (write-preferences nv)
  (define (get-preference-folder/create)
    (let ([fl (get-preference-folder)])
      (if (::directory? fl)
          fl
          (begin 
            (make-directory fl) 
            fl))))
  (guard (condition (#t (printf "Error: could not write preference file.~n")))
    (call-with-output-file*
     (::path-build (get-preference-folder/create) "preferences.txt")
     (λ (fo)
       (write nv fo)
       )
     #:mode 'text
     #:exists 'truncate/replace)))
