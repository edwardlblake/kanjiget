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

(define (get-preference-folder)
  (case (system-type)
    [[windows] 
     (let ([ipd (find-system-path 'pref-dir)])
       (let-values (((a b c) (split-path ipd)))
         (let-values (((g h k) (split-path a)))
           (if (equal? h (string->path "Roaming"))
               (build-path a "KanjiGet")
               (build-path ipd "KanjiGet")
               ))))]
    [[macosx]
     (let ([pd (find-system-path 'pref-dir)])
       (build-path pd "KanjiGet"))]
    [else 
     (let ([pd (find-system-path 'home-dir)])
       (build-path pd ".KanjiGet"))]))

(define (read-preferences)
  (guard (condition (#t (make-hash-table eq?)))
    (call-with-input-file* 
        (build-path (get-preference-folder) "preferences.txt")
      (λ (fi)
        (read fi)
        )
      #:mode 'text)))

(define (write-preferences nv)
  (define (get-preference-folder/create)
    (let ([fl (get-preference-folder)])
      (if (directory-exists? fl)
          fl
          (begin 
            (make-directory fl) 
            fl))))
  (guard (condition (#t (printf "Error: could not write preference file.~n")))
    (call-with-output-file*
     (build-path (get-preference-folder/create) "preferences.txt")
     (λ (fo)
       (write nv fo)
       )
     #:mode 'text
     #:exists 'truncate/replace)))
