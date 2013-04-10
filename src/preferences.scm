
;;;
;;;    KanjiGet
;;;    Copyright 2011-2012 Edward L. Blake
;;;
;;;    This source file is part of KanjiGet.
;;;    See LICENSE.txt
;;;

;; Requires: SRFI-34, SRFI-69

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
    (::call-with-input-file/text
        (::path-build (get-preference-folder) "preferences.txt")
      (lambda (fi)
        (read fi)
        ))))

(define (write-preferences nv)
  (define (get-preference-folder/create)
    (let ([fl (get-preference-folder)])
      (if (::directory? fl)
          fl
          (begin 
            (make-directory fl) 
            fl))))
  (guard (condition (#t (printf "Error: could not write preference file.~n")))
    (::call-with-output-file/text/replace
     (::path-build (get-preference-folder/create) "preferences.txt")
     (lambda (fo)
       (write nv fo)
       ))))
