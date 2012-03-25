#lang racket/base

#|
 | KanjiChk
 | Preferences
 |
 | Copyright 2011 Edward Blake
 |
 | @author  Edward L. Blake <edward.blake@rovoscape.com>
 | @package kanjichk
 |
 |#

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
               (build-path a "Kanjikun")
               (build-path ipd "Kanjikun")
               ))))]
    [[macosx]
     (let ([pd (find-system-path 'pref-dir)])
       (build-path pd "Kanjikun"))]
    [else 
     (let ([pd (find-system-path 'home-dir)])
       (build-path pd ".kanjikun"))]))

(define (read-preferences)
  (with-handlers ([exn:fail? (lambda (x) (make-hasheq))])
    (call-with-input-file* 
        (build-path (get-preference-folder) "preferences.txt")
      (lambda (fi)
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
  (with-handlers ([exn:fail? (lambda (x) (printf "Error: could not write preference file.~n"))])
    (call-with-output-file*
     (build-path (get-preference-folder/create) "preferences.txt")
     (lambda (fo)
       (write nv fo)
       )
     #:mode 'text
     #:exists 'truncate/replace)))
