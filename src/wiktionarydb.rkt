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

(provide wikt-wordlist-from-word
         wikt-has-definition?
         wikt-get-definition
         load-wikt-data-files
         create-wiktionary-data-file-if-needed
         make-wiktionary-data-files)

(require racket/list
         racket/set
         "constants-filenames.rkt")

#|
|| "Database" lookup variables.
|#
(define wikt-lookup-hash (make-hash))
(define wikt-index-hash (make-hash))
(define wikt-data-file #f)

#|
|| wikt-wordlist-from-word
||   Get a word list from a word
|#
(define (wikt-wordlist-from-word wrd)
  (set->list
   (apply set-intersect 
          (for/list ([c (string->list wrd)]) 
            (let ([l (hash-ref wikt-lookup-hash c '())]) 
              (list->set l))))))

#|
|| wikt-has-definition?
||   Checks if a word is in the database
|#
(define (wikt-has-definition? wrd)
  (hash-has-key? wikt-index-hash wrd))

#|
|| wikt-get-definition
||   Gets the text definition of a word
|#
(define (wikt-get-definition wrd)
  (define v (hash-ref wikt-index-hash wrd))
  (define z (first v))
  (define y (second v))
  (call-with-input-file wikt-data-file
    (lambda (fd)
      (file-position fd z)
      (read-string y fd) )
    #:mode 'text))

#|
|| load-wikt-data-files
||   Simply load the data files required to access the database.
|#
(define (load-wikt-data-files wiktDataFile wiktIndex wiktLookup)
  (when (and (file-exists? wiktIndex) (file-exists? wiktDataFile) (file-exists? wiktLookup))
    (set! wikt-data-file wiktDataFile)
    (set! wikt-index-hash
          (call-with-input-file wiktIndex
            (lambda (fx)
              (define newhsh (make-hash))
              (let loop ([d (read fx)])
                (if (eof-object? d)
                    newhsh
                    (let ()
                      (hash-set! newhsh (first d) (list (second d) (third d)))
                      (loop (read fx))))
                ))))
    (set! wikt-lookup-hash
          (call-with-input-file wiktLookup
            (lambda (fli)
              (read fli)))))
  )

#|
|| create-wiktionary-data-file-if-needed
||   Main function for extracting the XML file into the simple database
||   used by our Wiktionary viewer.
|#
(define (create-wiktionary-data-file-if-needed xmlfile kanjIDX wiktDataFile wiktIndex wiktLookup)
  (define (run-wiktionary-xml-file emit-content emit-tag-open emit-tag-head emit-tag-attr-name emit-tag-attr-value emit-tag-close)
    (define (send-out buf typ)
      (typ (apply string (reverse buf)))
      '()
      )
    
    (define (munch-content buf c)
      (if (eq? c #\<)
          (values (send-out (send-out buf emit-content) emit-tag-open) munch-taghead)
          (values (cons c buf) munch-content)))
    (define (munch-taghead-start buf c)
      (if (eq? c #\space)
          (values buf munch-taghead-start)
          (if (eq? c #\/)
              (values (cons c buf) munch-taghead-start)
              (if (eq? c #\>)
                  (values (send-out (send-out buf emit-tag-head) emit-tag-close) munch-content)
                  (values (cons c buf) munch-taghead)))))
    (define (munch-taghead buf c)
      (if (eq? c #\space)
          (values (send-out buf emit-tag-head) munch-tagattrsname-start)
          (if (eq? c #\/)
              (values (cons c buf) munch-taghead)
              (if (eq? c #\>)
                  (values (send-out (send-out buf emit-tag-head) emit-tag-close) munch-content)
                  (values (cons c buf) munch-taghead)))))
    (define (munch-tagattrsname-start buf c)
      (if (eq? c #\space)
          (values buf munch-tagattrsname-start)
          (if (eq? c #\/)
              (values buf munch-tagattrsname-start)
              (if (eq? c #\>)
                  (values (send-out buf emit-tag-close) munch-content)
                  (values (cons c buf) munch-tagattrsname)))))
    (define (munch-tagattrsname buf c)
      (if (eq? c #\space)
          (raise "bad")
          (if (eq? c #\=)
              (values (send-out buf emit-tag-attr-name) munch-tagattrsvalue-start)
              (values (cons c buf) munch-tagattrsname))))
    (define (munch-tagattrsvalue-start buf c)
      (if (eq? c #\")
          (values '() munch-tagattrsvalue)
          (values '() munch-tagattrsvalue-start)))
    (define (munch-tagattrsvalue buf c)
      (if (eq? c #\")
          (values (send-out buf emit-tag-attr-value) munch-tagattrsname-start)
          (values (cons c buf) munch-tagattrsvalue)))
    
    (call-with-input-file xmlfile
      (lambda(fi)
        (for/fold ([buf '()]
                   [munch munch-content]) 
          ([c (in-input-port-chars fi)])
          (munch buf c)
          ) ) )
    
    )
  
  (define allkanj
    (call-with-input-file kanjIDX
      (lambda (fi)
        (let loop ([hsh (make-immutable-hash)]
                   [a (read fi)])
          (if (eof-object? a)
              hsh
              (loop (hash-set hsh (string-ref (first a) 0) #t) (read fi)))))
      #:mode 'text) )
  
  (unless (or (file-exists? wiktDataFile) (file-exists? wiktIndex))
    (call-with-output-file wiktDataFile
      (lambda (fo)
        (call-with-output-file wiktIndex
          (lambda (fx)
            (define last-title "")
            (define hshlookup (make-hash))
            (define title "")
            (define text "")
            (define capture void)
            
            (run-wiktionary-xml-file 
             
             #|
             || content
             |#
             (lambda (a)
               (capture a))
             
             #|
             || tag-open
             |#
             (lambda (a) (void))
             
             #|
             || tag-head
             |#
             (lambda (a)
               (cond
                 [(equal? "page" a)
                  (set! text "")
                  (set! title "")]
                 [(equal? "text" a)
                  (set! capture (lambda (a) (set! text a)))]
                 [(equal? "title" a)
                  (set! capture (lambda (a) (set! title a)))]
                 [else
                  (set! capture (lambda (a) (void)))] ))
             
             #|
             || tag-attr-name and tag-attr-value
             |#
             (lambda (a) (void))
             (lambda (a) (void))
             
             #|
             || tag-close
             |#
             (lambda (a)
               (unless (or (equal? title "") (equal? text "") (equal? last-title title))
                 (define d (string-ref title 0))
                 (set! last-title title)
                 (when (eq? (char-upcase d) (char-downcase d))
                   (when (for/or ([c (in-string title)])
                           (hash-ref allkanj c #f))
                     (printf "Incl: ~s~n" title)
                     
                     (let ([text (let ([bt (open-output-string)])
                                   (for/list ([l (in-lines (open-input-string text) 'any)])
                                     (if (and (> (string-length l) 6)
                                              (eq? (string-ref l 0) #\[)
                                              (eq? (string-ref l 1) #\[)
                                              (eq? (string-ref l 4) #\:))
                                         (void)
                                         (let ()
                                           (display l bt) (newline bt)) ))
                                   (regexp-replace* #rx"&amp;"
                                    (regexp-replace* #rx"&lt;"
                                     (regexp-replace* #rx"&gt;"
                                      (regexp-replace* #rx"&quot;"
                                       (get-output-string bt) "\"") ">") "<") "&") )])
                       (for ([c (in-string title)])
                         (hash-set! hshlookup c (cons title (hash-ref hshlookup c '()))))
                       (flush-output fo)
                       (write (list title (file-position fo) (string-length text)) fx) 
                       (newline fx)
                       (fprintf fo "~a~n~n" text)
                       (set! title "")
                       (set! text "")))) )))
            (call-with-output-file wiktLookup
              (lambda (flo)
                (write hshlookup flo)
                (flush-output flo) )
              #:mode 'text
              #:exists 'replace )
            (flush-output fo)
            (flush-output fx)
            )
          #:mode 'text ) )
      #:mode 'text ) )
  )

#|
|| make-wiktionary-data-files
|| Shortcut function to using create-wiktionary-data-file-if-needed
|#
(define (make-wiktionary-data-files xmlfile)
  (create-wiktionary-data-file-if-needed 
   xmlfile 
   (resolve-data-file-path CONST_FILE_KANJIIDX0)
   (resolve-data-file-path CONST_FILE_WIKTDATA)
   (resolve-data-file-path CONST_FILE_WIKTINDX)
   (resolve-data-file-path CONST_FILE_WIKTLKUP))
  )
