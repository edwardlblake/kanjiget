
;;;
;;;    KanjiGet
;;;    Copyright 2011-2012 Edward L. Blake
;;;
;;;    This source file is part of KanjiGet.
;;;    See LICENSE.txt
;;;

;;;
;;; "Database" lookup variables.
;;;
(define wikt-lookup-hash (make-hash-table))
(define wikt-index-hash (make-hash-table))
(define wikt-data-file #f)

;;;
;;; wikt-wordlist-from-word
;;;   Get a word list from a word
;;;
(define (wikt-wordlist-from-word wrd)
  (apply lset-intersection equal?
         (for/list ([c (string->list wrd)]) 
           (hash-table-ref wikt-lookup-hash c (lambda _ '())))))

;;;
;;; wikt-has-definition?
;;;   Checks if a word is in the database
;;;
(define (wikt-has-definition? wrd)
  (hash-table-exists? wikt-index-hash wrd))

;;;
;;; wikt-get-definition
;;;   Gets the text definition of a word
;;;
(define (wikt-get-definition wrd)
  (define v (hash-table-ref wikt-index-hash wrd))
  (define z (car  v))
  (define y (cadr v))
  (::call-with-input-file/text wikt-data-file
    (lambda (fd)
      (define sb (make-string y #\nul))
      (::input-port-byte-position fd z)
      (substring sb 0 (::read-substring! sb 0 y fd)))))

;;;
;;; load-wikt-data-files
;;;   Simply load the data files required to access the database.
;;;
(define (load-wikt-data-files wiktDataFile wiktIndex wiktLookup)
  (when (and (file-exists? wiktIndex) (file-exists? wiktDataFile) (file-exists? wiktLookup))
    (set! wikt-data-file wiktDataFile)
    (set! wikt-index-hash
          (::call-with-input-file/bin wiktIndex
            (lambda (fx)
              (define newhsh (make-hash-table))
              (let loop ([d (read fx)])
                (if (eof-object? d)
                    newhsh
                    (let ()
                      (hash-table-set! newhsh (car d) (list (cadr d) (caddr d)))
                      (loop (read fx))))
                ))))
    (set! wikt-lookup-hash
          (alist->hash-table
           (::call-with-input-file/bin wiktLookup
             (lambda (fli)
               (read fli))) eqv?)))
  )

;;;
;;; create-wiktionary-data-file-if-needed
;;;   Main function for extracting the XML file into the simple database
;;;   used by our Wiktionary viewer.
;;;
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
    
    (::call-with-input-file/bin xmlfile
      (lambda (fi)
        (for/fold ([buf '()]
                   [munch munch-content]) 
          ([c (in-input-port-chars fi)])
          (munch buf c)
          ) ) )
    
    )
  
  (define allkanj
    (::call-with-input-file/text kanjIDX
      (lambda (fi)
        (define hsh (make-hash-table))
        (let loop ((a (read fi)))
          (if (eof-object? a)
              hsh
              (begin
                (hash-table-set! hsh (string-ref (car a) 0) #t)
                (loop (read fi))))))))
  
  (unless (or (file-exists? wiktDataFile) (file-exists? wiktIndex))
    (::call-with-output-file/text/keep wiktDataFile
      (lambda (fo)
        (::call-with-output-file/text/keep wiktIndex
          (lambda (fx)
            (define last-title "")
            (define hshlookup (make-hash-table))
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
                           (hash-table-ref allkanj c (lambda _ #f)))
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
                         (hash-table-set! hshlookup c (cons title (hash-table-ref hshlookup c (lambda _ '())))))
                       (::force-output fo)
                       (write (list title (::output-port-byte-position fo) (string-length text)) fx) 
                       (newline fx)
                       (fprintf fo "~a~n~n" text)
                       (set! title "")
                       (set! text "")))) )))
            (::call-with-output-file/text/replace wiktLookup
              (lambda (flo)
                (write hshlookup flo)
                (::force-output flo) ))
            (::force-output fo)
            (::force-output fx)
            )))))
  )

;;;
;;; make-wiktionary-data-files
;;; Shortcut function to using create-wiktionary-data-file-if-needed
;;;
(define (make-wiktionary-data-files xmlfile)
  (create-wiktionary-data-file-if-needed 
   xmlfile 
   (resolve-data-file-path CONST_FILE_KANJIIDX0)
   (resolve-data-file-path CONST_FILE_WIKTDATA)
   (resolve-data-file-path CONST_FILE_WIKTINDX)
   (resolve-data-file-path CONST_FILE_WIKTLKUP))
  )
