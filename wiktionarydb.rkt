#lang racket/base

(provide wikt-wordlist-from-word
         wikt-has-definition?
         wikt-get-definition
         load-wikt-data-files
         create-wiktionary-data-file-if-needed)

(require racket/list
         racket/set)

(define wikt-lookup-hash (make-hash))
(define wikt-index-hash (make-hash))
(define wikt-data-file #f)

(define (wikt-wordlist-from-word wrd)
  (set->list
   (apply set-intersect 
          (for/list ([c (string->list wrd)]) 
            (let ([l (hash-ref wikt-lookup-hash c '())]) 
              (list->set l))))))

(define (wikt-has-definition? wrd)
  (hash-has-key? wikt-index-hash wrd))

(define (wikt-get-definition wrd)
  (define v (hash-ref wikt-index-hash wrd))
  (define z (first v))
  (define y (second v))
  (call-with-input-file wikt-data-file
    (lambda (fd)
      (file-position fd z)
      (read-string y fd) )
    #:mode 'text))

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
            (define hshlookup (make-hash))
            (define title "")
            (define text "")
            (define capture void)
            (run-wiktionary-xml-file 
             (lambda (a)
               (capture a))
             (lambda (a) (void)) ; tag-open
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
                  (set! capture (lambda (a) (void)))]
                 )
               )
             (lambda (a) (void)) ; tag-attr-name
             (lambda (a) (void)) ; tag-attr-value
             (lambda (a)
               (unless (or (equal? title "") (equal? text ""))
                 (define d (string-ref title 0))
                 (printf "~s " title)
                 (when (eq? (char-upcase d) (char-downcase d))
                   (when (for/or ([c (in-string title)])
                           (hash-ref allkanj c #f))
                     (printf "INCLUDE")
                     
                     (let ([text (let ([bt (open-output-string)])
                                   (for/list ([l (in-lines (open-input-string text) 'any)])
                                     (if (and (> (string-length l) 6)
                                              (eq? (string-ref l 0) #\[)
                                              (eq? (string-ref l 1) #\[)
                                              (eq? (string-ref l 4) #\:))
                                         (void)
                                         (let ()
                                           (display l bt) (newline bt)) ))
                                   (get-output-string bt) )])
                       (for ([c (in-string title)])
                         (hash-set! hshlookup c (cons title (hash-ref hshlookup c '()))))
                       (flush-output fo)
                       (write (list title (file-position fo) (string-length text)) fx) 
                       (newline fx)
                       (fprintf fo "~a~n~n" text)
                       (set! title "")
                       (set! text ""))))
                 (printf "~n")))
             )
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
      #:mode 'text ) ))

;;; To recreate wiktionary data files
;;; (create-wiktionary-data-file-if-needed "enwiktionary-20120812-pages-articles.xml" "knjidxl0.dat" "wiktdata.dat" "wiktindx.dat" "wiktlkup.dat")

;(load-wikt-data-files "wiktdata.dat" "wiktindx.dat" "wiktlkup.dat")

(create-wiktionary-data-file-if-needed "enwiktionary-20120812-pages-articles.xml" "knjidxl0.dat" "wiktdata.dat" "wiktindx.dat" "wiktlkup.dat")