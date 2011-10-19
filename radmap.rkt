#lang racket/gui

(require xml
         racket/block
         racket/flonum
         racket/unsafe/ops
         ffi/unsafe)

(define radk-bystroke (make-hasheqv))
(define radk-list (make-hasheqv))

(define (load-kradfile2 rkf)
  (call-with-input-file rkf
    (lambda (fi)
      (let ([fic (reencode-input-port fi "EUC-JP" #f)]
            [rad #f])
        (let loop ([ln (read-line fic 'any)])
          (unless (eof-object? ln)
            (case (string-ref ln 0)
              [[#\#] (void)]
              [[#\$]
               (set! rad (string-ref ln 2))
               (let ([strk (string->number (second (regexp-match "\\$ . ([0-9]+)" ln)))])
                 (hash-set! radk-bystroke strk (set-add (hash-ref radk-bystroke strk (seteqv)) rad)))
               ]
              [else  (hash-set! radk-list rad (append (hash-ref radk-list rad '()) (string->list ln)))]
              )
            
            (loop (read-line fic 'any))))
        ))))

(load-kradfile2 "edict/kradzip/radkfile")
(load-kradfile2 "edict/kradzip/radkfile2")

(define frame (new frame%
                   [label "ya..."]
                   [width 800]
                   [height 300]))

(let ([pl (new horizontal-pane% [parent frame] [alignment '(left top)])])
  (for ([(n st) (in-hash radk-bystroke)])
    (for ([k (in-set st)]
          [idx (in-range 0 (hash-count radk-list))])
      (when (= (modulo idx 25) 0)
        (set! pl (new horizontal-pane% [parent frame] [alignment '(left top)])))
      (new check-box%
           [label (format "~a" k)]
           [parent pl]
           [callback
            (lambda (chk evt)
              (void)
              )]
           [value #f]
           [stretchable-height #f])
      )
    )
  )

(send frame show #t)
