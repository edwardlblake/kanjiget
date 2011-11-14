#lang racket/gui

(provide pick-radical-from-kanji)

(define (pick-radical-from-kanji parentwin knji)
  (let*([rdk #f]
        [knjrads (make-hasheqv)]
        [radk-bystroke (make-hasheqv)]
        [radk-list (make-hasheqv)]
        [frame (new dialog%
                    [parent parentwin]
                    [label "Select Radical"]
                    [width 50]
                    [height 50])]
        [lbl (new message%
                  [label (format "Radicals for ~a" knji)]
                  [parent frame]	 
                  [stretchable-height #f]	 
                  [auto-resize #f])]
        )
    
    (define (load-kradfile2 rkf)
      (call-with-input-file rkf
        (lambda (fi)
          (let ([fic (reencode-input-port fi "EUC-JP" #f)])
            (let loop ([ln (read-line fic 'any)]
                       [lr #f])
              (unless (eof-object? ln)
                (case (string-ref ln 0)
                  [[#\#] (loop (read-line fic 'any) lr)]
                  [[#\$]
                   (loop (read-line fic 'any) (string-ref ln 2))
                   ]
                  [else
                   (for ([k (string->list ln)])
                     (when (equal? k knji)
                       (hash-set! knjrads lr #t))
                     )
                   (loop (read-line fic 'any) lr)]
                  )
                
                ))
            ))))
    
    (load-kradfile2 "edict/kradzip/radkfile")
    (load-kradfile2 "edict/kradzip/radkfile2")
    
    (let ([pl (new horizontal-pane% [parent frame] [alignment '(left top)])])
      (for ([(k y) (in-hash knjrads)])
        (new check-box%
             [label (format "~a" k)]
             [parent pl]
             [callback
              (lambda (chk evt)
                (set! rdk k)
                (send frame show #f)
                )]
             [value #f]
             [stretchable-height #f])
        )
      )
    (send frame show #t)
    rdk
    )
  )
