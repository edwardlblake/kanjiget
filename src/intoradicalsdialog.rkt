#lang typed/racket/base/no-check

(require racket/class
         racket/gui/base
         "kanjidb.rkt")

(provide pick-radical-from-kanji)

(define (pick-radical-from-kanji parentwin knji)
  (let*([rdk #f]
        [knjrads (make-hasheqv '())]
        [frame (new dialog%
                    [parent parentwin]
                    [label "Select Radical"]
                    [width 50]
                    [height 50])]
        [lbl (new message%
                  [label (format "Radicals for ~a" knji)]
                  [parent frame]	 
                  [stretchable-height #f]	 
                  [auto-resize #f])])
    
    (for ([(lr ln) (in-hash radk-list)])
      (for ([k ln])
        (when (equal? k knji)
          (hash-set! knjrads lr #t) )))
    
    (let ([pl (new horizontal-pane% [parent frame] [alignment '(left top)])])
      (for ([(k y) (in-hash knjrads)])
        (new check-box%
             [label (format "~a" k)]
             [parent pl]
             [callback
              (lambda (chk evt)
                (set! rdk k)
                (send frame show #f) )]
             [value #f]
             [stretchable-height #f]) ))
    (send frame show #t)
    rdk ))
