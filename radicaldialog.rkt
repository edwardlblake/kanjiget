#lang typed/racket/base/no-check

(require racket/class
         racket/gui/base
         racket/set
         "kanjidb.rkt")

(provide pick-radical-from-list)

(define (pick-radical-from-list parentwin)
  (let ([rdk #f]
        [frame (new dialog%
                    [parent parentwin]
                    [label "Select Radical"]
                    [width 800]
                    [height 300])])
    
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
                  (set! rdk k)
                  (send frame show #f)
                  )]
               [value #f]
               [stretchable-height #f]) )))
    
    (send frame show #t)
    rdk ))