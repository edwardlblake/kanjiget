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

(require srfi/69
         (only-in racket/class new send)
         (only-in racket/gui/base dialog% message% horizontal-pane% check-box%)
         "rkt-common.rkt"
         "kanjidb.rkt")

(provide pick-radical-from-kanji)

(define (pick-radical-from-kanji parentwin knji)
  (let*([rdk #f]
        [knjrads (make-hash-table eqv?)]
        [frame (new dialog%
                    [parent parentwin]
                    [label STR_LABEL_SELECTRADICALS]
                    [width 50]
                    [height 50])]
        [lbl (new message%
                  [label (format STR_FORMAT_RADICALSFOR knji)]
                  [parent frame]	 
                  [stretchable-height #f]	 
                  [auto-resize #f])])
    
    (hash-table-fold radk-list
      (lambda (lr ln _)
        (for ([k ln])
          (when (equal? k knji)
            (hash-table-set! knjrads lr #t) )))
      #f)
    
    (let ([pl (new horizontal-pane% [parent frame] [alignment '(left top)])])
      (hash-table-fold knjrads
        (lambda (k y _)
          (new check-box%
             [label (format "~a" k)]
             [parent pl]
             [callback
              (λ (chk evt)
                (set! rdk k)
                (send frame show #f) )]
             [value #f]
             [stretchable-height #f]))
        #f)
      )
    (send frame show #t)
    rdk ))
