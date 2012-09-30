#lang typed/racket/base/no-check

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
