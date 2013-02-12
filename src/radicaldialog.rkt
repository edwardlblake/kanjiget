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

(require (only-in racket/class new send)
         (only-in racket/gui/base dialog% horizontal-pane% check-box%)
         "kanjidb.rkt"
         "app-labels-en.rkt")

(provide pick-radical-from-list)

(define (pick-radical-from-list parentwin)
  (let ([rdk #f]
        [frame (new dialog%
                    [parent parentwin]
                    [label STR_WIN_SELECTRADICAL]
                    [width 800]
                    [height 300])])
    
    (let ([pl (new horizontal-pane% [parent frame] [alignment '(left top)])])
      (for ([(n st) (in-hash radk-bystroke)])
        (for ([k (in-list st)]
              [idx (in-range 0 (hash-count radk-list))])
          (when (= (modulo idx 25) 0)
            (set! pl (new horizontal-pane% [parent frame] [alignment '(left top)])))
          (new check-box%
               [label (format "~a" k)]
               [parent pl]
               [callback
                (λ (chk evt)
                  (set! rdk k)
                  (send frame show #f)
                  )]
               [value #f]
               [stretchable-height #f]) )))
    
    (send frame show #t)
    rdk ))