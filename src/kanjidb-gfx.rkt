#lang racket/base

#|

    KanjiGet
    Copyright 2011-2012 Edward L. Blake

    This source file is part of KanjiGet.
    See LICENSE.txt

|#

(require srfi/4
         (only-in racket/flonum for/flvector flvector-length in-flvector make-flvector flvector-copy flvector-ref)
         (only-in racket/unsafe/ops unsafe-fl* unsafe-fl+ unsafe-fx* unsafe-fx+ unsafe-flvector-set! unsafe-flvector-ref unsafe-fl> unsafe-fl- unsafe-fx- unsafe-fx< unsafe-fx>= unsafe-flsqrt unsafe-fx->fl unsafe-fl< unsafe-flabs unsafe-bytes-set!)
         (only-in racket/class new send make-object)
         (only-in racket/gui/base make-bitmap bitmap-dc% color% font% frame% canvas%)
         )

(define RECMATRIX_WIDTH 32)
(define RECMATRIX_HEIGHT 32)

;;;
;;; dc200x200->vector100x100/session
;;;
(provide dc200x200->vector100x100/session)
(define (dc200x200->vector100x100/session)
  (let* ([rbt  (make-bitmap RECMATRIX_WIDTH RECMATRIX_HEIGHT #t)]
         [drbt (new bitmap-dc% [bitmap rbt])]
         [mcpx0 (make-u8vector (* 200 1 4))]
         )
    (λ (tbt)
      (let ([rscl-left   #f]
            [rscl-right  #f]
            [rscl-top    #f]
            [rscl-bottom #f])
        
        (define (sumpxlarr)
          (for/fold ([sum 0])
            ([r (in-range 0 200)])
            (let ([ycl (unsafe-fx* r 4)])
              (+ sum
                 (- 765
                    (u8vector-ref mcpx0 (unsafe-fx+ 1 ycl))
                    (u8vector-ref mcpx0 (unsafe-fx+ 2 ycl))
                    (u8vector-ref mcpx0 (unsafe-fx+ 3 ycl)))) )))
        
        (for ([m-> (in-range 0   100  1)]
              [m<- (in-range 200 100 -1)])
          (when (eq? #f rscl-left)
            (send tbt get-argb-pixels m-> 0 1 200 mcpx0 #f #t)
            (when (> (sumpxlarr) 0)
              (set! rscl-left m->) ))
          (when (eq? #f rscl-right)
            (send tbt get-argb-pixels m<- 0 1 200 mcpx0 #f #t)
            (when (> (sumpxlarr) 0)
              (set! rscl-right m<-) ))
          (when (eq? #f rscl-top)
            (send tbt get-argb-pixels 0 m-> 200 1 mcpx0 #f #t)
            (when (> (sumpxlarr) 0)
              (set! rscl-top m->) ))
          (when (eq? #f rscl-bottom)
            (send tbt get-argb-pixels 0 m<- 200 1 mcpx0 #f #t)
            (when (> (sumpxlarr) 0)
              (set! rscl-bottom m<-) )))
        
        (when (eq? #f rscl-left)
          (set! rscl-left 0) )
        (when (eq? #f rscl-right)
          (set! rscl-right 200) )
        (when (eq? #f rscl-top)
          (set! rscl-top 0) )
        (when (eq? #f rscl-bottom)
          (set! rscl-bottom 200) )
        
        (send drbt set-scale
              (/ RECMATRIX_WIDTH (- rscl-right rscl-left))
              (/ RECMATRIX_HEIGHT (- rscl-bottom rscl-top)))
        (send drbt draw-bitmap tbt (- rscl-left) (- rscl-top))
        (let ([px0 (make-u8vector (* RECMATRIX_WIDTH RECMATRIX_HEIGHT 4))]
              [vk0 (make-flvector (* RECMATRIX_WIDTH RECMATRIX_HEIGHT))])
          (send drbt get-argb-pixels 0 0 RECMATRIX_WIDTH RECMATRIX_HEIGHT px0 #f #t)
          (for ([i (in-range 0 (flvector-length vk0))])
            (let* ([ycl (unsafe-fx* i 4)]
                   [pxsum (+ (u8vector-ref px0 (unsafe-fx+ 1 ycl))
                             (u8vector-ref px0 (unsafe-fx+ 2 ycl))
                             (u8vector-ref px0 (unsafe-fx+ 3 ycl)))]
                   [pxinv (max 0 (- 255 pxsum))]
                   [pxnrm (exact->inexact (/ pxinv 255))])
              (unsafe-flvector-set! vk0 i pxnrm) ))
          (let ([vk1 (flvector-copy vk0)])
            (for ([y (in-range 0 RECMATRIX_HEIGHT)])
              (for ([x (in-range 0 RECMATRIX_WIDTH)])
                (let ([v (unsafe-flvector-ref vk0 (unsafe-fx+ x (unsafe-fx* y RECMATRIX_WIDTH)))])
                  (when (unsafe-fl> v 0.5)
                    (for ([iy (in-range (unsafe-fx- y 15) (unsafe-fx+ y 15))]
                          #:unless (unsafe-fx< iy 0)
                          #:unless (unsafe-fx>= iy RECMATRIX_HEIGHT))
                      (for ([ix (in-range (unsafe-fx- x 15) (unsafe-fx+ x 15))]
                            #:unless (unsafe-fx< ix 0)
                            #:unless (unsafe-fx>= ix RECMATRIX_WIDTH)
                            )
                        (let* ([ivi (unsafe-fx+ ix (unsafe-fx* iy RECMATRIX_WIDTH))]
                               [dx (unsafe-fx- x ix)]
                               [dy (unsafe-fx- y iy)]
                               [oiv (unsafe-flvector-ref vk1 ivi)]
                               [niv (unsafe-fl*
                                     v 
                                     (unsafe-fl-
                                      (expt
                                       1.80 
                                       (unsafe-fl-
                                        0.0 
                                        (unsafe-flsqrt
                                         (unsafe-fx->fl
                                          (unsafe-fx+ (unsafe-fx* dx dx)
                                                      (unsafe-fx* dy dy))))))
                                      0.5))])
                          (when (unsafe-fl< oiv (unsafe-flabs niv))
                            (unsafe-flvector-set! vk1 ivi niv) ))))))))
            vk1 ))))))

;;;
;;; kanjiletter->vector100x100/session
;;;
;;; Create matrix from letter
;;;
(provide kanjiletter->vector100x100/session)
(define (kanjiletter->vector100x100/session)
  (define ->vec (dc200x200->vector100x100/session))
  (define kbt (make-bitmap 200 200 #t))
  (define dkbt (new bitmap-dc% [bitmap kbt]))
  (send dkbt set-scale 1 1)
  (send dkbt set-text-foreground (make-object color% 0 0 0 1.0))
  (send dkbt set-font (make-object font% 200 'default 'normal 'normal #f 'default 100))
  (λ (ltr)
    (let-values ([(tw th j0 j1) (send dkbt get-text-extent ltr)])
      (let ([v (- (/ 200 2) (/ tw 2)) ]
            [w (- (/ 200 2) (/ th 2)) ])
        (send dkbt clear)
        (send dkbt draw-text ltr v w)
        (->vec kbt) ))))

;;;
;;; debug-display-vk0-bitmap
;;;
;;; Debugging function for displaying matrices
;;;
(provide debug-display-vk0-bitmap)
(define (debug-display-vk0-bitmap vk0 [vky #f] [scr #f])
  (let* ([rbt  (make-bitmap RECMATRIX_WIDTH RECMATRIX_HEIGHT #t)]
         [ybt  (make-bitmap RECMATRIX_WIDTH RECMATRIX_HEIGHT #t)]
         [px0  (make-u8vector (* RECMATRIX_WIDTH RECMATRIX_HEIGHT 4))]
         [vlen (flvector-length vk0)])
    (for ([i (in-range 0 vlen)])
      (let* ([pxval (inexact->exact (round (* 255 (flvector-ref vk0 i))))]
             [ycl (* i 4)])
        (unsafe-bytes-set! px0 (+ 0 ycl) 255)
        (unsafe-bytes-set! px0 (+ 1 ycl) pxval)
        (unsafe-bytes-set! px0 (+ 2 ycl) pxval)
        (unsafe-bytes-set! px0 (+ 3 ycl) pxval) ))
    (send rbt set-argb-pixels 0 0 RECMATRIX_WIDTH RECMATRIX_HEIGHT px0 #f #t)
    (unless (eq? #f vky)
      (for ([i (in-range 0 vlen)])
        (let* ([pxval (inexact->exact (round (* 255 (flvector-ref vky i))))]
               [ycl (* i 4)])
          (unsafe-bytes-set! px0 (+ 0 ycl) 255)
          (unsafe-bytes-set! px0 (+ 1 ycl) pxval)
          (unsafe-bytes-set! px0 (+ 2 ycl) pxval)
          (unsafe-bytes-set! px0 (+ 3 ycl) pxval) ))
      (send ybt set-argb-pixels 0 0 RECMATRIX_WIDTH RECMATRIX_HEIGHT px0 #f #t) )
    
    (define frx
      (new frame%
           [label (if (eq? #f scr) "Debugging Vector" (format "Debug: ~s" scr))]
           [width 200]
           [height 200]))
    (new canvas%
         [parent frx]
         [paint-callback
          (λ (canvas dc)
            (send dc draw-bitmap rbt 0 0) )]
         [style '(border)]
         [vert-margin 10]	 
         [horiz-margin 10]
         [min-width 204]
         [min-height 204]
         [stretchable-width #f]
         [stretchable-height #f])
    (unless (eq? #f vky)
      (new canvas%
           [parent frx]
           [paint-callback
            (λ (canvas dc)
              (send dc draw-bitmap ybt 0 0)
              )]
           [style '(border)]
           [vert-margin 10]	 
           [horiz-margin 10]
           [min-width 204]
           [min-height 204]
           [stretchable-width #f]
           [stretchable-height #f]))
    
    (send frx show #t) ))
