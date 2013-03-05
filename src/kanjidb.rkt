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

(require srfi/1
         srfi/4
         srfi/9
         srfi/69
         (only-in racket/port reencode-input-port)
         (only-in xml read-xml xml->xexpr element-content element-attributes attribute-name attribute-value document-element element? element-name)
         (only-in racket/flonum for/flvector flvector-length in-flvector make-flvector flvector-copy flvector-ref)
         (only-in racket/unsafe/ops unsafe-fl* unsafe-fl+ unsafe-fx* unsafe-fx+ unsafe-flvector-set! unsafe-flvector-ref unsafe-fl> unsafe-fl- unsafe-fx- unsafe-fx< unsafe-fx>= unsafe-flsqrt unsafe-fx->fl unsafe-fl< unsafe-flabs unsafe-bytes-set!)
         (only-in racket/class new send make-object)
         (only-in racket/gui/base make-bitmap bitmap-dc% color% font% frame% canvas%)
         "constants-filenames.rkt"
         )

(provide kanjivectors
         kanjiinfo
         radk-list
         radk-bystroke
         UDT-kanji-info
         UDT-kanji-info-kanjichar
         UDT-kanji-info-???
         UDT-kanji-info-grade
         UDT-kanji-info-strokenum
         UDT-kanji-info-variant
         UDT-kanji-info-freq
         UDT-kanji-info-jlpt
         UDT-kanji-info-readings
         UDT-kanji-info-meanings
         UDT-kanji-info-nanori
         UDT-kanji-info-dicref
         do-kanjisearch
         load-datafiles
         create-indexes-if-needed
         debug-display-vk0-bitmap
         dc200x200->vector100x100/session
         kanjiletter->vector100x100/session
         make-data-files-from-kanjidic2
         make-data-file-from-radkfiles
         )

;; Implementation specific stuff is isolated here
(define (::read-u8vector! u8v fi) (read-bytes! u8v fi))
(define ::output-port-byte-position file-position)
(define (::sort lst ls ky) (sort lst ls #:key ky))
(define (::call-with-input-file/bin f c) ( call-with-input-file f c))
(define (::call-with-output-file/bin/keep f c) ( call-with-output-file f c))
(define (::call-with-input-file/text f c) ( call-with-input-file f c #:mode  'text))

(define RECMATRIX_WIDTH 32)
(define RECMATRIX_HEIGHT 32)

(define-record-type :UDT-kanji-info
  (UDT-kanji-info
     kanjichar ; 0
     ???       ; 1
     grade     ; 2
     strokenum ; 3
     variant   ; 4
     freq      ; 5
     jlpt      ; 6
     readings  ; 7
     meanings  ; 8
     nanori    ; 9
     dicref)  ; 10
  UDT-kanji-info?
  (kanjichar UDT-kanji-info-kanjichar)
  (???       UDT-kanji-info-???)
  (grade     UDT-kanji-info-grade)
  (strokenum UDT-kanji-info-strokenum)
  (variant   UDT-kanji-info-variant)
  (freq      UDT-kanji-info-freq)
  (jlpt      UDT-kanji-info-jlpt)
  (readings  UDT-kanji-info-readings)
  (meanings  UDT-kanji-info-meanings)
  (nanori    UDT-kanji-info-nanori)
  (dicref    UDT-kanji-info-dicref)
  
  )

(define kanjivectors '())
(define kanjiinfo    #f)
(define radk-list     (make-hash-table eqv?))
(define radk-bystroke (make-hash-table eqv?))

;;;
;;; do-kanjisearch
;;;
;;; Perform Kanji character pattern recognition from drawing
;;;
(define (do-kanjisearch stroke strokefactor bt)
  (define (/->0 o d) (if (eq? 0 d) o (/ o d)))
  (let*([v ((dc200x200->vector100x100/session) bt)]
        [vlen (flvector-length v)]
        [lsz (for/list ([p kanjivectors])
               (cons (for/fold ([sum 0.0])
                       ([e0 (in-flvector v)]
                        [e1 (in-flvector (cdr p))])
                       (unsafe-fl+ sum (unsafe-fl* e0 e1)))
                     (car p)) )]
        [lst (for/list ([zc lsz])
               (let ([zd (- stroke (car (UDT-kanji-info-strokenum (hash-table-ref kanjiinfo (cdr zc)))))])
                 (cons (+ (car zc) (/->0 strokefactor zd))
                       (cdr zc)) ))])
    (reverse (::sort lst < car))))

;;;
;;; load-indexes
;;;
;;; Load data and recognizer matrices into memory
;;;
(define (load-indexes FileIDX FileMTX)
  (::call-with-input-file/bin FileIDX
    (λ (fi)
      (::call-with-input-file/bin FileMTX
        (λ (fim)
          (set! kanjiinfo (make-hash-table eqv?))
          (let*([vlen (* RECMATRIX_WIDTH RECMATRIX_HEIGHT)]
                [bs (make-u8vector (* 4 vlen))])
            (set! kanjivectors
                  (let loop ([u (read fi)])
                    (if (eof-object? u)
                        '()
                        (begin
                          (::read-u8vector! bs fim)
                          (hash-table-set! kanjiinfo (string-ref (car u) 0)
                                     (apply UDT-kanji-info u))
                          (cons (cons (string-ref (car u) 0)
                                      (for/flvector ([i (in-range 0 vlen)])
                                        (floating-point-bytes->real bs #t (* i 4) (+ 4 (* i 4)))))
                                (loop (read fi))) )))) ))))))

;;;
;;; load-radicals
;;;
;;; Load RDC file into memory
;;;
(define (load-radicals FileRDC)
  (::call-with-input-file/bin FileRDC
    (λ (fi)
      (define signature    (read fi))
      (define file-list    (read fi))
      (define file-strokes (read fi))
      (when (or (not (equal? signature "Kanji Radicals"))
                (eof-object? file-list)
                (eof-object? file-strokes))
        (raise "Failure to load radical file"))
      (set! radk-list     (alist->hash-table file-list    eqv?))
      (set! radk-bystroke (alist->hash-table file-strokes eqv?) ))))

;;;
;;; load-datafiles
;;;
;;; FileIDX Path to IDX file
;;; FileMTX Path to MTX file
;;; FileRDC Path to RDC file
;;;
(define (load-datafiles FileIDX FileMTX FileRDC)
  (load-indexes FileIDX FileMTX)
  (load-radicals FileRDC) )


;;;
;;; create-indexes-if-needed
;;;
;;; Create graphical search indices and data from KANJIDIC2 XML file
;;;
;;; FileIDX Path to IDX file
;;; FileMTX Path to MTX file
;;; kanjidic2-xml-path Path to kanjidic2.xml
;;;
(define (create-indexes-if-needed FileIDX FileMTX kanjidic2-xml-path)
  (unless (file-exists? FileIDX)
    (::call-with-output-file/bin/keep FileIDX
      (λ (fo)
        (::call-with-output-file/bin/keep FileMTX
          (λ (fom)
            (define bs (make-u8vector (* 4 RECMATRIX_WIDTH RECMATRIX_HEIGHT)))
            (define knji->vec (kanjiletter->vector100x100/session))
            (::call-with-input-file/text kanjidic2-xml-path
              (λ (fi)
                (define (pick-elem-cont z)
                  ((compose xml->xexpr car element-content) z))
                (define (pick-elem-attr z at [df ""])
                  (let ([ret df])
                    (for ([zi (element-attributes z)])
                      (when (equal? at (attribute-name zi))
                        (set! ret (attribute-value zi)) ))
                    ret ))
                (for ([a (element-content (document-element (read-xml fi)))])
                  (when (element? a)
                    (case (element-name a)
                      [[character]
                       (let ([knj-letter ""]
                             [knj-grade #f]
                             [knj-strokenum #f]
                             [knj-variant #f]
                             [knj-freq #f]
                             [knj-jlpt #f]
                             [knj-readings #f]
                             [knj-meanings #f]
                             [knj-nanori #f]
                             [knj-dicref #f])
                         (for ([b (element-content a)])
                           (when (element? b)
                             (case (element-name b)
                               [[literal]
                                (set! knj-letter (pick-elem-cont b)) ]
                               
                               [[codepoint query_code] (void)]
                               [[radical] (void)] ; (radical () "\n" (rad_value ((rad_type "classical")) "7") "\n" (rad_value ((rad_type "nelson_c")) "1") "\n")
                               
                               [[misc]
                                (for ([c (element-content b)])
                                  (when (element? c)
                                    (case (element-name c)
                                      [[grade]
                                       (set! knj-grade (string->number (pick-elem-cont c))) ]
                                      [[stroke_count]
                                       (set! knj-strokenum (map string->number
                                                                (regexp-match* #rx"[0-9]+" (pick-elem-cont c)))) ]
                                      [[variant]
                                       (set! knj-variant (pick-elem-cont c)) ]
                                      [[freq]
                                       (set! knj-freq (string->number (pick-elem-cont c))) ]
                                      [[jlpt]
                                       (set! knj-jlpt (string->number (pick-elem-cont c))) ]
                                      [else (void)])))]
                               
                               [[dic_number]
                                (let ([tmplist-dicref (make-hash-table)])
                                  (for ([c (element-content b)])
                                    (when (element? c)
                                      (case (element-name c)
                                        [[dic_ref]
                                         (let ([type (pick-elem-attr c 'dr_type)]
                                               [nval (string->number (pick-elem-cont c))])
                                           (when (equal? "moro" type)
                                             (set! type (format "~a p. ~a vol. ~a" type
                                                                (pick-elem-attr c 'm_page)
                                                                (pick-elem-attr c 'm_vol))) )
                                           (hash-table-set! tmplist-dicref type
                                                      (cons nval (hash-table-ref tmplist-dicref type (lambda _ '())))) )]
                                        [else (void)])))
                                  (set! knj-dicref (hash-table->alist tmplist-dicref)) )]
                               
                               [[reading_meaning]
                                (let ([tmplist-nanori   (make-hash-table)]
                                      [tmplist-readings (make-hash-table)]
                                      [tmplist-meanings (make-hash-table)])
                                  
                                  (for ([c (element-content b)])
                                    (when (element? c)
                                      (case (element-name c)
                                        [[rmgroup]
                                         (for ([d (element-content c)])
                                           (when (element? d)
                                             (case (element-name d)
                                               [[reading]
                                                (let ([type (string->symbol (pick-elem-attr d 'r_type))]
                                                      [nval (pick-elem-cont d)])
                                                  (hash-table-set! tmplist-readings type
                                                             (cons nval (hash-table-ref tmplist-readings type (lambda _ '())))) )]
                                               [[meaning]
                                                (let ([lang (string->symbol (pick-elem-attr d 'm_lang "en"))]
                                                      [nval (pick-elem-cont d)])
                                                  (hash-table-set! tmplist-meanings lang
                                                             (cons nval (hash-table-ref tmplist-meanings lang (lambda _ '())))) )]
                                               [else (void)])))]
                                        
                                        [[nanori]
                                         (let ([unkn (pick-elem-attr c 'unkn)] ; in case later
                                               [nval (pick-elem-cont c)])
                                           (hash-table-set! tmplist-nanori unkn
                                                      (cons nval (hash-table-ref tmplist-nanori unkn (lambda _ '())))) )]
                                        
                                        [else (void)])))
                                  (set! knj-readings (hash-table->alist tmplist-readings))
                                  (set! knj-meanings (hash-table->alist tmplist-meanings))
                                  (set! knj-nanori   (hash-table->alist tmplist-nanori)) )]
                               [else (void)])))
                         
                         (display knj-letter)
                         
                         (let ([mtxpos (::output-port-byte-position fom)])
                           (let*([kflv (knji->vec knj-letter)]
                                 [kflvlen (flvector-length kflv)])
                             (for ([e (in-flvector kflv)]
                                   [i (in-range 0 kflvlen)])
                               (real->floating-point-bytes e 4 #t bs (* i 4)))
                             (write-bytes bs fom) )
                           (write (list knj-letter mtxpos knj-grade knj-strokenum knj-variant knj-freq knj-jlpt knj-readings knj-meanings knj-nanori knj-dicref) fo) ))]
                      [else (void)]))))
              ) ))))))

;;;
;;; create-radicalsfile-if-needed-from-radkfile2list
;;;
;;; Derives data files from RADKFILE
;;;
;;; FileRDC Path to RDC file
;;; rkflst List of paths to RADKFILE files, i.e. "radkfile" "radkfile2"
;;;
(define (create-radicalsfile-if-needed-from-radkfiles FileRDC rkflst)
  (unless (file-exists? FileRDC)
    (when (andmap file-exists? rkflst)
      (define radk-list     (make-hash-table eqv?))
      (define radk-bystroke (make-hash-table eqv?))
      (::call-with-output-file/bin/keep FileRDC
        (λ (fo)
          (for ([rkf rkflst])
            (::call-with-input-file/bin rkf
              (λ (fi)
                (let ([fic (reencode-input-port fi "EUC-JP" #f)]
                      [rad #f])
                  (let loop ([ln (read-line fic 'any)])
                    (unless (eof-object? ln)
                      (case (string-ref ln 0)
                        [[#\#] (void)]
                        [[#\$]
                         (set! rad (string-ref ln 2))
                         (let ([strk (string->number (cadr (regexp-match "\\$ . ([0-9]+)" ln)))])
                           (hash-table-set! radk-bystroke strk (lset-union eqv? (hash-table-ref radk-bystroke strk (lambda _ '())) (list rad))))
                         ]
                        [else
                         (hash-table-set! radk-list rad (append (hash-table-ref radk-list rad (lambda _ '())) (string->list ln))) ])
                      
                      (loop (read-line fic 'any)) ))))))
          (write "Kanji Radicals" fo)
          (write (hash-table->alist radk-list) fo)
          (write (hash-table->alist radk-bystroke) fo) )))))
  
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


#|
|| make-data-files-from-kanjidic2
|#
(define (make-data-files-from-kanjidic2 kanjidic2-xml-path)
  (create-indexes-if-needed
   (resolve-data-file-path CONST_FILE_KANJIIDX0)
   (resolve-data-file-path CONST_FILE_KANJIMTX)
   kanjidic2-xml-path))

#|
|| make-data-file-from-kradfile2list
|#
(define (make-data-file-from-radkfiles rkflst)
  (create-radicalsfile-if-needed-from-radkfiles
   (resolve-data-file-path CONST_FILE_KANJIRDC0)
   rkflst))
