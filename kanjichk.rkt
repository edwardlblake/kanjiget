#lang racket/gui

(require xml
         racket/flonum
         racket/unsafe/ops)

(define STR_DRAW_KANJI_HERE "Draw Kanji Here")
(define STR_BTN_RESTARTOVER "Restart Over")
(define STR_BTN_SEARCH      "Search")
(define STR_WIN_KANJIFINDER "Kanji Finder")
(define STR_WIN_KANJIRESULTS "Kanji Results")

(define CONST_FILE_KANJIIDX0 "knjidxl0.dat")
(define CONST_FILE_KANJIMTX  "kanjimtx.dat")

(define RECMATRIX_WIDTH 32)
(define RECMATRIX_HEIGHT 32)

(define kanjivectors '())
(define kanjiinfo    #f)


(define frame (new frame%
                   [label STR_WIN_KANJIFINDER]
                   [width 780]
                   [height 300]))

  (define mnu
    (new menu-bar%
         [parent frame]))
  (define mnu.file
    (new menu%
         [label "&File"]
         [parent mnu]
         [help-string "File related options"]))
  (define mnu.edit
    (new menu%
         [label "Edit"]
         [parent mnu]))
  (define mnu.edit.___
    (append-editor-operation-menu-items mnu.edit))

(define mainpane
  (new horizontal-pane%
       [parent frame]
       [alignment '(right center)]
       )
  )

(define leftsidepane
  (new vertical-pane%
       [parent mainpane]
       [alignment '(right center)]
       [stretchable-width #f]
       )
  )

(define rightsidepane
  (new vertical-pane%
       [parent mainpane]
       [alignment '(left center)]
       )
  )


(define bt (make-bitmap 200 200 #t))
(define btg (make-bitmap 200 200 #t))
(define dbt (new bitmap-dc% [bitmap bt]))
(let ()
  (send dbt set-pen (make-object color% 0 0 0 1.0) 10 'solid)
  (send dbt set-scale 1 1)
  (send dbt set-text-foreground (make-object color% 0 0 0 1.0))
  (send dbt clear)
  )
(let ([dbtg (new bitmap-dc% [bitmap btg])])
  (send dbtg set-pen (make-object color% 0 0 0 0.6) 1 'solid)
  (send dbtg set-scale 1 1)
  (send dbtg set-text-foreground (make-object color% 0 0 0 0.6))
  (let-values([(tw th j0 j1) (send dbtg get-text-extent STR_DRAW_KANJI_HERE)])
    (let ([v ((200 . / . 2) . - . (tw . / . 2)) ]
          [w ((200 . / . 2) . - . (th . / . 2)) ])
      (send dbtg draw-text STR_DRAW_KANJI_HERE v w)
      )
    )
  (send dbtg set-pen (make-object color% 0 0 0 0.1) 1 'solid)
  (send dbtg draw-line 0 40 200 40)
  (send dbtg draw-line 0 100 200 100)
  (send dbtg draw-line 0 160 200 160)
  
  (send dbtg draw-line 40 0 40 200)
  (send dbtg draw-line 100 0 100 200)
  (send dbtg draw-line 160 0 160 200)
  (send dbtg set-pen (make-object color% 0 0 0 0.2) 2 'solid)
  (for ([a (list 1 2 3 4 5 6)])
    (let* ([v (3   . * . a)]
           [w (200 . - . v)])
      (send dbtg draw-line 0 v 200 v)
      (send dbtg draw-line 0 w 200 w)
      (send dbtg draw-line v 0 v 200)
      (send dbtg draw-line w 0 w 200)
      )
    )
  )

(define lx #f)
(define ly #f)
(define (kanjidraw-stop x y)
  (send dbt draw-line lx ly x y)
  (set! lx #f)
  (set! ly #f))
(define (kanjidraw-line x y)
  (when (not (equal? lx #f))
    (send dbt draw-line lx ly x y))
  (set! lx x)
  (set! ly y))
(define (kanjidraw-clear)
  (send dbt clear)
  (set! lx #f)
  (set! ly #f))

(define (do-kanjisearch)
  (let*([v ((dc200x200->vector100x100/session) bt)]
        [vlen (flvector-length v)]
        [lst (for/list ([p kanjivectors])
               (cons (for/fold ([sum 0.0])
                       ([e0 (in-flvector v)]
                        [e1 (in-flvector (cdr p))])
                       (unsafe-fl+ sum (unsafe-fl* e0 e1)))
                     (string (car p)))
               )
             ])
    (reverse (sort lst <
                   #:key car
                   #:cache-keys? #t))
    )
  )

(define mycanvas%
  (class canvas%
    (define adraw #f)
    (init parent)
    (super-new 
     [parent parent]
     [paint-callback
      (lambda (canvas dc)
        (send dc draw-bitmap bt 1 1)
        (send dc draw-bitmap btg 1 1)
        )]
     [style '(border)]
     [vert-margin 10]	 
     [horiz-margin 10]
     [min-width 204]
     [min-height 204]
     [stretchable-width #f]
     [stretchable-height #f])
    (define/override (on-event event)
      (let ([x (send event get-x)]
            [y (send event get-y)]
            [bd? (send event button-down? 'left)]
            [bu? (send event button-up? 'left)])
        (cond
          [bd? (set! adraw #t) (kanjidraw-line x y)]
          [bu? (set! adraw #f) (kanjidraw-stop x y)]
          [else (when adraw (kanjidraw-line x y) (send this refresh))]
          )
        )
      (super on-event event))
    ))

(define knjcanvas (new mycanvas% [parent leftsidepane]))

(define btnpane
  (new horizontal-pane%
       [parent leftsidepane]
       [alignment '(right center)]
       )
  )

(define btn-start-over
  (new button%
       [label STR_BTN_RESTARTOVER]
       [parent btnpane]
       [callback
        (lambda(btn evt)
          (kanjidraw-clear)
          (send knjcanvas refresh)
          (void))]
       [enabled #t]
       )
  )

(define btn-search
  (new button%
       [label STR_BTN_SEARCH]
       [parent btnpane]
       [callback
        (lambda(btn evt)
          (define (cmaify l)
            (string-append* (add-between l ", ")))
          (define (cmaify2 l)
            (string-append* (add-between l " / ")))
          (let*([kanjilst  (do-kanjisearch)]
                [kanjilst2 (if (> (length kanjilst) 500) (take kanjilst 500) kanjilst)])
            (send kanji-results-list clear)
            (for ([e kanjilst2])
              (let* ([scr (car e)]
                     [ltr (cdr e)]
                     [knfl (hash-ref kanjiinfo ltr)])
                (let ([lix (send kanji-results-list get-number)]
                      [knf-grade (list-ref knfl 2)]
                      ;[knf-strokenum (list-ref knfl 3)]
                      [knj-readings (list-ref knfl 7)]
                      [knj-meanings (list-ref knfl 8)]
                      )
                  (let*([knj-readings2 (if (equal? #f knj-readings) (make-hash) knj-readings)]
                        [knj-meanings2 (if (equal? #f knj-meanings) (make-hash) knj-meanings)]
                        [readingslist (append (hash-ref knj-readings2 "ja_on" '()) (hash-ref knj-readings2 "ja_ku" '()))]
                        [meaningslist (hash-ref knj-meanings2 "en" '())])
                  
                    (send kanji-results-list append (format "~a" (add1 lix)) ltr)
                    (send kanji-results-list set-string lix ltr 1)
                    (send kanji-results-list set-string lix (if (equal? #f knf-grade) "" (format "~a" knf-grade)) 2)
                    (send kanji-results-list set-string lix (cmaify2 readingslist) 3)
                    (send kanji-results-list set-string lix (cmaify  meaningslist) 4)
                    (send kanji-results-list set-string lix (format "~a" scr) 5)
                    )
                  )
                )
              )
            ))]
       [enabled #t]
       )
  )

(define kanji-results-editcanvaslft%
  (class editor-canvas%
    (init parent)
    (super-new [parent parent]
               [style '(no-border no-hscroll no-vscroll hide-hscroll hide-vscroll)]
 	 	;[scrolls-per-page scrolls-per-page]
 	 	;[label label]
 	 	;[wheel-step wheel-step]
 	 	;[line-count line-count]
 	 	;[horizontal-inset horizontal-inset]
 	 	;[vertical-inset vertical-inset]
 	 	;[enabled enabled]
 	 	;[vert-margin vert-margin]
 	 	;[horiz-margin horiz-margin]
 	 	[min-width 140]
 	 	[min-height 140]
 	 	[stretchable-width #f]
 	 	;[stretchable-height #t]
               )
    (define/override (on-event event)
      ;(when (send event button-down? 'left)
      ;  (printf "( ~a , ~a )~n" (send event get-x) (send event get-y)))
      (super on-event event)
      )
    (define/override (on-char event)
      (super on-char event))
    (define/override (on-paint)
      (super on-paint))
    ))

(define kanji-results-editcanvas%
  (class editor-canvas%
    (init parent)
    (super-new [parent parent]
               [style '(resize-corner)]
               )
    (define/override (on-event event)
      ;(when (send event button-down? 'left)
      ;  (printf "( ~a , ~a )~n" (send event get-x) (send event get-y)))
      (super on-event event)
      )
    (define/override (on-char event)
      (super on-char event))
    (define/override (on-paint)
      (super on-paint))
    ))


(define kanji-results-list
  (new list-box%
       [label STR_WIN_KANJIRESULTS]
       [choices '()]
       [parent rightsidepane]
       [callback
        (lambda (lst evt)
          (let ([sel (send lst get-selection)])
            (unless (equal? #f sel)
              (let ([ltr (send lst get-data sel)])
                (send mytxt select-all)
                (send mytxt clear)
                (let ([eb (send mytxt last-position)])
                  (send mytxt insert ltr eb)
                  (send mytxt change-style sty-kanji eb 'end #f))
                (send mytxt2 select-all)
                (send mytxt2 clear)
                (let ([eb (send mytxt2 last-position)])
                  (send mytxt2 insert (format "~a ~a ~a~n" ltr ltr ltr) eb)
                  (send mytxt2 change-style sty2-normal eb 'end #f))
                ;(let loop ([u kanjivectors])
                ;  (unless (empty? u)
                ;    (display (car (first u)))
                ;    (when (equal? ltr (string (car (first u))))
                ;      (debug-display-vk0-bitmap (cdr (first u))))
                ;    (loop (rest u))))
                )
              )
            
            ))]
       [style '(single vertical-label column-headers)]
       [selection #f]
       [font view-control-font]
       [label-font normal-control-font]
       [enabled #t]
       [vert-margin 2]
       [horiz-margin 2]
       [stretchable-width #t]
       [stretchable-height #t]
       [columns '("#" "Kanji" "Strokes" "Readings" "Meanings" "Score")]
       )
  )

;(send kanji-results-list set-column-width 0 30 20 10000)
;(send kanji-results-list set-column-width 1 30 20 10000)
;(send kanji-results-list set-column-width 2 30 20 10000)
(send kanji-results-list set-column-width 3 160 20 10000)
(send kanji-results-list set-column-width 4 200 20 10000)
(send kanji-results-list set-column-width 5 50 20 10000)


(define panekanjidesc
  (new horizontal-pane%
       [parent rightsidepane]
       [alignment '(right center)]
       )
  )

(define mytxtconv (new kanji-results-editcanvaslft% [parent panekanjidesc]))
(define mytxtconv2 (new kanji-results-editcanvas% [parent panekanjidesc]))
(define mytxt (new text%))
(define mytxt2 (new text%))

(define stl (send mytxt get-style-list))
(define stl2 (send mytxt get-style-list))
(define sty2-normal (send stl2 basic-style))
(define sty-normal (send stl basic-style))
(define sty-kanji (send stl find-or-create-style sty-normal
                        (let ([y (make-object style-delta%)])
                          (send y set-size-mult 8.0)
                          (send y set-alignment-off 'center)
                          (send y set-alignment-on 'center)
                          (send y set-delta-foreground (make-object color% 30 40 30))
                          y
                          )))
(send mytxtconv set-editor mytxt)
(send mytxtconv2 set-editor mytxt2)

(send frame show #t)

(define (load-datafiles-if-exists)
  (when (file-exists? CONST_FILE_KANJIIDX0)
    (call-with-input-file CONST_FILE_KANJIIDX0
      (lambda (fi)
        (call-with-input-file CONST_FILE_KANJIMTX
          (lambda (fim)
            (set! kanjiinfo (make-hash))
            (let*([vlen (* RECMATRIX_WIDTH RECMATRIX_HEIGHT)]
                  [bs (make-bytes (* 4 vlen))])
              (set! kanjivectors
                    (let loop ([u (read fi)])
                      (if (eof-object? u)
                          '()
                          (begin
                            (read-bytes! bs fim)
                            (hash-set! kanjiinfo (first u) u)
                            ;(display (first u))
                            (cons (cons (string-ref (first u) 0)
                                        (for/flvector ([i (in-range 0 vlen)])
                                          (floating-point-bytes->real bs #t (* i 4) (+ 4 (* i 4)))))
                                  (loop (read fi)))
                            )
                          )
                      )
                    )
              )
            )
          )
        )
      )
    )
  )
(define (create-indexes-if-needed)
  (if (file-exists? CONST_FILE_KANJIIDX0)
      (load-datafiles-if-exists)
      (call-with-output-file CONST_FILE_KANJIIDX0
        (lambda (fo)
          (call-with-output-file CONST_FILE_KANJIMTX
            (lambda (fom)
              (define bs (make-bytes (* 4 RECMATRIX_WIDTH RECMATRIX_HEIGHT)))
              (define knji->vec (kanjiletter->vector100x100/session))
              (call-with-input-file "edict/kanjidic2.xml"
                (lambda (fi)
                  (define (pick-elem-cont z)
                    ((compose xml->xexpr
                              first
                              element-content) z))
                  (define (pick-elem-attr z at [df ""])
                    (let ([ret df])
                      (for ([zi (element-attributes z)])
                        (when (equal? at (attribute-name zi))
                          (set! ret (attribute-value zi)))
                        )
                      ret)
                    )
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
                               [knj-dicref #f]
                               )
                           (for ([b (element-content a)])
                             (when (element? b)
                               (case (element-name b)
                                 [[literal]
                                  (set! knj-letter (pick-elem-cont b))
                                  ;(send mytxt insert (format "~a" (first (element-content b))))
                                  ]
                                 [[codepoint query_code] (void)]
                                 [[radical]
                                  (void)] ; (radical () "\n" (rad_value ((rad_type "classical")) "7") "\n" (rad_value ((rad_type "nelson_c")) "1") "\n")
                                 [[misc]
                                  (for ([c (element-content b)])
                                    (when (element? c)
                                      (case (element-name c)
                                        [[grade]
                                         (set! knj-grade (string->number (pick-elem-cont c)))
                                         ]
                                        [[stroke_count]
                                         (set! knj-strokenum ((compose string->number first)
                                                              (regexp-match #rx"^[0-9]+" "1 2 3")))
                                         ]
                                        [[variant]
                                         (set! knj-variant (pick-elem-cont c))
                                         ]
                                        [[freq]
                                         (set! knj-freq (string->number (pick-elem-cont c)))
                                         ]
                                        [[jlpt]
                                         (set! knj-jlpt (string->number (pick-elem-cont c)))
                                         ]
                                        [else (void)]
                                        )
                                      )
                                    )]
                                 [[dic_number]
                                  (let ([tmplist-dicref (make-hash)])
                                    (for ([c (element-content b)])
                                      (when (element? c)
                                        (case (element-name c)
                                          [[dic_ref]
                                           (let ([type (pick-elem-attr c 'dr_type)]
                                                 [nval (string->number (pick-elem-cont c))])
                                             (when (equal? "moro" type)
                                               (set! type (format "~a p. ~a vol. ~a"
                                                                  type
                                                                  (pick-elem-attr c 'm_page)
                                                                  (pick-elem-attr c 'm_vol))))
                                             (hash-set! tmplist-dicref type
                                                        (cons nval (hash-ref tmplist-dicref type '()))))
                                           ]
                                          [else (void)])
                                        )
                                      )
                                    (set! knj-dicref tmplist-dicref)
                                    )]
                                 [[reading_meaning]
                                  (let ([tmplist-nanori   (make-hash)]
                                        [tmplist-readings (make-hash)]
                                        [tmplist-meanings (make-hash)])
                                    
                                    (for ([c (element-content b)])
                                      (when (element? c)
                                        (case (element-name c)
                                          [[rmgroup]
                                           (for ([d (element-content c)])
                                             (when (element? d)
                                               (case (element-name d)
                                                 [[reading]
                                                  (let ([type (pick-elem-attr d 'r_type)]
                                                        [nval (pick-elem-cont d)])
                                                    (hash-set! tmplist-readings type
                                                               (cons nval (hash-ref tmplist-readings type '()))))
                                                  ]
                                                 [[meaning]
                                                  (let ([lang (pick-elem-attr d 'm_lang "en")]
                                                        [nval (pick-elem-cont d)])
                                                    (hash-set! tmplist-meanings lang
                                                               (cons nval (hash-ref tmplist-meanings lang '()))))
                                                  ]
                                                 [else (void)])
                                               )
                                             )
                                           ]
                                          [[nanori]
                                           (let ([unkn (pick-elem-attr c 'unkn)] ; in case later
                                                 [nval (pick-elem-cont c)])
                                             (hash-set! tmplist-nanori unkn
                                                        (cons nval (hash-ref tmplist-nanori unkn '()))))
                                           ]
                                          [else (void)]
                                          )
                                        )
                                      )
                                    (set! knj-readings tmplist-readings)
                                    (set! knj-meanings tmplist-meanings)                    
                                    (set! knj-nanori   tmplist-nanori)
                                    )
                                  ]
                                 [else (void)])
                               )
                             )
                           (display knj-letter)
                           
                           (let ([mtxpos (file-position fom)])
                             (let*([kflv (knji->vec knj-letter)]
                                   [kflvlen (flvector-length kflv)])
                               (for ([e (in-flvector kflv)]
                                     [i (in-range 0 kflvlen)])
                                 (real->floating-point-bytes e 4 #t bs (* i 4)))
                               (write-bytes bs fom)
                               )
                             (write (list knj-letter mtxpos knj-grade knj-strokenum knj-variant knj-freq knj-jlpt knj-readings knj-meanings knj-nanori knj-dicref) fo)
                             )
                           
                           )
                         ]
                        [else (void)]
                        ))
                    )
                  )
                #:mode 'text)
              )
            )
          )
        )
      )
  )


(define (debug-display-vk0-bitmap vk0 [vky #f] [scr #f])
  (let* ([rbt  (make-bitmap RECMATRIX_WIDTH RECMATRIX_HEIGHT #t)]
         [ybt  (make-bitmap RECMATRIX_WIDTH RECMATRIX_HEIGHT #t)]
         [px0  (make-bytes (* RECMATRIX_WIDTH RECMATRIX_HEIGHT 4))]
         [vlen (flvector-length vk0)])
    (for ([i (in-range 0 vlen)])
      (let* ([pxval (inexact->exact (round (* 255 (flvector-ref vk0 i))))]
             [ycl (* i 4)])
        (unsafe-bytes-set! px0 (+ 0 ycl) 255)
        (unsafe-bytes-set! px0 (+ 1 ycl) pxval)
        (unsafe-bytes-set! px0 (+ 2 ycl) pxval)
        (unsafe-bytes-set! px0 (+ 3 ycl) pxval)
        ))
    (send rbt set-argb-pixels 0 0 RECMATRIX_WIDTH RECMATRIX_HEIGHT px0 #f #t)
    (unless (equal? #f vky)
      (for ([i (in-range 0 vlen)])
        (let* ([pxval (inexact->exact (round (* 255 (flvector-ref vky i))))]
               [ycl (* i 4)])
          (unsafe-bytes-set! px0 (+ 0 ycl) 255)
          (unsafe-bytes-set! px0 (+ 1 ycl) pxval)
          (unsafe-bytes-set! px0 (+ 2 ycl) pxval)
          (unsafe-bytes-set! px0 (+ 3 ycl) pxval)
          ))
      (send ybt set-argb-pixels 0 0 RECMATRIX_WIDTH RECMATRIX_HEIGHT px0 #f #t)
      )
    
    (define frx (new frame%
                     [label (if (equal? #f scr) "Debugging Vector" (format "Debug: ~s" scr))]
                     [width 200]
                     [height 200]))
    (new canvas%
         [parent frx]
         [paint-callback
          (lambda (canvas dc)
            (send dc draw-bitmap rbt 0 0)
            )]
         [style '(border)]
         [vert-margin 10]	 
         [horiz-margin 10]
         [min-width 204]
         [min-height 204]
         [stretchable-width #f]
         [stretchable-height #f])
    (unless (equal? #f vky)
      (new canvas%
           [parent frx]
           [paint-callback
            (lambda (canvas dc)
              (send dc draw-bitmap ybt 0 0)
              )]
           [style '(border)]
           [vert-margin 10]	 
           [horiz-margin 10]
           [min-width 204]
           [min-height 204]
           [stretchable-width #f]
           [stretchable-height #f])
      )
    
    (send frx show #t)
    )
  )

(define (dc200x200->vector100x100/session)
  (let* ([rbt  (make-bitmap RECMATRIX_WIDTH RECMATRIX_HEIGHT #t)]
         [drbt (new bitmap-dc% [bitmap rbt])]
         [mcpx0 (make-bytes (* 200 1 4))]
         )
    (lambda (tbt)
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
                    (bytes-ref mcpx0 (unsafe-fx+ 1 ycl))
                    (bytes-ref mcpx0 (unsafe-fx+ 2 ycl))
                    (bytes-ref mcpx0 (unsafe-fx+ 3 ycl)))
                 )
              )
            )
          )
        
        (for ([m-> (in-range 0   100  1)]
              [m<- (in-range 200 100 -1)])
          (when (equal? #f rscl-left)
            (send tbt get-argb-pixels m-> 0 1 200 mcpx0 #f #t)
            (when (> (sumpxlarr) 0) (set! rscl-left m->)))
          (when (equal? #f rscl-right)
            (send tbt get-argb-pixels m<- 0 1 200 mcpx0 #f #t)
            (when (> (sumpxlarr) 0) (set! rscl-right m<-)))
          (when (equal? #f rscl-top)
            (send tbt get-argb-pixels 0 m-> 200 1 mcpx0 #f #t)
            (when (> (sumpxlarr) 0) (set! rscl-top m->)))
          (when (equal? #f rscl-bottom)
            (send tbt get-argb-pixels 0 m<- 200 1 mcpx0 #f #t)
            (when (> (sumpxlarr) 0) (set! rscl-bottom m<-)))
          )
        
        (when (equal? #f rscl-left) (set! rscl-left 0))
        (when (equal? #f rscl-right) (set! rscl-right 200))
        (when (equal? #f rscl-top) (set! rscl-top 0))
        (when (equal? #f rscl-bottom) (set! rscl-bottom 200))
        
        (send drbt set-scale
              (/ RECMATRIX_WIDTH (- rscl-right rscl-left))
              (/ RECMATRIX_HEIGHT (- rscl-bottom rscl-top)))
        (send drbt draw-bitmap tbt (- rscl-left) (- rscl-top))
        (let ([px0 (make-bytes (* RECMATRIX_WIDTH RECMATRIX_HEIGHT 4))]
              [vk0 (make-flvector (* RECMATRIX_WIDTH RECMATRIX_HEIGHT))])
          (send drbt get-argb-pixels 0 0 RECMATRIX_WIDTH RECMATRIX_HEIGHT px0 #f #t)
          (for ([i (in-range 0 (flvector-length vk0))])
            (let* ([ycl (unsafe-fx* i 4)]
                   [pxsum (+ (bytes-ref px0 (unsafe-fx+ 1 ycl))
                             (bytes-ref px0 (unsafe-fx+ 2 ycl))
                             (bytes-ref px0 (unsafe-fx+ 3 ycl)))]
                   [pxinv (max 0 (- 255 pxsum))]
                   [pxnrm (exact->inexact (/ pxinv 255))])
              (unsafe-flvector-set! vk0 i pxnrm)
              ))
          (let ([vk1 (flvector-copy vk0)])
            (for ([y (in-range 0 RECMATRIX_HEIGHT)])
              (for ([x (in-range 0 RECMATRIX_WIDTH)])
                (let ([v (unsafe-flvector-ref vk0 (unsafe-fx+ x (unsafe-fx* y RECMATRIX_WIDTH)))])
                  (when (unsafe-fl> v 0.5)
                    (for ([iy (in-range (unsafe-fx- y 15) (unsafe-fx+ y 15) 2)]
                          #:unless (unsafe-fx< iy 0)
                          #:unless (unsafe-fx>= iy RECMATRIX_HEIGHT))
                      (for ([ix (in-range (unsafe-fx- x 15) (unsafe-fx+ x 15) 2)]
                            #:unless (unsafe-fx< ix 0)
                            #:unless (unsafe-fx>= ix RECMATRIX_WIDTH)
                            )
                        (let* ([ivi (unsafe-fx+ ix (unsafe-fx* iy RECMATRIX_WIDTH))]
                               [dx (unsafe-fx- x ix)]
                               [dy (unsafe-fx- y iy)]
                               [oiv (unsafe-flvector-ref vk1 ivi)]
                               [niv (unsafe-fl* v 
                                                (unsafe-fl- (expt 1.80 
                                                                  (unsafe-fl- 0.0 
                                                                              (unsafe-flsqrt (unsafe-fx->fl (unsafe-fx+ (unsafe-fx* dx dx)
                                                                                                                        (unsafe-fx* dy dy))))))
                                                            0.5))])
                          (when (unsafe-fl< oiv (unsafe-flabs niv))
                            (unsafe-flvector-set! vk1 ivi niv))
                          )))))))
            vk1
            )
          )
        )
      )
    )
  )

(define (kanjiletter->vector100x100/session)
  (define ->vec (dc200x200->vector100x100/session))
  (define kbt (make-bitmap 200 200 #t))
  (define dkbt (new bitmap-dc% [bitmap kbt]))
  (send dkbt set-scale 1 1)
  (send dkbt set-text-foreground (make-object color% 0 0 0 1.0))
  (send dkbt set-font (make-object font% 200 'default 'normal 'normal #f 'default 100))
  (lambda (ltr)
    (let-values ([(tw th j0 j1) (send dkbt get-text-extent ltr)])
      (let ([v (- (/ 200 2) (/ tw 2)) ]
            [w (- (/ 200 2) (/ th 2)) ])
        (send dkbt clear)
        (send dkbt draw-text ltr v w)
        (->vec kbt)))))

(let ()
  ;(load-datafiles-if-exists)
  (define knji (kanjiletter->vector100x100/session))
  (set! kanjivectors
        (for/list ([j "廓"])
          (cons j (knji (string j)))
          ))
  )

