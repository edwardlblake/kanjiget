#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         racket/string
         racket/dict
         racket/set
         racket/port
         xml
         racket/block
         racket/flonum
         racket/unsafe/ops
         ffi/unsafe
         "preferences.rkt"
         "radicaldialog.rkt"
         "intoradicalsdialog.rkt")

(define WINAPI_SetWindowPos
  (case (system-type)
    [[windows]
     (get-ffi-obj "SetWindowPos" "user32" (_fun _pointer _int _int _int _int _int _int -> _int)
                  (lambda () (void)))]
    [else void]))
(define WINAPI_HWND_TOPMOST  -1)
(define WINAPI_HWND_NOTOPMOST -2)

(define STR_DRAW_KANJI_HERE "Draw Kanji Here")
(define STR_BTN_RESTARTOVER "Restart Over")
(define STR_BTN_SEARCH      "Search")
(define STR_WIN_KANJIFINDER "Kanji Finder")

(define CONST_FILE_KANJIIDX0 "knjidxl0.dat")
(define CONST_FILE_KANJIMTX  "kanjimtx.dat")

(define RECMATRIX_WIDTH 32)
(define RECMATRIX_HEIGHT 32)

; Consider integrating http://kanjidamage.com/japanese_symbols
; Add links from kanji to entries
; Also, do same for other online references

; TODO: Difficult to find: 気 幸
; TODO: Add star button to push to fav a kanji entry
; TODO: Store faved kanji

(struct UDT-kanji-info
  (kanjichar ; 0
   ???       ; 1
   grade     ; 2
   strokenum ; 3
   variant   ; 4
   freq      ; 5
   jlpt      ; 6
   readings  ; 7
   meanings  ; 8
   nanori    ; 9
   dicref))  ; 10

(define kanjivectors '())
(define kanjiinfo    #f)
(define stn^ontop    #t)

(define cursel-kanji #f)
(define cursel-radical #f)
(define radicalcells (make-vector 4 #f))

(define frame (new frame%
                   [label STR_WIN_KANJIFINDER]
                   [width 800]
                   [height 300]))

(define mnu
  (new menu-bar%
       [parent frame]))
(define mnu.file
  (new menu%
       [label "&File"]
       [parent mnu]
       [help-string "File related options"]))
(define mnu.file.exit
  (new menu-item%
       [label "E&xit"]
       [parent mnu.file]
       [callback
        (lambda (itm evt)
          (exit 0)
          )]
       [shortcut #\Q]
       [help-string "Exit"]
       [shortcut-prefix '(ctl)]))
(define mnu.edit
  (new menu%
       [label "Edit"]
       [parent mnu]))

(define mnu.edit.___
  (append-editor-operation-menu-items mnu.edit))
(define mnu.view
  (new menu%
       [label "View"]
       [parent mnu]))
(define mnu.tools
  (new menu%
       [label "Tools"]
       [parent mnu]))
(define mnu.help
  (new menu%
       [label "&Help"]
       [parent mnu]
       [help-string "Help related options"]))

(define stn^hidenograde #t)
(define stn^hidenojlpt #t)
(define stn^hidenofreq #f)

(define mnu.view.hidenograde
  (new checkable-menu-item%
       [label "Hide Entries without JP &Grade"]
       [parent mnu.view]
       [callback
        (lambda (itm evt)
          (set! stn^hidenograde (not stn^hidenograde))
          (send itm check  stn^hidenograde)
          (refresh-results)
          )]
       [help-string "Set whether to hide entries that do not have a Grade set in Japan"]
       [checked stn^hidenograde]
       ))
(define mnu.view.hidenojlpt
  (new checkable-menu-item%
       [label "Hide Entries without &JLPT"]
       [parent mnu.view]
       [callback
        (lambda (itm evt)
          (set! stn^hidenojlpt (not stn^hidenojlpt))
          (send itm check  stn^hidenojlpt)
          (refresh-results)
          )]
       [help-string "Set whether to hide entries that are not part of a JLPT Level"]
       [checked stn^hidenojlpt]
       ))
(define mnu.view.hidenofreq
  (new checkable-menu-item%
       [label "Hide Entries without Freq."]
       [parent mnu.view]
       [callback
        (lambda (itm evt)
          (set! stn^hidenofreq (not stn^hidenofreq))
          (send itm check  stn^hidenofreq)
          (refresh-results)
          )]
       [help-string "Set whether to hide entries that do not have a Usage Frequency in Japan"]
       [checked stn^hidenofreq]
       ))

(define mnu.view.manualradical
  (new menu%
       [label "Add Filter by Radical"]
       [parent mnu.view]
       [help-string "Set whether to hide entries that do not have a Usage Frequency in Japan"]
       ))

(define (add-manual-radical-slot a)
  (new menu-item%
       [label (format "Slot ~a" (add1 a))]
       [parent mnu.view.manualradical]
       [callback
        (lambda (itm evt)
          (radicalcells-setcell! a (pick-radical-from-list frame))
          (refresh-radical-filter)
          (refresh-results)
          )]
       [help-string (format "Manually add radical to slot ~a" (add1 a))]
       )
  )
(define mnu.view.manualradical.slot1
  (add-manual-radical-slot 0))
(define mnu.view.manualradical.slot2
  (add-manual-radical-slot 1))
(define mnu.view.manualradical.slot3
  (add-manual-radical-slot 2))
(define mnu.view.manualradical.slot4
  (add-manual-radical-slot 3))



(define mnu.tools.stayontop
  (case (system-type)
    [[windows]
     (new checkable-menu-item%
       [label "&Stay on Top"]
       [parent mnu.tools]
       [callback
        (lambda (itm evt)
          (set! stn^ontop (not stn^ontop))
          (send itm check  stn^ontop)
          (if stn^ontop
              (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_TOPMOST 0 0 0 0 3)
              (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_NOTOPMOST 0 0 0 0 3))
          )]
       [help-string "Set whether window stays on top"]
       [checked stn^ontop]
       )
     ]
    [else #f]))
(define mnu.tools.genfiles
  (case (system-type)
    [[windows]
     (new menu-item%
       [label "(Re-)generate Matrices..."]
       [parent mnu.tools]
       [callback
        (lambda (itm evt)
          ; prompt to be sure
          (void) ; TODO
          )]
       [help-string "Set whether window stays on top"]
       )
     ]
    [else #f]))

(define mnu.help.about
  (new menu-item%
       [label "&About"]
       [parent mnu.help]
       [callback
        (lambda (itm evt)
          (void);(make-about-dialog frame)
          )]
       [help-string "Information about"]))


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


(define btstrokenum 0)
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
  (for ([a '(40 100 160)])
    (send dbtg draw-line 0 a 200 a)
    (send dbtg draw-line a 0 a 200)
    )
  (send dbtg set-pen (make-object color% 0 0 0 0.2) 2 'solid)
  (for ([a (list 1 2 3 4 5 6)])
    (for ([v (list (3   . * . a) (200 . - . (3   . * . a)))])
      (send dbtg draw-line 0 v 200 v)
      (send dbtg draw-line v 0 v 200)
      )
    )
  )

(define lx #f)
(define ly #f)
(define (kanjidraw-stop x y)
  (set! btstrokenum (add1 btstrokenum))
  (send dbt draw-line lx ly x y)
  (set! lx #f)
  (set! ly #f))
(define (kanjidraw-line x y)
  (when (not (equal? lx #f))
    (send dbt draw-line lx ly x y))
  (set! lx x)
  (set! ly y))
(define (kanjidraw-clear)
  (set! btstrokenum 0)
  (send dbt clear)
  (set! lx #f)
  (set! ly #f))

(define (do-kanjisearch stroke strokefactor)
  (define (/->0 o d) (if (eq? 0 d) o (/ o d)))
  (let*([v ((dc200x200->vector100x100/session) bt)]
        [vlen (flvector-length v)]
        [lsz (for/list ([p kanjivectors])
               (cons (for/fold ([sum 0.0])
                       ([e0 (in-flvector v)]
                        [e1 (in-flvector (cdr p))])
                       (unsafe-fl+ sum (unsafe-fl* e0 e1)))
                     (car p))
               )
             ]
        [lst (for/list ([zc lsz])
               (let ([zd (- stroke (first (UDT-kanji-info-strokenum (hash-ref kanjiinfo (cdr zc)))))])
                 (cons (+ (car zc) (/->0 strokefactor zd))
                       (cdr zc))))])
    (reverse (sort lst <
                   #:key car
                   #:cache-keys? #t))
    )
  )
;(define (do-kanjistroke)
;  )

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
     [vert-margin 2]
     [horiz-margin 2]
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

(define strokescoringcombo
  (new choice%
       [label "Stroke Num. Factor:"]
       [choices (list "None" "Low" "Medium" "High")]
       [parent leftsidepane]
       ;[callback callback]
       [style '(horizontal-label)]
       [selection 2]
       ;[font normal-control-font]
       [enabled #t]
       ;[vert-margin 2]
       ;[horiz-margin 2]
       ;[min-width graphical-minimum-width]
       ;[min-height graphical-minimum-height]
       [stretchable-width #f]
       [stretchable-height #f]
       ))

(define btnpane
  (new horizontal-pane%
       [parent leftsidepane]
       [alignment '(right top)]
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

(define lastresults '())

(define (shrink-string-for-list-if-needed str)
  (if (<= (string-length str) 200)
      str
      (format "~a..." (substring str 0 197))))

(define (refresh-results)
  (define (cmaify l)
    (string-append* (add-between l ", ")))
  (define (cmaify2 l)
    (string-append* (add-between l " / ")))
  (let*([kanjilst  lastresults]
        [kanjilst1 (for/list([i (in-range 0 (length kanjilst))]
                             [e kanjilst]
                             #:when (let*([k (cdr e)]
                                          [kfl (hash-ref kanjiinfo k)])
                                      (if (eq? i 0)
                                          (viewradicalfilter k)
                                          (and
                                           (if stn^hidenograde (number? (UDT-kanji-info-grade kfl)) #t)
                                           (if stn^hidenojlpt  (number? (UDT-kanji-info-jlpt kfl)) #t)
                                           (if stn^hidenofreq  (number? (UDT-kanji-info-freq kfl)) #t)
                                           (viewradicalfilter k)))))
                     e)]
        [kanjilst2 (if (> (length kanjilst1) 200) (take kanjilst1 200) kanjilst1)])
    (send kanji-results-list clear)
    (for ([e kanjilst2])
      (let* ([scr (car e)]
             [ltr (cdr e)]
             [knfl (hash-ref kanjiinfo ltr)])
        (let ([lix (send kanji-results-list get-number)]
              [knf-grade    (UDT-kanji-info-grade knfl)]
              [knj-readings (UDT-kanji-info-readings knfl)]
              [knj-meanings (UDT-kanji-info-meanings knfl)]
              )
          (let*([knj-readings2 (if (equal? #f knj-readings) (make-hash) knj-readings)]
                [knj-meanings2 (if (equal? #f knj-meanings) (make-hash) knj-meanings)]
                [readingslist (append (dict-ref knj-readings2 'ja_on '()) (dict-ref knj-readings2 'ja_kun '()))]
                [meaningslist (dict-ref knj-meanings2 'en '())])
            
            (send kanji-results-list append (format "~a" (add1 lix)) (string ltr))
            (send kanji-results-list set-string lix (string ltr) 1)
            (send kanji-results-list set-string lix (if (equal? #f knf-grade) "" (format "~a" knf-grade)) 2)
            (send kanji-results-list set-string lix (shrink-string-for-list-if-needed (cmaify2 readingslist)) 3)
            (send kanji-results-list set-string lix (shrink-string-for-list-if-needed (cmaify  meaningslist)) 4)
            (send kanji-results-list set-string lix (format "~a" scr) 5)
            )
          )
        )
      )
    )
  )

(define btn-search
  (new button%
       [label STR_BTN_SEARCH]
       [parent btnpane]
       [callback
        (lambda(btn evt)
          (set! lastresults (do-kanjisearch btstrokenum
                                            (list-ref '(0 50.0 200.0 400.0)
                                                      (send strokescoringcombo get-selection))))
          (refresh-results)
          )]
       [enabled #t]
       )
  )

(define kanji-results-editcanvaslft%
  (class editor-canvas%
    (init parent)
    (super-new [parent parent]
               [style '(no-border no-hscroll no-vscroll hide-hscroll hide-vscroll)]
               [min-width 140]
               [min-height 140]
               [stretchable-width #f]
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

(define pnlviewfilter
  (new horizontal-pane%
       [parent rightsidepane]
       ;[label "Filter:"]
       [alignment '(left top)]
       [stretchable-height #f]
       )
  )

(define pnlviewfilterlabel
  (new message%
       [label "Filter by Radicals:"]
       [parent pnlviewfilter]	 
       [stretchable-height #f]	 
       [auto-resize #f]))

(define (refresh-radical-filter)
  (let*([rl (for/list ([chk (list btnfilterview-check1
                                  btnfilterview-check2
                                  btnfilterview-check3
                                  btnfilterview-check4)]
                       [idx (list 0 1 2 3)]
                       #:when (if (send chk get-value)
                                  (hash-ref radk-list (vector-ref radicalcells idx) #f)
                                  #f))
              (vector-ref radicalcells idx)
              )])
    (if (equal? '() rl)
        (set! viewradicalfilter (lambda (k) #t))
        (let*([o (let*([kl (map (lambda (r)
                                  (apply seteqv (cons r (hash-ref radk-list r)))) rl)]
                       [setop set-intersect])
                   (apply setop kl)
                   )])
          (set! viewradicalfilter (lambda (k) (set-member? o k)))
          )
        )
    )
  )

(define viewradicalfilter (lambda (k) #t))
(define (make-btnfilterview-checkbox a)
  (new check-box%
       [label "[    ]  "]
       [parent pnlviewfilter]
       [callback
        (lambda (chk evt)
          (when (and 
                 (equal? #f (vector-ref radicalcells a))
                 (equal? #t (send chk get-value)))
            (radicalcells-setcell! a (pick-radical-from-list frame))
            )
          (refresh-radical-filter)
          (refresh-results)
          )]
       ;[style style]
       [value #f]
       ;[font font]
       ;[enabled enabled]
       ;[vert-margin vert-margin]
       ;[horiz-margin horiz-margin]
       ;[min-width min-width]
       ;[min-height min-height]
       ;[stretchable-width stretchable-width]
       [stretchable-height #f]))
(define btnfilterview-check1
  (make-btnfilterview-checkbox 0))
(define btnfilterview-check2
  (make-btnfilterview-checkbox 1))
(define btnfilterview-check3
  (make-btnfilterview-checkbox 2))
(define btnfilterview-check4
  (make-btnfilterview-checkbox 3))

(define kanji-results-list
  (block
  (define (kanji-results-list-event-select lst evt)
    (let ([sel (send lst get-selection)])
      (unless (equal? #f sel)
        (let*([ltr (send lst get-data sel)]
              [knfl (hash-ref kanjiinfo (string-ref ltr 0))]
              [knf-grade     (UDT-kanji-info-grade knfl)]
              [knf-strokenum (UDT-kanji-info-strokenum knfl)]
              [knf-variant   (UDT-kanji-info-variant knfl)]
              [knf-freq      (UDT-kanji-info-freq knfl)]
              [knf-jlpt      (UDT-kanji-info-jlpt knfl)]
              [knf-readings  (UDT-kanji-info-readings knfl)]
              [knf-meanings  (UDT-kanji-info-meanings knfl)]
              [knf-nanori    (UDT-kanji-info-nanori knfl)]
              [knf-dicref    (UDT-kanji-info-dicref knfl)])
          (set! cursel-kanji (string-ref ltr 0))
          (cond
            [(> (length (hash-ref radk-list (string-ref ltr 0) '())) 0)
             (set! cursel-radical (string-ref ltr 0))
             (enabledisable-actions)
             ]
            [else
             (set! cursel-radical #f)
             (enabledisable-actions)
             ])
          
          (send mytxt  lock #f)
          (send mytxt2 lock #f)
          
          (send mytxt select-all)
          (send mytxt clear)
          (let ([eb (send mytxt last-position)])
            (send mytxt insert ltr eb)
            (send mytxt change-style sty-kanji eb 'end #f))
          (send mytxt2 select-all)
          (send mytxt2 clear)
          
          (block
           (define (mytxt2-append-dh-dd kttl knfv knfi)
             (unless (equal? #f knfv)
               (let ([eb (send mytxt2 last-position)])
                 (send mytxt2 insert (format kttl) eb)
                 (send mytxt2 change-style sty2-normal eb 'end #f))
               (let ([eb (send mytxt2 last-position)])
                 (send mytxt2 insert (knfi knfv) eb)
                 (send mytxt2 change-style sty2-normal eb 'end #f)))
             )
           (define mytxt2deffmt (lambda (a) (format "\t~a~n" a)))
           (mytxt2-append-dh-dd "Grade:" knf-grade mytxt2deffmt)
           (mytxt2-append-dh-dd "Stroke #:" knf-strokenum
                                (lambda (a)
                                  (if ((length a) . > . 1)
                                      (format "\t~a ~a~n" (first a) 
                                              (cons "miscounts:" (rest a)))
                                      (format "\t~a~n" (first a)))))
           (mytxt2-append-dh-dd "Variants:"   knf-variant mytxt2deffmt)
           (mytxt2-append-dh-dd "Usage Freq:" knf-freq    mytxt2deffmt)
           (mytxt2-append-dh-dd "JLPT:"       knf-jlpt    mytxt2deffmt)
           )
          
          (block
           (define (mytxt2-append-dh-dl kttl knfl)
             (unless (or (equal? #f knfl) ((dict-count knfl) . < . 1))
               (let ([eb (send mytxt2 last-position)])
                 (send mytxt2 insert (format "~a~n" kttl) eb)
                 (send mytxt2 change-style sty2-normal eb 'end #f))
               (for ([(k v) (in-dict knfl)])
                 (let ([eb (send mytxt2 last-position)])
                   (send mytxt2 insert (format "\t~a\t~a~n" k (first v)) eb)
                   (send mytxt2 change-style sty2-normal eb 'end #f))
                 (for ([w (rest v)])
                   (let ([eb (send mytxt2 last-position)])
                     (send mytxt2 insert (format "\t\t~a~n" w) eb)
                     (send mytxt2 change-style sty2-normal eb 'end #f)))))
             )
           (mytxt2-append-dh-dl "Readings:" knf-readings)
           (mytxt2-append-dh-dl "Meanings:" knf-meanings)
           (mytxt2-append-dh-dl "Nanori:"   knf-nanori)
           (mytxt2-append-dh-dl "Dictionary References:" knf-dicref)
           )
          
          (send mytxt  lock #t)
          (send mytxt  set-position 0 'same #f #t 'default)
          (send mytxt2 lock #t)
          (send mytxt2 set-position 0 'same #f #t 'default)
          )))
    )
  (define (kanji-results-list-event-dclick lst evt)
    (printf "(TODO) Dclick")
    )
  (define (kanji-results-list-event-colsort lst evt)
    (printf "(TODO) Column sort?")
    )
  
  (new list-box%
       [label #f];STR_WIN_KANJIRESULTS]
       [choices '()]
       [parent rightsidepane]
       [callback
        (lambda (lst evt)
          (case (send evt get-event-type)
            [[list-box]        (kanji-results-list-event-select lst evt)]
            [[list-box-dclick] (kanji-results-list-event-dclick lst evt)]
            [[list-box-column] (kanji-results-list-event-colsort lst evt)]
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
       [columns '("#" "Kanji" "Grade" "Readings" "Meanings" "Score")]
       )
  )
  )

(define pnlkanjiactions ; add radical
  (new horizontal-pane%
       [parent rightsidepane]
       [alignment '(left top)]
       [stretchable-height #f]
       )
  )

(define (enabledisable-actions)
  (let ([enableradact (if (equal? cursel-radical #f) #f #t)])
    (for-each (lambda (h) (send h enable enableradact))
              (list btnpnlkanjiactions-addasrad
                    ;pnlkanjiactionslabel btnpnlkanjiactions-addrad1 btnpnlkanjiactions-addrad2 btnpnlkanjiactions-addrad3 btnpnlkanjiactions-addrad4
                    ))
    )
  )

;(define pnlkanjiactionslabel
;  (new message%
;       [label "Add as Radical:"]
;       [parent pnlkanjiactions]	 
;       [stretchable-height #f]	 
;       [auto-resize #f]))

(define (radicalcells-setcell! a kanjc)
  (vector-set! radicalcells a kanjc)
  (send (list-ref (list btnfilterview-check1
                        btnfilterview-check2
                        btnfilterview-check3
                        btnfilterview-check4) a)
        set-label (format "  ~a  " kanjc))
  )

(define btnpnlkanjiactions-addasrad
  (new button%
       [label "Add as Radical"]
       [parent pnlkanjiactions]
       [callback
        (lambda(btn evt)
          (define pmn
            (new popup-menu% 
                 ;[title title]
                 ;[popdown-callback popdown-callback]
                 ;[demand-callback demand-callback]
                 ;[font font]
                 ))
          (define (make-addradical-menuitem a)
            (new menu-item%
                 [label (format "Slot ~a" (add1 a))]
                 [parent pmn]
                 [callback
                  (lambda(btn evt)
                    (radicalcells-setcell! a cursel-radical)
                    )]
                 [help-string (format "Add to slot ~a" (add1 a))]))
          (make-addradical-menuitem 0)
          (make-addradical-menuitem 1)
          (make-addradical-menuitem 2)
          (make-addradical-menuitem 3)
          
          (send btn popup-menu pmn 0 0)
          )]
       [enabled #t]))

(define btnpnlkanjiactions-knj2rad
  (new button%
       [label "Radicals of Kanji"]
       [parent pnlkanjiactions]
       [callback
        (lambda(btn evt)
          (define pmn
            (new popup-menu% 
                 ))
          (define (make-addradical-menuitem a)
            (new menu-item%
                 [label (format "Slot ~a" (add1 a))]
                 [parent pmn]
                 [callback
                  (lambda(btn evt)
                    (radicalcells-setcell! a (pick-radical-from-kanji frame cursel-kanji))
                    )]
                 [help-string (format "Add to slot ~a" (add1 a))]))
          (make-addradical-menuitem 0)
          (make-addradical-menuitem 1)
          (make-addradical-menuitem 2)
          (make-addradical-menuitem 3)
          
          (send btn popup-menu pmn 0 0)
          )]
       [enabled #t]))

(define btnpnlkanjiactions-favknj
  (new button%
       [label "(TODO) Bookmark Kanji"]
       [parent pnlkanjiactions]
       [callback
        (lambda(btn evt)
          (printf "TODO: Bookmark ~s~n" cursel-kanji)
          (void)
          )]
       [enabled #t]))

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
(let ()
  (send mytxtconv set-editor mytxt)
  (send mytxtconv2 set-editor mytxt2)
  (send frame show #t)
  (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_TOPMOST 0 0 0 0 3)
  (void)
  )

(define (load-datafiles-if-exists)
  (when (file-exists? CONST_FILE_KANJIIDX0)
    (call-with-input-file CONST_FILE_KANJIIDX0
      (lambda (fi)
        (call-with-input-file CONST_FILE_KANJIMTX
          (lambda (fim)
            (set! kanjiinfo (make-hasheqv))
            (let*([vlen (* RECMATRIX_WIDTH RECMATRIX_HEIGHT)]
                  [bs (make-bytes (* 4 vlen))])
              (set! kanjivectors
                    (let loop ([u (read fi)])
                      (if (eof-object? u)
                          '()
                          (begin
                            (read-bytes! bs fim)
                            (hash-set! kanjiinfo (string-ref (first u) 0)
                                       (apply UDT-kanji-info u))
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
                                         (set! knj-strokenum (map string->number
                                                                  (regexp-match* #rx"[0-9]+" (pick-elem-cont c))))
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
                                    (set! knj-dicref (hash->list tmplist-dicref))
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
                                                  (let ([type (string->symbol (pick-elem-attr d 'r_type))]
                                                        [nval (pick-elem-cont d)])
                                                    (hash-set! tmplist-readings type
                                                               (cons nval (hash-ref tmplist-readings type '()))))
                                                  ]
                                                 [[meaning]
                                                  (let ([lang (string->symbol (pick-elem-attr d 'm_lang "en"))]
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
                                    (set! knj-readings (hash->list tmplist-readings))
                                    (set! knj-meanings (hash->list tmplist-meanings))
                                    (set! knj-nanori   (hash->list tmplist-nanori))
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
                    (for ([iy (in-range (unsafe-fx- y 15) (unsafe-fx+ y 15))]; 2)]
                          #:unless (unsafe-fx< iy 0)
                          #:unless (unsafe-fx>= iy RECMATRIX_HEIGHT))
                      (for ([ix (in-range (unsafe-fx- x 15) (unsafe-fx+ x 15))]; 2)]
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

;(let ()
;  ;(load-datafiles-if-exists)
;  (define knji (kanjiletter->vector100x100/session))
;  (set! kanjivectors
;        (for/list ([j "廓"])
;          (cons j (knji (string j)))
;          ))
;  )

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
              [[#\$] (set! rad (string-ref ln 2))]
              [else  (hash-set! radk-list rad (append (hash-ref radk-list rad '()) (string->list ln)))]
              )
            
            (loop (read-line fic 'any))))
        ))))

(load-kradfile2 "edict/kradzip/radkfile")
(load-kradfile2 "edict/kradzip/radkfile2")
(load-datafiles-if-exists)
(enabledisable-actions)

