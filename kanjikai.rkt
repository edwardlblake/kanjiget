#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         racket/string
         racket/dict
         racket/set
         racket/block
         "kanjidb.rkt"
         "wiktionarydb.rkt"
         "wiktionaryviewer.rkt"
         "preferences.rkt"
         "radicaldialog.rkt"
         "intoradicalsdialog.rkt"
         "constants-filenames.rkt"
         "stayontop.rkt")

(define STR_DRAW_KANJI_HERE "Draw Kanji Here")
(define STR_BTN_RESTARTOVER "Restart Over")
(define STR_BTN_SEARCH      "Search")
(define STR_WIN_KANJIFINDER "Kanjikai")

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
#|
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
|#

(define mnu.help.about
  (new menu-item%
       [label "&About"]
       [parent mnu.help]
       [callback
        (lambda (itm evt)
          (void);(make-about-dialog frame)
          )]
       [help-string "Information about Kanjikai"]))


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
       [label "Stroke # wt:"]
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
                                                      (send strokescoringcombo get-selection)) bt))
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
       [label ">>"]
       [parent pnlkanjiactions]
       [callback
        (lambda(btn evt)
          ;(send btnpnlkanjiactions-knjtxtbox set-value 
          ;      (format "~a~a" (send btnpnlkanjiactions-knjtxtbox get-value) cursel-kanji))
          (autocomplete-word cursel-kanji)
          (send btnpnlkanjiactions-knjtxtbox focus)
          )]
       [enabled #t]))
(define (autocomplete-word newc)
  (define edt (send btnpnlkanjiactions-knjtxtbox get-editor))
  (send edt insert "")
  (if (eq? #f newc)
      (let ()
        (define txt (send btnpnlkanjiactions-knjtxtbox get-value))
        (when (> (string-length txt) 0)
          (define lst (wikt-wordlist-from-word txt))
          (with-handlers ([exn:fail? void])
            (for/or ([a lst])
              (if (equal? (substring a 0 (string-length txt)) txt)
                  (let ()
                    (send edt insert (substring a (string-length txt)))
                    (send edt set-position (string-length txt) (string-length (send btnpnlkanjiactions-knjtxtbox get-value)))
                    #t
                    )
                  #f)))))
      (let ()
        (send edt insert (format "~a" newc))
        (autocomplete-word #f))))

(define btnpnlkanjiactions-knjtxtbox
  (new text-field%
       [parent pnlkanjiactions]
       [label ""]
       [callback
        (lambda (txt evt)
          (void);(autocomplete-word #f) ; maybe later
          )]
       [init-value ""]
       [style '(single horizontal-label)]
       ))

(define btnpnlkanjiactions-lookwikt
  (new button%
       [label "Wiktionary"]
       [parent pnlkanjiactions]
       [callback
        (lambda(btn evt)
        (define txt (send btnpnlkanjiactions-knjtxtbox get-value))
          (open-wiktionary txt)
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
  (load-datafiles CONST_FILE_KANJIIDX0
                  CONST_FILE_KANJIMTX
                  CONST_FILE_KANJIRDC0)
  (thread
   (lambda ()
     (load-wikt-data-files CONST_FILE_WIKTDATA CONST_FILE_WIKTINDX CONST_FILE_WIKTLKUP)))
  (enabledisable-actions)
  )
