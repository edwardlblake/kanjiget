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
         srfi/69
         (only-in racket/class new send make-object class init super-new define/override this super)
         (only-in racket/gui/base frame% menu-bar% menu% menu-item% append-editor-operation-menu-items checkable-menu-item% horizontal-pane% vertical-pane% make-bitmap bitmap-dc% color% canvas% choice% button% editor-canvas% message% check-box% list-box% view-control-font normal-control-font popup-menu% text-field% message-box text% style-delta%)
         (only-in racket/list add-between)
         "kanjidb.rkt"
         "wiktionarydb.rkt"
         "wiktionaryviewer.rkt"
         "preferences.rkt"
         "radicaldialog.rkt"
         "intoradicalsdialog.rkt"
         "constants-filenames.rkt"
         "stayontop.rkt"
         "app-labels-en.rkt"
         "aboutdialog.rkt")

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
       [label STR_MENU_FILE]
       [parent mnu]
       [help-string STR_MENU_FILE_HELPSTRING]))
(define mnu.file.exit
  (new menu-item%
       [label STR_MENU_FILE_EXIT]
       [parent mnu.file]
       [callback
        (λ (itm evt)
          (exit 0)
          )]
       [shortcut #\Q]
       [help-string STR_MENU_FILE_EXIT_HELPSTRING]
       [shortcut-prefix '(ctl)]))
(define mnu.edit
  (new menu%
       [label STR_MENU_EDIT]
       [parent mnu]))

(define mnu.edit.___
  (append-editor-operation-menu-items mnu.edit))
(define mnu.view
  (new menu%
       [label STR_MENU_VIEW]
       [parent mnu]))
(define mnu.tools
  (new menu%
       [label STR_MENU_TOOLS]
       [parent mnu]))
(define mnu.help
  (new menu%
       [label STR_MENU_HELP]
       [parent mnu]
       [help-string STR_MENU_HELP_HELPSTRING]))

(define stn^hidenograde #t)
(define stn^hidenojlpt #t)
(define stn^hidenofreq #f)

(define mnu.view.hidenograde
  (new checkable-menu-item%
       [label STR_MENU_VIEW_HIDENOGRADE]
       [parent mnu.view]
       [callback
        (λ (itm evt)
          (set! stn^hidenograde (not stn^hidenograde))
          (send itm check  stn^hidenograde)
          (refresh-results)
          )]
       [help-string STR_MENU_VIEW_HIDENOGRADE_HELPSTRING]
       [checked stn^hidenograde]
       ))
(define mnu.view.hidenojlpt
  (new checkable-menu-item%
       [label STR_MENU_VIEW_HIDENOJLPT]
       [parent mnu.view]
       [callback
        (λ (itm evt)
          (set! stn^hidenojlpt (not stn^hidenojlpt))
          (send itm check  stn^hidenojlpt)
          (refresh-results)
          )]
       [help-string STR_MENU_VIEW_HIDENOJLPT_HELPSTRING]
       [checked stn^hidenojlpt]
       ))
(define mnu.view.hidenofreq
  (new checkable-menu-item%
       [label STR_MENU_VIEW_HIDENOFREQ]
       [parent mnu.view]
       [callback
        (λ (itm evt)
          (set! stn^hidenofreq (not stn^hidenofreq))
          (send itm check  stn^hidenofreq)
          (refresh-results)
          )]
       [help-string STR_MENU_VIEW_HIDENOFREQ_HELPSTRING]
       [checked stn^hidenofreq]
       ))

(define mnu.view.manualradical
  (new menu%
       [label STR_MENU_VIEW_ADDFILTERBYRADICAL]
       [parent mnu.view]
       [help-string STR_MENU_VIEW_ADDFILTERBYRADICAL_HELPSTRING]
       ))

(define (add-manual-radical-slot a)
  (new menu-item%
       [label (format STR_FORMAT_SLOT (add1 a))]
       [parent mnu.view.manualradical]
       [callback
        (λ (itm evt)
          (radicalcells-setcell! a (pick-radical-from-list frame))
          (refresh-radical-filter)
          (refresh-results)
          )]
       [help-string (format STR_FORMAT_SLOT_HELPSTRING (add1 a))]
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
       [label STR_MENU_TOOLS_STAYONTOP]
       [parent mnu.tools]
       [callback
        (λ (itm evt)
          (set! stn^ontop (not stn^ontop))
          (send itm check  stn^ontop)
          (if stn^ontop
              (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_TOPMOST 0 0 0 0 3)
              (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_NOTOPMOST 0 0 0 0 3))
          )]
       [help-string STR_MENU_TOOLS_STAYONTOP_HELPSTRING]
       [checked stn^ontop]
       )
     ]
    [else #f]))

(define mnu.help.about
  (new menu-item%
       [label STR_MENU_HELP_ABOUT]
       [parent mnu.help]
       [callback
        (λ (itm evt)
          (show-about-dialog frame)
          )]
       [help-string STR_MENU_HELP_ABOUT_HELPSTRING]))


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
      (λ (canvas dc)
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
       [label STR_STROKENUMWT]
       [choices
        (list 
         STR_STROKENUMWT_NONE
         STR_STROKENUMWT_LOW
         STR_STROKENUMWT_MEDIUM
         STR_STROKENUMWT_HIGH)]
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
        (λ (btn evt)
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
    (apply string-append (add-between l ", ")))
  (define (cmaify2 l)
    (apply string-append (add-between l " / ")))
  (let*([kanjilst  lastresults]
        [kanjilst1 (for/list([i (in-range 0 (length kanjilst))]
                             [e kanjilst]
                             #:when (let*([k (cdr e)]
                                          [kfl (hash-table-ref kanjiinfo k)])
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
             [knfl (hash-table-ref kanjiinfo ltr)])
        (let ([lix (send kanji-results-list get-number)]
              [knf-grade    (UDT-kanji-info-grade knfl)]
              [knj-readings (UDT-kanji-info-readings knfl)]
              [knj-meanings (UDT-kanji-info-meanings knfl)]
              )
          (let*([knj-readings2 (if (eq? #f knj-readings) (make-hash-table) knj-readings)]
                [knj-meanings2 (if (eq? #f knj-meanings) (make-hash-table) knj-meanings)]
                [readingslist (append (cdr (or (assoc 'ja_on knj-readings2) (cons #f '())))
                                      (cdr (or (assoc 'ja_kun knj-readings2) (cons #f '()))))]
                [meaningslist (cdr (or (assoc 'en knj-meanings2) (cons #f '())))])
            
            (send kanji-results-list append (format "~a" (add1 lix)) (string ltr))
            (send kanji-results-list set-string lix (string ltr) 1)
            (send kanji-results-list set-string lix (if (eq? #f knf-grade) "" (format "~a" knf-grade)) 2)
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
        (λ (btn evt)
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
       [alignment '(left top)]
       [stretchable-height #f]
       )
  )

(define pnlviewfilterlabel
  (new message%
       [label STR_LABEL_FILTERBYRADICAL]
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
                                  (hash-table-ref radk-list
                                        (vector-ref radicalcells idx)
                                        (lambda _ #f))
                                  #f))
              (vector-ref radicalcells idx)
              )])
    (if (eq? '() rl)
        (set! viewradicalfilter (λ (k) #t))
        (let*([o (let*([kl (map (λ (r)
                                  (delete-duplicates (cons r (hash-table-ref radk-list r)) eqv?)) rl)]
                       [setop lset-intersection])
                   (apply setop eqv? kl))])
          (set! viewradicalfilter (λ (k) (memv k o))))))
  )

(define viewradicalfilter (λ (k) #t))
(define (make-btnfilterview-checkbox a)
  (new check-box%
       [label "[    ]  "]
       [parent pnlviewfilter]
       [callback
        (λ (chk evt)
          (when (and 
                 (eq? #f (vector-ref radicalcells a))
                 (equal? #t (send chk get-value)))
            (radicalcells-setcell! a (pick-radical-from-list frame))
            )
          (refresh-radical-filter)
          (refresh-results)
          )]
       [value #f]
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
  (let ()
    (define (kanji-results-list-event-select lst evt)
      (let ([sel (send lst get-selection)])
        (unless (eq? #f sel)
          (let*([ltr (send lst get-data sel)]
                [knfl (hash-table-ref kanjiinfo (string-ref ltr 0))]
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
              [(> (length (hash-table-ref radk-list (string-ref ltr 0) (lambda _ '()))) 0)
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
            
            (let ()
              (define (mytxt2-append-dh-dd kttl knfv knfi)
                (unless (eq? #f knfv)
                  (let ([eb (send mytxt2 last-position)])
                    (send mytxt2 insert (format kttl) eb)
                    (send mytxt2 change-style sty2-normal eb 'end #f))
                  (let ([eb (send mytxt2 last-position)])
                    (send mytxt2 insert (knfi knfv) eb)
                    (send mytxt2 change-style sty2-normal eb 'end #f)))
                )
              (define mytxt2deffmt (λ (a) (format "\t~a~n" a)))
              (mytxt2-append-dh-dd STR_DESCTEXT_GRADE knf-grade mytxt2deffmt)
              (mytxt2-append-dh-dd STR_DESCTEXT_STROKENUM knf-strokenum
                                   (λ (a)
                                     (if ((length a) . > . 1)
                                         (format "\t~a ~a~n" (car a) 
                                                 (cons STR_DESCTEXT_MISCOUNTS (cdr a)))
                                         (format "\t~a~n" (car a)))))
              (mytxt2-append-dh-dd STR_DESCTEXT_VARIANTS   knf-variant mytxt2deffmt)
              (mytxt2-append-dh-dd STR_DESCTEXT_USAGEFREQ  knf-freq    mytxt2deffmt)
              (mytxt2-append-dh-dd STR_DESCTEXT_JLPT       knf-jlpt    mytxt2deffmt)
              )
            
            (let ()
              (define (mytxt2-append-dh-dl kttl knfl)
                (unless (or (eq? #f knfl) (< (length knfl) 1))
                  (let ([eb (send mytxt2 last-position)])
                    (send mytxt2 insert (format "~a~n" kttl) eb)
                    (send mytxt2 change-style sty2-normal eb 'end #f))
                  (for ([ae knfl])
                    (let ((k (car ae))
                          (v (cdr ae)))
                      (let ([eb (send mytxt2 last-position)])
                        (send mytxt2 insert (format "\t~a\t~a~n" k (car v)) eb)
                        (send mytxt2 change-style sty2-normal eb 'end #f))
                      (for ([w (cdr v)])
                        (let ([eb (send mytxt2 last-position)])
                          (send mytxt2 insert (format "\t\t~a~n" w) eb)
                          (send mytxt2 change-style sty2-normal eb 'end #f))))))
                  )
              (mytxt2-append-dh-dl STR_DESCTEXT_READINGS knf-readings)
              (mytxt2-append-dh-dl STR_DESCTEXT_MEANINGS knf-meanings)
              (mytxt2-append-dh-dl STR_DESCTEXT_NANORI   knf-nanori)
              (mytxt2-append-dh-dl STR_DESCTEXT_DICTREFS knf-dicref)
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
         [label #f]
         [choices '()]
         [parent rightsidepane]
         [callback
          (λ (lst evt)
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
         [columns 
          (list
           "#"
           STR_RESULTSLIST_COLUMN_KANJI
           STR_RESULTSLIST_COLUMN_GRADE
           STR_RESULTSLIST_COLUMN_READINGS
           STR_RESULTSLIST_COLUMN_MEANINGS
           STR_RESULTSLIST_COLUMN_SCORE)]
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
    (for-each (λ (h) (send h enable enableradact))
              (list btnpnlkanjiactions-addasrad
                    ))
    )
  )

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
       [label STR_BTN_ADDASRADICAL]
       [parent pnlkanjiactions]
       [callback
        (λ (btn evt)
          (define pmn
            (new popup-menu% 
                 ;[title title]
                 ;[popdown-callback popdown-callback]
                 ;[demand-callback demand-callback]
                 ;[font font]
                 ))
          (define (make-addradical-menuitem a)
            (new menu-item%
                 [label (format STR_FORMAT_SLOT (add1 a))]
                 [parent pmn]
                 [callback
                  (λ (btn evt)
                    (radicalcells-setcell! a cursel-radical)
                    )]
                 [help-string (format STR_FORMAT_ADDTOSLOT_HELPSTRING (add1 a))]))
          (make-addradical-menuitem 0)
          (make-addradical-menuitem 1)
          (make-addradical-menuitem 2)
          (make-addradical-menuitem 3)
          
          (send btn popup-menu pmn 0 0)
          )]
       [enabled #t]))

(define btnpnlkanjiactions-knj2rad
  (new button%
       [label STR_BTN_RADICALSOFKANJI]
       [parent pnlkanjiactions]
       [callback
        (λ (btn evt)
          (define pmn
            (new popup-menu% 
                 ))
          (define (make-addradical-menuitem a)
            (new menu-item%
                 [label (format STR_FORMAT_SLOT (add1 a))]
                 [parent pmn]
                 [callback
                  (λ (btn evt)
                    (radicalcells-setcell! a (pick-radical-from-kanji frame cursel-kanji))
                    )]
                 [help-string (format STR_FORMAT_ADDTOSLOT_HELPSTRING (add1 a))]))
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
        (λ (btn evt)
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
        (λ (txt evt)
          (void);(autocomplete-word #f) ; maybe later
          )]
       [init-value ""]
       [style '(single horizontal-label)]
       ))

(define btnpnlkanjiactions-lookwikt
  (new button%
       [label STR_BTN_WIKTIONARY]
       [parent pnlkanjiactions]
       [callback
        (λ (btn evt)
          (let ([txt (send btnpnlkanjiactions-knjtxtbox get-value)])
            (with-handlers ([exn:fail? (λ _ (message-box "Definitions" "No entry found!"))])
              (open-wiktionary txt)
              )
            )
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
  
  (with-handlers 
      ([exn:fail? 
        (λ (e) 
          (new message% [parent frame] [label (format "Error: ~a" e)]))])
    (load-datafiles
     (resolve-data-file-path CONST_FILE_KANJIIDX0)
     (resolve-data-file-path CONST_FILE_KANJIMTX)
     (resolve-data-file-path CONST_FILE_KANJIRDC0)))
    
  (thread
   (λ ()
     (load-wikt-data-files
      (resolve-data-file-path CONST_FILE_WIKTDATA)
      (resolve-data-file-path CONST_FILE_WIKTINDX)
      (resolve-data-file-path CONST_FILE_WIKTLKUP))))
  (enabledisable-actions)
  (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_TOPMOST 0 0 0 0 3)
  )
