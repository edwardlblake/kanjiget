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

(require racket/class
         racket/gui/base
         racket/match
         racket/string
         racket/dict
         racket/block
         "wiktionarydb.rkt"
         "wiktionarytemplates.rkt"
         "stayontop.rkt"
         "constants-filenames.rkt")

(provide open-wiktionary)

(define (open-wiktionary wikttitle)
  (define STR_WIN_WIKTIONARYVIEWER "Wiktionary Viewer")
  
  (define stn^ontop    #t)
  
  (define (shrink-string-if-needed str)
    (if (<= (string-length str) 200)
        str
        (format "~a..." (substring str 0 197))))
  
  (define frame (new frame%
                     [label (shrink-string-if-needed (format "~a - ~a" STR_WIN_WIKTIONARYVIEWER wikttitle))]
                     [width 600]
                     [height 400]))
  
  (define mnu
    (new menu-bar%
         [parent frame]))
  (define mnu.edit
    (new menu%
         [label "Edit"]
         [parent mnu]))
  
  (define mnu.edit.___
    (append-editor-operation-menu-items mnu.edit))
  (define mnu.tools
    (new menu%
         [label "Tools"]
         [parent mnu]))
  
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
  
  
  (define mainpane
    (new horizontal-pane%
         [parent frame]
         [alignment '(right center)] ))
  
  (define kanji-results-editcanvas%
    (class editor-canvas%
      (init parent)
      (super-new [parent parent]
                 [style '(resize-corner)]
                 )
      (define/override (on-event event)
        (super on-event event) )
      (define/override (on-char event)
        (super on-char event) )
      (define/override (on-paint)
        (super on-paint) )))
  
  
  (define (enabledisable-actions)
    (void)
    )
  
  (define mytxtconv2 (new kanji-results-editcanvas% [parent mainpane]))
  (define mytxt2 (new text%))
  
  (define stl (send mytxt2 get-style-list))
  (define sty-normal (send stl find-or-create-style (send stl basic-style)
                           (let ([y (make-object style-delta%)])
                             (send y set-size-mult 1.5)
                             (send y set-delta-foreground (make-object color% 10 10 10))
                             y
                             )))
  (define sty-link (send stl find-or-create-style sty-normal
                          (let ([y (make-object style-delta%)])
                            (send y set-delta-foreground (make-object color% 40 90 150))
                            y
                            )))
  (define sty-nolink (send stl find-or-create-style sty-normal
                          (send (make-object style-delta%) set-delta-foreground (make-object color% 60 0 0 ))))
  (define sty-italic (send stl find-or-create-style sty-normal
                          (let ([y (make-object style-delta%)])
                            (send y set-style-on 'italic)
                            y
                            )))
  (define sty-bold (send stl find-or-create-style sty-normal
                          (let ([y (make-object style-delta%)])
                            (send y set-weight-on 'bold)
                            y
                            )))
  (define sty-italicbold (send stl find-or-create-style sty-normal
                          (let ([y (make-object style-delta%)])
                            (send y set-style-on 'italic)
                            (send y set-weight-on 'bold)
                            y
                            )))
  
  (define sty-title (send stl find-or-create-style sty-normal
                          (let ([y (make-object style-delta%)])
                            (send y set-size-mult 2.5)
                            (send y set-alignment-off 'center)
                            (send y set-alignment-on 'center)
                            (send y set-delta-foreground (make-object color% 30 40 30))
                            y
                            )))
  (define sty-h2 (send stl find-or-create-style sty-title
                       (let ([y (make-object style-delta%)])
                         (send y set-size-mult 0.8) y)))
  (define sty-h3 (send stl find-or-create-style sty-h2
                       (let ([y (make-object style-delta%)])
                         (send y set-size-mult 0.8) y )))
  (define sty-h4 (send stl find-or-create-style sty-h3
                       (let ([y (make-object style-delta%)])
                         (send y set-size-mult 0.8) y )))
  (define sty-h5 (send stl find-or-create-style sty-h4
                       (let ([y (make-object style-delta%)])
                         (send y set-size-mult 0.8) y )))
  (define sty-h6 (send stl find-or-create-style sty-h5
                       (let ([y (make-object style-delta%)])
                         (send y set-size-mult 0.8) y )))
  
  (define (curlyxpansion txt rewriteshash)
    (define (curlyxpansion-rewrite txt posn)
      (define inside (substring txt (+ (car posn) 2) (- (cdr posn) 2)))
      (define args (regexp-split #rx"\\|" inside))
      (string-append
       (substring txt 0 (car posn))
       ((hash-ref rewriteshash (regexp-replace* #rx"^ +| +$" (string-downcase (car args)) "")
                  (lambda () (lambda (a) (format "[[Template:~a]]" (car args)))))
        (cdr args))
       (substring txt (cdr posn)))
      )
    (define lst (reverse (regexp-match-positions* #rx"{{" txt)))
    (for ([a lst])
      (set! txt (curlyxpansion-rewrite txt (car (regexp-match-positions #rx"{{[^{}]+?}}" txt (car a))))) )
    txt)
  
  (define current-wikt-title wikttitle)
  
  (define current-wikt-text-source (wikt-get-definition wikttitle))
  (define current-wikt-text
    (let ([txt current-wikt-text-source])
      (curlyxpansion (regexp-replace* "<!--(.*?)-->" txt "")
                     (get-wiktionary-templates))
      ))
  
  (define (parse-to-markup-tree txt parse-info)
    (if (or (pair? txt) (procedure? txt))
        (if (pair? txt)
            (for/fold ([alst '()]) ([a txt])
              (define b (parse-to-markup-tree a parse-info))
              (if (and (pair? b) (not (procedure? (car b))))
                  (append alst b)
                  (append alst (list b))))
            txt)
        (let-values ([(posns clofs msyms) (parse-info txt)])
          (let walk ([posns posns]
                     [clofs clofs]
                     [msyms msyms]
                     [start 0]
                     [alst '()])
            (if (pair? posns)
                (let ([posn (car posns)]
                      [clof (car clofs)]
                      [msym (car msyms)])
                  (walk (cdr posns) (cdr clofs) (cdr msyms) (cdr posn)
                        `((,msym ,@clof)
                          ,(substring txt start (car posn)) . ,alst)) )
                (reverse (cons (substring txt start) alst)) )))))
  
  (define (wiki-markup->tree txt)
    (define (parse-wikt-links txt)
      (define lst 
        (regexp-match-positions* #rx"\\[\\[([^]]+)\\]\\]" txt))
      (values lst
              (for/list ([posn lst])
                (list (substring txt (+ (car posn) 2) (- (cdr posn) 2))) )
              (for/list ([posn lst]) 
                (define wrd (substring txt (+ (car posn) 2) (- (cdr posn) 2)))
                (lambda (eb edt)
                  (if (wikt-has-definition? wrd)
                      (let ()
                        (send edt set-clickback eb (send edt last-position)
                              (lambda (e s b) (open-wiktionary wrd) )
                              (send (make-object style-delta%) set-delta-foreground (make-object color% 40 255 30)))
                        (send edt change-style sty-link eb 'end #f))
                      (send edt change-style sty-nolink eb 'end #f))))))
    
    (define (parse-wikt-bolditalic txt)
      (define lst 
        (regexp-match-positions* #rx"'''''(.+?)'''''|'''(.+?)'''|''(.+?)''" txt))
      (values lst
              (for/list ([posn lst])
                (define tak
                  (cond
                    [(equal? (substring txt (car posn) (+ (car posn) 5)) "'''''") 5]
                    [(equal? (substring txt (car posn) (+ (car posn) 3)) "'''") 3]
                    [(equal? (substring txt (car posn) (+ (car posn) 2)) "''") 2]))
                (list (substring txt (+ (car posn) tak) (- (cdr posn) tak))) )
              (for/list ([posn lst])
                (define tak
                  (cond
                    [(equal? (substring txt (car posn) (+ (car posn) 5)) "'''''") sty-italicbold]
                    [(equal? (substring txt (car posn) (+ (car posn) 3)) "'''") sty-bold]
                    [(equal? (substring txt (car posn) (+ (car posn) 2)) "''") sty-italic]))
                (lambda (eb edt)
                  (send edt change-style tak eb 'end #f) ))))
    (parse-to-markup-tree 
     (parse-to-markup-tree 
      txt
      parse-wikt-bolditalic )
     parse-wikt-links) )
  
  (define (apply-markup-tree eb te edt)
    (if (pair? te)
        (if (procedure? (car te))
            (let ()
              (apply-markup-tree eb (cdr te) edt)
              ((car te) eb edt) )
            (for ([x te])
              (apply-markup-tree (send edt last-position) x edt) ))
        (let ()
          (send edt insert te eb)
          (send edt change-style sty-normal eb 'end #f) )))
    
  (define (generate-wiktionary-page)

    (define (wikt-add-text txt edt)
      (let ([eb (send edt last-position)]
            [te (wiki-markup->tree txt)])
        (apply-markup-tree eb te edt)
        ))
    
    (define last-listspec '())
    (define last-listeditors (make-vector 30 #f))
    (define last-listincrements (make-vector 30))
    
    (define (make-subtext edt)
      (define subtxt (new text%))
      (define edtsnp
        (new editor-snip%
             [editor subtxt]
             [with-border? #f]
             [left-margin 0]
             [top-margin 0]
             [right-margin 0]
             [bottom-margin 0]
             [left-inset 0]
             [top-inset 0]
             [right-inset 0]
             [bottom-inset 0]
             ))
      
      ; NOTE: It would be nice to use add-wide-snip here to make the editor-snips word-wrap,
      ;       but it requires framework which is hefty.
      (send edt insert edtsnp
            (send edt last-position))
      (send subtxt set-style-list (send edt get-style-list))
      (send edtsnp set-align-top-line #t)
      subtxt
      )
    (define (wikt-add-line txt edt)
      (if (> (string-length txt) 0)
          (case (string-ref txt 0)
            #|
            || Numbered and bullet lists, and indentation
            |#
            [[#\: #\# #\*]
             (define tt
               (regexp-match #rx"^([#*:]+) *([^ ].*)$" txt))
             (when tt
               (define listspec (string->list (cadr tt)))
               (define listtext (caddr tt))
               (define listspeclen (length listspec))
               (define addnewln #f)
               
               (define txtedt
                 (let loop ([listspec listspec]
                            [last-listspec (append last-listspec (build-list 30 (lambda _ #f)))]
                            [i 0]
                            [edt edt]
                            [addnl (lambda _ (set! addnewln #t))])
                   (if (and (pair? listspec) (pair? last-listspec))
                       (let ([lastone (not (pair? (cdr listspec)))]
                             [a (car listspec)]
                             [x (car last-listspec)])
                         (if (and (not lastone) (eq? a x))
                             (loop (cdr listspec) (cdr last-listspec) (add1 i)
                                   (vector-ref last-listeditors i)
                                   (lambda _ (wikt-add-text (format "~n") (vector-ref last-listeditors i))))
                             (let ()
                               (addnl)
                               (for ([j (in-range (add1 i) 30)])
                                 (vector-set! last-listincrements j 0)
                                 )
                               (case a
                                 [[#\#] 
                                  (define num (add1 (vector-ref last-listincrements i)))
                                  (vector-set! last-listincrements i num)
                                  (wikt-add-text (format " ~a. " num) edt)]
                                 [[#\*] (wikt-add-text "  • " edt)]
                                 [[#\:] (wikt-add-text "    " edt)]
                                 )
                               
                               (let ([newedt (make-subtext edt)])
                                 (vector-set! last-listeditors i newedt)
                                 (loop (cdr listspec) (cdr last-listspec) (add1 i) newedt
                                       (lambda _ (wikt-add-text (format "~n") newedt))) ))))
                       edt )))
               (set! last-listspec listspec)
               (wikt-add-text listtext txtedt)
               (when addnewln
                 (wikt-add-text (format "~n") edt) ))
             ]
            
            #|
            || Any other non-empty line of text
            |#
            [else 
             (set! last-listincrements (make-vector 30))
             (wikt-add-text (format "~a~n" txt) edt) ])
          
          #|
          || Empty line of text
          |#
          (let ()
            (set! last-listincrements (make-vector 30))
            (wikt-add-text (format "~n") edt) )))
    
    (define (wikt-add-heading txt sty edt)
      (let ([eb (send edt last-position)])
        (send edt insert (format "~a~n" txt) eb)
        (send edt change-style sty eb 'end #f) ))
    
    (send mytxt2 lock #f)
    (send mytxt2 select-all)
    (send mytxt2 clear)
    
    (wikt-add-heading current-wikt-title sty-title mytxt2)
      
      (let ([strp (open-input-string current-wikt-text)])
        (for ([ln (in-lines strp)])
          (match ln
            [[regexp #rx"^======(.+)======$" (list _ a)] (wikt-add-heading a sty-h6 mytxt2)]
            [[regexp #rx"^=====(.+)=====$" (list _ a)] (wikt-add-heading a sty-h5 mytxt2)]
            [[regexp #rx"^====(.+)====$" (list _ a)] (wikt-add-heading a sty-h4 mytxt2)]
            [[regexp #rx"^===(.+)===$" (list _ a)] (wikt-add-heading a sty-h3 mytxt2)]
            [[regexp #rx"^==(.+)==$" (list _ a)] (wikt-add-heading a sty-h2 mytxt2)]
            [_ (wikt-add-line ln mytxt2)]
            )
          ))
    
    (send mytxt2 lock #t)
    (send mytxt2 set-position 0 'same #f #t 'default)
    )
  
  (let ()
    (send mytxtconv2 set-editor mytxt2)
    (send mytxt2 auto-wrap #t)
    (send frame show #t)
    (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_TOPMOST 0 0 0 0 3)
    (enabledisable-actions)
    (generate-wiktionary-page) ))

; Temporary below
(load-wikt-data-files
 (resolve-data-file-path CONST_FILE_WIKTDATA)
 (resolve-data-file-path CONST_FILE_WIKTINDX)
 (resolve-data-file-path CONST_FILE_WIKTLKUP))
(open-wiktionary "高")