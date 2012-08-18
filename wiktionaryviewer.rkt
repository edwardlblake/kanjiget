#lang racket/base

(require racket/class
         racket/gui/base
         racket/match
         racket/string
         racket/dict
         racket/block
         "wiktionarydb.rkt"
         "stayontop.rkt")

(provide open-wiktionary)

(define (open-wiktionary wikttitle)
  (define STR_WIN_WIKTIONARYVIEWER "Wiktionary Viewer")
  
  (define stn^ontop    #t)
  
  (define cursel-kanji #f)
  
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
         [alignment '(right center)]
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
  
  (define current-wikt-title wikttitle)
  
  (define current-wikt-text0
    "==Japanese==
{{ja-kanjitab|高}}

===Adjective===
{{ja-adj|kk|decl=i|hira=たかい|rom=takai}}

# [[high]], [[tall]]
# [[expensive]]

====Antonyms====
* {{sense|high}} [[低い]] ([[ひくい]], [[hikui]])
* {{sense|expensive}} [[安い]] ([[やすい]], [[yasui]])

====Declension====
{{ja-i|高|たか|taka}}

====Derived terms====
* [[悪名高い]]
* [[価値高い]]
* [[勘定高い]]
* [[甲高い]]
* [[計算高い]]
* [[気高い]]
* [[小高い]]
* [[空高い]]
* [[算盤高い]]
* [[名高い]]
* [[誇り高い]]
* [[物見高い]]



")
  
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
  
  (define current-wikt-text
    (let ()
      (define templates (make-hash))
      (hash-set! templates "sense" 
                 (lambda (args)
                   (format "(''~a''):" (car args))
                   ))
      (curlyxpansion current-wikt-text0 templates)))
  
  
  
  
  (define (parse-wikt-links txt)
    (define lst (regexp-match-positions* #rx"\\[\\[([^]]+)\\]\\]" txt))
    (values lst
            (for/list ([posn lst]) (list (substring txt (+ (car posn) 2) (- (cdr posn) 2))))
            (for/list ([_ lst]) 'a)))
  
  (define (parse-wikt-bolditalic txt)
    (define lst (regexp-match-positions* #rx"'''''(.+)'''''|'''(.+)'''|''(.+)''" txt))
    (values lst
            (for/list ([posn lst]) (list (substring txt (+ (car posn) 2) (- (cdr posn) 2))))
            (for/list ([_ lst]) 'i)))
  
  
  (define (parse-to-markup-tree txt parse-info)
    (if (or (pair? txt) (symbol? txt))
        (if (pair? txt)
            (for/fold ([alst '()]) ([a txt])
              (define b (parse-to-markup-tree a parse-info))
              (if (and (pair? b) (not (symbol? (car b))))
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
    (parse-to-markup-tree 
     (parse-to-markup-tree 
      txt
      
      parse-wikt-bolditalic )
     parse-wikt-links) )
  (define (apply-markup-tree eb te)
    (if (pair? te)
        (case (car te)
          [[a] (apply-markup-tree eb (cdr te))
               (send mytxt2 set-clickback eb (send mytxt2 last-position)
                     (lambda (e s b) (display "Testing" ))
                     (send (make-object style-delta%) set-delta-foreground (make-object color% 40 255 30)))
               (send mytxt2 change-style sty-link eb 'end #f)]
          [[b] (apply-markup-tree eb (cdr te)) (send mytxt2 change-style sty-link eb 'end #f)]
          [[i] (apply-markup-tree eb (cdr te)) (send mytxt2 change-style sty-link eb 'end #f)]
          [else (for ([x te])
                  (apply-markup-tree (send mytxt2 last-position) x))])
        (let ()
          (send mytxt2 insert te eb)
          (send mytxt2 change-style sty-normal eb 'end #f)
          )))
    
  (define (generate-wiktionary-page)
    (define (wikt-add-line txt)
      (let ([eb (send mytxt2 last-position)]
            [te (wiki-markup->tree txt)])
        (apply-markup-tree eb te)
        (send mytxt2 insert (format "~n") (send mytxt2 last-position))
        ))
    
    (define (wikt-add-heading txt sty)
      (let ([eb (send mytxt2 last-position)])
        (send mytxt2 insert (format "~a~n" txt) eb)
        (send mytxt2 change-style sty eb 'end #f) ))
    
    (send mytxt2 lock #f)
    (send mytxt2 select-all)
    (send mytxt2 clear)
    
    (wikt-add-heading current-wikt-title sty-title)
      
      (let ([strp (open-input-string current-wikt-text)])
        (for ([ln (in-lines strp)])
          (match ln
            [[regexp #rx"^======(.+)======$" (list _ a)] (wikt-add-heading a sty-h6)]
            [[regexp #rx"^=====(.+)=====$" (list _ a)] (wikt-add-heading a sty-h5)]
            [[regexp #rx"^====(.+)====$" (list _ a)] (wikt-add-heading a sty-h4)]
            [[regexp #rx"^===(.+)===$" (list _ a)] (wikt-add-heading a sty-h3)]
            [[regexp #rx"^==(.+)==$" (list _ a)] (wikt-add-heading a sty-h2)]
            [_ (wikt-add-line ln)]
            )
          ))
    
    (send mytxt2 lock #t)
    (send mytxt2 set-position 0 'same #f #t 'default)
    )
  
  (let ()
    (send mytxtconv2 set-editor mytxt2)
    (send frame show #t)
    (WINAPI_SetWindowPos (send frame get-handle) WINAPI_HWND_TOPMOST 0 0 0 0 3)
    ;(thread
    ; (lambda ()
    ;   (load-wikt-data-files CONST_FILE_WIKTDATA CONST_FILE_WIKTINDX CONST_FILE_WIKTLKUP)))
    (enabledisable-actions)
    (generate-wiktionary-page)
    )
  )

(open-wiktionary "高")