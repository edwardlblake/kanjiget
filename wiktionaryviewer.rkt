#lang racket/base

(require racket/class
         racket/gui/base
         racket/list
         racket/string
         racket/dict
         racket/block
         ffi/unsafe
         "kanjidb.rkt"
         "wiktionarydb.rkt"
         "constants-filenames.rkt")

(define (open-wiktionary wikttitle)
  (define WINAPI_SetWindowPos
    (case (system-type)
      [[windows]
       (get-ffi-obj "SetWindowPos" "user32" (_fun _pointer _int _int _int _int _int _int -> _int)
                    (lambda () (void)))]
      [else void]))
  (define WINAPI_HWND_TOPMOST  -1)
  (define WINAPI_HWND_NOTOPMOST -2)
  
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
  (define stl2 (send mytxt2 get-style-list))
  (define sty2-normal (send stl2 basic-style))
  (define sty-normal (send stl basic-style))
  (define sty-title (send stl find-or-create-style sty-normal
                          (let ([y (make-object style-delta%)])
                            (send y set-size-mult 3.0)
                            (send y set-alignment-off 'center)
                            (send y set-alignment-on 'center)
                            (send y set-delta-foreground (make-object color% 30 40 30))
                            y
                            )))
  
  (define current-wikt-title wikttitle)
  
  (define current-wikt-text
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
  
  (define (generate-wiktionary-page)
    (define (wikt-add-line txt)
      (let ([eb (send mytxt2 last-position)])
        (send mytxt2 insert (format "~a~n" txt) eb)
        (send mytxt2 change-style sty2-normal eb 'end #f)))
    
    (define (wikt-add-title txt)
      (let ([eb (send mytxt2 last-position)])
        (send mytxt2 insert (format "~a~n" txt) eb)
        (send mytxt2 change-style sty-title eb 'end #f)))
    
    (send mytxt2 lock #f)
    (send mytxt2 select-all)
    (send mytxt2 clear)
    
    (wikt-add-title current-wikt-title)
    (wikt-add-line current-wikt-text)
    
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