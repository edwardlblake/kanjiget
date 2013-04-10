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

(require (only-in racket/class new make-object send)
         (only-in racket/gui/base dialog% vertical-panel% normal-control-font font% message% editor-canvas% text% style-delta% color% horizontal-panel% button%)
         "rkt-common.rkt"
         )

(provide show-about-dialog)

(define about-info-gpl-par1
  (string-append
   "KanjiGet is free software: you can redistribute it and/or modify "
   "it under the terms of the GNU General Public License as published by "
   "the Free Software Foundation, either version 3 of the License, or "
   "(at your option) any later version." ))

(define about-info-gpl-par2
  (string-append
   "KanjiGet is distributed in the hope that it will be useful, "
   "but WITHOUT ANY WARRANTY; without even the implied warranty of "
   "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the "
   "GNU General Public License for more details." ))

(define about-info-edict-par1
  (string-append
   "This package uses the EDICT and KANJIDIC dictionary files. These "
   "files are the property of the Electronic Dictionary Research and "
   "Development Group, and are used in conformance with the Group's licence. "
   "For more information see http://www.edrdg.org/edrdg/licence.html" ))

(define about-info-wikt-par1
  (string-append
   "This package makes use of the Wiktionary project dictionary data. "
   "This data is made available under a Creative Commons Attribution-"
   "ShareAlike 3.0 Unported License. For more information see "
   "http://en.wiktionary.org/wiki/Wiktionary:Copyrights" ))

(define (show-about-dialog parentwin)
  (let ([frame (new dialog%
                    [parent parentwin]
                    [label STR_WIN_ABOUTDIALOG]
                    [width 500]
                    [height 500])])
    
  (define frameparts
    (new vertical-panel%
         [parent frame]
         [enabled #t]
         [vert-margin 10]
         [horiz-margin 10]
         [alignment '(left top)]
         )
    )
    
    (define bold-control-font
      (let ([ctlfont normal-control-font])
        (make-object font%
          (send ctlfont get-point-size)
          (send ctlfont get-face)
          (send ctlfont get-family)	 	 	 	 
          'normal
          'bold
          #f
          (send ctlfont get-smoothing)
          (send ctlfont get-size-in-pixels)
          (send ctlfont get-hinting))
        ))

  (define aboutline1
    (new message%
         [parent frameparts]
         [label (format "Kanjiget ~a" CONST_KANJIGET_VERSION)]
         [font bold-control-font]
         [vert-margin 6]
         [horiz-margin 5]
         [min-width 400]))
  (define aboutline2
    (new message%
         [parent frameparts]
         [label "Copyright 2011-2012 Edward L. Blake."]
         [font normal-control-font]
         [vert-margin 3]
         [horiz-margin 5]
         [min-width 400]))

  (define aboutline3
    (new message%
         [parent frameparts]
         [label (format "This program makes use of the EDICT and KANJIDIC files")]
         [font normal-control-font]
         [vert-margin 3]
         [horiz-margin 5]
         [min-width 400]))

  (define aboutline4
    (new message%
         [parent frameparts]
         [label (format "This program makes use of the Wiktionary project dictionary data")]
         [font normal-control-font]
         [vert-margin 3]
         [horiz-margin 5]
         [min-width 400]))

  (define editor^details-pane
    (new editor-canvas%
         [parent frameparts]))
  (define text^details-pane
    (new text% [auto-wrap #t]))
  

    (define stl     (send text^details-pane get-style-list))
    (define sty-nrm (send stl basic-style))
    (define sty-header1 (send stl find-or-create-style sty-nrm
                    (let ([y (make-object style-delta%)])
                      (send y set-delta-foreground (make-object color% 20 90 50))
                      (send y set-delta-background (make-object color% 240 255 250))
                      (send y set-weight-on 'bold)
                      y
                      )))
  
  (define buttonbar
    (new horizontal-panel%
         [parent frameparts]
         [style '()]
         [enabled #t]
         [vert-margin 2]
         [horiz-margin 2]
         [border 0]
         [spacing 0]
         [alignment '(right center)]
         [stretchable-width  #t]
         [stretchable-height #f]))
  
  (define btnokay
    (new button%
         [parent buttonbar]
         [label "OK"]
         (callback
          (lambda (button event)
            (send frame show #f)
            ))
         [enabled #t]))
  
  (define (display-about-details)
    (define (insert-h1 txt)
      (let* ([m txt]
             [e text^details-pane]
             [eb (send e last-position)])
        (send e insert txt eb)
        (send e change-style sty-header1 eb 'end #f)
        (send e insert "\n")
        )
      )
    (define (insert-txt txt)
      (let* ([m txt]
             [e text^details-pane]
             [eb (send e last-position)])
        (send e insert txt eb)
        (send e change-style sty-nrm eb 'end #f)
        (send e insert "\n")
        )
      )
    (send text^details-pane lock #f)
    (send text^details-pane select-all)
    (send text^details-pane clear)
    (send text^details-pane set-style-list stl)
    
    (insert-h1 "Kanjiget License Information")
    (insert-txt about-info-gpl-par1)
    (insert-txt "")
    (insert-txt about-info-gpl-par2)
    (insert-txt "")
    (insert-h1 "EDICT and KANJIDIC")
    (insert-txt about-info-edict-par1)
    (insert-txt "")
    (insert-h1 "Wiktionary")
    (insert-txt about-info-wikt-par1)
    (insert-txt "")
    
    (send text^details-pane lock #t)
    (send text^details-pane set-position 0 'same #f #t 'default)
    )
  
    (send editor^details-pane
          set-editor text^details-pane)
    (display-about-details)
    
    (send frame show #t) )
  )

(module+ main
  (show-about-dialog #f))