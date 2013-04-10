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
         "rkt-common.rkt"
         "kanjidb-gfx.rkt"
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
         make-data-files-from-kanjidic2
         make-data-file-from-radkfiles
         )

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
