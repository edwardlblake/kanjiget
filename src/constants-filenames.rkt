#lang racket/base
(require racket/gui/base)

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

(provide CONST_FILE_KANJIIDX0
         CONST_FILE_KANJIMTX
         CONST_FILE_KANJIRDC0
         CONST_FILE_WIKTDATA
         CONST_FILE_WIKTINDX
         CONST_FILE_WIKTLKUP
         resolve-data-file-path)

(define CONST_FILE_KANJIIDX0 "knjidxl0.dat")
(define CONST_FILE_KANJIMTX  "kanjimtx.dat")
(define CONST_FILE_KANJIRDC0 "knjirdc0.dat")

(define CONST_FILE_WIKTDATA  "wiktdata.dat")
(define CONST_FILE_WIKTINDX  "wiktindx.dat")
(define CONST_FILE_WIKTLKUP  "wiktlkup.dat")

(define (resolve-data-file-path a)
  (define b (build-path ".." a))
  (define c (build-path ".." "data" a))
  (define d (build-path "data" a))
  (define init-dir
    (find-system-path 'init-dir))
  (define exec-dir
    (let-values ([(a b c) (split-path (find-system-path 'exec-file))]) a))
  (define run-dir 
    (let-values ([(a b c) (split-path (find-system-path 'run-file))]) a))
  (define orig-dir
    (find-system-path 'orig-dir))
  (define ia (build-path init-dir a))
  (define ib (build-path init-dir b))
  (define ic (build-path init-dir c))
  (define id (build-path init-dir d))
  (define ea (build-path exec-dir a))
  (define eb (build-path exec-dir b))
  (define ec (build-path exec-dir c))
  (define ed (build-path exec-dir d))
  (define ra (build-path run-dir a))
  (define rb (build-path run-dir b))
  (define rc (build-path run-dir c))
  (define rd (build-path run-dir d))
  (define oa (build-path orig-dir a))
  (define ob (build-path orig-dir b))
  (define oc (build-path orig-dir c))
  (define od (build-path orig-dir d))
  (define h (build-path (find-system-path 'home-dir) ".KanjiGetData" a))
  
  ; For debugging on Mac
  ;(message-box "Init" (format "~s~n~s~n~s~n~s" ia ib ic id))
  ;(message-box "Exec" (format "~s~n~s~n~s~n~s" ea eb ec ed))
  ;(message-box "RunT" (format "~s~n~s~n~s~n~s" ra rb rc rd))
  ;(message-box "Orig" (format "~s~n~s~n~s~n~s" oa ob oc od))
  (cond
    [(file-exists? a) a ]
    [(file-exists? b) b ]
    [(file-exists? c) c ]
    [(file-exists? d) d ]
    [(file-exists? ia) ia ]
    [(file-exists? ib) ib ]
    [(file-exists? ic) ic ]
    [(file-exists? id) id ]
    [(file-exists? ea) ea ]
    [(file-exists? eb) eb ]
    [(file-exists? ec) ec ]
    [(file-exists? ed) ed ]
    [(file-exists? ra) ra ]
    [(file-exists? rb) rb ]
    [(file-exists? rc) rc ]
    [(file-exists? rd) rd ]
    [(file-exists? oa) oa ]
    [(file-exists? ob) ob ]
    [(file-exists? oc) oc ]
    [(file-exists? od) od ]
    [(file-exists? h) h ]
    [else a]
    )
  )
