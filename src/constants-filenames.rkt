#lang racket/base

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
  (define ia (build-path (find-system-path 'init-file) a))
  (define ib (build-path (find-system-path 'init-file) b))
  (define ic (build-path (find-system-path 'init-file) c))
  (define id (build-path (find-system-path 'init-file) d))
  (define ra (build-path (find-system-path 'run-file) a))
  (define rb (build-path (find-system-path 'run-file) b))
  (define rc (build-path (find-system-path 'run-file) c))
  (define rd (build-path (find-system-path 'run-file) d))
  (define h (build-path (find-system-path 'home-dir) ".KanjiGetData" a))
  (cond
    [(file-exists? a) a ]
    [(file-exists? b) b ]
    [(file-exists? c) c ]
    [(file-exists? d) d ]
    [(file-exists? ia) ia ]
    [(file-exists? ib) ib ]
    [(file-exists? ic) ic ]
    [(file-exists? id) id ]
    [(file-exists? ra) ra ]
    [(file-exists? rb) rb ]
    [(file-exists? rc) rc ]
    [(file-exists? rd) rd ]
    [(file-exists? h) h ]
    [else a]
    )
  )
