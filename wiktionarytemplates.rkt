#lang racket/base

(provide get-wiktionary-templates)

(define (get-wiktionary-templates)
  (define templates (make-hash))
  (hash-set! templates "sense" 
             (lambda (args)
               (format "(''~a''):" (car args))
               ))
  templates
  )