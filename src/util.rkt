#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts two symbols to strings in order to append them, and then
;; converts the resulting string back into a symbol before returning it.
(define (symbol-append a b)
  (string->symbol
   (string-append (symbol->string a)
                  (symbol->string b))))

;; Fills in the false values in the maybes with the values from vals
;; in the order that they appear in vals.
(define (fill-in-falses maybes vals)
  (match maybes
    [(list) empty]
    [(cons #f ms)
     (cons (first vals) (fill-in-falses ms (rest vals)))]
    [(cons m ms)
     (cons m (fill-in-falses ms vals))]))

;; Maps fill-in-falses over a set of values.
(define (fill-in-falses* maybes val-sets)
  (map (lambda (vals) (fill-in-falses maybes vals)) val-sets))