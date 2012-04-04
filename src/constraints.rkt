#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraints for sorts and constants.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A constraint indicating that a sort should have exactly the given size.
(define-struct/contract sort-size-constraint
  ([sort symbol?]
   [count exact-nonnegative-integer?])
  #:transparent)

;; A constraint indicating that a sort should contain the given constant.
(define-struct/contract sort-constant-constraint
  ([sort symbol?]
   [const symbol?])
  #:transparent)

;;;;
;; Helpers for sort and constant constraints.
;;;;

;; A helper for creating a list of constant constraints for when there are
;; many constants in a sort.
(define (make-sort-constant-constraints sort consts)
  (map (lambda (c) (sort-constant-constraint sort c)) consts))

;; A helper for creating a list of constant constraints and a size constraint
;; for when both are specified.
(define (make-sort-constraints sort count consts)
  (cons (sort-size-constraint sort count)
        (make-sort-constant-constraints sort consts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraints for predicates.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A constraint indicating that a predicate should have the given size.
(define-struct/contract pred-size-constraint
  ([pred symbol?]
   [fixed (listof (or/c #f symbol?))]
   [count exact-nonnegative-integer?])
  #:transparent)

;; A constraint indicating that a predicate should contain the given tuple.
(define-struct/contract pred-contains-constraint
  ([pred symbol?]
   [tuple (listof symbol?)])
  #:transparent)

;; A constraint indicating that a predicate should omit the given tuple.
(define-struct/contract pred-omits-constraint
  ([pred symbol?]
   [tuple (listof symbol?)])
  #:transparent)

;;;;
;; Helpers for predicate constraints.
;;;;

;; A helper for creating a list of constraints when there are multiple tuples
;; that a predicate should contain.
(define (make-pred-contains-constraints pred tuples)
  (map (lambda (t) (pred-contains-constraint pred t)) tuples))

;; A helper for creating a list of constraints when there are multiple tuples
;; that a predicate should omit.
(define (make-pred-omits-constraints pred tuples)
  (map (lambda (t) (pred-omits-constraint pred t)) tuples))

;; A helper for creating a list of constraints for both contained and omitted
;; tuples.
(define (make-pred-tuple-constraints pred contains omits)
  (append (make-pred-contains-constraints pred contains)
          (make-pred-omits-constraints pred omits)))

;; A helper for creating a list of constraints for both contained and omitted
;; tuples and the predicate size.
(define (make-pred-constraints pred fixed count contains omits)
  (cons (pred-size-constraint pred fixed count)
        (make-pred-tuple-constraints pred contains omits)))