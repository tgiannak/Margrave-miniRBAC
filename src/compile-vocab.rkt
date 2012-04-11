#lang racket

(provide constraints->vocab)

(require margrave
         "constraints.rkt")

;; Helper for writing down predicate declarations
(define-syntax pred-decls
  (syntax-rules (:)
    [(_ (pred : arity ...) ...)
     (make-hash (list (cons pred (m-predicate pred (list arity ...))) ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixed parts of the miniRBAC policies.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Makes the types for a miniRBAC vocabulary.
(define rbac-types
  (hash "User" (m-type "User" empty)
        "Role" (m-type "Role" empty)
        "Perm" (m-type "Perm" empty)))

;; Makes the predicates for a miniRBAC vocabulary.
(define rbac-preds
  (pred-decls ("ua" : "User" "Role")
              ("ra" : "Role" "Perm")))

;; Makes the functions for a miniRBAC vocabulary.
(define rbac-funcs
  (hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parts of the miniRBAC vocabulary based on the contraints.
;; At the moment, this is just the set of constants.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts a list of constraints into a hash of constant declarations
;; suitable for use in an m-vocabulary.
(define (constraints->constants constrs)
  ;; Helper for adding a constant to a vocabulary.
  (define (add-constant sort const constants)
    (let ([sort (symbol->string sort)]
          [const (symbol->string const)])
      (hash-set constants const (m-constant const sort))))
  ;; Primary part of the function
  (foldl (lambda (constr constants)
           (match constr
             [(sort-constant-constraint sort const)
              (add-constant sort const constants)]
             [_ constants]))
         (hash)
         constrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile all of the constraints into a miniRBAC vocabulary.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compile constraints into a miniRBAC vocabulary.
;; The name argument is the name of the vocab.
(define (constraints->vocab name constrs)
  (m-vocabulary name
                rbac-types
                rbac-preds
                (constraints->constants constrs)
                rbac-funcs))


;; For testing and debugging...
(define (test)
  (define constrs
    (append (make-sort-constraints 'A 10 (list 'a1 'a2 'a3))
            (make-sort-constant-constraints 'B (list 'b1 'b2))
            (make-sort-constant-constraints 'A (list 'a4))))
  (constraints->vocab "foo" constrs))
