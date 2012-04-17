#lang racket

(require margrave
         "compile-theory.rkt"
         "constraints.rkt")

(provide constraints->m-policy)

;; Converts a list of constraints to a m-policy, including the
;; theory/vocab. The constraints primarily appear as formulas in the
;; theory--much of the vocab and the whole policy is fixed.
;; Queries can be written over the constants defined by the
;; constraints, the ua : User -> Role relation and the
;; ra : Role -> Perm relation.
(define (constraints->m-policy name constrs)
  (m-policy
   ; policy id
   name
   ; theory for the poilcy
   (constraints->m-theory name constrs)
   ; variable declarations
   (hash 'u (m-vardec 'u 'User)
         'p (m-vardec 'p 'Perm))
   ; list of rule names
   '("rule1")
   ; rules
   (hash
    "rule1"
    (m-rule "rule1" "permit" (list 'u 'p)
            '(exists r Role (and (ua u r) (ra r p)))))
   ; conflict resolution declaration
   ""
   ; target
   'true
   ; rule types
   (hash "permit" (list 'User 'Perm))))


;; for testing
(define my-policy
  (constraints->m-policy "my-policy"
   (append (make-sort-constraints 'Role 10 '(student prof facutly grad ta ra))
           (make-sort-constraints 'Subject 8 '(sarah theo francis salman tim))
           (make-sort-constraints 'Permission 4 '(grade submit-hw))
           (make-pred-constraints 'ua '(#f ta) 2 '((sarah)) '((salman) (tim))))))
