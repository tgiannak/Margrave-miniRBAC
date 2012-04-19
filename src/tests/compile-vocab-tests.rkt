#lang racket

(require rackunit
         rackunit/text-ui
         margrave
         "test-util.rkt"
         "../compile-vocab.rkt"
         "../constraints.rkt")

(provide compile-vocab-tests)

(define some-constraints
  (append (make-sort-constant-constraints 'Perm (list 'b1 'b2))
          (make-sort-constant-constraints 'Role (list 'a4))
          (make-sort-constant-constraints 'User empty)))

(define compile-vocab-tests
  (test-suite "compile-vocab tests"
    ;; only one function is complicated enough that it can fail on its own
    ;; it's not exported, so we test it via the top level
    (test-case "constraints->constants"
      (check-true
       (m-vocabulary-equal?
        (constraints->vocab "foo" some-constraints)
        (m-vocabulary
         "foo"
         (make-hash
          (list
           (cons "Role" (m-type "Role" empty))
           (cons "Perm" (m-type "Perm" empty))
           (cons "User" (m-type "User" empty))))
         (make-hash
          (list
           (cons "ua" (m-predicate "ua" (list "User" "Role")))
           (cons "ra" (m-predicate "ra" (list "Role" "Perm")))))
         (make-hash
          (list
           (cons "b1" (m-constant "b1" "Perm"))
           (cons "b2" (m-constant "b2" "Perm"))
           (cons "a4" (m-constant "a4" "Role"))))
         (make-hash)))))))

(run-tests compile-vocab-tests)