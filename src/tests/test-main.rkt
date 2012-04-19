#lang racket

(require rackunit
         rackunit/text-ui
         "util-tests.rkt"
         "constraints-tests.rkt"
         "compile-vocab-tests.rkt")

(define all-tests
  (test-suite "miniRBAC"
    util-tests
    constraints-tests
    compile-vocab-tests))

(run-tests all-tests)