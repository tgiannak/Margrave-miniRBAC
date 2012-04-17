#lang racket

(require rackunit
         rackunit/text-ui
         "util-tests.rkt"
         "constraints-tests.rkt")

(define all-tests
  (test-suite "miniRBAC"
    util-tests
    constraints-tests))

(run-tests all-tests)