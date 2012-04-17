#lang racket

(require rackunit
         rackunit/text-ui
         "../util.rkt")

(provide util-tests)

(define symbol-append-tests
  (test-suite "symbol-append tests"
    (test-case "empty first"
      (check-equal? (symbol-append '|| 'foo)
                    'foo))
    (test-case "empty second"
      (check-equal? (symbol-append 'foo '||)
                    'foo))
    (test-case "both empty"
      (check-equal? (symbol-append '|| '||)
                    '||))
    (test-case "both symbols"
      (check-equal? (symbol-append 'foo 'bar)
                    'foobar))))

(define fill-in-falses-tests
  (test-suite "fill-in-falses tests"
    (test-case "empty list"
      (check-equal? (fill-in-falses empty '(a b c))
                    empty)
      (check-equal? (fill-in-falses empty empty)
                    empty))
    (test-case "no falses"
      (check-equal? (fill-in-falses '(1 2 3) '(a b c))
                    '(1 2 3))
      (check-equal? (fill-in-falses '(1 2 3) empty)
                    '(1 2 3)))
    (test-case "all falses"
      (check-equal? (fill-in-falses '(#f) '(a b c))
                    '(a))
      (check-equal? (fill-in-falses '(#f #f #f) '(a b c))
                    '(a b c)))
    (test-case "general"
      (check-equal? (fill-in-falses '(1 #f 3) '(a b c))
                    '(1 a 3))
      (check-equal? (fill-in-falses '(#f 2 #f) '(a b c))
                    '(a 2 b)))
    (test-case "errors"
      (check-exn exn:fail? (lambda () (fill-in-falses '(#f) '())))
      (check-exn exn:fail? (lambda () (fill-in-falses '(#f #f) '(a)))))))

(define fill-in-falses*-tests
  (test-suite "fill-in-falses tests"
    (test-case "no values"
      (check-equal? (fill-in-falses* empty empty)
                    empty)
      (check-equal? (fill-in-falses* '(1 #f 3) empty)
                    empty)
      (check-equal? (fill-in-falses* '(1 2 3) empty)
                    empty)
      (check-equal? (fill-in-falses* '(1 2 3) (list empty))
                    (list '(1 2 3)))
      (check-equal? (fill-in-falses* '(1 2 3) (list empty empty))
                    (list '(1 2 3)
                          '(1 2 3))))
    (test-case "no falses"
      (check-equal? (fill-in-falses* '(1 2 3) (list empty))
                    '((1 2 3)))
      (check-equal? (fill-in-falses* '(1 2 3) '(() ()))
                    '((1 2 3) (1 2 3)))
      (check-equal? (fill-in-falses* '(1 2 3) '((a b c) (d e f)))
                    '((1 2 3) (1 2 3))))
    (test-case "all falses"
      (check-equal? (fill-in-falses* '(#f) '((a) (b) (c)))
                    '((a) (b) (c)))
      (check-equal? (fill-in-falses* '(#f #f) '((a b) (c d e) (f g)))
                    '((a b) (c d) (f g))))
    (test-case "general"
      (check-equal? (fill-in-falses* '(#f 2 #f) '((a b) (c d e) (f g)))
                    '((a 2 b) (c 2 d) (f 2 g))))))
      

(define util-tests
  (test-suite "util-tests"
    symbol-append-tests
    fill-in-falses-tests
    fill-in-falses*-tests))