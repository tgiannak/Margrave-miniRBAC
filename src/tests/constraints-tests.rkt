#lang racket

(require rackunit
         rackunit/text-ui
         "../constraints.rkt")

(provide constraints-tests)

;; the only thing in constraints.rkt that is not dead simple is
;; the make-pred-tuple-constraints, and that is because it depends
;; on fill-in-falses*.
(define constraints-tests
  (test-suite "constraints tests"
    (test-case "make-pred-tuple-constraints"
      (check-equal? (make-pred-tuple-constraints 'foo '(a #f b #f)
                                                 '((d e) (d f) (g h))
                                                 '((h g)))
                    (list
                     (pred-contains-constraint 'foo '(a d b e))
                     (pred-contains-constraint 'foo '(a d b f))
                     (pred-contains-constraint 'foo '(a g b h))
                     (pred-omits-constraint 'foo '(a h b g))))
      (check-equal? (make-pred-tuple-constraints 'foo '(a #f b #f)
                                                 '()
                                                 '((h g)))
                    (list
                     (pred-omits-constraint 'foo '(a h b g))))
      (check-equal? (make-pred-tuple-constraints 'foo '(a #f b #f)
                                                 '((d e) (d f) (g h))
                                                 '())
                    (list
                     (pred-contains-constraint 'foo '(a d b e))
                     (pred-contains-constraint 'foo '(a d b f))
                     (pred-contains-constraint 'foo '(a g b h))))
      (check-equal? (make-pred-tuple-constraints 'foo '(a #f b #f)
                                                 '()
                                                 '())
                    empty))))

(run-tests constraints-tests)