#lang racket

(require margrave
         "compile-theory.rkt"
         "constraints.rkt")

(provide constraints->m-policy)

(define (constraints->m-policy name constrs)
  (let ([theory (constraints->m-theory name constrs)]
        [vardecs (hash 'u (m-vardec 'u 'User)
                       'p (m-vardec 'p 'Permissions))]
        [rule-names '("rule1")]
        [rules
         (hash
          "rule1"
          (m-rule "rule1" "permit" (list 'u 'p)
                  (list '(exists r Role (and (ua u r) (ra r p))))))]
        [rcomb ""]
        [target 'true]
        [idbs
         (hash "permit" (list 'User 'Permission))])
    (m-policy name
              theory
              vardecs
              rule-names
              rules
              rcomb
              target
              idbs)))

(define my-policy
  (constraints->m-policy "my-policy"
   (append (make-sort-constraints 'Role 10 '(student prof facutly grad ta ra))
           (make-sort-constraints 'Subject 8 '(sarah theo francis salman tim))
           (make-sort-constraints 'Permission 4 '(grade submit-hw))
           (make-pred-constraints 'ua '(#f ta) 2 '((sarah)) '((salman) (tim))))))
