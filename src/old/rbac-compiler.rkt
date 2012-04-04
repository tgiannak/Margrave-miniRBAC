#lang racket
(require "rbac-ast.rkt")
(require "rbac-parser.rkt")
(require margrave)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling the vocab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper for writing down predicate declarations
(define-syntax pred-decls
  (syntax-rules (:)
    [(_ (pred : arity ...) ...)
     (make-hash (list (cons pred (m-predicate pred (list arity ...))) ...))]))

;; Helper for writing down the constants from a unary relation.
(define (unary-reln-consts type get vals)
  (map (lambda (v)
         (cons v (m-constant v type)))
       (get vals)))

;; Makes the types from an AC state.
(define rbac-types
  (hash "User" (m-type "User" empty)
        "Role" (m-type "Role" empty)
        "Perm" (m-type "Perm" empty)))

;; Makes the predicates from an AC state.
(define rbac-preds
  (pred-decls ("ua" : "User" "Role")
              ("ra" : "Role" "Perm")))

;; Makes the constants from an AC state.
(define (make-rbac-consts users roles perms)
  (make-hash (append (unary-reln-consts "User" users-info-names users)
                     (unary-reln-consts "Role" roles-info-names roles)
                     (unary-reln-consts "Perm" perms-info-names perms))))

;; Makes the functions from an AC state.
(define rbac-funcs
  (hash))

;; Compiles a vocab from an AC state.
(define (compile-vocab name state)
  (match state
    [(ac-state users roles perms _ _)
     (m-vocabulary name empty
                   rbac-types
                   rbac-preds
                   (make-rbac-consts users roles perms)
                   rbac-funcs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling the theory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fold-constraints compile-constraint vals)
  (foldl (lambda (v constraint)
           `(and ,(compile-constraint v) ,constraint))
         'true vals))

;; TODO
(define (compile-ua-user-constraint user)
  (match user
    [(ua-user-info name count roles omits)
     'true]))

;; TODO
(define (compile-ua-role-constraint role)
  (match role
    [(ua-role-info name count users omits)
     'true]))

(define (compile-ua-constraints ua)
  (match ua
    [(ua-info users roles)
     (let ([ua-users (fold-constraints compile-ua-user-constraint users)]
           [ua-roles (fold-constraints compile-ua-role-constraint roles)])
       `(and ,ua-users ,ua-roles))]))

;; TODO
(define (compile-ra-constraints ra)
  (match ra
    [(ra-info roles perms)
     'true]))

(define (compile-theory name state)
  (match state
    [(ac-state users roles perms ua ra)
     (let ([vocab (compile-vocab name state)]
           [ua-constraints (compile-ua-constraints ua)]
           [ra-constraints (compile-ra-constraints ra)])
       `(and ,ua-constraints ,ra-constraints))]))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling the policy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
(define (compile-policy name state)
  (match state
    [(ac-state users roles perms ua ra)
     (let ([theory (compile-theory name state)]
           [vardecs empty]
           [rules empty]
           [rcomb empty]
           [target empty]
           [idbs empty])
       (m-policy name "/" theory
                 vardecs
                 rules
                 rcomb
                 target
                 idbs))]))

;; examples
(define state1
  (rbac-state
   (Users 5 also theo salman kathi francis dan)
   (Roles prof teaching-assistant research-assistant)
   (Perms ? including grade enter-lab)
   (UA (salman has 3 roles including (research-assistant))
       (kathi has 3 roles including (professor) but not (student))
       (theo has 2 roles also (teaching-assistant))
       (tim has roles (alas-member research-assistant grad-student))
       (research-assistant has 5 users)
       (teaching-assistant has 3 users also (francis) but not (salman))
       (professor has ? users including (dan) but not (theo)))
   (RA (alas-member has 3 perms including (lab-entry))
       (lab-entry has 3 roles))))

;;; q1 gets string
;(define query1 (rbac-query query))
;(define result1 (rbac-exec-query state1 query1))
;(define result-pretty (rbac-show-result result1))
;(display result1)
