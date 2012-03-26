#lang racket
(require 
 (for-meta 2 racket/base)
 (for-meta 3 racket/base)
 (for-meta 4 racket/base))

;; AST
;; the raw AC state that needs to be converted to a vocab and query-part
(define-struct raw-ac-state (users roles perms ua ra) #:transparent)
(define-struct users-info (count names) #:transparent)
(define-struct roles-info (count names) #:transparent)
(define-struct perms-info (count names) #:transparent)
(define-struct ua-info (users roles) #:transparent)
(define-struct ua-user-info (name count roles omits) #:transparent)
(define-struct ua-role-info (name count users omits) #:transparent)
(define-struct ra-info (roles perms) #:transparent)
(define-struct ra-role-info (name count perms omits) #:transparent)
(define-struct ra-perm-info (name count roles omits) #:transparent)

;; intermediate form
(define-struct ac-state (vocab query-part))

;; Generic parser for defining unary relations
(begin-for-syntax
  (define-syntax unary-relation-parser
    (syntax-rules ()
      [(_ stx name make)
       (syntax-case stx (name also including ?)
         [(name n)
          #'(make n empty)]
         [(name ? including item (... ...))
          #'(make #f (list 'item (... ...)))]
         [(name n also item (... ...))
          #'(let ([items (list 'item (... ...))])
              (make (+ n (length items)) items))]
         [(name n including item (... ...))
          #'(make n (list 'item (... ...)))]
         [(name item (... ...))
          #'(let ([items (list 'item (... ...))])
              (make (length items) items))])])))


;; Parser for the Users clause.
(define-for-syntax (parse-users stx)
  (unary-relation-parser stx Users make-users-info))


;; Parser for the Roles clause.
(define-for-syntax (parse-roles stx)
  (unary-relation-parser stx Roles make-roles-info))

;; Parser for the Perms clause.
(define-for-syntax (parse-perms stx)
  (unary-relation-parser stx Perms make-perms-info))


(begin-for-syntax
  (define-for-syntax (binary-half kind make)
    (list #`[(name has ? #,kind including (item (... ...)) but not (omit (... ...)))
             #'(#,make 'name #f (list 'item (... ...)) (list 'omit (... ...)))]
          #`[(name has ? #,kind including (item (... ...)))
             #'(#,make 'name #f (list 'item (... ...)) empty)]
          #`[(name has n #,kind including (item (... ...)) but not (omit (... ...)))
             #'(#,make 'name n (list 'item (... ...)) (list 'omit (... ...)))]
          #`[(name has n #,kind including (item (... ...)))
             #'(#,make 'name n (list 'item (... ...)) empty)]
          #`[(name has n #,kind also (item (... ...)) but not (omit (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (+ n (length items)) items (list 'omit (... ...))))]
          #`[(name has n #,kind also (item (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (+ n (length items)) items empty))]
          #`[(name has n #,kind but not (omit (... ...)))
             #'(#,make 'name n empty (list 'omit (... ...)))]
          #`[(name has n #,kind)
             #'(#,make 'name n empty empty)]
          #`[(name has #,kind (item (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (length items) items empty))]
          #`[(name has #,kind (item (... ...)) but not (omit (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (length items) items (list 'omit (... ...))))]
          #`[(name has #,kind but not (omit (... ...)))
             #'(#,make 'name #f empty (list 'omit (... ...)))]))

    (define-syntax (handle-binrel astx)
      (syntax-case astx ()
        [(_ kind-lhs make-lhs kind-rhs make-rhs)
         (let ([handle-lhs (binary-half #'kind-lhs #'make-lhs)]
               [handle-rhs (binary-half #'kind-rhs #'make-rhs)])
           #`(lambda (stx)
               (syntax-case stx (has kind-lhs kind-rhs including also but not ?)
                 #,@handle-lhs
                 #,@handle-rhs)))]))


  (define-syntax (parse-binary-relation astx)
    (syntax-case astx ()
      [(_ stx name make pred-lhs? kind-lhs make-lhs kind-rhs make-rhs)
       #'(let ([parse-inner (handle-binrel kind-lhs make-lhs kind-rhs make-rhs)])
           (syntax-case stx ()
             [(name item (... ...))
              (let ([items (map parse-inner (syntax->list #'(item (... ...))))])
                #`(let-values ([(lhs rhs)
                                (partition pred-lhs? (list #,@items))])
                    (make lhs rhs)))]))])))

(define-for-syntax (parse-ua-info stx)
  (parse-binary-relation stx UA make-ua-info ua-user-info?
                         roles make-ua-user-info
                         users make-ua-role-info))
(define-for-syntax (parse-ra-info stx)
  (parse-binary-relation stx RA make-ra-info ra-role-info?
                         perms make-ra-role-info
                         roles make-ra-perm-info))




;; parser macros
(define-syntax (rbac-state stx)
  (syntax-case stx ()
    [(_ users roles perms uas ras)
     (let ([users (parse-users #'users)]
           [roles (parse-roles #'roles)]
           [perms (parse-perms #'perms)]
           [ua (parse-ua-info #'uas)]
           [ra (parse-ra-info #'ras)])
       #`(make-raw-ac-state #,users #,roles #,perms #,ua #,ra))]))


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