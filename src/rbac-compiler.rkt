#lang racket
(require (for-meta 2 racket/base))

;; AST
;; The overall AC state.
;; Note that the contents of an AC state may be inconsistant. This is perfectly
;; fine: it just means that no scenarios will be found when a query is issued.
(define-struct ac-state (users roles perms ua ra) #:transparent)
;; Info about the users in an AC state. The count includes the named users.
(define-struct users-info (count names) #:transparent)
;; Info about the roles in an AC state. The count includes the named roles.
(define-struct roles-info (count names) #:transparent)
;; Info about the permissions in an AC state. The count includes the named
;; permissions.
(define-struct perms-info (count names) #:transparent)
;; The UA relation information.
(define-struct ua-info (users roles) #:transparent)
;; Info about the a particular user in the UA binary relation.
;; The count is the number of roles, or #f if unspecified. The roles are
;; required roles, and the omits are the roles that are not allowed for the
;; user.
(define-struct ua-user-info (name count roles omits) #:transparent)
;; Same for roles in the UA relation.
(define-struct ua-role-info (name count users omits) #:transparent)
;; The RA relation information.
(define-struct ra-info (roles perms) #:transparent)
;; Same for roles in the RA relation.
(define-struct ra-role-info (name count perms omits) #:transparent)
;; Same for perms in the RA relation.
(define-struct ra-perm-info (name count roles omits) #:transparent)


;; Macros and helpers for parsing the unary relations.
(begin-for-syntax
  ;; Helper macro for making a unary relation parser. The users, roles, etc.
  ;; parsers are defined by using this one.
  ;; Examples of the forms that are supported by the resulting macro are listed
  ;; along with the cases.
  (define-syntax unary-relation-parser
    (syntax-rules ()
      [(_ stx name make)
       (syntax-case stx (name also including ?)
         ;; (Users 5)
         [(name n)
          #'(make n empty)]
         ;; (Users ? including theo)
         [(name ? including item (... ...))
          #'(make #f (list 'item (... ...)))]
         ;; (Users 5 including theo salman)
         [(name n also item (... ...))
          #'(let ([items (list 'item (... ...))])
              (make (+ n (length items)) items))]
         ;; (Users 3 also theo salman)
         [(name n including item (... ...))
          #'(make n (list 'item (... ...)))]
         ;; (Users theo salman tim)
         ;; equivalent to (Users 0 also theo salman tim)
         [(name item (... ...))
          #'(let ([items (list 'item (... ...))])
              (make (length items) items))])]))

    ;; Parser for the Users clause.
    (define (parse-users stx)
     (unary-relation-parser stx Users make-users-info))

    ;; Parser for the Roles clause.
    (define (parse-roles stx)
     (unary-relation-parser stx Roles make-roles-info))

    ;; Parser for the Perms clause.
    (define (parse-perms stx)
     (unary-relation-parser stx Perms make-perms-info)))


;; Macros and helpers for parsing binary relations.
(begin-for-syntax
  ;; Helper for generating the macro for parsing a binary relation. This
  ;; macro generates half the cases necessary to parse a declaration in the
  ;; binary relation. In particular, if the relation is from users to roles, it
  ;; creates the cases for handling constraints on one of users or roles.
  ;; Examples of the particular syntaxes it supports are listed within the function.
  (define-for-syntax (binary-half kind make)
          ;; (tim has ? roles including (student research-assitant) but not (professor faculty))
    (list #`[(name has ? #,kind including (item (... ...)) but not (omit (... ...)))
             #'(#,make 'name #f (list 'item (... ...)) (list 'omit (... ...)))]
          ;; (tim has ? roles including (student research-assistant))
          #`[(name has ? #,kind but not (item (... ...)))
             #'(#,make 'name #f empty (list 'item (... ...)))]
          ;; (tim has ? roles but not (professor faculty))
          #`[(name has ? #,kind including (item (... ...)))
             #'(#,make 'name #f (list 'item (... ...)) empty)]
          ;; (tim has 5 roles including (student research-assistant) but not (professor faculty))
          #`[(name has n #,kind including (item (... ...)) but not (omit (... ...)))
             #'(#,make 'name n (list 'item (... ...)) (list 'omit (... ...)))]
          ;; (tim has 5 roles including (student research-assistant))
          #`[(name has n #,kind including (item (... ...)))
             #'(#,make 'name n (list 'item (... ...)) empty)]
          ;; (tim has 3 roles also (student research-assistant) but not (professor faculty)
          #`[(name has n #,kind also (item (... ...)) but not (omit (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (+ n (length items)) items (list 'omit (... ...))))]
          ;; (tim has 3 roles also (student research-assistant))
          #`[(name has n #,kind also (item (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (+ n (length items)) items empty))]
          ;; (tim has 5 roles but not (professor faculty))
          #`[(name has n #,kind but not (omit (... ...)))
             #'(#,make 'name n empty (list 'omit (... ...)))]
          ;; (tim has 5 roles)
          #`[(name has n #,kind)
             #'(#,make 'name n empty empty)]
          ;; (tim has roles (student research-assistant))
          ;; equiv. to (tim has 0 roles also (student research-assistant))
          #`[(name has #,kind (item (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (length items) items empty))]
          ;; (tim has roles (student research-assistant) but not (professor faculty))
          #`[(name has #,kind (item (... ...)) but not (omit (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (length items) items (list 'omit (... ...))))]
          ;; (tim has roles but not (professor faculty))
          #`[(name has #,kind but not (omit (... ...)))
             #'(#,make 'name #f empty (list 'omit (... ...)))]))

  ;; Helper macro for creating the binary relation parser. Creates the function
  ;; to handle each individual binary relation constraint.
  (define-syntax (handle-binrel astx)
    (syntax-case astx ()
      [(_ kind-lhs make-lhs kind-rhs make-rhs)
       (let ([handle-lhs (binary-half #'kind-lhs #'make-lhs)]
             [handle-rhs (binary-half #'kind-rhs #'make-rhs)])
         #`(lambda (stx)
             (syntax-case stx (has kind-lhs kind-rhs including also but not ?)
               #,@handle-lhs
               #,@handle-rhs)))]))
  
  ;; Macro for creating the binary relation parser. The resulting macro
  ;; takes a list of constraints (in special syntax), turns them into real
  ;; constraint struct instances, and then splits them into constraints on the
  ;; left-hand and right-hand sides of the relation, which are stored in the
  ;; overall struct.
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

;; Binary relation parser for UA.
(define-for-syntax (parse-ua-info stx)
  (parse-binary-relation stx UA make-ua-info ua-user-info?
                         roles make-ua-user-info
                         users make-ua-role-info))

;; Binary relation parser for RA.
(define-for-syntax (parse-ra-info stx)
  (parse-binary-relation stx RA make-ra-info ra-role-info?
                         perms make-ra-role-info
                         roles make-ra-perm-info))




;; Overall parser macro for an AC state.
(define-syntax (rbac-state stx)
  (syntax-case stx ()
    [(_ users roles perms uas ras)
     (let ([users (parse-users #'users)]
           [roles (parse-roles #'roles)]
           [perms (parse-perms #'perms)]
           [ua (parse-ua-info #'uas)]
           [ra (parse-ra-info #'ras)])
       #`(make-ac-state #,users #,roles #,perms #,ua #,ra))]))


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
