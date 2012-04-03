#lang racket
(require (for-meta 2 racket/base))
(require "rbac-ast.rkt")

(provide rbac-state
         (all-from-out "rbac-ast.rkt"))


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
     (unary-relation-parser stx Users users-info))

    ;; Parser for the Roles clause.
    (define (parse-roles stx)
     (unary-relation-parser stx Roles roles-info))

    ;; Parser for the Perms clause.
    (define (parse-perms stx)
     (unary-relation-parser stx Perms perms-info)))


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
             #'(#,make 'name #f
                       (list 'item (... ...))
                       (list 'omit (... ...)))]
          ;; (tim has ? roles including (student research-assistant))
          #`[(name has ? #,kind but not (item (... ...)))
             #'(#,make 'name #f empty
                       (list 'item (... ...)))]
          ;; (tim has ? roles but not (professor faculty))
          #`[(name has ? #,kind including (item (... ...)))
             #'(#,make 'name #f
                       (list 'item (... ...))
                       empty)]
          ;; (tim has 5 roles including (student research-assistant) but not (professor faculty))
          #`[(name has n #,kind including (item (... ...)) but not (omit (... ...)))
             #'(#,make 'name n
                       (list 'item (... ...))
                       (list 'omit (... ...)))]
          ;; (tim has 5 roles including (student research-assistant))
          #`[(name has n #,kind including (item (... ...)))
             #'(#,make 'name n
                       (list 'item (... ...))
                       empty)]
          ;; (tim has 3 roles also (student research-assistant) but not (professor faculty)
          #`[(name has n #,kind also (item (... ...)) but not (omit (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (+ n (length items))
                         items
                         (list 'omit (... ...))))]
          ;; (tim has 3 roles also (student research-assistant))
          #`[(name has n #,kind also (item (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (+ n (length items))
                         items
                         empty))]
          ;; (tim has 5 roles but not (professor faculty))
          #`[(name has n #,kind but not (omit (... ...)))
             #'(#,make 'name n
                       empty
                       (list 'omit (... ...)))]
          ;; (tim has 5 roles)
          #`[(name has n #,kind)
             #'(#,make 'name n
                       empty
                       empty)]
          ;; (tim has roles (student research-assistant))
          ;; equiv. to (tim has 0 roles also (student research-assistant))
          #`[(name has #,kind (item (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (length items)
                         items
                         empty))]
          ;; (tim has roles (student research-assistant) but not (professor faculty))
          #`[(name has #,kind (item (... ...)) but not (omit (... ...)))
             #'(let ([items (list 'item (... ...))])
                 (#,make 'name (length items)
                         items
                         (list 'omit (... ...))))]
          ;; (tim has roles but not (professor faculty))
          #`[(name has #,kind but not (omit (... ...)))
             #'(#,make 'name #f
                       empty
                       (list 'omit (... ...)))]))

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
  (parse-binary-relation stx UA ua-info ua-user-info?
                         roles ua-user-info
                         users ua-role-info))

;; Binary relation parser for RA.
(define-for-syntax (parse-ra-info stx)
  (parse-binary-relation stx RA ra-info ra-role-info?
                         perms ra-role-info
                         roles ra-perm-info))




;; Overall parser macro for an AC state.
(define-syntax (rbac-state stx)
  (syntax-case stx ()
    [(_ users roles perms uas ras)
     (let ([users (parse-users #'users)]
           [roles (parse-roles #'roles)]
           [perms (parse-perms #'perms)]
           [ua (parse-ua-info #'uas)]
           [ra (parse-ra-info #'ras)])
       #`(ac-state #,users #,roles #,perms #,ua #,ra))]))

