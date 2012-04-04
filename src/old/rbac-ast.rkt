#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Info about the a particular user in the UA binary relation.
;; The count is the number of roles, or #f if unspecified. The roles are
;; required roles, and the omits are the roles that are not allowed for the
;; user.
(define-struct/contract ua-user-info
  ([name symbol?]
   [count (or/c #f exact-nonnegative-integer?)]
   [roles (listof symbol?)]
   [omits (listof symbol?)])
  #:transparent)
;; Same for roles in the UA relation.
(define-struct/contract ua-role-info
  ([name symbol?]
   [count (or/c #f exact-nonnegative-integer?)]
   [users (listof symbol?)]
   [omits (listof symbol?)])
  #:transparent)
;; The UA relation information.
(define-struct/contract ua-info
  ([users (listof ua-user-info?)]
   [roles (listof ua-role-info?)])
  #:transparent)

;; Same for roles in the RA relation.
(define-struct/contract ra-role-info
  ([name symbol?]
   [count (or/c #f exact-nonnegative-integer?)]
   [perms (listof symbol?)]
   [omits (listof symbol?)])
  #:transparent)
;; Same for perms in the RA relation.
(define-struct/contract ra-perm-info
  ([name symbol?]
   [count (or/c #f exact-nonnegative-integer?)]
   [roles (listof symbol?)]
   [omits (listof symbol?)])
  #:transparent)
;; The RA relation information.
(define-struct/contract ra-info
  ([roles (listof ra-role-info?)]
   [perms (listof ra-perm-info?)])
  #:transparent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unary relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Info about the users in an AC state. The count includes the named users.
(define-struct/contract users-info
  ([count (or/c #f exact-nonnegative-integer?)]
   [names (listof symbol?)])
  #:transparent)
;; Info about the roles in an AC state. The count includes the named roles.
(define-struct/contract roles-info
  ([count (or/c #f exact-nonnegative-integer?)]
   [names (listof symbol?)])
  #:transparent)
;; Info about the permissions in an AC state. The count includes the named
;; permissions.
(define-struct/contract perms-info
  ([count (or/c #f exact-nonnegative-integer?)]
   [names (listof symbol?)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AC state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The overall AC state.
;; Note that the contents of an AC state may be inconsistant. This is perfectly
;; fine: it just means that no scenarios will be found when a query is issued.
(define-struct/contract ac-state
  ([users users-info?]
   [roles roles-info?]
   [perms perms-info?]
   [ua ua-info?]
   [ra ra-info?])
  #:transparent)