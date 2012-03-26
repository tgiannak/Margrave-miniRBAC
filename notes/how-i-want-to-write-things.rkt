#lang racket

;; Note that make-vocab and make-policy need not be constructors for
;; define-struct. More likely they will be functions that do some processing
;; or error checking first... or that give things to java and return
;; identifiers.

;(define voc1 (Vocab ...))
;; or
(define voc1 (make-vocab '(User Role Permission Class Object)
                         (list (make-subtype 'Object '(User Role)))
                         (list (make-voc-const 'theo 'User)
                               (make-voc-const 'tim 'User)
                               (make-voc-const 'student 'Role))
                         (list (make-voc-pred 'ua '(User Role))
                               (make-voc-pred 'ra '(Role Permission)))))

;; Write the following as one normally would define a poilcy, except that now
;; "voc1" is a genunine vocab value. To load from a file becomes something
;; like (include-vocab "filename.v"). Alternatively, one may force the file to
;; properly export the vocabulary (e.g. it is just embedded in Racket) and
;; so a normal Racket requires will do the trick.
(define pol1 (Poilcy voc1 ...))
;; or
(define pol1
  (create-policy voc1
               (list (make-binding a User) (make-binding b Permission))
               (make-target fmla)
               (list (make-pol-rule permit (list (make-var a) (make-var b)) fmla1)
                     (make-pol-rule deny (list (make-var a) (make-var b)) fmla2))))

(define query1 (Query voc1 ...))
;; or
(define pol1 (make-policy voc1 ...))

(let* ([result (run-query pol1 query1)]
       [result-pretty (show-result result)])
  (print result-pretty))
;; or
;(let ([result (run-query pol1 query1)])
;  (display-result result))
;; or
;(display-query-result pol1 query1)