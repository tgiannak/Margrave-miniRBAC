#lang racket

(require margrave
         "constraints.rkt"
         "compile-vocab.rkt"
         "util.rkt")

(provide constraints->m-theory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates ngroups groups of nsyms gensym symbols. For example
;; (gensym-groups 3 2)
;; yields
;; '((g141152 g141153) (g141154 g141155) (g141156 g141157))
(define (gensym-groups ngroups nsyms)
    (build-list ngroups (lambda (_) (build-list nsyms (lambda (_) (gensym))))))

;; Gets the arity of the predicate from the vocab. The arity is
;; returned as a list of symbols.
(define (get-pred-arity voc pred)
  (map string->symbol
       (m-predicate-arity
        (hash-ref (m-vocabulary-predicates voc)
                  (symbol->string pred)))))

;;;;;;;;;;
;; The following are used in the size constraints for both sorts
;; and preds.

;; Makes a formula that makes sure that no two groups in the given
;; var-groups are equal.
(define (all-vars-distinct var-groups)
  (define (group-neq vs xs)
    `(and ,@(map (lambda (v x) `(not (= ,v ,x))) vs xs)))
  (define (all-vars-distinct* var-groups)
    (match var-groups
      [(list) empty]
      [(cons vs gs)
       (append (map (lambda (xs) (group-neq vs xs)) gs)
               (all-vars-distinct* gs))]))
  `(and ,@(all-vars-distinct* var-groups)))

;; Makes a formula that makes sure that at least one pair of groups
;; in the given var-groups are equal.
(define (some-var-same var-groups)
  (define (group-eq vs xs)
    `(and ,@(map (lambda (v x) `(= ,v ,x)) vs xs)))
  (define (some-var-same* var-groups)
    (match var-groups
      [(list) empty]
      [(cons vs gs)
       (append (map (lambda (xs) (group-eq vs xs)) gs)
               (some-var-same* gs))]))
  `(or ,@(some-var-same* var-groups)))
  
;; Takes the list of vars with sorts and prefixes the matrix with their
;; existential quantifications.
(define (existentials-prefix vars/sorts matrix)
  (foldl (lambda (v matrix) `(exists ,(first v) ,(second v) ,matrix))
         matrix vars/sorts))

;; Takes the list of vars with sorts and prefixes the matrix with their
;; universal quantifications.
(define (universals-prefix vars/sorts matrix)
  (foldl (lambda (v matrix) `(forall ,(first v) ,(second v) ,matrix))
         matrix vars/sorts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts sort-size-constraints into Margrave constraints.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sort-size-constraint->m-constraints voc sort count)
  ;; Worker function for creating the formula that captures the minimum
  ;; sort size for the sort size constraint.
  (define (min-sort-size count)
    (let* ([vars (gensym-groups count 1)]
           [vars/sorts (map (lambda (v) (list (first v) sort)) vars)]
           [matrix (all-vars-distinct vars)])
      (existentials-prefix vars/sorts matrix)))
  ;; Worker function for creating the formula that captures the maximum
  ;; sort size for the sort size constraint.
  (define (max-sort-size count)
    (let* ([vars (gensym-groups (add1 count) 1)]
           [vars/sorts (map (lambda (v) (list (first v) sort)) vars)]
           [matrix (some-var-same vars)])
      (universals-prefix vars/sorts matrix)))
  ;; The formulas to constrain the upper and lower bounds of the sort.
  (list `(formula ,(min-sort-size count))
        `(formula ,(max-sort-size count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts sort-constant-constraints into Margrave constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sort-constant-constraint->m-constraints voc sort const)
  empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts pred-size-constraints into Margrave constraints.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates a constraint on the given predicate that ensures that any
;; instance of the predicate will have exactly count tuples as elements
;; with the given fixed porition of the tuples.
;; For example, saying that predicate r : A*B has 2 tuples with a fixed
;; part of '(#f b) means that
;; (exists x A (exists y A (and (not (= x y)) (r x b) (r y b))))
;; Predicates with more than one blank look more confusing, but follow
;; the same pattern.
(define (pred-size-constraint->m-constraints voc pred fixed count)
  (define nvars (length (filter not fixed)))
  ;; the arity of pred
  (define pred-arity (get-pred-arity voc pred))
  ;; the sorts of the holes in the tuple
  (define var-sorts (for/list ((fx (in-list fixed))
                               (sort (in-list pred-arity))
                               #:when (not fx))
                      sort))
  ;; given the var groups associates them with the sorts of the holes
  ;; in the predicates
  (define (var-groups/sorts var-groups var-sorts)
    (map (lambda (vs) (map (lambda (v g) (list v g)) vs var-sorts))
         var-groups))
  
  ;;
  (define min-pred-size
    (let* ([var-groups (gensym-groups count nvars)]
           [vars/sorts (var-groups/sorts var-groups var-sorts)]
           [matrix (all-vars-distinct var-groups)])
      (existentials-prefix
       (append* vars/sorts)
       `(and ,@(map (lambda (vars) `(,pred ,@vars)) var-groups)
             ,matrix))))
  
  (define max-pred-size
    (let* ([var-groups (gensym-groups count nvars)]
           [vars/sorts (var-groups/sorts var-groups var-sorts)]
           [matrix (some-var-same var-groups)])
      (universals-prefix
       (append* vars/sorts)
       `(implies (and ,@(map (lambda (vars) `(,pred ,@vars)) var-groups))
                 ,matrix))))
  
  (list `(formula ,min-pred-size)
        `(formula ,max-pred-size)))
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts pred-contains-constraints into Margrave constraints.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pred-contains-constraint->m-constraints voc pred tuple)
  (list `(formula (,pred ,@tuple))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts pred-omits-constraints into Margrave constraints.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pred-omits-constraint->m-constraints voc pred tuple)
  (list `(formula (not (,pred ,@tuple)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts a list of constraints into a list of Margrave constraints.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (constraint->margrave-constraints voc constr)
  (match constr
    [(sort-size-constraint sort count)
     (sort-size-constraint->m-constraints voc sort count)]
    [(sort-constant-constraint sort const)
     (sort-constant-constraint->m-constraints voc sort const)]
    [(pred-size-constraint pred fixed count)
     (pred-size-constraint->m-constraints voc pred fixed count)]
    [(pred-contains-constraint pred tuple)
     (pred-contains-constraint->m-constraints voc pred tuple)]
    [(pred-omits-constraint pred tuple)
     (pred-omits-constraint->m-constraints voc pred tuple)]))

(define (constraints->margrave-constraints voc constraints)
  (append-map (lambda (c) (constraint->margrave-constraints voc c))
              constraints))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts a list of constraints into a Margrave theory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (constraints->m-theory name constraints)
  (let* ([voc (constraints->vocab name constraints)]
         [axioms (constraints->margrave-constraints voc constraints)])
    (m-theory name (string->path "/dev/null") voc axioms)))
