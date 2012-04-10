#lang racket

(require margrave
         "constraints.rkt"
         "compile-vocab.rkt"
         "util.rkt")

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
(define (constraints->theory name constraints)
  (let* ([voc (constraints->vocab name constraints)]
         [marg-constrs (constraints->margrave-constraints voc constraints)])
    (m-theory name empty voc marg-constrs)))

(define my-theory
  (constraints->theory "my-theory"
   (append (make-sort-constraints 'Role 10 '(student prof facutly grad ta ra))
           (make-sort-constraints 'Subject 8 '(sarah theo francis salman tim))
           (make-sort-constraints 'Permission 4 '(grade submit-hw))
           (make-pred-constraints 'ua '(#f ta) 2 '((sarah)) '((salman) (tim))))))

#|
;; The sort constraints are here converted into a formula that restricts the number
;; of elements that can appear in a sort.
(define (sort-constraint->formula name count consts)
  'true)

;; Finds all 2-subsets of a list. For example
;; > (2-subsets '(a b c d))
;; '((a b) (a c) (a d) (b c) (b d) (c d))
(define (2-subsets xs)
  (cond [(empty? xs) empty]
        [else (append (map (lambda (y) (list (first xs) y))
                           (rest xs))
                      (2-subsets (rest xs)))]))

;; Creates a formula that asserts that for x_1...x_n, y_1...y_n it is the case
;; that x_i = y_i for all i.
(define (all-equal xs ys)
  `(and ,@(map (lambda (x y) `(= ,x ,y)) xs ys)))

;; Creates a formula that asserts that for x_1...x_n, y_1...y_n it is the case
;; that not x_i = y_i for some i.
(define (any-not-equal xs ys)
  `(or ,@(map (lambda (x y) `(not (= ,x ,y))) xs ys)))

;; Generates groups of gensym symbols with per symbols in each group and count
;; groups. For example
;; > (gen-vars 2 4)
;; '((v182687 v182688) (v182689 v182690) (v182691 v182692) (v182693 v182694))
(define (gen-vars per count)
  (define (gen-vars* per)
    (if (zero? per) empty
        (cons (gensym 'v) (gen-vars* (sub1 per)))))
  (if (zero? count) empty
      (cons (gen-vars* per) (gen-vars per (sub1 count)))))

;; Quantifies the variables with the sorts around the given matrix.
;; The variables are a list of lists, each sublist being the length of
;; the list of sorts. The variables are attached to the sorts in order.
;; For example:
;; > (make-quantifiers 'forall '((a1 b1) (a2 b2) (a3 b3)) '(A B) 'true)
;; '(forall b3 B (forall a3 A (forall b2 B (forall a2 A (forall b1 B (forall a1 A true))))))
(define (make-quantifiers quant vars sorts matrix)
  (define (make-quant vs sorts matrix)
    (foldl (lambda (v s matrix) `(,quant ,v ,s ,matrix)) matrix vs sorts))
  (foldl (lambda (vs matrix) (make-quant vs sorts matrix)) matrix vars))

;; Looks up the sort of the predicate in the vocab
(define (predicate-sort pred voc)
  (hash-ref (m-vocabulary-predicates voc) pred))

;; Makes the prefix and generates the vars for the "at most" portion of the size constraint.
;; Consists of forall quantifiers over all of the sorts in the pred.
(define (make-prefix-at-most pred fixed count voc)
  (let ([vars (gen-vars (length fixed) (add1 count))]
        [sorts (predicate-sort pred voc)])
    (values vars (lambda (matrix) (make-quantifiers 'forall vars sorts matrix)))))

;; Makes the prefix and generates the vars for the "at least" portion of the size constraint.
;; Consists of exists quantifiers over the sorts in the pred with no given values.
(define (make-prefix-at-least pred fixed count voc)
  (let ([vars (gen-vars (length (filter not fixed)) count)]
        [selected-sorts (foldr (lambda (x v rest) (if x rest (cons v rest))) empty fixed (predicate-sort pred voc))])
    (values vars (lambda (matrix) (make-quantifiers 'exists vars selected-sorts matrix)))))

;; Replaces the #f values from cs with values from vs in the order that they appear.
;; For example:
;; > (replace-falses '(#f a b #f c) '(x y z))
;; '(x a b y c)
(define (replace-falses cs vs)
  (cond [(empty? cs) empty]
        [(not (first cs)) (cons (first vs) (replace-falses (rest cs) (rest vs)))]
        [else (cons (first cs) (replace-falses (rest cs) vs))]))

;; Creates a formula that asserts things about the size of the instance of
;; a predicate. In particular there is an assertion that the instance of the
;; predicate contains exactly count tuples where the non-false values of fixed
;; are in the correct slots.
;; For example (size-constraint->formula 'R '(a #f) 3)
;; says that there are exactly 3 tuples whose first element is a in the
;; instance of 'R.
(define (size-constraint->formula pred fixed count voc)
  `(and
    ;; At most count tuples
    ,(let-values ([(vars prefix) (make-prefix-at-most pred fixed count voc)])
       ;; pigeonhole principle: if there are only count things in pred, if 
       ;; we see count+1 things, then two of them must be the same
       (prefix `(impiles (and ,@(map (lambda (xs) `(,pred ,@xs)) vars))
                         (or ,@(map (lambda (x) (apply all-equal x)) (2-subsets vars))))))
    ;; At least count tuples
    ,(let-values ([(vars prefix) (make-prefix-at-least pred fixed count voc)])
       (let* ([vars-with-consts (map (lambda (vs) (replace-falses fixed vs)) vars)])
         (prefix `(and ,@(map (lambda (xs) `(,pred ,@xs)) vars-with-consts)
                       ,@(map (lambda (x) (apply any-not-equal x)) (2-subsets vars))))))))

;; Creates a formula that asserts that the predicate contains the tuples in
;; contains and does not have any tuple in omits.
(define (content-constraint->formula pred contains omits)
  `(and ,@(map (lambda (x) `(,pred ,@x)) contains)
        ,@(map (lambda (x) `(not (,pred ,@x))) omits)))

;; Turns a single constraint into a formula.
(define (constraint->formula constr voc)
  (match constr
    [(sort-constraint name count consts)
     (sort-constraint->formula name count consts)]
    [(size-constraint pred fixed count)
     (size-constraint->formula pred fixed count voc)]
    [(contents-constraint name contains omits)
     (content-constraint->formula name contains omits)]))

;; Turns a list of constraints into a formula.
(define (constraints->formula constrs voc)
  `(and ,@(map constraint->formula constrs voc)))|#