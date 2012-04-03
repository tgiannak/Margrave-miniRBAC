#lang racket

(define-struct/contract predicate
  ([name symbol?]
   [sort (listof symbol?)])
  #:transparent)

(define-struct/contract sort-constraint
  ([name symbol?]
   [count (or/c #f exact-nonnegative-integer?)]
   [names (listof symbol?)])
  #:transparent)

(define-struct/contract size-constraint
  ([pred predicate?]
   [fixed (listof (or/c #f symbol?))]
   [count exact-nonnegative-integer?])
  #:transparent)

(define-struct/contract contents-constraint
  ([name symbol?]
   [contains (listof (listof symbol?))]
   [omits (listof (listof symbol?))])
  #:transparent)

;; an ac-state is a list of constraints

(define (sort-constraint->formula name count names)
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

;; Makes the prefix and generates the vars for the "at most" portion of the size constraint.
;; Consists of forall quantifiers over all of the sorts in the pred.
(define (make-prefix-at-most pred fixed count)
  (let ([vars (gen-vars (length fixed) (add1 count))]
        [sorts (predicate-sort pred)])
    (values vars (lambda (matrix) (make-quantifiers 'forall vars sorts matrix)))))

;; Makes the prefix and generates the vars for the "at least" portion of the size constraint.
;; Consists of exists quantifiers over the sorts in the pred with no given values.
(define (make-prefix-at-least pred fixed count)
  (let ([vars (gen-vars (length (filter not fixed)) count)]
        [selected-sorts (foldr (lambda (x v rest) (if x rest (cons v rest))) empty fixed (predicate-sort pred))])
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
(define (size-constraint->formula pred fixed count)
  (let ([name (predicate-name pred)])
    `(and
      ;; At most count tuples
      ,(let-values ([(vars prefix) (make-prefix-at-most pred fixed count)])
             ;; pigeonhole principle: if there are only count things in pred, if 
             ;; we see count+1 things, then two of them must be the same
             (prefix `(impiles (and ,@(map (lambda (xs) `(,name ,@xs)) vars))
                               (or ,@(map (lambda (x) (apply all-equal x)) (2-subsets vars))))))
      ;; At least count tuples
      ,(let-values ([(vars prefix) (make-prefix-at-least pred fixed count)])
         (let* ([vars-with-consts (map (lambda (vs) (replace-falses fixed vs)) vars)])
           (prefix `(and ,@(map (lambda (xs) `(,name ,@xs)) vars-with-consts)
                         ,@(map (lambda (x) (apply any-not-equal x)) (2-subsets vars)))))))))

;; Creates a formula that asserts that the predicate contains the tuples in
;; contains and does not have any tuple in omits.
(define (content-constraint->formula name contains omits)
  `(and ,@(map (lambda (x) `(,name ,@x)) contains)
        ,@(map (lambda (x) `(not (,name ,@x))) omits)))

;; Turns a single constraint into a formula.
(define (constraint->formula constr)
  (match constr
    [(sort-constraint name count names)
     (sort-constraint->formula name count names)]
    [(size-constraint pred fixed count)
     (size-constraint->formula pred fixed count)]
    [(contents-constraint name contains omits)
     (content-constraint->formula name contains omits)]))

;; Turns a list of constraints into a formula.
(define (constraints->formula constrs)
  `(and ,@(map constraint->formula constrs)))