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
  ([pred predicate?]
   [contains (listof (listof symbol?))]
   [omits (listof (listof symbol?))])
  #:transparent)

;; an ac-state is a list of constraints

(define (sort-constraint->formula name count names)
  'true)

;; helper
(define (2-subsets xs)
  (cond [(empty? xs) empty]
        [else (append (map (lambda (y) (list (first xs) y))
                           (rest xs))
                      (2-subsets (rest xs)))]))

;; helper
(define (all-equal xs ys)
  `(and ,@(map (lambda (x y) `(= ,x ,y)) xs ys)))
(define (any-not-equal xs ys)
  `(or ,@(map (lambda (x y) `(not (= ,x ,y))) xs ys)))

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

(define (make-prefix-at-least pred fixed count)
  (let ([vars (gen-vars (length fixed) (add1 count))]
        [sorts (predicate-sort pred)])
    (values vars (lambda (matrix) (make-quantifiers 'forall vars sorts matrix)))))

(define (make-prefix-at-most pred fixed count)
  (let ([vars (gen-vars (length (filter not fixed)) count)]
        [selected-sorts (append-map (lambda (x v) (if x '() (list v))) fixed (predicate-sort pred))])
    (values vars (lambda (matrix) (make-quantifiers 'exists vars selected-sorts matrix)))))

(define (insert-consts cs vs)
  (cond [(empty? cs) empty]
        [(not (first cs)) (cons (first vs) (insert-consts (rest cs) (rest vs)))]
        [else (cons (first cs) (insert-consts (rest cs) vs))]))

(define (size-constraint->formula pred fixed count)
  (let ([name (predicate-name pred)])
    `(and ,(let-values ([(vars prefix) (make-prefix-at-least pred fixed count)])
             ;; pigeonhole principle: if there are only count things in pred, if 
             ;; we see count+1 things, then two of them must be the same
             (prefix `(impiles (and ,@(map (lambda (xs) `(,name ,@xs)) vars))
                               (or ,@(map (lambda (x) (apply all-equal x)) (2-subsets vars))))))
          ,(let-values ([(vars prefix) (make-prefix-at-most pred fixed count)])
             (let* ([vars-with-consts (map (lambda (vs) (insert-consts fixed vs)) vars)])
               (prefix `(and ,@(map (lambda (xs) `(,name ,@xs)) vars-with-consts)
                             ,@(map (lambda (x) (apply any-not-equal x)) (2-subsets vars)))))))))

(define (content-constraint->formula pred contains omits)
  `(and ,@(map (lambda (x) `((predicate-name pred) ,@x)) contains)
        ,@(map (lambda (x) `(not ((predicate-name pred) ,@x))) omits)))

(define (constraint->formula constr)
  (match constr
    [(sort-constraint name count names)
     (sort-constraint->formula name count names)]
    [(size-constraint name fixed count)
     (size-constraint->formula name fixed count)]
    [(contents-constraint name contains omits)
     (content-constraint->formula name contains omits)]))

(define (constraints->formula constrs)
  `(and ,@(map constraint->formula constrs)))