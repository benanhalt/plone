#lang plai
;; login :415095692

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])

;; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (cond
   [(number? sexp) (num sexp)]
   [(valid-id? sexp) (id sexp)]
   [(list? sexp)
    (match sexp
      [(list-rest op operands)
       (cond
        [(member op '(+ - * /)) (build-binop op operands)]
        [(eq? 'with op) (build-with operands)]
        [(eq? 'if0 op) (build-if operands)]
        [(eq? 'fun op) (build-fun operands)]
        [else (app (parse op) (map parse operands))])]
      [_ (error "parse error" sexp)])]
    [else (error "parse error" sexp)]))

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp expr)
  (intrp expr (mtEnv)))

(print-only-errors #t)

(define (build-binop op operands)
  (match operands
    [(list l r) (binop (second (assoc op `((+ ,+) (- ,-) (* ,*) (/ ,/))))
                       (parse l) (parse r))]
    [_ (error "wrong number of operands for binop")]))

(define (build-with operands)
  (match operands
    [(list bindings body) (with (build-bindings bindings '()) (parse body))]
    [_ (error "improper with form")]))

(define (build-bindings bindings seen)
  (if (empty? bindings) '()
      (match (first bindings)
        [(list name expr)
         (cond
          [(not (valid-id? name)) (error "invalid identifier" name)]
          [(member name seen) (error "duplicate name in binding list")]
          [else (cons (binding name (parse expr))
                      (build-bindings
                       (rest bindings)
                       (cons name seen)))])]
        [_ (error "improper bindings" (first bindings))])))

(define (valid-id? name)
  (cond
   [(not (symbol? name)) #f]
   [(member name '(+ - / * if0 with fun)) #f]
   [else #t]))

(define (build-if operands)
  (match operands
    [(list pred then alt) (if0 (parse pred) (parse then) (parse alt))]
    [_ (error "improper if0")]))

(define (build-fun operands)
  (match operands
    [(list args body) (fun (build-args args) (parse body))]
    [_ (error "improper fun")]))

(define (build-args args)
  (cond
   [(not (list? args)) (error "improper fun args list" args)]
   [(not (andmap valid-id? args)) (error "bad identifier in args list" args)]
   [(duplicates? args) (error "duplicate args for fun" args)]
   [else args]))

(define (duplicates? args)
  (cond
   [(empty? args) #f]
   [(member (first args) (rest args)) #t]
   [else (duplicates? (rest args))]))

(define (intrp expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [binop (op l r) (do-binop op (intrp l env) (intrp r env))]
    [with (binds body) (intrp body (extend-env binds env))]
    [id (name) (lookup name env)]
    [if0 (c t e) (intrp (do-if0 (intrp c env) t e) env)]
    [fun (args body) (closureV args body env)]
    [app (f args) (do-app (intrp f env)
                          (map (lambda (a) (intrp a env)) args))]))

;; do-app : CFWAE-Value * (listof CFWAE-Value) : CFWAE-Value
(define (do-app clos args)
  (type-case CFWAE-Value clos
    [closureV (params body env)
               (intrp body (add-args-to-env params args env))]
    [else (error "can't apply non-function value" clos)]))

;; add-args-to-env : (listof symbol) * (listof CFWAE-Value) * Env -> Env
(define (add-args-to-env params args env)
  (cond
   [(and (empty? params) (empty? args)) env]
   [(empty? params) (error "too many arguments in application")]
   [(empty? args) (error "too few arguments in application")]
   [else (add-args-to-env
          (rest params)
          (rest args)
          (anEnv (first params) (first args) env))]))

;; lookup : symbol * Env -> CFWAE-Value
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error "unbound identifier:" name)]
    [anEnv (n val more-env)
           (if (symbol=? n name) val
               (lookup name more-env))]))

;; do-binop : operation * CFWAE-Value * CFWAE-Value -> CFWAE-Value
(define (do-binop op l r)
  (cond
   [(not (member op (list + - * /))) (error "unsupported operation" op)]
   [(not (and (numV? l) (numV? r))) (error "one operand was not a number value")]
   [(and (equal? op /) (equal? r (numV 0))) (error "div by zero")]
   [else (numV (op (numV-n l) (numV-n r)))]))

;; do-if0 : CFWAE-Value * CFWAE * CFWAE -> CFWAE
(define (do-if0 pred then alt)
  (type-case CFWAE-Value pred
    [numV (n) (if (equal? n 0) then alt)]
    [else (error "non-numeric predicate")]))

;; extend-env : (listof Binding) * Env -> Env
(define (extend-env binds orig-env)
  (local [(define (rextend binds env seen)
            (if (empty? binds) env
                (type-case Binding (first binds)
                  [binding
                   (name expr)
                   (if (member name seen) (error "duplicate name in binding list")
                       (rextend
                        (rest binds)
                        (anEnv name (intrp expr orig-env) env)
                        (cons name seen)))])))]
         (rextend binds orig-env '())))

(test (extend-env (list (binding 'a (num 0))) (mtEnv))
      (anEnv 'a (numV 0) (mtEnv)))
(test/exn (extend-env (list (binding 'a (num 0)) (binding 'a (num 1))) (mtEnv))
          "duplicate name in binding list")

;; interp tests
(test (interp (num 0)) (numV 0))
(test (interp (binop + (num 3) (num 4))) (numV 7))
(test (interp (if0 (binop - (num 3) (num 3)) (num 10) (num 20))) (numV 10))
(test (interp (if0 (binop + (num 3) (num 3)) (num 10) (num 20))) (numV 20))
(test (interp (if0 (num 0) (binop + (num 2) (num 3)) (binop - (num 2) (num 3)))) (numV 5))
(test (interp (if0 (num 1) (binop + (num 2) (num 3)) (binop - (num 2) (num 3)))) (numV -1))
(test (interp (with (list (binding 'a (num 0))) (num 1))) (numV 1))
(test (intrp (id 'a) (anEnv 'a (numV 0) (mtEnv))) (numV 0))

(test (parse '4) (num 4))

(test (parse 'foo) (id 'foo))
(for-each (lambda (bad)
            (test/exn (parse bad) "parse error"))
          '(fun if0 with + / - *))

(test/exn (parse '{+}) "wrong number of operands for binop")
(test/exn (parse '{+ 1}) "wrong number of operands for binop")
(test/exn (parse '{+ 1 2 3}) "wrong number of operands for binop")

(test (parse '{with {{x 1} {y 2}} x})
      (with (list (binding 'x (num 1)) (binding 'y (num 2))) (id 'x)))
(test (parse '{with {} x}) (with '() (id 'x)))
(test/exn (parse '{with {x 1 y 1} x}) "improper bindings")
(test/exn (parse '{with {}}) "improper with form")
(test/exn (parse '{with}) "improper with form")
(test/exn (parse '{with {} x y}) "improper with form")
(test/exn (parse '{with {{fun 7}} 0}) "invalid identifier")

(test (parse '{if0 p t e}) (if0 (id 'p) (id 't) (id 'e)))
(test/exn (parse '{if0 a b c d}) "improper if0")
(test/exn (parse '{if0 a b}) "improper if0")
(test/exn (parse '{if0 a}) "improper if0")
(test/exn (parse '{if0}) "improper if0")

(test (parse '{fun {} 0}) (fun '() (num 0)))
(test (parse '{fun {a b c} a}) (fun '(a b c) (id 'a)))
(test/exn (parse '{fun a b}) "improper fun args list")
(test/exn (parse '{fun {with 0} 4}) "bad identifier")
(test/exn (parse '{fun}) "improper fun")
(test/exn (parse '{fun {}}) "improper fun")
(test/exn (parse '{fun {} 0 1}) "improper fun")
(test/exn (parse '{fun {a a} 0}) "duplicate args for fun")

(test (parse '{f}) (app (id 'f) '()))
(test (parse '{f a}) (app (id 'f) (list (id 'a))))
(test (parse '{f a b}) (app (id 'f) (list (id 'a) (id 'b))))
(test (parse '{f a b c}) (app (id 'f) (list (id 'a) (id 'b) (id 'c))))
(test/exn (parse '{}) "parse error")

(test (do-binop + (numV 3) (numV 4)) (numV 7))
(test (do-binop - (numV 4) (numV 6)) (numV -2))
(test (do-binop * (numV 2) (numV 5)) (numV 10))
(test (do-binop / (numV 100) (numV 10)) (numV 10))

(test/exn (do-binop > (numV 5) (numV 4)) "unsupported operation")
(test/exn (do-binop + (numV 5) (closureV '(a b c) (num 4) (mtEnv)))
          "one operand was not a number value")
(test/exn (do-binop / (numV 1) (numV 0)) "div by zero")

(test (do-if0 (numV 0) (num 1) (num 2)) (num 1))
(test (do-if0 (numV 1) (num 1) (num 2)) (num 2))
(test/exn (do-if0 (closureV '(a) (num 0) (mtEnv)) (num 1) (num 2)) "non-numeric predicate")

(test/exn (lookup 'a (mtEnv)) "unbound identifier")
(test (lookup 'a (anEnv 'a (numV 0) (mtEnv))) (numV 0))


(define (ev sexp)
  (numV-n (interp (parse sexp))))

(test (ev '{+ 4 5}) 9)
(test (ev '{/ 10 5}) 2)
(test (ev '{- 4 5}) -1)
(test (ev '{* 4 5}) 20)

(test (ev '{with {{x 3} {y 4}} {+ x y}}) 7)
(test (ev '{with {{x 2}
                  {y 3}}
                 {with {{z {+ x y}}}
                       {+ x z}}})
      7)

(test/exn (ev '{with {{x 2}
                      {x 3}}
                     {+ x 2}})
          "duplicate name in binding list")

(test (ev '{{fun {x y} {* x y}} 2 3}) 6)
(test/exn (ev '{{fun {x x} {* x x}} 4 5}) "duplicate args")
(test (ev '{{fun {} 5}}) 5)
(test/exn (ev '{{fun {} 5} 4}) "too many arguments")
(test/exn (ev '{{fun {x y} x} 5}) "too few arguments")

(test/exn (ev '{with {{x 10} {x 20}} 50}) "duplicate name in binding list")

(test/exn (ev '{{+ 3 4} 5}) "can't apply non-function value")

(test (ev '{with {{x 10}} {{fun {y} x} 20}}) 10)
(test (ev '{with {{x 10} {y 20}} {{fun {x} {+ x y}} 30}}) 50)
(test (ev '{{fun {x y} {* {with {{y 10}} {+ x y}} y}} 2 3}) 36)
(test (ev '{with {{f {fun {x} {* x x}}}} {f 2}}) 4)
(test (ev '{with {{f {fun {x} {* x x}}}} {f {f 2}}}) 16)

(test (ev
       '{with {{Y {fun {f}
                       {{fun {x} {f {fun {v} {{x x} v}}}}
                        {fun {x} {f {fun {v} {{x x} v}}}}}}}}

              {with {{fact
                      {Y {fun {f}
                              {fun {n}
                                   {if0 n 1
                                        {* n {f {- n 1}}}}}}}}}
                    {fact 5}}})
      120)
