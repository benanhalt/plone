#lang plai-typed

(define-type Binding
  [bind (id : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type ExprC
  [appC (fun : ExprC) (arg : ExprC)]
  [idC (s : symbol)]
  [ifC (p : ExprC) (c : ExprC) (a : ExprC)]
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])


(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [lamC (a b) (closV a b env)]
    [idC (s) (lookup s env)]
    [appC (fun arg)
          (let ([f-value (interp fun env)])
            (interp (closV-body f-value)
                    (extend-env (bind (closV-arg f-value)
                                      (interp arg env))
                                (closV-env f-value))))]
    [ifC (p c a) (interp (interpIf (interp p env) c a) env)]
    [numC (n) (numV n)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]))

(define (interpIf [pred : Value] [consq : ExprC] [altern : ExprC]) : ExprC
  (cond
   [(numV? pred) (if (= 0 (numV-n pred)) altern consq)]
   [else (error 'interpIf "if predicate was not a number")]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
   [else (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
   [else (error 'num* "one argument was not a number")]))

(define (lookup [s : symbol] [env : Env]) : Value
  (cond [(empty? env)  (error 'lookup "unbound variable")]
        [(eq? s (bind-id (first env))) (bind-val (first env))]
        [else (lookup s (rest env))]))


(define-type ExprS
  [idS (s : symbol)]
  [appS (fun : ExprS) (arg : ExprS)]
  [ifS (p : ExprS) (c : ExprS) (a : ExprS)]
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS (l : ExprS) (r : ExprS)])

(define (parse [e : s-expression]) : ExprS
  (cond
   [(s-exp-number? e) (numS (s-exp->number e))]
   [(s-exp-symbol? e) (idS (s-exp->symbol e))]
   [(s-exp-list? e) (let ([l (s-exp->list e)])
    (case (s-exp->symbol (first l))
      ['if (ifS (parse (second l)) (parse (third l)) (parse (fourth l)))]
      ['+ (plusS (parse (second l)) (parse (third l)))]
      ['* (multS (parse (second l)) (parse (third l)))]
      ['- (if (= 2 (length l)) (uminusS (parse (second l)))
              (bminusS (parse (second l)) (parse (third l))))]
      [else (appS (parse (first l)) (parse (second l)))]))]))

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [idS (s) (idC s)]
    [appS (f a) (appC (desugar f) (desugar a))]
    [ifS (p c a) (ifC (desugar p) (desugar c) (desugar a))]
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l)
                          (multC
                           (numC -1) (desugar r)))]))

