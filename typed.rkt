#lang plai-typed


(define-type FuncDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (id : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type ExprC
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [ifC (p : ExprC) (c : ExprC) (a : ExprC)]
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

; interp : ExprC * (listOf FuncDefC) -> number
(define (interp [e : ExprC] [fds : (listof FuncDefC)] [env : Env]) : number
  (type-case ExprC e
    [idC (s) (lookup s env)]
    [appC (fun arg)
          (let ([fd (get-fundef fun fds)])
            (interp (fdC-body fd) fds
                    (extend-env (bind (fdC-arg fd)
                          (interp arg fds env)) mt-env)))]
    [ifC (p c a) (if (eq? (interp p fds env) 0) (interp a fds env) (interp c fds env))]
    [numC (n) n]
    [plusC (l r) (+ (interp l fds env) (interp r fds env))]
    [multC (l r) (* (interp l fds env) (interp r fds env))]))

; lookup : symbol * Env -> number
(define (lookup [s : symbol] [env : Env]) : number
  (cond [(empty? env)  (error 'lookup "unbound variable")]
        [(eq? s (bind-id (first env))) (bind-val (first env))]
        [else (lookup s (rest env))]))

; get-fundef : symbol * (listOf FuncDefC) -> FuncDefC
(define (get-fundef [name : symbol] [fds : (listof FuncDefC)]) : FuncDefC
  (cond [(empty? fds) (error 'get-fundef "undefined function")]
        [(eq? name (fdC-name (first fds))) (first fds)]
        [else (get-fundef name (rest fds))]))

; subst : ExprC * symbol * ExprC -> ExprC
(define (subst what for in)
  (type-case ExprC in
    [idC (s) (if (eq? s for) what in)]
    [appC (f a) (appC f (subst what for a))]
    [ifC (p c a) (ifC (subst what for p)
                      (subst what for c)
                      (subst what for a))]
    [numC (n) in]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))


(define-type ExprS
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : ExprS)]
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
      [else (appS (s-exp->symbol (first l)) (parse (second l)))]))]))

(define (desugar as)
  (type-case ExprS as
    [idS (s) (idC s)]
    [appS (f a) (appC f (desugar a))]
    [ifS (p c a) (ifC (desugar p) (desugar c) (desugar a))]
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l)
                          (multC
                           (numC -1) (desugar r)))]))


(define fds
  (list
   (fdC 'double 'x (desugar (parse '(+ x x))))
   (fdC 'f 'x (desugar (parse '(* x y))))
   (fdC 'g 'y (desugar (parse '(+ (f y) 3))))
   (fdC 'h 'y (desugar (parse '(+ y (f 3)))))))
