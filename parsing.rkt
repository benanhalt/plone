#lang plai

(define-type FuncDefC
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])

(define-type Env
  [mt]
  [binding (id symbol?) (val number?) (rest Env?)])

(define-type ExprC
  [idC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [ifC (p ExprC?) (c ExprC?) (a ExprC?)]
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)])

; interp : ExprC * (listOf FuncDefC) -> number
(define (interp e fds env)
  (type-case ExprC e
    [idC (s) (lookup s env)]
    [appC (fun arg)
          (let ([fd (get-fundef fun fds)])
            (interp (fdC-body fd) fds
                    (binding (fdC-arg fd)
                             (interp arg fds env)
                             (mt))))]
    [ifC (p c a) (if (eq? (interp p fds env) 0) (interp a fds env) (interp c fds env))]
    [numC (n) n]
    [plusC (l r) (+ (interp l fds env) (interp r fds env))]
    [multC (l r) (* (interp l fds env) (interp r fds env))]))

; lookup : symbol * Env -> number
(define (lookup s env)
  (type-case Env env
    [mt () (error "unbound variable")]
    [binding (id val rest)
             (if (eq? id s) val
                 (lookup s rest))]))

; get-fundef : symbol * (listOf FuncDefC) -> FuncDefC
(define (get-fundef name fds)
  (cond [(null? fds) (error "undefined function")]
        [(eq? name (fdC-name (car fds))) (car fds)]
        [else (get-fundef name (cdr fds))]))

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
  [idS (s symbol?)]
  [appS (fun symbol?) (arg ExprS?)]
  [ifS (p ExprS?) (c ExprS?) (a ExprS?)]
  [numS (n number?)]
  [plusS (l ExprS?) (r ExprS?)]
  [bminusS (l ExprS?) (r ExprS?)]
  [uminusS (e ExprS?)]
  [multS (l ExprS?) (r ExprS?)])

(define (parse e)
  (cond
   [(number? e) (numS e)]
   [(symbol? e) (idS e)]
   [(list? e)
    (case (car e)
      ['if (ifS (parse (cadr e)) (parse (caddr e)) (parse (cadddr e)))]
      ['+ (plusS (parse (cadr e)) (parse (caddr e)))]
      ['* (multS (parse (cadr e)) (parse (caddr e)))]
      ['- (if (null? (cddr e)) (uminusS (parse (cadr e)))
              (bminusS (parse (cadr e)) (parse (caddr e))))]
      [else (appS (car e) (parse (cadr e)))])]))

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
   (fdC 'f 'x (desugar (parse '(* x y))))
   (fdC 'g 'y (desugar (parse '(+ (f y) 3))))
   (fdC 'h 'y (desugar (parse '(+ y (f 3)))))))
