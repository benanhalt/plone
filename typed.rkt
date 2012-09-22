#lang plai-typed

(define-type Binding
  [bind (id : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type-alias Location number)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  [v*s (v : Value) (s : Store)])

(define-type ExprC
  [appC (fun : ExprC) (arg : ExprC)]
  [varC (s : symbol)]
  [ifC (p : ExprC) (c : ExprC) (a : ExprC)]
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [setC (id : symbol) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define new-loc : (-> Location)
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))


(define (interp [e : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC e
    [setC (var expr) (type-case Result (interp expr env sto)
                     [v*s (val-v val-s)
                          (v*s val-v (override-store
                                      (cell (lookup var env) val-v)
                                      val-s))])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1)])]
    [lamC (a b) (v*s (closV a b env) sto)]
    [varC (s) (v*s (fetch (lookup s env) sto) sto)]
    [appC (fun arg)
          (type-case Result (interp fun env sto)
            [v*s (f-v f-s)
                 (type-case Result (interp arg env f-s)
                   [v*s (a-v a-s)
                        (let ([where (new-loc)])
                          (interp (closV-body f-v)
                                  (extend-env (bind (closV-arg f-v) where)
                                              (closV-env f-v))
                                  (override-store (cell where a-v) a-s)))])])]
    [ifC (p c a)
         (type-case Result (interp p env sto)
           [v*s (p-v p-s) (interp (interpIf p-v c a) env p-s)])]
    [numC (n) (v*s (numV n) sto)]
    [plusC (l r)
           (type-case Result (interp l env sto)
             [v*s (v-l s-l)
                  (type-case Result (interp r env s-l)
                    [v*s (v-r s-r)
                         (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r)
           (type-case Result (interp l env sto)
             [v*s (v-l s-l)
                  (type-case Result (interp r env s-l)
                    [v*s (v-r s-r)
                         (v*s (num* v-l v-r) s-r)])])]))

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

(define (lookup [s : symbol] [env : Env]) : Location
  (cond [(empty? env)  (error 'lookup "unbound variable")]
        [(eq? s (bind-id (first env))) (bind-val (first env))]
        [else (lookup s (rest env))]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond [(empty? sto) (error 'fetch "undefined storage location")]
        [(eq? loc (cell-location (first sto))) (cell-val (first sto))]
        [else (fetch loc (rest sto))]))

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
    [idS (s) (varC s)]
    [appS (f a) (appC (desugar f) (desugar a))]
    [ifS (p c a) (ifC (desugar p) (desugar c) (desugar a))]
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l)
                          (multC
                           (numC -1) (desugar r)))]))

