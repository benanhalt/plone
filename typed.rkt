#lang plai-typed

(define-type Binding
  [bind (id : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)])

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
  [idC (s : symbol)]
  [ifC (p : ExprC) (c : ExprC) (a : ExprC)]
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define new-loc : (-> Location)
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))


(define (interp [e : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC e
    [boxC (a) (type-case Result (interp a env sto)
                [v*s (v-a s-a)
                     (let ([where (new-loc)])
                       (v*s (boxV where)
                            (override-store (cell where v-a)
                                            s-a)))])]
    [unboxC (a) (type-case Result (interp a env sto)
                  [v*s (v-a s-a)
                       (v*s (fetch (boxV-l v-a) s-a) s-a)])]
    [setboxC (b v) (type-case Result (interp b env sto)
                     [v*s (b-v b-s)
                          (type-case Result (interp v env b-s)
                            [v*s (v-v v-s)
                                 (v*s v-v
                                      (override-store (cell (boxV-l b-v)
                                                            v-v)
                                                     v-s))])])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1)])]
    [lamC (a b) (v*s (closV a b env) sto)]
    [idC (s) (v*s (fetch (lookup s env) sto) sto)]
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

(test
 (type-case Result
   (interp
    (appC (lamC 'b (seqC (seqC (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b))))
                               (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b)))))
                         (unboxC (idC 'b))))
          (boxC (numC 0))) mt-env mt-store)
   [v*s (val sto) val]) (numV 2))
