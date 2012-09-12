#lang plai

(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

(define (interp a)
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)]
  [uminusS (e ArithS?)]
  [multS (l ArithS?) (r ArithS?)])

(define (parse e)
  (cond
   [(number? e) (numS e)]
   [(list? e)
    (case (car e)
      ['+ (plusS (parse (cadr e)) (parse (caddr e)))]
      ['* (multS (parse (cadr e)) (parse (caddr e)))]
      ['- (if (null? (cddr e)) (uminusS (parse (cadr e)))
              (bminusS (parse (cadr e)) (parse (caddr e))))])]))

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l)
                          (multC
                           (numC -1) (desugar r)))]))
