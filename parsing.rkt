#lang plai

(define-type ExprC
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)])

(define (interp a)
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(define-type ExprS
  [numS (n number?)]
  [plusS (l ExprS?) (r ExprS?)]
  [bminusS (l ExprS?) (r ExprS?)]
  [uminusS (e ExprS?)]
  [multS (l ExprS?) (r ExprS?)])

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
  (type-case ExprS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l)
                          (multC
                           (numC -1) (desugar r)))]))
