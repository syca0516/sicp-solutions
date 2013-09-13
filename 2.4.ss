;;;下面是序对的另一种过程性表示方式。请针对这一表示方式验证，对于任意的x和y(car (cons x y))都将产生出x。
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
