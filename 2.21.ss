;;过程square-list以一个数值表为参数，返回每个数的平方构成的表，下面是其两个定义
(load "sicp.ss")
(define (square-list lst)
  (map (lambda (x) (* x x)) lst))
(define (another-square-list lst)
  (if (null? lst)
      '()
      (cons (square (car lst)) (another-square-list (cdr lst)))))
