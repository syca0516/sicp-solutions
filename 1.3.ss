
;;;题目：请定义一个过程，它以三个数为参数，返回其中较大的两个数之和

(define (bigger x y)
  (if (> x y) x y))

(define (smaller x y)
  (if (> x y) y x))

(define (sum-of-greater x y z)
  (+ (bigger x y)
     (bigger z
             (smaller x y))))