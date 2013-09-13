;;;Alyssa的程序是不完整的，因为她还没有确定区间抽象的实现。这里是区间构造符的定义
;;构造函数和选择函数
(define (make-interval a b) (cons a b))
(define (upper-bound a) (cdr a))
(define (lower-bound a) (car a))
;;操作符
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Division error (interval spans 0)" y)
      (mul-interval x
		    (make-interval (/ 1. (upper-bound y))
				   (/ 1. (lower-bound y))))))
