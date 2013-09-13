;;;通过类似于Alyssa的推理，说明两个区间的差应该怎样计算。请定义出相应的减法过程sub-interval
(load "2.7.ss")
(define (sub-interval a b)
  (add-interval a
		(make-interval (* -1.0 (upper-bound b))
			       (* -1.0 (lower-bound b)))))
(define (display-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))
;;Usage
(define i (make-interval 2 7))
(define j (make-interval 8 3))

(display-interval i)
(display-interval (sub-interval i j))
(display-interval (sub-interval j i))
