;;; a) 请写出一个类似sum的称为product的过程，它返回在给定范围中各点的某个函数值的乘积。请说明如何用product定义factorial，并计算pi的近似值
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (identity x) x)
(define (factorial x)
  (define (fact-next x)
    (+ x 1))
  (product identity 1 fact-next x))

  ;;;根据17世纪英国数学家John Walls的公式计算pi
(define (pi n)
  (define (next x) (+ x 1))
  (define (term x)
    (if (even? x)
	(/ (+ x 2.0) (+ x 1.0))
	(/ (+ x 1.0) (+ x 2.0))))
  (* 4  (product term 1 next n)))
  
   ;;; b) 给出product的迭代版本
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))
