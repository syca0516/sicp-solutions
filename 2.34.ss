;;对于x的某个给定值，求出一个多项式在x的值，也可以形式化为一种累积。
;;采用著名的Horner规则
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* x higher-terms)
		   this-coeff))
	      0
	      coefficient-sequence))
;; Usage:
;;计算 1+3x+5x^3+x^5在x=2的值
(horner-eval 2 (list 1 3 0 5 0 1))
