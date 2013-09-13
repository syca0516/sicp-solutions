;;;写出sum过程的迭代版本
(load "1.29.ss")
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter (term a) 0))
