;;;д��sum���̵ĵ����汾
(load "1.29.ss")
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter (term a) 0))
