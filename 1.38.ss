;;;����ŷ����չ��ʽ���e�Ľ���ֵ
(load "1.37.ss")
(define (caculate-e foo)
  (define (d x)
    (cond ((or
	    (= (remainder x 3) 0)
	    (= (remainder x 3) 1))
	   1)
	  (else
	   (* 2
	      (+ 1
		 (div x 3))))))
  (+ 2
     (cont-frac (lambda (i) 1.0)
		d
		foo)))