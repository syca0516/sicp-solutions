;;;����Lambert��ʽ�������к����Ľ���ֵ
(load "1.37.ss")
(define (tan-cf x k)
  (cont-frac
   (lambda (i)
     (cond ((= i 1) x)
	   (else (- (* x x)))))
   (lambda (i)
     (- (* 2.0 i) 1))
   k))
