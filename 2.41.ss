;;��д��һ�����̣����ܲ���������С�ڵ��ڸ�������n��������������i��j��k��������Ԫ�飬ʹÿ����Ԫ�������Ԫ֮�͵��ڸ���������s��
(load "sicp.ss")
(load "2.40.ss")
(define (unique-triples n)
  (flatmap (lambda (i)
	     (map (lambda (j)
		    (cons i j))
		  (unique-pairs (- i 1))))
	   (enumerate-interval 1 n)))

(define (triple-sum-equal-to? sum triple)
  (= sum
     (+ (car triple)
	(cadr triple)
	(caddr triple))))
(define (triple-sum-equal-to-s n s)
  (filter (lambda (triple)
	    (triple-sum-equal-to? s triple))
	  (unique-triples n)))
