;;�붨�����unique-pair����������n������������ԣ�i��j��������1<= j < i <= n.����unique-pairȥ�������prime-sum-pair�Ķ��塣
(load "sicp.ss")
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum? (unique-pair n))))
