;;请定义过程unique-pair，给它整数n，它产生出序对（i，j），其中1<= j < i <= n.请用unique-pair去简化上面的prime-sum-pair的定义。
(load "sicp.ss")
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum? (unique-pair n))))
