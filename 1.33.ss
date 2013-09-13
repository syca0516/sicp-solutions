;;;����filtered-accumulate�Ķ���
(load "sicp.ss")  ;;������prime?��identity�Ķ���
(define (filtered-accumulate  combiner null-value filter-func term  a next b)
  (if (> a b)
      null-value
      (if (filter-func a)
	  (combiner (term a)
		    (filtered-accumulate combiner null-value filter-func term (next a) next b))
	  (combiner null-value
		    (filtered-accumulate combiner null-value filter-func term (next a) next b)))))

(define (sum-prime a b)
  (define (next x) (+  x  1))
  (filtered-accumulate + 0 prime?  identity a next b))

;;;С��n��������n���ص�������֮�˻�
(define (product-of-relative n)
  (define (relative-prime? m)
    (= (gcd m n) 1))
  (define (next x) (+ x 1))
  (filtered-accumulate * 1 relative-prime?  identity 1 next (- n 1)))
