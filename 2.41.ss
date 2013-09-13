;;请写出一个过程，它能产生出所有小于等于给定整数n的正的相异整数i、j和k的有序三元组，使每个三元组的三个元之和等于给定的整数s。
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
