(load "sicp.ss")
(load "2.38.ss")
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (reverse sequence)
  (fold-right (lambda (x y)
		(append y (list x)))
	      '()
	      sequence))
;;ÕâÌâÉËÄÔ½î£¬
