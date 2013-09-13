;;;���޸�fixed-point,ʹ���ܴ�ӡ�������в����Ľ���ֵ����
(load "sicp.ss")
(define (fixed-point f first-guess)
  (define step 0)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (begin
	(display next)
	(newline)
	(if (close-enough? guess next)
	    (begin
	      (display "took ")
	      (display step)
	      (display " steps")
	      (newline)
	      next)
	    (begin
	      (set! step (+ step 1))
	      (try next))))))
  (try first-guess))

(fixed-point
 (lambda (x)
   (/ (log 1000)
      (log x)))
 2.0)
(fixed-point
 (lambda (x)
   (/ (+ x (/ (log 1000)
	      (log x)))
      2))
 2.0)