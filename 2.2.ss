;;;ƽ�����߶εı�ʾ����
(define (make-point x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point x)
  (cdr x))
;;���漸����֪����ʲô���壬�о���һ��
(define (make-segment x y)
  (cons x y))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))
;;���������ʾ
(define (print-point x)
  (newline)
  (display "(")
  (display (x-point x))
  (display ",")
  (display (y-point x))
  (display ")"))
(define (midpoint-segment x)
  (cons (/ (+ (car (car x))
	      (cdr (car x)))
	   2)
	(/ (+ (car (cdr x))
	      (cdr (cdr x)))
	   2)))