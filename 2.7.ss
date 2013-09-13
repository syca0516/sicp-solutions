;;;Alyssa�ĳ����ǲ������ģ���Ϊ����û��ȷ����������ʵ�֡����������乹����Ķ���
;;���캯����ѡ����
(define (make-interval a b) (cons a b))
(define (upper-bound a) (cdr a))
(define (lower-bound a) (car a))
;;������
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Division error (interval spans 0)" y)
      (mul-interval x
		    (make-interval (/ 1. (upper-bound y))
				   (/ 1. (lower-bound y))))))
