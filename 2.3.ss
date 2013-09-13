;;;��ʵ��һ��ƽ����εı�ʾ��������Ĺ��캯����ѡ�������弸�����̣�����������ε��ܳ�
;;;������ȡ���������Ϊ����ʵ����һ�ֱ�ʾ��ʽ����Ӧ������������ϵͳ��ʹ֮���ṩ�ʵ��ĳ�
;;;���ϣ�ʹͬһ���ܳ�����������̶Բ�ͬ��ʾ���ܹ���
(load "sicp.ss")
(load "2.2.ss")
(define (rectangle length width)
  (cons length width))
(define (rectangle-width x)
  (cdr x))
(define (rectangle-length x)
  (car x))
(define (rectangle-perimeter x)
  (* 2
     (+ (rectangle-width x)
	(rectangle-length x))))
(define (rectangle-area x)
  (* (rectangle-width x)
     (rectangle-length x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))
(define (rect-bottom-left rect)
  (car rect))
(define (rect-bottom-right rect)
  (make-point (x-point (cdr rect))
	      (y-point (car rect))))
(define (rect-top-left rect)
  (make-point (x-point (car rect))
	      (y-point (cdr rect))))
(define (rect-top-right rect)
  (cdr rect))
(define (rect-width  rect)
  (abs (- (x-point (rect-bottom-left rect))
	  (x-point (rect-bottom-right rect)))))
(define (rect-height rect)
  (abs (- (y-point (rect-bottom-left rect))
	  (y-point (rect-top-left rect)))))
(define (rect-area rect)
  (* (rect-width rect)
     (rect-height rect)))
(define (rect-perimeter rect)
  (* 2
     (+ (rect-width rect)
	(rect-height rect))))