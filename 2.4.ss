;;;��������Ե���һ�ֹ����Ա�ʾ��ʽ���������һ��ʾ��ʽ��֤�����������x��y(car (cons x y))����������x��
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
