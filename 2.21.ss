;;����square-list��һ����ֵ��Ϊ����������ÿ������ƽ�����ɵı�����������������
(load "sicp.ss")
(define (square-list lst)
  (map (lambda (x) (* x x)) lst))
(define (another-square-list lst)
  (if (null? lst)
      '()
      (cons (square (car lst)) (another-square-list (cdr lst)))))
