;;�޸�reverse���̣��õ�һ��deep-reverse���̡�����һ����Ϊ������������һ������Ϊֵ��������е�Ԫ�ط�ת���������е�����Ҳ��ת
(load "2.18.ss")
(define (deep-reverse lst)
  (cond ((null? lst) '())
	((not (pair? lst)) lst)
	(else
	 (reverse (list (deep-reverse (car lst))
			(deep-reverse (cadr lst)))))))
