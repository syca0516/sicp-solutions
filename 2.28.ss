;;дһ������fringe������һ��������ʾΪ��Ϊ����������һ�������е�Ԫ�����������������Ҷ�����մ����ҵ�˳��
(define (fringe lst)
  (cond ((null? lst) '())
	((not (pair? lst)) (list lst))
	(else
	 (append (fringe (car lst))
		 (fringe (cadr lst))))))
