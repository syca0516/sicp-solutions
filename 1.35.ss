;;; ��֤���ƽ�ָ����Ǳ任x -> 1 + 1/x �Ĳ�����
(load "sicp.ss")
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
	     1.0)
