;;����accumulateҲ��Ϊfold-right����Ϊ�������еĵ�һ��Ԫ����ϵ��ұ�����Ԫ�ص���Ͻ���ϡ�Ҳ��һ��fold-left������fold-right���ƣ���ȴ�ǰ����෴����ȥ��������Ԫ�أ�
(load "sicp.ss")
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))
;;���Ҫ����ĳ��opʱ��֤fold-right��fold-left���κ����ж�����ͬ���Ľ���������opӦ����������ʡ�

;;������opӦ���Ƕ���ֵ˳��û��Ҫ��ĺ���.

