;;;�뿼�ǣ���һ�����ԶԹ��������ֲ����������������ȫ����û������������ֻ���ǷǸ�����������£������Խ�0�ͼ�һ����ʵ��Ϊ��
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;;;��ֱ�Ӷ���one��two������zero��add-1������ʾ�����ô���ȥ��ֵ��add-1 zero������
;;;������ӷ�����+��һ��ֱ�Ӷ��壨��Ҫͨ������Ӧ��add-1����

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define add
  (lambda (m)
    (lambda (n)
      (lambda (f)
	(lambda (x)
	  ((m f)
	   ((n f) x)))))))
