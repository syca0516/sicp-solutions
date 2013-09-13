;;; a) ��д��һ������sum�ĳ�Ϊproduct�Ĺ��̣��������ڸ�����Χ�и����ĳ������ֵ�ĳ˻�����˵�������product����factorial��������pi�Ľ���ֵ
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (identity x) x)
(define (factorial x)
  (define (fact-next x)
    (+ x 1))
  (product identity 1 fact-next x))

  ;;;����17����Ӣ����ѧ��John Walls�Ĺ�ʽ����pi
(define (pi n)
  (define (next x) (+ x 1))
  (define (term x)
    (if (even? x)
	(/ (+ x 2.0) (+ x 1.0))
	(/ (+ x 1.0) (+ x 2.0))))
  (* 4  (product term 1 next n)))
  
   ;;; b) ����product�ĵ����汾
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))
