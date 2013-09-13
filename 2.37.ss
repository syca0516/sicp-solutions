;;���ǿ��������в������ı������ľ�������������
(load "sicp.ss")

;;���
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;;���������
(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v))
       m))

;; Test:
(define test-matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define test-vector (list 1 2 3))

;;ת��
(define (transpose mat)
  (accumulate-n cons '() mat))

;;����˾���
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (vector) (matrix-*-vector cols vector)) m))) 
