;;һ����������������֧��ɣ�һ�������֧����һ�����ҷ�֧��ÿ����֧��һ������ȷ�����ȵĸˣ�������ߵ���һ�����������ߵ�����һ�������塣���ǿ����ø������ݶ����ʾ���ֶ����壬����ͨ����������֧�������������磬ʹ��list����

;;���캯��
(define (make-mobile left right)
  (list left right))
;;��֧���Դ�һ��length����Ӧ����һ�������ټ���һ��structure������������structure����һ��������ʾһ��������������������һ����壺
(define (make-branch length structure)
  (list length structure))
;; a) ��д����Ӧ��ѡ����left-branch��right-branch�����Ƿֱ𷵻ػ���������֧������branch-length��branch-structure�����Ƿ���һ����֧�ϵĳɷ֡�
;;ѡ����
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (structure-is-mobile? structure)
  (pair? structure))
;; b) �����ѡ�����������total-weight��������һ��������������
(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (structure-is-mobile? s)
	(total-weight s)
	s)))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
;;test
;;Level
;;-----
;;3                      4 | 8
;;              +----------+----------+ 2
;;2           3 | 9
;;       +------+------+ 1
;;1    2 | 1
;;   +---+---+
;;   1       2
(define level-1-mobile (make-mobile (make-branch 2 1)
				    (make-branch 1 2)))
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile)
				    (make-branch 9 1)))
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile)
				    (make-branch 8 2)))
(total-weight level-1-mobile)
(total-weight level-2-mobile)
(total-weight level-3-mobile)

;; c)һ������Ϊ��ƽ��ģ���������֧�����ص������ҷ�֧�����أ�Ҳ����˵���������˵ĳ��ȳ��Ե��ڸ��ϵ������������������ұߵ�ͬ���˻�������������ÿ����֧�ϵ��ŵ��ӻ��Ҳƽ�⡣�����һ�����̣����ܼ��һ���������Ƿ�ƽ�⡣
(define (branch-torque branch)
  (* (branch-weight branch)
     (branch-length branch)))
;; Test:
(branch-torque (make-branch 3 9))

(define (branch-balanced? branch)
  (let ((s (branch-structure branch)))
    (if (structure-is-mobile? s)
	(mobile-balanced? s)
	true)))
;; Test:
(branch-balanced? (make-branch 1 2))

;;һ����������ƽ��ĵ��ҽ��������ҷ�֧����������������ҷ�֧Ҳ��ƽ��ġ�
(define (mobile-balanced? mobile)
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (=  (branch-torque left)
	     (branch-torque right))
	 (branch-balanced? left)
	 (branch-balanced? right))))
;; Usage:
(mobile-balanced? (make-mobile (make-branch 4 6)
			       (make-branch 3 8)))
(mobile-balanced? level-3-mobile)

;; d)�ٶ����Ǹı���ı�ʾ���������湹�췽ʽ��
;;(define (make-mobile left right)
;;   (cons left right))
;;(define (make-branch length structure)
;;   (cons length structure))
;;����Ҫ���Լ��ĳ����������޸ģ����ܽ�����Ϊʹ�������±�ʾ��


