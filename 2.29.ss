;;一个二叉活动体由两个分支组成，一个是左分支，另一个是右分支。每个分支是一个具有确定长度的杆，上面或者吊着一个重量，或者吊着另一个二叉活动体。我们可以用复合数据对象表示这种二叉活动体，将它通过其两个分支构造起来（例如，使用list）：

;;构造函数
(define (make-mobile left right)
  (list left right))
;;分支可以从一个length（它应该是一个数）再加上一个structure构造出来，这个structure或者一个数（表示一个简单重量），或者是另一个活动体：
(define (make-branch length structure)
  (list length structure))
;; a) 请写出相应的选择函数left-branch和right-branch，它们分别返回活动体的两个分支。还有branch-length和branch-structure，它们返回一个分支上的成分。
;;选择函数
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
;; b) 用你的选择函数定义过程total-weight，它返回一个活动体的总重量。
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

;; c)一个活动体称为是平衡的，如果其左分支的力矩等于其右分支的力矩（也就是说，如果其左杆的长度乘以吊在杆上的重量，等于这个活动体右边的同样乘积），而且在其每个分支上吊着的子活动体也平衡。请设计一个过程，它能检查一个二叉活动体是否平衡。
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

;;一个二叉活动体是平衡的当且仅当它左右分支的力矩相等且它左右分支也是平衡的。
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

;; d)假定我们改变活动体的表示，采用下面构造方式：
;;(define (make-mobile left right)
;;   (cons left right))
;;(define (make-branch length structure)
;;   (cons length structure))
;;你需要对自己的程序做多少修改，才能将它改为使用这种新表示？


