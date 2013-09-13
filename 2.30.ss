;;请定义一个与练习2.21中square-list过程类似的square-tree过程。请以两种方式定义square-tree，直接定义（即不使用任何高阶函数），以及使用map和递归定义
(load "sicp.ss")

;; a)
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))
;; Usage:
(define my-tree
  (list 1
	(list 2
	      (list 3 4
		    (list 5 6)))))

;; b)
(define (square-tree-use-map tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree-use-map sub-tree)
	     (square sub-tree)))
       tree))
