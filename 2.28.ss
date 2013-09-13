;;写一个过程fringe，它以一个树（表示为表）为参数，返回一个表，表中的元素是这棵树的所有树叶，按照从左到右的顺序
(define (fringe lst)
  (cond ((null? lst) '())
	((not (pair? lst)) (list lst))
	(else
	 (append (fringe (car lst))
		 (fringe (cadr lst))))))
