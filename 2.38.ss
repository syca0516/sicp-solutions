;;过程accumulate也称为fold-right，因为它将序列的第一个元素组合到右边所有元素的组合结果上。也有一个fold-left，它与fold-right类似，但却是按照相反方向去操作各个元素：
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
;;如果要求用某个op时保证fold-right和fold-left对任何序列都产生同样的结果，请给出op应该满足的性质。

;;这样的op应该是对求值顺序没有要求的函数.

