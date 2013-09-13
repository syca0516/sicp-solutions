;;;请定义出过程last-pair，它返回只包含给定（非空）表里最后一个元素的表

(define (last-pair lst)
  (let ((last (car lst)))
    (if (null? (cdr lst))
        (list last)
        (last-pair (cdr lst)))))