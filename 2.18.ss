;;;请定义出过程reverse，它以一个表为参数，返回的表中所包含的元素与参数表相同，但排列
;;;顺序与参数表相反：

(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst))
              (list (car lst)))))
