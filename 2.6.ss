;;;请考虑，在一个可以对过程做各种操作的语言里，我们完全可以没有数（至少在只考虑非负整数的情况下），可以将0和加一操作实现为：
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;;;请直接定义one和two（不用zero和add-1）（提示：利用代换去求值（add-1 zero））。
;;;请给出加法过程+的一个直接定义（不要通过反复应用add-1）。

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define add
  (lambda (m)
    (lambda (n)
      (lambda (f)
	(lambda (x)
	  ((m f)
	   ((n f) x)))))))
