
;;;函数f由如下的规则定义：如果n<3,那么f(n)=n;如果n>=3,那么f(n)=f(n-1)+2*f(n-2)+3*f(n-3)。
;;;请写一个采用递归过程计算过程计算f的过程。再写一个采用迭代计算过程f的过程。

(define (f n)
 (define (iter a b c count)
   (if (= count n)
       c
       (iter (+ a (* 2 b) (* 3 c))
             a
             b
             (+ count 1))))
  (iter 2 1 0 0))

       
        
        

