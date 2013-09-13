
;;;帕斯卡三角形或杨辉三角形：三角形边界上的数都是1，内部的每个数是位于它上面的两个数之和。
;;;请写一个过程用递归过程计算出帕斯卡三角形。

(define (pascal-rec row col)                                 ;递归版                                  
  (cond ((or (= col row) (= col 0)) 1)
        (else (+ (pascal-rec (- row 1) col)
                 (pascal-rec (- row 1) (- col 1))))))
(define (pascal row col)                                     ;用公式计算
  (/ (factorial row)
     (* (factorial col)
        (factorial (- row col)))))
(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1) 
                   max-count)))
  (fact-iter 1 1 n))