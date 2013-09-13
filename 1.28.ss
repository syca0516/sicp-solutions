
;;;费马检查的一种不会被欺骗的变形称为Miller-Rabin检查，它来源于费马小定理的一种变形。
(define (square x) (* x x))
(define (nontrivial-square-root? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (square a) n))))
(define (expmod base exp m)                                    ;一个数的幂对另一个数取模  
  (cond ((= exp 0) 
         1)
        ((nontrivial-square-root? base m)
         0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (Miller-Rabin-test n )                                       ;费马检查
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((Miller-Rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
