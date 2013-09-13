(define (square x) (* x x))
;;;通过寻找因子来确定是不是质数
(define (smallest-divisor n)                                  
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next x)
    (if (= x 2) (+ x 1) (+ x 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime?  n)                                             
  (= n (smallest-divisor n)))


(define (expmod base exp m)                                    ;一个数的幂对另一个数取模  
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n )                                       ;费马检查
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;;检查给定范围内连续的各个奇数的素性,请用你的过程找出大于1000、大于10000、大于100000和大于1000000的三个
;;;最小的素数
(define (sfp from)
  (define (search-for-primes from num)
    (cond ((= num 3) 'done)
          ((even? from) (search-for-primes (+ from 1) num))
          ((prime? from)(begin (display from)
                               (newline)
                               (time (prime? from))
                               (newline)
                               (search-for-primes (+ from 2) (+ num 1))))
          (else (search-for-primes (+ from 2) num))))
    (search-for-primes from 0))