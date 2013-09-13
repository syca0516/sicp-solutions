(define true #t)
(define false #f)
(define (abs x)                                 ;绝对值函数
  (cond ((< x 0) (- x))
        (else x)))
(define (square x)                             ;平方函数
  (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (sum-of-squares x y)                   ;平方和函数
  (+ (square x)
     (square y)))
(define (bigger x y)                           ;两个数中较大的一个
  (if (> x y)
      x 
      y))
(define (smaller x y)                           ;两个数中较小的一个
  (if (> x y)
      y
      x))
;(define bigger (bigger x y))
;(define another-bigger (bigger (smaller x y) z))
(define (bigger-sum-of-squares x y z)
  (sum-of-squares (bigger x y)
                  (bigger (smaller x y) z)))
(define (a-plus-abs-b x y)                      ;高阶函数的一个例子
  ((if (> y 0) + -) x y))
(define (p) (p))                                ;调用这个过程会导致无限循环
(define (test x y)                              ;检测解释器运用的是应用序（CBV）还是正则序（CBN）
  (if (= x 0)
      x
      y))
(define (sqrt x)                                ;牛顿迭代法求平方根
  (define (good-enough? guess new-guess)
    (> 0.0001
       (/ (abs (- new-guess guess))
          guess)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(define (factorial n)                           ;线性迭代版的阶乘函数
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter)
              (+ counter 1))))
  (iter 1 1))
(define (dec x)                                 ;减一
  (- x 1))
(define (inc x)                                 ;加一
  (+ x 1))
(define (add a b)                               ;线性迭代版的加法定义（基于基本过程inc和dec）
  (if (= a 0)
      b
      (add (dec a) (inc b))))
(define (tree-rec-fib n)                        ;树形递归版本的Fibonacci函数
  (cond ((= 0 n) 0)                             ;步骤数目的增长性像Fib（n）一样快
        ((= 1 n) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib n)                                 ;线性迭代版本的Fibonacci函数
  (define (iter a b count)                      ;计算步骤相对于n为线性的
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))
(define (log-fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount 
                        (first-denomination kinds-of-coins))
                      kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1 ) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(define (dot-product left-vector right-vector)                 ;点乘
  (apply + (map (lambda (x y) (* x y)) left-vector right-vector)))
(define (transpose matrix)                                     ;转置
  (apply map (cons list matrix)))

(define (fast-expt b n)                                       ;快速求幂
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (my-gcd  a b)                                         ;最大公约数
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))


;;;通过寻找因子来确定是不是质数
(define (smallest-divisor n)                                  
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime?  n)
  (if (= n 1)
      false
      (= n (smallest-divisor n))))


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


;;;抽象出的公共模式，序列求和中的抽象模式
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube n) (* n n n))
(define (identity x) x)

(define (sum-cubes a b)
  (sum cube a inc b))
(define (sum-integers a b)
  (sum identity a inc b))
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;;;通过区间折半寻找方程的根

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
;;;寻找函数不动点
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (isqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))


;;;平均阻尼
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (iisqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

;;;牛顿法
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (nsqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;;有理数
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))
;;区间算术
;;
;;
;;;表操作
(define (i-list-ref items n)
  (if (= n 0)
      (car items)
      (i-list-ref (cdr items) (- n 1))))
(define (i-length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (i-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(define (i-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (i-map proc (cdr items)))))
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;;;序列作为一种约定的界面
;;;过滤器
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
;;;累积器
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;;;枚举
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
;;;映射、并用append做累积
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;;;符号求导
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (s-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (s-deriv (addend exp) var)
                   (s-deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (s-deriv (multiplicand exp) var))
          (make-product (s-deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unkonw expression type -- DERIV" exp))))

(define adda
  (let ((a 0))
    (lambda () (begin
                 (set! a (+ a 1))
                 a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;the little schemer 书中代码
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x )))))
(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(define member?
  (lambda (a lat)
    (cond ((null? lat) #t)
          (else (or (eq? a (car lat))
                    (member? a (cdr lat)))))))
(define rember
  (lambda (a lat)
    (cond
      ((null? lat)  '())
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))))




    
