;;;辛普森规则是另一种比上面所用规则更精确的数值积分方法。

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y x)
    (f (+ a (* x h))))
  (define (simpson-next x)
    (+ x 1))
  (define (simpson-term x)
    (cond ((= x 0) (y 0))
	  ((= x n) (y n))
	  ((odd? x) (* 4 (y x)))
	  (else (* 2 (y x)))))
  (* (/ h 3)
     (sum simpson-term 0 simpson-next n)))
(define (cube x) (* x x x))
