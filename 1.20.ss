
;;;最大公约数
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;;;如果是正则序就会显得很麻烦很麻烦