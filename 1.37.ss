;;;���Եݹ�汾
(define (cont-frac n d k)
  (define (iter start)
    (if (> start k)
	0
	(/ (n start)
	   (+ (d start)
	      (iter (+ start 1))))))
  (iter 1))

;; (cont-frac (lambda (i) 1.0)
;; 	   (lambda (i) 1.0)
;; 	   10)

;;; �����汾
;; (define (cont-frac n d k)
;;   (define (loop result term)
;;     (if (= term 0)
;; 	result
;; 	(loop  (/ (n term)
;; 		  (+ (d term) result))
;; 	       (- term 1))))
;;   (loop 0 k))
