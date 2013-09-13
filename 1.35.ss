;;; 请证明黄金分割率是变换x -> 1 + 1/x 的不动点
(load "sicp.ss")
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
	     1.0)
