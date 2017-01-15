(define (xor x y) (or (and x (not x)) (and y (not y))))

(define (abs x)
  (if (> x 0)
    x
    (- 0 x)))
