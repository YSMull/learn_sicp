#lang racket

;;1.3
(define (max_tow_sum a b c)
        (cond ((and (>= a b) (>= b c)) (+ a b))
              ((and (>= a c) (>= c b)) (+ a c))
              ((and (>= b a) (>= a c)) (+ b a))
              ((and (>= b c) (>= c a)) (+ b c))
              ((and (>= c a) (>= a b)) (+ c a))
              ((and (>= c b) (>= b a)) (+ c b))))

(define (max_tow_sum2 a b c)
        (- (+ a b c) (min a b c)))

;;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

;;1.6
(define (decrease a)
  (new-if (> a 0)
      (decrease (- a 1))
      a))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;;1.7
;很大的数，good_enough会爆掉；很小的数，即使0.001作为衡量标准也很大了。
(define (improve guess n)
  (/ (+ guess (/ n guess)) 2))
(define (good-enough? x y)
  (< (abs (- x y)) 0.000001))

(define (sqrt-iter1 last-guess cur-guess n)
  (if (good-enough? last-guess cur-guess)
      cur-guess
      (sqrt-iter1 cur-guess (improve cur-guess n) n)))
(define (my-sqrt1 n)
  (sqrt-iter1 (/ n 2) (+ (/ n 2) 1.0) n))


(define (sqrt-iter2 guess n)
  (let ((next-guess (improve guess n)))
  (if (good-enough? guess next-guess)
      next-guess
      (sqrt-iter2 next-guess n))))
(define (my-sqrt2 n)
  (sqrt-iter2 1.0 n))


;;1.8
(define (improve3 guess n)
  (/ (+ (/ n (* guess guess)) (* 2 guess)) 3))
(define (sqrt3-iter guess n)
  (let ((next-guess (improve3 guess n)))
  (if (good-enough? guess next-guess)
      next-guess
      (sqrt3-iter next-guess n))))
(define (my-sqrt3 n)
  (sqrt3-iter 1.0 n))

; (my-sqrt3 27.0)

;; fibonacci
; 0 1 2 3 4 5 6
; 1 1 2 3 5 8 13
(define (fibonacci1 n)
  (if (= n 0)
      1
      (if (= n 1)
          1
          (+ (fibonacci1 (- n 1))
             (fibonacci1 (- n 2))))))

; (fibonacci1 6)

(define (fibonacci2 n)
        (fib-iter 1 1 n))

(define (fib-iter a b n)
        (if (= n 1)
            b
            (fib-iter b (+ a b) (- n 1))))
; (fibonacci2 6)


;; count-change

(define (coin i)
  (cond ((= i 1) 1)
        ((= i 2) 5)
        ((= i 3) 10)
        ((= i 4) 25)
        ((= i 5) 50)))
(define (calcNumbers amount use-coin-i-or-not)
            (cond ((or (< amount 0) (> use-coin-i-or-not 5)) 0)
                  ((= amount 0) 1)
                  (else (+ (calcNumbers amount (+ use-coin-i-or-not 1))
                           (calcNumbers (- amount (coin use-coin-i-or-not)) use-coin-i-or-not)))))

(define (exchange-coin amount)
            (calcNumbers amount 1))

(exchange-coin 100)

;1.12
(define (pascal row col)
  (cond ((= row 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (+ col 1))))))

(pascal 5 3)



