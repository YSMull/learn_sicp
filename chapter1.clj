; 1.16
(defn square [n] (* n n))
(defn multiply [a b]
    (* a b))
(println (multiply 3 8))
(defn fast-expt [b n]
    (cond (= n 0) 1
          (even? n) (fast-expt (square b) (/ n 2))
          :else (* b (fast-expt b (- n 1)))))

(defn fast-expt-iter [b n a]
  (cond (= n 0) a
        (even? n) (fast-expt-iter (square b) (/ n 2) a)
        :else (fast-expt-iter b (- n 1) (* a b))))

; (println (fast-expt 2 10))
; (println (fast-expt-iter 2 10 1))


; 1.17
(defn double' [n] (* 2 n))
(defn halve' [n] (/ n 2))
(defn mul' [a b]
    (cond
        (or (= b 0) (= a 0)) 0
        (= b 1) a
        (even? b) (mul' (double' a) (halve' b))
        :else (+ a (mul' a (- b 1) ))))
; (println (mul' 12 13))

; 1.18
(defn mul-iter [a b r]
    (cond
        (= b 0) r
        (even? b) (mul-iter (double' a) (halve' b) r)
        :else (mul-iter a (- b 1) (+ r a))))

; (println (mul-iter 13 12 0))

; 1.20
(defn gcd1 [a b]
    (if (= b 0)
        a
        (gcd1 b (mod a b))))

(defn gcd2 [a b]
    (cond (= b 0) a
          :else (gcd2 b (mod a b))))
; (println (gcd1 206 40))

; 产生了11次mod操作
; (gcd1 206 40)
; (gcd1 40 (mod 206 40))
; (gcd1 (mod 206 40) (mod 40 (mod 206 40)))
; (gcd1 (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))
; (gcd1 (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))
(println (gcd1 (mod (mod 206 40)
                    (mod 40 (mod 206 40)))
               (mod (mod 40 (mod 206 40))
                    (mod (mod 206 40)
                         (mod 40 (mod 206 40))))))



(defn expmod [base exp m]
    (cond (= exp 0) 1
          (even? exp) ))





