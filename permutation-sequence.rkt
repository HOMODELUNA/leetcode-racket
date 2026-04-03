#lang racket
(require math/number-theory)

(define (inner-permutation n k nums)
  (cond [(= n 1) (list (car nums))]
        [else
         (define-values (q k1) (quotient/remainder k (/ (factorial n) n) ))
         (define x (list-ref nums q))
         (cons x (inner-permutation (sub1 n) k1 (remove x nums)))]))

(define/contract (get-permutation n k)
  (-> exact-integer? exact-integer? string?)
  (cond [(= n 1) "1"]
        [else (string-join (map ~v (inner-permutation n (sub1 k) (range 1 (add1 n)))) "")]))

(println (get-permutation 3 1))
(println (get-permutation 3 3))
(println (get-permutation 4 9))
