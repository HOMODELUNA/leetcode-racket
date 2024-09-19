#lang racket
;   https://leetcode.cn/problems/majority-element/description/
(define/contract (majority-element nums)
  (-> (listof exact-integer?) exact-integer?)
  (car (foldl (λ (x acc)
                (cond
                  [(empty? acc) (cons x 1)]
                  [(eq? x (car acc)) (cons x (add1 (cdr acc)))]
                  [(= 0 (cdr acc)) (cons x 1)]
                  [else (cons (car acc) (sub1 (cdr acc)))]))
              '()
              nums)))

(println (majority-element '(3 2 3)))
(println (majority-element '(2 2 1 1 1 2 2)))
