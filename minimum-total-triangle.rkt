#lang racket
; https://leetcode.cn/problems/triangle/?envType=study-plan-v2&envId=dynamic-programming
(define (fold-row row bottom)
  (for/list ([c (in-list row)]
             [ls (in-list bottom)]
             [rs (in-list (cdr bottom))])
    (+ c (min ls rs))))

(define/contract (minimum-total triangle)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (car (foldr fold-row (make-list (add1 (length triangle)) 0) triangle)))

(require "util/checker.rkt")

(test-to-answer minimum-total '([[2] [3 4] [6 5 7] [4 1 8 3]]) 11)
(test-to-answer minimum-total '([[-10]]) -10)
