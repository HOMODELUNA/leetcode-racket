#lang racket

; 2025.10.25
; https://leetcode.cn/problems/house-robber/description/?envType=study-plan-v2&envId=dynamic-programming

(define (accum-money current moneys )
  (cons (max (+ current (cadr moneys))
             (car moneys))
        moneys))

(define/contract (rob nums)
  (-> (listof exact-integer?) exact-integer?)
  (car (foldr accum-money (list 0 0) (cons 0 nums)))
  )

(require "util/checker.rkt")

(test-to-answer rob '((1 2 3 1)) 4)
(test-to-answer rob '((2 7 9 3 1)) 12)
