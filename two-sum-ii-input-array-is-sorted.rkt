#lang racket
; https://leetcode.cn/problems/two-sum-ii-input-array-is-sorted/submissions/712053822/?envType=study-plan-v2&envId=top-interview-150
; 2026.03.26

(define/contract (two-sum numbers target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define nums (list->vector numbers))
  (define n (vector-length nums))
  (let loop-i ([i 0])
    (cond [(>= i n) #f]
          [else (define ni (vector-ref nums i))
                (or (let loop-j ([j (add1 i)])
                      (cond [(>= j n) #f]
                            [else (define nj (vector-ref nums j))
                                  (define sum (+ ni nj))
                                  (cond [(= target sum) (list (add1 i) (add1 j))]
                                        [(< target sum) #f]
                                        [else (loop-j (add1 j))])]))
                    (loop-i (add1 i)))])))

(require "util/checker.rkt")

(test-to-answer two-sum '((5 25 75) 100) '(2 3))
