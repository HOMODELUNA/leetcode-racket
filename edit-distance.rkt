#lang racket
; https://leetcode.cn/problems/edit-distance/description/?envType=study-plan-v2&envId=dynamic-programming
; https://leetcode.cn/problems/edit-distance/submissions/683137605/?envType=study-plan-v2&envId=dynamic-programming
; 2025.12.7

(define/match (d l1 l2)
  [('() l) (length l)]
  [(l '()) (length l)]
  [((cons a as) (cons b bs)) #:when (char=? a b) (d as bs)]
  [((cons a as) (cons b bs)) (add1 (min (d as bs) (d l1 bs) (d as l2)))])


(define/contract (min-distance word1 word2)
  (-> string? string? exact-integer?)
  (define cache (make-hash))
  (define (cached-distance l1 l2)
    (define/match (distance l1 l2)
      [('() l) (length l)]
      [(l '()) (length l)]
      [((cons a as) (cons b bs)) #:when (char=? a b) (cached-distance as bs)]
      [((cons a as) (cons b bs)) (add1 (min (cached-distance as bs)
                                            (cached-distance l1 bs)
                                            (cached-distance as l2)))])
    (let ([cached (hash-ref cache (cons l1 l2) #f)])
      (or cached
          (let ([res (distance l1 l2)])
            (hash-set! cache (cons l1 l2) res)
            res))))
  (cached-distance (string->list word1) (string->list word2)))

(require "util/checker.rkt")

(test-to-answer min-distance '("horse" "ros") 3)
(test-to-answer min-distance '("intention" "execution") 5)
