#lang racket
; https://leetcode.cn/problems/is-subsequence/?envType=study-plan-v2&envId=top-interview-150
; https://leetcode.cn/problems/is-subsequence/submissions/712053111/?envType=study-plan-v2&envId=top-interview-150
; 2026.03.26

;  [] _ = true
;  _ [] = false
;  la@(a:as) lb@(b:bs) = if a == b
;                        then ok? as bs || ok? la bs
;                        else ok? la bs

(define/contract (is-subsequence s t)
  (-> string? string? boolean?)
  (define len-s (string-length s))
  (define len-t (string-length t))
  (let loop ([is 0]
             [it 0])
    (cond
      [(>= is len-s) #t]
      [(>= it len-t) #f]
      [else
       (define hs (string-ref s is))
       (define ht (string-ref t it))
       (if (char=? hs ht)
           (or (loop (add1 is) (add1 it)) (loop is (add1 it)))
           (loop is (add1 it)))])))

(require "util/checker.rkt")

(test-to-answer is-subsequence '("abc" "ahbgdc") #t)
(test-to-answer is-subsequence '("axc" "ahbgdc") #f)
