#lang racket
; https://leetcode.cn/problems/top-k-frequent-elements/?envType=study-plan-v2&envId=top-100-liked

(define ((on g f) . args)
  (apply g (map f args)))
(define (gather-frequency nums)
  (let ([h (make-hasheq)])
    (for ([n (in-list nums)])
      (hash-update! h n add1 0))
    (hash->list h)))

(define/contract (top-k-frequent nums k)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define freqs (sort (gather-frequency nums) (> . on . cdr)))
  (map car (take freqs k)))


(require "util/checker.rkt")

(test-to-answer top-k-frequent '((1 1 1 2 3 3) 2) '(1 2))
(test-to-answer top-k-frequent '((1) 1) '(1))
