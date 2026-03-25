#lang racket
; https://leetcode.cn/problems/minimum-size-subarray-sum/submissions/712055973/?envType=study-plan-v2&envId=top-interview-150
; 2026.03.26

(define (distance start fin)
  (- fin start))

(define (running-foldl f start lst)
  (cond [(empty? lst) (list start)]
        [else (define acc (f (car lst) start))
              (cons start (running-foldl f acc (cdr lst)))]))

(define/contract (min-sub-array-len target nums)
  (-> exact-integer? (listof exact-integer?) exact-integer?)
  (define sums (list->vector (running-foldl + 0 nums)))
  (define (sum-between start fin)
    (- (vector-ref sums fin) (vector-ref sums start)))
  (define (search-interval start fin)
    ;(printf "search ~a to ~a~%" start fin)
    (cond
      [(> start fin) #f]
      [(< (sum-between start fin) target) #f]
      [(= 1 (- fin start)) 1]
      [else
       (define dist1 (distance start fin))
       ;(printf "ok: ~a ~a, dist=~a~%" start fin (distance start fin))
       (define res-left (search-interval start (sub1 fin)))
       (cond
         [(not res-left)
          (define res-right (search-interval (add1 start) fin))
          (or res-right dist1)]
         [(= 1 res-left) 1]
         [else
          ; 在此之前都不用搜索了, 这样可以避免重复搜索
          (define r-start (add1 (- fin res-left)))
          (define res-right (search-interval r-start fin))
          (min res-left (or res-right res-left))])]))

  (or (search-interval 0 (sub1 (vector-length sums))) 0))

(require "util/checker.rkt")

(test-to-answer min-sub-array-len '(7 (2 3 1 2 4 3)) 2)
(test-to-answer min-sub-array-len '(4 (1 4 4)) 1)
(test-to-answer min-sub-array-len '(11 (1 1 1 1 1 1 1 1)) 0)
