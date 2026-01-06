#lang racket
; 2026.01.06
; https://leetcode.cn/problems/best-time-to-buy-and-sell-stock-ii/submissions/689527530/?envType=study-plan-v2&envId=top-interview-150
; 1235ms

(define (pair-eqeq? p1 p2)
  (and (eq? (car p1) (car p2))
       (eq? (cdr p1) (cdr p2))))

(define-custom-hash-types eqeq-hash
  #:key? cons?
  pair-eqeq?)

; 股票的上升期和下降期, 期间的时间都可以忽略
(define/match (remove-continuous-interval lst [current-max #f])
  [('() #f) '()]
  [('() x) (list x)]
  [((cons y ys) #f) (remove-continuous-interval ys y)]
  [((list y) x) (if (>= x y) (list x) (list x y))]
  [((list y1 y2 ys ...) x)
   (cond [(>= x y1 y2) (remove-continuous-interval (cdr lst) x)]
         [(> x y1 y2) (remove-continuous-interval (cdr lst) x)]
         [else  (cons x (remove-continuous-interval (cdr lst) y1))])])


; x: 之前的最小值
; f #f [] = 0
; f #f [y] = 0
; f x [] = 0
; f x [y] = max 0 (- y x)
; f x y:ys
;  | x == y = f x ys
;  | x < y  = max (+ (- y x) (f y ys))
;                 (f x ys)
;  | else   = (f y ys)
(define/contract (max-profit prices)
  (-> (listof exact-integer?) exact-integer?)
  (define cache (make-mutable-eqeq-hash))
  (define (f current prices)
    (define key (cons current prices))
    (define c-res (dict-ref cache key #f))
    (or c-res
        (let ([res (inner-f current prices)])
          (dict-set! cache key res)
          res)))
  (define/match (inner-f current prices)
    [(x '()) 0]
    [(#f (list x)) 0]
    [(x (list y)) (max 0 (- y x))]
    [(#f (cons y ys)) (f y ys)]
    [(x (cons y ys))
     (cond [(= x y) (f x ys)]
           [(< x y) (max (+ (- y x) (f y ys)) ; 卖
                         (f x ys))] ; 不卖
           [else (f y ys)])]) ; 遇见更小的 x 值
  (f #f (remove-continuous-interval prices)))

(require "util/checker.rkt")

(test-to-answer max-profit '((7 1 5 3 6 4)) 7)
(test-to-answer max-profit '((1 2 3 4 5)) 4)
(test-to-answer max-profit '((7 6 4 3 1)) 0)
(test-to-answer max-profit '((3 2 1 3 2 1 3 2 1)) 4)
