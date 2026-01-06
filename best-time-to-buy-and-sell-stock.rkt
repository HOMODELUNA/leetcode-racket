#lang racket
; https://leetcode.cn/problems/best-time-to-buy-and-sell-stock/description/?envType=study-plan-v2&envId=top-interview-150
; https://leetcode.cn/problems/best-time-to-buy-and-sell-stock/submissions/689508411/?envType=study-plan-v2&envId=top-interview-150
; 2026.01.06

; `scanl` as in haskell
; see https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.22.0.0-66f8/Data-List.html#g:6
(define (scanl f start lst)
  (if (empty? lst) (list start)
      (let ([new-start (f start (car lst))])
        (cons new-start (scanl f new-start (cdr lst))))))

(define/match (drop-last lst)
  [('()) '()]
  [((list _)) '()]
  [((list x _)) (list x)]
  [((list x y xs ...))
   (if (empty? xs)
       (list x)
       (cons x (cons y (drop-last xs))))])

(define/contract (max-profit prices)
  (-> (listof exact-integer?) exact-integer?)
  (define min-befores (drop-last (scanl min 1000000 prices)))
  (println min-befores)
  (define (fold-max-price price former result)
    (cond [(>= former price) result]
          [(< result (- price former))
           (- price former)]
          [else result]))
  (foldl fold-max-price 0 prices min-befores))


(require "util/checker.rkt")
(test-to-answer max-profit '((7 1 5 3 6 4)) 5)
(test-to-answer max-profit '((7 6 4 3 1)) 0)
