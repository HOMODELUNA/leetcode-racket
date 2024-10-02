#lang racket
; https://leetcode.cn/problems/combination-sum/?envType=study-plan-v2&envId=top-100-liked
(define ((append-to l1) l2)
  (append l1 l2))

(define/match (inner-f candidates target)
  [('() _) '()]
  [(_ 0) (list (list))]
  [((list x) target)
   (let-values ([(q r) (quotient/remainder target x)])
     (if (zero? r)
         (list (make-list q x))
         '()))]
  [((list x xs ...) target)
   (let ([q (quotient target x)])
     ;(printf "  x=~a target=~a xs=~a~%" x target xs)
     (append-map (λ (n) (map (append-to (make-list n x))
                             (inner-f xs (- target (* n x)))))
                 (reverse (inclusive-range 0 q))))])

(define/contract (combination-sum candidates target)
  (-> (listof exact-integer?) exact-integer? (listof (listof exact-integer?)))
  (define sorted-candidates (sort candidates <))
  (inner-f sorted-candidates target))

(displayln (combination-sum '(2 3 6 7) 7))
(displayln (combination-sum '(2 3 5) 8))
(displayln (combination-sum '(2 3 5) 10))
(displayln (combination-sum '(2 3 4 6) 12))
(displayln (combination-sum '(1 2 4) 8))
