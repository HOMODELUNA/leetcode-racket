#lang racket
; https://leetcode.cn/problems/n-queens-ii/submissions/714040061/
; 2026.3.31
(define ((on g f) . args) (apply g (map f args)))

(define/match (attack? p1 p2)
  [((cons r1 c1) (cons r2 c2))
   (or (= r1 r2)
       (= c1 c2)
       ((= . on . abs) (- r1 r2) (- c1 c2)))])

(define ((attacked-by p) p2) (attack? p p2))

(define (first-row poses)
  (cond [(empty? poses) '()]
        [else (let ([r (caar poses)])
                (filter (λ (p) (= r (car p))) poses))]))

(define/match (place-queens n poses former-queens)
  [(0 _ qs) (list qs)]
  [(_ '() _) '()]
  [(n poses qs) (append-map (λ (p) (place-queens (sub1 n)
                                                 (filter-not (attacked-by p) poses)
                                                 (cons p qs)))
                            (first-row poses))])
(define (empty-plate n)
  (for*/list ([r (in-range n)]
              [c (in-range n)])
    (cons r c)))

(define/contract (total-n-queens n)
  (-> exact-integer? exact-integer?)
  (length
   (place-queens n (empty-plate n) '())))

(println (total-n-queens 1))
(println (total-n-queens 2))
(println (total-n-queens 3))
(println (total-n-queens 4))
(println (total-n-queens 5))
(println (total-n-queens 6))
(println (total-n-queens 7))
(println (total-n-queens 8))
(println (total-n-queens 9))
