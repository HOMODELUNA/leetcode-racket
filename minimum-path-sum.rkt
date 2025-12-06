#lang racket
; https://leetcode.cn/problems/minimum-path-sum/?envType=study-plan-v2&envId=dynamic-programming
; 2025.12.7

(struct mat (rows cols data) #:transparent)

(define (mat-ref m rc-pair)
  (let ([r (car rc-pair)]
        [c (cdr rc-pair)])
    (vector-ref (vector-ref (mat-data m) r) c)))

(define (listlist->mat lstlst)
  (let ([rows (length lstlst)]
        [cols (if (not (null? lstlst)) (length (first lstlst)) 0)])
    (mat rows cols (list->vector (map list->vector lstlst)))))

(define/contract (min-path-sum grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define mgrid (listlist->mat grid))
  (define rows (mat-rows mgrid))
  (define cols (mat-cols mgrid))
  (define cache (make-hash))
  (define (f r c)
    (let ([cached (hash-ref cache (cons r c) #f)])
      (or cached
          (let ([real-value (cond [(and (= rows (add1 r))
                                        (= cols (add1 c)))
                                   (mat-ref mgrid (cons r c))]
                                  [(or (>= r rows) (>= c cols)) 1000000]
                                  [else (define v (mat-ref mgrid (cons r c)))
                                        (define low (f (add1 r) c))
                                        (define right (f r (add1 c)))
                                        (+ v (min low right))])])
            (hash-set! cache (cons r c) real-value)
            real-value))))
  (f 0 0))

(require "util/checker.rkt")
(test-to-answer min-path-sum '([[1 3 1] [1 5 1] [4 2 1]]) 7)
(test-to-answer min-path-sum '([[1 2 3] [4 5 6]]) 12)


