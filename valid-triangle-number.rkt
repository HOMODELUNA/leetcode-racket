#lang racket

; https://leetcode.cn/problems/valid-triangle-number/submissions/713789580/
; 2026.3.31

(define (vector-lower-bound v start last value comp)
  (define count (- last start))
  (define (search first count)
    (if (<= count 0)
        first
        (let* ([step (quotient count 2)]
               [it (+ first step)])
          (if (comp (vector-ref v it) value)
              (search (+ it 1) (- count step 1))
              (search first step)))))
  (search start count))

(define (vector-upper-bound v start last value comp)
  (define count (- last start))
  (define (search first count)
    (if (<= count 0)
        first
        (let* ([step (quotient count 2)]
               [it (+ first step)])
          (if (not (comp value (vector-ref v it)))
              (search (+ it 1) (- count step 1))
              (search first step)))))
  (search start count))

(define/contract (triangle-number nums)
  (-> (listof exact-integer?) exact-integer?)
  (define vec (list->vector (sort nums <)))
  (define n (vector-length vec))
  (cond [(< n 3) 0]
        [else (for*/sum ([i1 (in-range 0 (- n 2))]
                         [i2 (in-range (add1 i1) (sub1 n))])
                (define max-edge (+ (vector-ref vec i1) (vector-ref vec i2)))
                (define limit (vector-lower-bound vec (add1 i2) n max-edge <))
                ; (printf "~a ~a ~a -> ~a~%" i1 i2 limit (sub1 (- limit i2)))
                (sub1 (- limit i2)))]))

(when #f
  (define v (vector 1 2 3 3 3 3 4 5 7 9 10))
  (define i1 (vector-lower-bound v 0 (vector-length v) 1.5 <))
  (define i2 (vector-lower-bound v 0 (vector-length v) 2 <))
  (define i3 (vector-lower-bound v 0 (vector-length v) 3 <))
  (define i4 (vector-upper-bound v 0 (vector-length v) 3 <))
  (println i1)
  (println i2)
  (println i3)
  (println i4))

(require "util/checker.rkt")

(test-to-answer triangle-number '((2 2 3 4)) 3)
(test-to-answer triangle-number '((4 2 3 4)) 4)
