#lang racket
;   https://leetcode.cn/problems/median-of-two-sorted-arrays/description/
(define (mean2  a b) (quotient (+ a b) 2))
(define (mean2-float  a b) (/ (+ a b) 2))

(define (lower-bound start end pred?)
  (cond [(>= start end) end]
        [(= 1 (- end start)) (if (pred? start) start end)]
        [else (let ([mid (mean2 start end)])
                (if (pred? mid)
                    (lower-bound start mid pred?)
                    (lower-bound mid end pred?)))]))

(define (do-part target-i vec1 start1 end1 vec2 start2 end2)
  ;(printf "target=~a [~a, ~a] [~a, ~a]~%" target-i start1 end1 start2 end2)
  (cond [(start1 . >= . end1) (vector-ref vec2  (+ target-i start2))]
        [(start2 . >= . end2) (vector-ref vec1 (+ target-i start1))]
        [((vector-ref vec1 (sub1 end1)) . <  . (vector-ref vec2 start2))
         (let ([l1 (- end1 start1)])
           (if (target-i . < . l1)
               (vector-ref vec1 (+ target-i start1))
               (vector-ref vec2  (+ (- target-i l1) start2))))]
        [((vector-ref vec2 (sub1 end2)) . <  . (vector-ref vec1 start1))
         (let ([l2 (- end2 start2)])
           (if (target-i . < . l2)
               (vector-ref vec2  (+ target-i start2))
               (vector-ref vec1 (+ (- target-i l2) start1))))]

        [else (let* ([l1 (- end1 start1)]
                     [l2 (- end2 start2)]
                     [pivot (if (l1 . < . l2)
                                (vector-ref vec2 (mean2 start2 end2))
                                (vector-ref vec1 (mean2 start1 end1)))]
                     [mid1 (lower-bound start1 end1 (λ (index) (>= (vector-ref vec1 index) pivot)))]
                     [greater1 (lower-bound start1 end1 (λ (index) (> (vector-ref vec1 index) pivot)))]
                     [mid2 (lower-bound start2 end2 (λ (index) (>= (vector-ref vec2 index) pivot)))]
                     [greater2 (lower-bound start2 end2 (λ (index) (> (vector-ref vec2 index) pivot)))]
                     [smaller-num (+ (- mid1 start1) (- mid2 start2))]
                     [leq-num (+ (- greater1 start1) (- greater2 start2))])
                (cond [(target-i . < . smaller-num)
                       (do-part target-i vec1 start1 mid1 vec2 start2 mid2)]
                      [(target-i . < . leq-num) pivot]
                      [else
                       (do-part (- target-i leq-num) vec1 greater1 end1 vec2 greater2 end2)]))]))

(define/contract (find-median-sorted-arrays nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) flonum?)
  (define vec1 (list->vector nums1))
  (define vec2 (list->vector nums2))
  (define len1 (vector-length vec1))
  (define len2 (vector-length vec2))
  (define total-length (+ len1 len2))
  (define mid (quotient total-length 2))
  (real->double-flonum (if (odd? total-length)
                           (do-part mid vec1 0 len1 vec2 0 len2)
                           (mean2-float (do-part mid vec1 0 len1 vec2 0 len2)
                                        (do-part (sub1 mid) vec1 0 len1 vec2 0 len2)))))



(require "util/checker.rkt")
(define ((on g f) . args) (apply g (map f args)))
(define cmp (= . on . real->double-flonum))
(test-to-answer find-median-sorted-arrays '((1 3) (2)) 2 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((1 2) (3 4)) 2.5 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((1 2 5) (3 4)) 3 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays (list '(1 2 5) (list )) 2 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((1 2 3) (4 5 6)) 3.5 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((0 0) (0 0)) 0 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((1 1) (0 2)) 1 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((2 2 4 4) (2 2 2 4 4)) 2 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((2 2 4 4) (2 2 3 4 4)) 3 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((1 2 4) (3 5)) 3 #:compare-by cmp)
(test-to-answer find-median-sorted-arrays '((3 5) (1 2 4)) 3 #:compare-by cmp)

