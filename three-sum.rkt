#lang racket
; 2025.10.20
; https://leetcode.cn/problems/3sum/submissions/671959708/?envType=study-plan-v2&envId=top-100-liked

; v[b] 到 v[e] 中第一个满足 v[i] >= x 的 i
(define (vector-lower-bound v lt? x b e)
  (cond
    [(= b e) e]
    [(= e (add1 b)) (if (lt? (vector-ref v b) x) e b)]
    [else
     (define mid (quotient (+ b e) 2))
     (if (lt? (vector-ref v mid) x)
         (vector-lower-bound v lt? x mid e)
         (vector-lower-bound v lt? x b mid))]))

; v[b] 到 v[e] 中第一个满足 x < v[i]  的 i
(define (vector-upper-bound v lt? x b e)
  (cond
    [(= b e) e]
    [(= e (add1 b)) (if (lt? x (vector-ref v b)) b e)]
    [else
     (define mid (quotient (+ b e) 2))
     (if (lt? x (vector-ref v mid))
         (vector-upper-bound v lt? x b mid)
         (vector-upper-bound v lt? x mid e))]))

(define (vector-equal-range v lt? x b e)
  (values (vector-lower-bound v lt? x b e) (vector-upper-bound v lt? x b e)))

(define/contract (three-sum nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (define v-nums (vector-sort (list->vector nums) <))
  (define len (vector-length v-nums))
  (cond
    [(zero? len) '()]
    [(and (zero? (vector-ref v-nums 0))
          (zero? (vector-ref v-nums (sub1 len))))
     '((0 0 0))]
    [else
     (define nega-end (vector-lower-bound v-nums < 0 0 len))
     (define pos-begin (vector-upper-bound v-nums < 0 nega-end len))
     ;(printf "zeros ~a until ~a~%" nega-end pos-begin)
     (define already (mutable-set))
     (define nzp
       (for*/list ([nega-i (in-range 0 nega-end)]
                   [pos-k (in-range (sub1 len) (sub1 pos-begin) -1)]
                   #:when (> (- pos-k nega-i) 1)
                   #:do
                   [(define vi (vector-ref v-nums nega-i))
                    (define vk (vector-ref v-nums pos-k))
                    (define vpair (cons vi vk))]
                   #:when (not (set-member? already vpair) )
                   #:do[(define dest (- (+ vi vk)))
                        (define-values (mid-begin mid-end)
                          (vector-equal-range v-nums < dest (add1 nega-i) pos-k))
                        ;(printf "find between ~a(~a) and ~a(~a) -> [~a,~a) ~%" nega-i vi pos-k vk mid-begin mid-end)
                        ]
                   #:when (not (= mid-begin mid-end)))
         (set-add! already vpair)
         (list vi dest vk)))
     (define 3-zeros (>= (- pos-begin nega-end) 3))
     (if 3-zeros
         (cons '(0 0 0) nzp)
         nzp)
     ]))

(require "util/checker.rkt")
(define ((on g f) . args)
  (apply g (map f args)))

(test-to-answer vector-lower-bound (list (vector 1 1 2 3 5 7) < 3 0 6) 3)
(test-to-answer vector-lower-bound (list (vector 1 1 2 3 5 7) < 4 0 6) 4)
(test-to-answer vector-lower-bound (list (vector 1 1 2 3 5 7) < 9 0 6) 6)
(test-to-answer vector-lower-bound (list (vector 1 1 2 3 5 7) < 0 0 6) 0)
(test-to-answer vector-lower-bound (list (vector 1 1 2 3 5 7) < 2 0 6) 2)
(test-to-answer vector-upper-bound (list (vector 1 1 2 3 5 7) < 2 0 6) 3)

(test-to-answer three-sum '((0 0 0)) '((0 0 0)) #:compare-by set=?)
(test-to-answer three-sum '((0 0 0 0)) '((0 0 0)) #:compare-by set=?)
(test-to-answer three-sum '((-1 0 1 0)) '((-1 0 1)) #:compare-by set=?)
(test-to-answer three-sum '((-1 0 1)) '((-1 0 1)) #:compare-by set=?)
(test-to-answer three-sum '((1 2 -2 -1)) '() #:compare-by set=?)
(test-to-answer three-sum '((-1 0 1 2 -1 4)) '((-1 0 1) (-1 -1 2)) #:compare-by set=?)
(test-to-answer three-sum
                '((-4 -2 1 -5 -4 -4 4 -2 0 4 0 -2 3 1 -5 0))
                '((-5 1 4) (-4 0 4) (-4 1 3) (-2 -2 4) (-2 1 1) (0 0 0))
                #:compare-by set=?)
;[-4,-2,1,-5,-4,-4,4,-2,0,4,0,-2,3,1,-5,0]
;[[-5,1,4],[-4,0,4],[-4,1,3],[-2,-2,4],[-2,1,1],[0,0,0]]
