#lang racket
; https://leetcode.cn/problems/minimum-insertion-steps-to-make-a-string-palindrome/description/?envType=study-plan-v2&envId=dynamic-programming
;;;; 辅助数据结构: 函数
(define ((on g f) . args) (apply g (map f args)))
(struct mat (rows cols data) #:transparent)

(define (make-mat row col [v #f])
  (mat row col (build-vector row (λ (_) (make-vector col v)))))
(define/match (mat-ref m pos)
  [((mat row col d) (cons r c))
   (if (and (< r row)
            (< c col)
            (>= r 0)
            (>= c 0))
       (vector-ref (vector-ref d r) c)
       #f)])

(define/match (mat-set! m pos value)
  [((mat row col d) (cons r c) v)
   (vector-set! (vector-ref d r) c v)])

(define/contract (min-insertions s)
  (-> string? exact-integer?)
  (define (at pos) (string-ref s pos))
  (define len (string-length s))
  (define m (make-mat len len))
  ; 递归做法,但是有表记录中间结果,以免算第二次
  (define (f l r)
    (define v
      (cond [(> l r) #f]
            [(= l r) 0]
            [(mat-ref m (cons l r)) (mat-ref m (cons l r))]
            [((char=? . on . at) l r)
             (if (= (add1 l) r)
                 0
                 (f (add1 l) (sub1 r)))]
            [else (add1 (min (f (add1 l) r)
                             (f l (sub1 r))))]))
    (mat-set! m (cons l r) v)
    v)
  (f 0 (sub1 (string-length s))))



;;;;
(define/contract (min-insertions/violent s)
  (-> string? exact-integer?)
  (define (at pos) (string-ref s pos))
  ; 递归做法
  (define (f l r)
    (cond [(> l r) #f]
          [(= l r) 0]
          [((char=? . on . at) l r)
           (if (= (add1 l) r)
               0
               (f (add1 l) (sub1 r)))]
          [else (add1 (min (f (add1 l) r)
                           (f l (sub1 r))))]))
  (f 0 (sub1 (string-length s))))


(require "util/checker.rkt")
(test-to-answer min-insertions '("zzazz") 0)
(test-to-answer min-insertions '("mbadm") 2)
(test-to-answer min-insertions '("leetcode") 5)
