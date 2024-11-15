#lang racket
; https://leetcode.cn/problems/find-median-from-data-stream/submissions/580847998/?envType=study-plan-v2&envId=top-100-liked
;
(define ((on g f) . args) (apply g (map f args)))


; 二叉堆会TLE,因为插入一次log的代价还是太高了
; 用三叉堆试试
; 用四叉堆试试
(struct node (basesize value c1 c2 c3 c4) #:mutable #:transparent)

(define (node-size n)
  (if (node? n)
      (node-basesize n)
      0))
(define (minmax a b #:by prior?)
  (if (prior? b a)
      (values b a)
      (values a b)))

(define  (single-node x) (node 1 x #f #f #f #f))

(define (node-insert! n x prior?)
  (match n
    [(node bs v #f _ _ _)
     (define-values (front back) (minmax x v #:by prior?))
     (set-node-value! n front)
     (set-node-basesize! n (add1 bs))
     (set-node-c1! n (single-node back))]
    [(node bs v _ #f _ _)
     (define-values (front back) (minmax x v #:by prior?))
     (set-node-value! n front)
     (set-node-basesize! n (add1 bs))
     (set-node-c2! n (single-node back))]
    [(node bs v lc mc #f _)
     (define-values (front back) (minmax x v #:by prior?))
     (set-node-value! n front)
     (set-node-basesize! n (add1 bs))
     (set-node-c3! n (single-node back))]
    [(node bs v lc mc _ #f)
     (define-values (front back) (minmax x v #:by prior?))
     (set-node-value! n front)
     (set-node-basesize! n (add1 bs))
     (set-node-c4! n (single-node back))]
    [(node bs v c1 c2 c3 c4)
     (define-values (front back) (minmax x v #:by prior?))
     (set-node-value! n front)
     (set-node-basesize! n (add1 bs))
     (define smallest-sub (argmin node-basesize (list c1 c2 c3 c4)))
     (node-insert! smallest-sub back prior?)
     ]))

(define (node-shift! n x prior?)
  (match n
    [(node bs v lc mc rc c4) #:when (prior? x v) x]
    [(node bs v c1 c2 c3 c4)
     (define final-out v)
     (define final-v (let loop ([mid x] [subs (list c1 c2 c3 c4)])
                       (cond [(empty? subs) mid]
                             [(not (node? (car subs)))
                              (loop  mid (cdr subs))]
                             [else
                              (define new-mid (node-shift! (car subs) mid prior?))
                              (loop new-mid (cdr subs))])))
     (set-node-value! n final-v)
     final-out]
    ))

(struct heap ([head #:mutable] prior?) #:transparent)

(define (heap-empty? h)
  (not (node? (heap-head h))))

(define (empty-heap prior?)
  (heap #f prior?))

(define (heap-size h)
  (node-size (heap-head h)))

(define/match (heap-insert! h element)
  [((heap #f prior?) x) (set-heap-head! h (single-node x))]
  [((heap n prior?) x)
   (node-insert! n x prior?)])

(define/match (heap-shift! h element)
  [((heap n prior?) x)
   (node-shift! n x prior?)])


(define/match (heap-get h)
  [((heap (node _ v _ _ _ _) _)) v])

(define (mean2 a b)
  (/ (exact->inexact (+ a b)) 2))

(define median-finder%
  (class object%
    (super-new)

    (init-field)
    (define smalls (empty-heap >))
    (define greaters (empty-heap <))

    ; add-num : exact-integer? -> void?
    (define/public (add-num num)
      (cond [(and (heap-empty? smalls) (heap-empty? greaters))
             (heap-insert! smalls num)]
            [(heap-empty? smalls)
             (if (< num (heap-get greaters))
                 (heap-insert! smalls num)
                 (let ([smaller-x (heap-shift! greaters num)])
                   (heap-insert! smalls smaller-x)))]
            [(heap-empty? greaters)
             (if (> num (heap-get smalls))
                 (heap-insert! greaters num)
                 (let ([bigger-x (heap-shift! smalls num)])
                   (heap-insert! greaters bigger-x)))]
            ;现在smallers和greaters都不空了
            [((> . on . heap-size) smalls greaters)
             (if (> num (heap-get smalls))
                 (heap-insert! greaters num)
                 (let ([bigger-x (heap-shift! smalls num)])
                   (heap-insert! greaters bigger-x)))]
            [else
             (if (< num (heap-get greaters))
                 (heap-insert! smalls num)
                 (let ([smaller-x (heap-shift! greaters num)])
                   (heap-insert! smalls smaller-x)))])
      ;(printf "insert ~a~%" num)
      ;(printf "  smalls: ~a~%" smalls)
      ;(printf "  greaters ~a~%" greaters)
      )
    ; find-median : -> flonum?
    (define/public (find-median)
      (define ssize (heap-size smalls))
      (define gsize (heap-size greaters))
      (cond [(heap-empty? smalls)
             (heap-get greaters)]
            [(heap-empty? greaters)
             (heap-get smalls)]
            [(= ssize gsize)
             ((mean2 . on . heap-get) smalls greaters)]
            [(< ssize gsize)
             (heap-get greaters)]
            [else (heap-get smalls)]
            ))))

;; Your median-finder% object will be instantiated and called as such:
;; (define obj (new median-finder%))
;; (send obj add-num num)
;; (define param_2 (send obj find-median))



