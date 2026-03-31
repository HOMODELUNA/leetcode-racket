#lang racket
; https://leetcode.cn/problems/trapping-rain-water-ii/submissions/713986301/
; 2026.3.31

; 参考: https://zhuanlan.zhihu.com/p/428944607

; 首先，最外层的一圈（边界）是不会接到任何雨水的（会从边界流出）。
;
; 我们定义从点 p = (x,y) 到边界的路径中出现的最大高度为「路径高度」，
; 路径高度 h
;  必然满足 h >= heightMap[x,y]
;
; 问题的本质是求「从点 (x,y) 到边界的所有路径高度的最小值为多少」，
;  这个路径高度的最小值与 (x,y) 本身的高度 heightMap[x,y]
;  之间的差值，即是该点能接到的雨水数量。


; 先前自己的思考:

; 0. 将输入图周围补一圈0
; 1. 将图按照等高关系分块, 然后按"相邻且高低差"分组
; 2. 对每个底层块, 找到它们到最高块的最小值
; 3. 对每个普通块,查找它们对应底层的水位,就是它们的水位
; 这个思路遭遇了反例 [[9 9 9 9 9] [9 2 1 2 9] [9 2 8 2 9] [9 2 3 2 9] [9 9 9 9 9]] 57

; leetcode 使用的racket 版本为8.15

(require data/heap)

(struct entry (r c h))

(define (entry<=? e1 e2)
  (<= (entry-h e1) (entry-h e2)))

(define (listlist->mat ll)
  (for/vector ([row (in-list ll)])
    (list->vector row)))

(define (mat-set-rc! m r c v)
  (vector-set! (vector-ref m r) c v))

(define (mat-ref m p)
  (vector-ref (vector-ref m (car p)) (cdr p)))

(define (mat-ref-rc m r c)
  (vector-ref (vector-ref m r) c))

(define (each-edge-node height width f)
  ;left
  (for ([r (in-range 0 height)])
    (f r 0))
  (when (> width 1)
    ;top
    (for ([c (in-range 1 width)])
      (f 0 c))
    ; right
    (define c-last (sub1 width))
    (for ([r (in-range 1 height)])
      (f r c-last))
    ; bottom
    (define r-last (sub1 height))
    (for ([c (in-range 1 c-last)])
      (f r-last c))))

(define dirs '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))

(define/contract (trap-rain-water heightMap)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define height-map (listlist->mat heightMap))
  (define height (vector-length height-map))
  (define width (vector-length (vector-ref height-map 0)))
  (define (in-limit? r c)
    (and (>= r 0) (>= c 0) (< r height) (< c width)))

  ; 队列和列表, 用于dijkstra
  (define q (make-heap entry<=?))
  (define vis (build-vector height (λ (_) (make-vector width #f))))
  (define (visited? r c) (mat-ref-rc vis r c))
  ; 预处理, 将边界节点加入队列
  (each-edge-node height width
                  (λ (r c)
                    (heap-add! q (entry r c (mat-ref-rc height-map r c)))
                    (mat-set-rc! vis r c #t)))
  ; dijkstra 本体
  (let loop ([ans 0])
    (cond [(zero? (heap-count q)) ans]
          [else
           (match-define (entry r c h) (heap-min q))
           (heap-remove-min! q)
           (define inc (for/sum ([d (in-list dirs)]
                                 #:do [(define nr (+ r (car d)))
                                       (define nc (+ c (cdr d)))]
                                 #:when (and (in-limit? nr nc))
                                 #:unless (visited? nr nc))
                         (define h-map (mat-ref-rc height-map nr nc))
                         (define new-ans (max 0 (- h h-map)))
                         (heap-add! q (entry nr nc (max h (mat-ref-rc height-map nr nc))))
                         (mat-set-rc! vis nr nc #t)
                         new-ans))
           (loop (+ ans inc))])))






;(each-edge-node 3 4 (λ (r c) (displayln (cons r c))))

(require "util/checker.rkt")

(test-to-answer trap-rain-water '([[3 3 3 3 3] [3 2 2 2 3] [3 2 1 2 3] [3 2 2 2 3] [3 3 3 3 3]]) 10)
(test-to-answer trap-rain-water '([[1 4 3 1 3 2] [3 2 1 3 2 4] [2 3 3 2 3 1]]) 4)
(test-to-answer trap-rain-water '([[9 9 9 9 9] [9 2 1 2 9] [9 2 8 2 9] [9 2 3 2 9] [9 9 9 9 9]]) 57)
