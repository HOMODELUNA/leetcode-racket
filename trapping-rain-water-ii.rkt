#lang racket

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

(struct entry (x y h))

(define (entry<=? e1 e2)
  (<= (entry-h e1) (entry-h e2)))

; nodes: (set node)
; sizes: num of node
; bottom: block
; bound: int
(struct layer (nodes size bottom height bound sym) #:mutable #:transparent)

(define (layer-show ly)
  (string-join (cons (symbol->string (layer-sym ly))
                     (map (λ (p) (format "~v.~v" (car p) (cdr p))) (set->list (layer-nodes ly))))))

(define (set-filter f s)
  (for/set ([x (in-set s)]
            #:when (f x))
    x))

(define (layer-add-point! ly p)
  (set-layer-nodes! ly (set-add (layer-nodes ly) p))
  (set-layer-size! ly (set-count (layer-nodes ly))))

(define (point->layer p height)
  (define ly (layer (set p) 1 #f height 10000000 (gensym)))
  (set-layer-bottom! ly ly)
  ly)

(define/match (layer-water l)
  [((layer nodes size bottom height bound sym))
   (cond
     [(>= height bound)
      #;(printf "~a, s:~a h:~a, b:~a, nodes: ~a~%" 0 size height bound (layer-show l))
      0]
     [(zero? height)
      #;(printf "~a, s:~a h:~a, b:~a, nodes: ~a~%" 0 size height bound (layer-show l))
      0]
     [else
      #;(printf "~a, s:~a h:~a, b:~a, nodes: ~a~%"
                (* size (- bound height))
                size
                height
                bound
                (layer-show l))
      (* size (- bound height))])])

(define (map-ref m p)
  (vector-ref (vector-ref m (car p)) (cdr p)))

(define (map-rc-set! m r c v)
  (vector-set! (vector-ref m r) c v))

(define/contract (preprocess-map height-map)
  (-> (listof (listof exact-integer?)) (vectorof (vectorof exact-integer?)))
  (define height (length height-map))
  (define width (length (car height-map)))
  (define out-width (+ 2 width))
  (define out-height (+ 2 height))
  (define res (build-vector out-height (λ (h) (make-vector out-width 0))))
  (for ([r (in-naturals 1)]
        [row (in-list height-map)])
    (for ([c (in-naturals 1)]
          [v (in-list row)])
      (map-rc-set! res r c v)))
  res)

(define ((cons-with v) l)
  (set-add l v))

(define ((on g f) a1 a2)
  (g (f a1) (f a2)))

(define (hash-map-value! h f)
  (for ([k (in-hash-keys h)])
    (hash-update! h k f)))

(define (each-edge height width f)
  (define rfin (sub1 height))
  (define cfin (sub1 width))
  (for ([c (in-range 0 cfin)]
        #:do [(define r rfin)])
    (f (cons r c) (cons r (add1 c))))
  (for ([r (in-range 0 rfin)]
        #:do [(define c cfin)])
    (f (cons r c) (cons (add1 r) c)))
  (for* ([r (in-range 0 rfin)]
         [c (in-range 0 cfin)])
    (f (cons r c) (cons (add1 r) c))
    (f (cons r c) (cons r (add1 c)))))

; lower: hash block -> block
; upper: hash block -> block
; p-in-b: hash point -> block
(define (build-dag height-map)
  (define lower (make-hash))
  (define higher (make-hash))
  (define blocks (make-hash))
  (define (add-to-blocks b)
    (hash-set! blocks (layer-sym b) b))
  (define point-in-block (make-hash))
  (define (layer-height ly)
    (map-ref height-map (set-first (layer-nodes ly))))
  (define (block-of p)
    (hash-ref point-in-block p #f))
  (define (block-of/create p)
    (define b (block-of p))
    (cond
      [b b]
      [else
       (define new-b (point->layer p (map-ref height-map p)))
       (add-to-blocks new-b)
       (hash-set! point-in-block p new-b)
       new-b]))
  (define height (vector-length height-map))
  (define width (vector-length (vector-ref height-map 0)))

  (define (merge-block! b1 b2)
    (when (not ((= . on . layer-height) b1 b2))
      (error 'merge-block "height mismatch: ~a != ~a" (layer-height b1) (layer-height b2)))

    (define-values (b-small b-large)
      (if ((>= . on . layer-size) b1 b2)
          (values b2 b1)
          (values b1 b2)))
    #;(printf "  merge ~a to ~a~%" (layer-show b-small) (layer-show b-large))
    (hash-remove! blocks (layer-sym b-small))
    (for ([p (in-set (layer-nodes b-small))])
      (layer-add-point! b-large p)
      (hash-set! point-in-block p b-large))
    #;(printf "    blocks after merge: ~a~%" (hash-count blocks)))

  (define (process-same-block p1 p2)
    (define block1 (block-of p1))
    (define block2 (block-of p2))
    (match* (block1 block2)
      [(#f #f)
       (define b1 (point->layer p1 (map-ref height-map p1)))
       #;(printf "    new block for ~a ~a~%" p1 p2)
       (layer-add-point! b1 p2)
       (add-to-blocks b1)
       (hash-set! point-in-block p1 b1)
       (hash-set! point-in-block p2 b1)
       #;(printf "    blocks: ~a~%" (hash-count blocks))]
      [(b1 #f)
       (layer-add-point! b1 p2)
       (hash-set! point-in-block p2 b1)]
      [(#f b2)
       (layer-add-point! b2 p1)
       (hash-set! point-in-block p1 b2)]
      [(b1 b2)
       #:when (not (eq? b1 b2))
       (merge-block! b1 b2)]
      [(_ _) #f]))

  (define (process-lower p-low p-high)
    (define b-low (block-of/create p-low))
    (define b-high (block-of/create p-high))
    (hash-update! lower (layer-sym b-high) (cons-with b-low) set)
    (hash-update! higher (layer-sym b-low) (cons-with b-high) set))

  (define (create-block-if-none p1)
    (define b (block-of p1))
    (when (not b)
      (define new-b (point->layer p1 (map-ref height-map p1)))
      (hash-set! point-in-block p1 new-b)
      (add-to-blocks new-b)))
  (define (process-level-block p1 p2)
    (define h1 (map-ref height-map p1))
    (define h2 (map-ref height-map p2))
    (cond
      [(= h1 h2) (process-same-block p1 p2)]
      [else
       (create-block-if-none p1)
       (create-block-if-none p2)]))

  (each-edge height width (λ (p1 p2) (process-level-block p1 p2)))

  (define (process-low-high p1 p2)
    (define h1 (map-ref height-map p1))
    (define h2 (map-ref height-map p2))
    (cond
      [(= h1 h2) #f]
      [((eq? . on . block-of) p1 p2) #f]
      [(< h1 h2) (process-lower p1 p2)]
      [(< h2 h1) (process-lower p2 p1)]
      [else (error "unreachable")]))

  (each-edge height width (λ (p1 p2) (process-low-high p1 p2)))

  #;(for ([b (in-hash-values blocks)])
      (printf "block: ~v~%" (layer-show b)))
  #;(displayln "")

  #;(for* ([(sym-k bs-v) (in-hash lower)]
           [b-v bs-v])
      (define b-k (hash-ref blocks sym-k))
      (printf "lower: ~v to ~v, ~v to ~v~%"
              (layer-show b-k)
              (layer-show b-v)
              (layer-height b-k)
              (layer-height b-v)))
  #;(displayln "")
  #;(for* ([(sym-k bs-v) (in-hash higher)]
           [b-v bs-v])
      (define b-k (hash-ref blocks sym-k))
      (printf "higher ~v to ~v, ~v to ~v~%"
              (layer-show b-k)
              (layer-show b-v)
              (layer-height b-k)
              (layer-height b-v)))
  #;(displayln "")
  (define (in-blocks? b)
    (cond
      [(hash-has-key? blocks (layer-sym b)) #t]
      #;(printf " not in blocks: ~v~%" (layer-show b))
      [else #f]))

  (hash-map-value! higher (λ (bs) (set-filter (λ (b) (in-blocks? b)) bs)))
  (hash-map-value! lower (λ (bs) (set-filter (λ (b) (in-blocks? b)) bs)))
  (values blocks lower higher point-in-block))

(define (layer-find-bottom l)
  (define b (layer-bottom l))
  (cond
    [(eq? b l) b]
    [else
     (define new-bottom (layer-find-bottom b))
     (set-layer-bottom! l new-bottom)
     new-bottom]))

(define (layer-update-bottom! b-high b-low)
  (set-layer-bottom! b-high (layer-find-bottom b-low)))

(define (layer-update-bound! b-low b-high)
  (define new-bound (min (layer-bound b-low) (layer-bound b-high)))
  #;(printf "new bound ~a for ~a~%" new-bound (layer-show b-low))
  (set-layer-bound! b-low new-bound))

(define/contract (trap-rain-water heightMap)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define height-map (preprocess-map heightMap))
  (define-values (blocks lower higher point-in-block) (build-dag height-map))

  (for* ([(sym-low bs-high) (in-hash higher)]
         #:do [(define b-low (hash-ref blocks sym-low))]
         [b-high (in-set bs-high)])
    (layer-update-bottom! b-high b-low))
  (define layers (hash-values blocks))
  ;(define sorted-layers-descend (sort layers (λ (l1 l2) (set-member? (hash-ref higher l2 set) l1))))

  (define (is-peak? b-high)
    (define uppers (hash-ref higher (layer-sym b-high) #f))
    (or (not uppers) (set-empty? uppers)))
  (define tops
    (for/list ([b-high (in-list layers)]
               #:when (is-peak? b-high))
      b-high))
  (define sorted-layers-descend
    (let loop ([current tops])
      (cond [(empty? current) '()]
            [else
             (define next-row (for*/list ([b-high (in-list current)]
                                          #:do [(define key (layer-sym b-high))]
                                          [b-low (in-set (hash-ref lower key set))])
                                b-low))
             (append (sort current > #:key layer-height) (loop next-row))])))


  (for ([b (in-list tops)])
    (set-layer-bound! b (layer-height b)))
  ; 从高到低修剪
  (let loop ([current (sort tops > #:key layer-height)])
    (when (not (empty? current))
      (define next-row
        (for*/list ([b-high (in-list current)]
                    #:do [(define key (layer-sym b-high))]
                    [b-low (in-set (hash-ref lower key set))])

          (printf "update(high)  ~a (~a) to ~a (~a)~%"
                  (layer-show b-high)
                  (layer-bound b-high)
                  (layer-show b-low)
                  (layer-bound b-low))
          (layer-update-bound! b-low b-high)
          b-low))
      (loop next-row)))
  ; 从低到高修剪
  (define edge
    (for/first ([b (in-hash-values blocks)]
                #:when (zero? (layer-height b)))
      b))
  (set-layer-bound! edge 0)
  (let loop ([current (list edge)])
    (when (not (empty? current))
      (define next-row
        (for*/list ([b-low (in-list current)]
                    #:do [(define key (layer-sym b-low))]
                    [b-high (in-set (hash-ref higher key set))])
          (printf "update(low) ~a (~a) from ~a (~a)~%"
                  (layer-show b-high)
                  (layer-bound b-high)
                  (layer-show b-low)
                  (layer-bound b-low))
          (set-layer-bound! b-high (layer-height b-high))
          b-high))
      (loop next-row)))

  ; calc bounds for bottoms
  (for* ([l-high (in-list sorted-layers-descend)]
         [l-low (in-set (hash-ref lower (layer-sym l-high) set))])
    ;(printf "update from ~a (~a) to ~a (~a)~%" (layer-show l-high) (layer-bound l-high) (layer-show l-low) (layer-bound l-low) )
    (layer-update-bound! l-low l-high))

  (for/sum ([l (in-list layers)]) (layer-water l)))

(require "util/checker.rkt")

; (test-to-answer trap-rain-water '([[3 3 3 3 3] [3 2 2 2 3] [3 2 1 2 3] [3 2 2 2 3] [3 3 3 3 3]]) 10)
; (test-to-answer trap-rain-water '([[1 4 3 1 3 2] [3 2 1 3 2 4] [2 3 3 2 3 1]]) 4)
(test-to-answer trap-rain-water '([[9 9 9 9 9] [9 2 1 2 9] [9 2 8 2 9] [9 2 3 2 9] [9 9 9 9 9]]) 57)
