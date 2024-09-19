#lang racket
;   https://leetcode.cn/problems/merge-intervals/description/
(define (pair-less p1 p2)
  (let ([v1 (car p1)]
        [v2 (car p2)]
        [s1 (cdr p1)]
        [s2 (cdr p2)])
    (or (< v1 v2)
        (and (= v1 v2)
             (s1 . > . s2)))))

(define (merge-sorted xs ys less?)
  (cond
    [(empty? xs) ys]
    [(empty? ys) xs]
    [((car xs) . less? . (car ys)) (cons (car xs) (merge-sorted (cdr xs) ys less?))]
    [else (cons (car ys) (merge-sorted xs (cdr ys) less?))]))

(define (latest-begin intervals)
  (first (car intervals)))
(define (latest-end intervals)
  (second (car intervals)))

(define/contract (merge intervals)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (let* ([begins (map (λ (x) (cons x 1)) (sort (map first intervals) <))]
         [ends (map (λ (x) (cons x -1)) (sort (map second intervals) <))]
         [points (merge-sorted begins ends pair-less)])
    (let loop ([ps points]
               [v 0]
               [last-positive #f]
               [results '()])
      (if (empty? ps)
          results
          (let* ([current (car ps)]
                [pos (car current)]
                [sign (cdr current)])
            (cond
              [(empty? ps) results]
              [(positive? sign)
               (if (v . <= . 0)
                   (if (and (not (empty? results)) (= (latest-end results) pos))
                       (loop (cdr ps) 1 (latest-begin results) (cdr results))
                       (loop (cdr ps) 1 pos results))
                   (loop (cdr ps) (add1 v) last-positive results))]
              [(= v 1) (loop (cdr ps) 0 #f (cons (list last-positive pos) results))]
              [else (loop (cdr ps) (sub1 v) last-positive results)]))))))

(println (merge '((1 3) (2 6) (8 10) (15 18))))
(println (merge '((1 4) (4 5))))
