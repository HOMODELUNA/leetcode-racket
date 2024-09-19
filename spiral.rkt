#lang racket
(require racket/generator)
;   https://leetcode.cn/problems/spiral-matrix/
(struct mat (vectors rows columns))
(define (mat-ref m xy)
  (vector-ref (vector-ref (mat-vectors m) (car xy)) (cdr xy)))

(define (in-reverse-range it-begin it-end)
  (in-generator (let loop ([current (sub1 it-end)])
                  (when (current . >= . it-begin)
                    (yield current)
                    (loop (sub1 current))))))

(define/contract (in-spiral-pos hbegin hend vbegin vend mode)
  (-> exact-integer? exact-integer? exact-integer? exact-integer? symbol? sequence?)
  (cond
    [(or (= hbegin hend) (= vbegin vend)) empty-sequence]
    [(eq? mode 'top)
     (sequence-append (sequence-map (λ (col) (cons vbegin col)) (in-range hbegin hend))
                      (in-spiral-pos hbegin hend (add1 vbegin) vend 'right))]
    [(eq? mode 'right)
     (let ([hlast (sub1 hend)])
       (sequence-append (sequence-map (λ (row) (cons row hlast)) (in-range vbegin vend))
                        (in-spiral-pos hbegin (sub1 hend) vbegin vend 'bottom)))]
    [(eq? mode 'bottom)
     (let ([vlast (sub1 vend)])
       (sequence-append (sequence-map (λ (col) (cons vlast col)) (in-reverse-range hbegin hend))
                        (in-spiral-pos hbegin hend vbegin (sub1 vend) 'left)))]
    [(eq? mode 'left)
     (sequence-append (sequence-map (λ (row) (cons row hbegin)) (in-reverse-range vbegin vend))
                      (in-spiral-pos (add1 hbegin) hend vbegin vend 'top))]
    [else (error "invalid mode" mode)]))

(define/contract (spiral-order matrix)
  (-> (listof (listof exact-integer?)) (listof exact-integer?))
  (let* ([row-size (length matrix)]
         [col-size (length (car matrix))]
         [vectors (for/vector #:length row-size
                              ([row (in-list matrix)])
                    (list->vector row))]
         [m (mat vectors row-size col-size)])
    (sequence->list (sequence-map (λ (rc) (mat-ref m rc))
                                  (in-spiral-pos 0 col-size 0 row-size 'top)))))
