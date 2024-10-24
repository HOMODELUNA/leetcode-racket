#lang racket
; https://leetcode.cn/problems/dungeon-game/description/

(struct mat (rows cols data) #:transparent)

(define (make-mat row col)
  (mat row col (build-vector row (λ (_) (make-vector col #f)))))

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

(define (listlist->matrix ll)
  (define rows (length ll))
  (define cols (length (car ll)))
  (mat rows cols (for/vector #:length rows ([r (in-list ll)])
                   (list->vector r))))

(define (in-descent-diagonal-order #:x-start [xstart 0] #:x-end xend #:y-start [ystart 0] #:y-end yend)
  (define xmax (sub1 xend))
  (define ymax (sub1 yend))
  (define (in-slice xy-sum)
    (cond [(or (>= xy-sum (sub1 (+ xend yend)))
               (< xy-sum (+ xstart ystart)))
           empty-stream]
          [else (for/stream ([x (in-range (max xstart (- xy-sum ymax))
                                          (min xend (add1 (- xy-sum ystart))))])
                  (cons x (- xy-sum x)))]))
  (define max-sum (sub1 (+ xend yend)))
  (define min-sum (+ ystart xstart))
  (apply stream-append (stream->list
                        (stream-map in-slice (in-inclusive-range max-sum min-sum -1)))))

(define/match (minimal-health-needed value bottom right)
  [(value #f #f) (max 1 (- 1 value))]
  [(v #f n) (max 1 (- 1 v) (- n v))]
  [(v n #f) (max 1 (- 1 v) (- n v))]
  [(v n1 n2) (max 1
                  (- 1 v)
                  (min (- n1 v) (- n2 v)))])

(define/match (bottom-to pos)
  [((cons r c)) (cons (add1 r) c)])
(define/match (right-to pos)
  [((cons r c)) (cons  r (add1 c))])

(define/contract (calculate-minimum-hp dungeon)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define dungeon-mat (listlist->matrix dungeon))
  (define row (mat-rows dungeon-mat))
  (define col (mat-cols dungeon-mat))
  (define healths-mat (make-mat row col))
  (define right-bottom (cons (sub1 row)
                             (sub1 col)))

  (mat-set! healths-mat right-bottom (mat-ref dungeon-mat right-bottom))

  (for ([pos (in-descent-diagonal-order #:x-end row #:y-end col)])
    (mat-set! healths-mat pos
              (minimal-health-needed (mat-ref dungeon-mat pos)
                                     (mat-ref healths-mat (bottom-to pos))
                                     (mat-ref healths-mat (right-to pos)))))

  (mat-ref healths-mat (cons 0 0)))


(require "util/checker.rkt")

(test-to-answer calculate-minimum-hp '(((-2 -3 3)
                                        (-5 -10 1)
                                        (10 30 -5))) 7)
(test-to-answer calculate-minimum-hp '(((0))) 1)
(test-to-answer calculate-minimum-hp '(((0 0))) 1)
