#lang racket

(struct mat (rows cols arrarr) #:transparent)

(define/match (mat-ref m pos)
  [((? mat? m) (cons r c)) (vector-ref (vector-ref (mat-arrarr m) r) c)])

(define/match (mat-set! m pos v)
  [((? mat? m) (cons r c) v)
   (vector-set! (vector-ref (mat-arrarr m) r) c v)])

(define/match (mat-in? m p)
  [((mat rs cs _) (cons r c))
   (and (< c cs)
        (< r rs))])

(define (mat-head m pos)
  (let ([v (mat-ref m pos)])
    (if (equal? v pos)
        pos
        (mat-head m v))))

(define (connect? m from to)
  (and (mat-ref m from)
       (mat-ref m to)))

(require racket/generator)

(define (in-edges m)
  (in-generator #:arity 1
                (for* ([r (in-range 0  (mat-rows m))]
                       [c (in-range 0  (mat-cols m))])
                  (let ([p (cons r c)]
                        [p-right (cons r (add1 c))]
                        [p-down (cons (add1 r) c)])
                    (when (and (mat-in? m p-right) (connect? m p p-right))
                      (yield (cons p p-right)))
                    (when (and (mat-in? m p-down) (connect? m p p-down))
                      (yield (cons p p-down)))))))

(define (make-checkunion rows cols)
  (define data (for/vector #:length rows ([r (in-range rows)])
                 (build-vector cols (λ (c) (cons r c)))))
  (mat rows cols data))

(define (checkunion-process! chku edges)
  (for ([e edges]
        #:do[(define from (car e))
             (define to (cdr e))])
    ;(printf "edge ~a -> ~a~%" from to)
    (let ([h-from (mat-head chku from)]
          [h-to (mat-head chku to)])
      (when (not (equal? h-from h-to))
        (mat-set! chku to h-from)
        (mat-set! chku h-to h-from)))))

(define (checkunion-head? chku pos)
  (equal? pos (mat-ref chku pos)))

(define/contract (num-islands grid)
  (-> (listof (listof char?)) exact-integer?)
  (define rows (length grid))
  (define cols (length (car grid)))
  (define m (mat rows cols
                 (for/vector #:length rows ([l (in-list grid)])
                   (for/vector #:length cols ([ch (in-list l)])
                     (char=? #\1 ch)))))
  (define chku (make-checkunion rows cols))
  (checkunion-process! chku (in-edges m))
  (for*/fold ([res 0]) ([r (in-range rows)]
                        [c (in-range cols)]
                        #:do[(define pos (cons r c))]
                        #:when (and (mat-ref m pos)
                                    (checkunion-head? chku pos)))
    (add1 res)))


(require "util/checker.rkt")

(define (from-strings .  strs)
  (map string->list strs))

(test-to-answer num-islands
                (list (from-strings "11110"
                                    "11110"
                                    "11010"
                                    "11000"
                                    "00000"))
                1)
(test-to-answer num-islands
                (list (from-strings "11000"
                                    "11000"
                                    "00100"
                                    "00011"))
                3)
(test-to-answer num-islands
                (list (from-strings "1011"
                                    "1101"
                                    "0010"
                                    "1011"))
                4)

