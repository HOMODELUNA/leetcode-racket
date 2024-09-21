#lang racket
;   https://leetcode.cn/problems/trapping-rain-water/description/
(define ((on g f) . args) (apply g (map f args)))

(define/contract (trap heights)
  (-> (listof exact-integer?) exact-integer?)
  (define vec-heights (list->vector heights))
  (define (ref n) (vector-ref vec-heights n))

  (define (left-peak-index start end)
    (let loop ([i start])
      (cond [((add1 i) . >= . end) i]
            [((> . on . ref) i (add1 i)) i]
            [else (loop (add1 i))])))

  (define (right-peak-index start end)
    (let loop ([i (sub1 end)])
      (cond [( i . <= . start) i]
            [((> . on . ref) i (sub1 i)) i]
            [else (loop (sub1 i))])))

  (define (getmax-and-gather start end water-limit)
    (for/fold ([res-i #f] [water 0])
              ([index (in-range start end)])
      (values
       (if (or (not res-i)
               ((< . on . ref) res-i index))
           index
           res-i)
       (+ water (max 0 (- water-limit (ref index)))))))

  (define (do-part start end lwater rwater)
    (if (start . >= . end)
        0
        (let*-values ([(min-water) (min lwater rwater)]
                      [(max-i gathered) (getmax-and-gather start end min-water)]
                      [(iwater) (ref max-i)])
          (if (iwater . <= . min-water)
              gathered
              (+ (do-part start max-i lwater iwater )
                 (do-part (add1 max-i) end iwater rwater))))))

  (define len (vector-length vec-heights))
  (define lpeak-i (left-peak-index  0 len))
  (define rpeak-i (right-peak-index lpeak-i len))

  (do-part (add1 lpeak-i) rpeak-i (ref lpeak-i) (ref rpeak-i) vec-heights))

(require "util/checker.rkt")

(test-to-answer trap '((0 1 0 2 1 0 1 3 2 1 2 1)) 6)
(test-to-answer trap '((4 2 0 3 2 5)) 9)
(test-to-answer trap '((0 0 0 0)) 0)
(test-to-answer trap '((0 1 2 3 2 1 0)) 0)
(test-to-answer trap '((2 1 0 1 2)) 4)
(test-to-answer trap '((2 1 0 0 1 2 1)) 6)

