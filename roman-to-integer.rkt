#lang racket

(define/contract (roman-to-int s)
  (-> string? exact-integer?)
  (let loop ([lst (string->list s)]
             [acc 0])
    (match lst
      [(list #\I #\X s1 ...) (loop s1 (+ acc 9))]
      [(list #\I #\V s1 ...) (loop s1 (+ acc 4))]
      [(list #\I s1 ...) (loop s1 (+ acc 1))]
      [(list #\V s1 ...) (loop s1 (+ acc 5))]
      [(list #\X #\L s1 ...) (loop s1 (+ acc 40))]
      [(list #\X #\C s1 ...) (loop s1 (+ acc 90))]
      [(list #\X s1 ...) (loop s1 (+ acc 10))]
      [(list #\C #\D s1 ...) (loop s1 (+ acc 400))]
      [(list #\C #\M s1 ...) (loop s1 (+ acc 900))]
      [(list #\L s1 ...) (loop s1 (+ acc 50))]
      [(list #\C s1 ...) (loop s1 (+ acc 100))]
      [(list #\D s1 ...) (loop s1 (+ acc 500))]
      [(list #\M s1 ...) (loop s1 (+ acc 1000))]
      ['() acc]
      [_ (begin
           (printf "failed to match \"~a\"~%" (list->string lst))
           0)])))

(println (string->list "III"))

(println (roman-to-int "III"))