#lang racket
; 2025.11.23
;https://leetcode.cn/problems/h-index/submissions/679988919/?envType=study-plan-v2&envId=top-interview-150
(define/contract (h-index citations)
  (-> (listof exact-integer?) exact-integer?)
  (define sorted (sort citations <))
  (define len (length sorted))
  (let loop ([n len]
             [cs sorted]
             [current 0])
    ;(printf "n=~v cs=~v current=~v ~%" n cs current)
    (cond
      [(empty? cs) current]
      [(<= n current) current]
      [(>= (car cs) n) (loop (sub1 n) (cdr cs) n)]
      [else (loop (sub1 n) (cdr cs) current)])))

(require "util/checker.rkt")

(test-to-answer h-index '((3 0 6 1 5)) 3)
(test-to-answer h-index '((1 3 1)) 1)
(test-to-answer h-index '((100)) 1)
