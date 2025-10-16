#lang racket
;; 2025.10.16
;; https://leetcode.cn/problems/longest-valid-parentheses/description/?envType=study-plan-v2&envId=top-100-liked

; pstr = '()'
;    | pstr pstr
;    | '(' pstr pstr ')'

(struct pstr (len) #:transparent)

(define (deduce-once l)
  (match l
    ['() '()]
    [(list #\( #\) xs ... ) (cons (pstr 2) (deduce-once xs))]
    [(list (pstr l1) (pstr l2) xs ...) (cons (pstr (+ l1 l2)) (deduce-once xs))]
    [(list #\( (pstr l) #\) xs ...) (cons (pstr (+ l 2)) (deduce-once xs))]
    [(list #\( #\( (pstr l) #\) #\) xs ...) (cons (pstr (+ l 4)) (deduce-once xs))]
    [(cons x xs) (cons x (deduce-once xs))]))

(define (deduce l)
  (define next (deduce-once l))
  (if (= (length l) (length next))
      (filter pstr? next)
      (deduce next)))

(define/contract (longest-valid-parentheses s)
  (-> string? exact-integer?)
  (define chars (string->list s))
  (define final-pstrs (deduce chars))
  (if (empty? final-pstrs)
      0
      (apply max (map pstr-len final-pstrs))))

(require "util/checker.rkt")
(test-to-answer longest-valid-parentheses '("(()") 2)
(test-to-answer longest-valid-parentheses '(")()())") 4)
(test-to-answer longest-valid-parentheses '("") 0)
