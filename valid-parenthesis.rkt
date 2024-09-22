#lang racket
;   https://leetcode.cn/problems/remove-invalid-parentheses/description/
(define (open-braket? c)
  (or (char=? c #\{) (char=?  #\(  c )  (char=? c #\[ )))

(define/contract (is-valid s)
  (-> string? boolean?)
  (let loop ([stack '()]
             [chars (string->list s)])
    (match/values (values stack chars)
                  [('() '()) #t]
                  [(_ '()) #f]
                  [((cons #\[ ss) (cons #\] cs)) (loop ss cs)]
                  [((cons #\( ss) (cons #\) cs)) (loop ss cs)]
                  [((cons #\{ ss) (cons #\} cs)) (loop ss cs)]
                  [(ss (cons c cs)) (if (open-braket? c) (loop (cons c ss) cs) #f)]
                  [(_ _) #f])))

(require "util/checker.rkt")

(test-to-answer is-valid '("{}") #t)
(test-to-answer is-valid '("()[]{}") #t)
(test-to-answer is-valid '("([]{}") #f)
(test-to-answer is-valid '("([])") #t)
(test-to-answer is-valid '("({[]})") #t)

