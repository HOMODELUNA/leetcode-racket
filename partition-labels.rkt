#lang racket
; https://leetcode.cn/problems/partition-labels/description/?envType=study-plan-v2&envId=top-100-liked
(struct interval (letters data)
  #:methods gen:custom-write
  [(define (write-proc intv port mode)
     (write-string (interval->string intv) port))])

(define (interval->string interval)
  (list->string (flatten (interval-data interval))))
(define (interval-length interval)
  (length (flatten (interval-data interval))))

(define (interval-has-char? i c)
  (set-member? (interval-letters i) c))

(define (interval-merge intvs)
  (for/fold ([cs (set)]
             [data '()]
             #:result (interval cs data))
            ([intv (in-list intvs)])
    (values (set-union cs (interval-letters intv))
            (cons data (interval-data intv)))))

(define (string->intervals s)
  (for/list ([c (in-string s)])
    (interval (set c) c)))

(define (splitf-3 lst pred)
  (let*-values ([(ok) (negate pred)]
                [(f m-p) (splitf-at lst ok)]
                [(m p) (splitf-at-right m-p ok)])
    (values f m p)))

(define (check-merge ch intervals)
  (match intervals
    [(list i) intervals]
    [intervals
     (define-values (formers laps latters)
       (splitf-3 intervals (λ (itv) (interval-has-char? itv ch))))
     ; (printf "c=~a f=~a, m=~a, p=~a~%" ch formers laps latters)
     (append formers (list (interval-merge laps)) latters)]))

(define/contract (partition-labels s)
  (-> string? (listof exact-integer?))
  (define chars (for/set ([c (in-string s)])
                  c))
  (define intervals (foldl check-merge (string->intervals s) (set->list chars)))
  (map interval-length intervals))

(require "util/checker.rkt")

(test-to-answer partition-labels '("ababcbacadefegdehijhklij") '(9 7 8))
(test-to-answer partition-labels '("eccbbbbdec") '(10))
(test-to-answer partition-labels '("abbcccdddd") '(1 2 3 4))
