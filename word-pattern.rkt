#lang racket
; https://leetcode.cn/problems/word-pattern/submissions/689540467/?envType=study-plan-v2&envId=top-interview-150
; 2026.01.06

(define (words->nums words)
  (define h (make-hash))
  (let loop ([w words]
             [n 0])
    (cond [(empty? w) '()]
          [else (match (hash-ref h (car w) #f)
                  [#f (hash-set! h (car w) n)
                      (cons n (loop (cdr w) (add1 n)))]
                  [x (cons x (loop (cdr w) n ))])])))

(struct match-state (p2n n2p) #:transparent)

(define (empty-match-state) (match-state (hash) (hash)))

(define/match (match-state-set m p n)
  [((match-state p2n n2p) p n)
   (match-state (hash-set p2n p n) (hash-set n2p n p))])

(define/match (match-state-ref m p n)
  [((match-state p2n n2p) p n)
   (define n1 (hash-ref p2n p #f))
   (define p1 (hash-ref n2p n #f))
   (cond [(and (eqv? n n1) (eqv? p p1)) 'match]
         [(and (not n1) (not p1)) 'new]
         [else 'wrong-match])])

(define/match (match-pattern pattern s current)
  [('() '() _) #t]
  [(_ '() _) #f]
  [('() _ _) #f]
  [((cons p ps) (cons n ns) cur)
   ;(printf "~a ~a ~a~%" pattern s current)
   (match (match-state-ref cur p n)
     ['new (match-pattern ps ns (match-state-set cur p n))]
     ['wrong-match
      ;(printf "wrong-match: p=~a n=~a~%" p n)
      #f]
     ['match (match-pattern ps ns cur)])])

(define/contract (word-pattern pattern s)
  (-> string? string? boolean?)
  (define words (string-split s))
  (define nums (words->nums words))
  (match-pattern (string->list pattern) nums (empty-match-state)))

(require "util/checker.rkt")

(test-to-answer word-pattern '("abba" "dog cat cat dog") #t)
(test-to-answer word-pattern '("aaaa" "dog cat cat dog") #f)
(test-to-answer word-pattern '("abba" "dog cat cat fish") #f)
(test-to-answer word-pattern '("deadbeef" "d e a d b e e f") #t)
