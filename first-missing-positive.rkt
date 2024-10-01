#lang racket
;  <https://leetcode.cn/problems/first-missing-positive/submissions/568904313/
(define (with-hash-table nums #:debug [debug? #f])
  (define ht-starts (make-hash))
  (define ht-ends (make-hash))
  (for ([n (in-list nums)])
    (match/values (values (hash-ref ht-ends n #f) (hash-ref ht-starts (add1 n) #f))
                  [(#f #f) (hash-set! ht-ends (add1 n) n) (hash-set! ht-starts n (add1 n))]
                  [(#f end)
                   (hash-remove! ht-starts (add1 n))
                   (hash-set! ht-starts n end)
                   (hash-set! ht-ends end n)]
                  [(start #f)
                   (hash-remove! ht-ends n)
                   (hash-set! ht-ends (add1 n) start)
                   (hash-set! ht-starts start (add1 n))]
                  [(start end)
                   (hash-remove! ht-starts (add1 n))
                   (hash-remove! ht-ends n)
                   (hash-set! ht-ends end start)
                   (hash-set! ht-starts start end)]))
  (define smallest-interval (argmin car (hash->list ht-ends)))
  (if ((cdr smallest-interval) . > . 1) 1 (car smallest-interval)))

(define/contract (first-missing-positive nums)
  (-> (listof exact-integer?) exact-integer?)
  (let ([positives (remove-duplicates (filter positive? nums ))]) (if (empty? positives) 1 (with-hash-table positives))))

(require "util/checker.rkt")
(define (check args ans)
  (define nums (first args))
  (define ht
    (for/hash ([n (in-list nums)])
      (values n n)))
  (define (has? n)
    (hash-has-key? ht n))
  (and (not (has? ans))
       (for/and ([n (in-range 1 ans)])
         (has? n))))

(test-to-answer with-hash-table '((1 2)) 3)
(test-to-answer with-hash-table '((3 4 1)) 2)
(test-to-answer with-hash-table '((7 8 9 11 12)) 1)
(test-to-checker with-hash-table check '((7 8 9 11 12)))
(test-to-checker first-missing-positive check (list (takef '(2 -2 -1 1 2 1 -3 1 1 -3) positive?)) #:detail #t)

(for ([size (make-list 100 10)])
  (test-to-checker first-missing-positive
                   check #:quiet #t #:detail #t #:fail-fast #t
                   (list (gen-list size #:bounds '(-3 . 3)))))
(for ([size (make-list 100 100)])
  (test-to-checker first-missing-positive
                   check #:quiet #t #:detail #t #:fail-fast #t
                   (list (gen-list size #:bounds '(-100 . 100)))))

(for ([size (make-list 20 100000)])
  (test-to-checker first-missing-positive
                   check #:quiet #t #:detail #t #:fail-fast #t
                   (list (gen-list size #:bounds '(-10000 . 10000)))))
