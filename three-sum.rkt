#lang racket

(define (tails-non-empty lst)
  (if (pair? lst)
      (cons lst (tails-non-empty (cdr lst)))
      '()))
(define (takef-during lst f)
  (takef (dropf lst (negate f)) f))
(define (repeat-cons n x xs)
  (if (n . <= . 0)
      xs
      (repeat-cons (sub1 n) x (cons x xs))))
(define ((equal-to a) b) (= a b))
(define (triple= tr1 tr2)
  (and (= (first tr1 ) (first tr2))
       (= (second tr1 ) (second tr2))))

(define (duplicate? triple triples)
  (and (pair? triples)
       (triple= triple (car triples))))

(define/contract (three-sum nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (let*-values ([(raw-positives negative-zeros) (partition positive? nums)]
                [(zeros raw-negatives) (partition zero? negative-zeros)]
                [(zero-num) (length zeros)]
                [(positives) (sort raw-positives >)]
                [(negatives) (sort raw-negatives <)])
    ; (println positives)
    ;(println negatives)
    (for*/fold ([res (if ((length zeros) . >= . 3) '( (0 0 0)) '())])
               ([ns (in-list  (tails-non-empty negatives))]
                [ps (in-list  (tails-non-empty positives))])
      (let* ([p (car ps)]
             [n (car ns)]
             [v (- 0 p n)]
             [triple (list n v p)])
        ;(println triple)
        (cond [(duplicate? triple res) res]
              [(= 0 v) (if (zero? zero-num) res (cons triple res))]
              [(and (v . < . 0) (pair? (cdr ns)))
               (repeat-cons (length (takef-during (cdr ns) (equal-to v)))
                            triple
                            res)]
              [(and (v . > . 0) (pair? (cdr ps)))
               (repeat-cons (length (takef-during (cdr ps) (equal-to v)))
                            triple
                            res)]
              [else res])))))

(require "util/checker.rkt")
(define ((on g f) . args)
  (apply g (map f args)))

(define/contract (violent nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (define sorted-nums (sort nums <))
  (for*/list([ns1 (in-list  (tails-non-empty sorted-nums))]
             #:when (pair? ns1)
             [ns2 (in-list (tails-non-empty (cdr ns1)))]
             #:when (pair? ns2)
             [ns3 (in-list (tails-non-empty (cdr ns2)))]
             #:when (and (pair? ns3)
                         (zero? ((+ . on . car) ns1 ns2 ns3))))
    (map car (list ns1 ns2 ns3))))
(test-to-answer three-sum '((0 0 0)) '((0 0 0)))
(test-to-answer three-sum '((0 0 0 0)) '((0 0 0)))
(test-to-answer three-sum '((-1 0 1 0)) '((-1 0 1)))
(test-to-answer three-sum '((1 2 -2 -1)) '())
(test-to-answer three-sum '((-1 0 1 2 -1 4)) '((-1 0 1) (-1 -1 2)))
(test-to-answer violent '((0 0 0)) '((0 0 0)))

(for ([size '(10 20 50 50 100 200 200 200 200 200 200 200)])
  (test-to-standard three-sum violent (list (gen-list size)) #:fail-fast #t #:compare-by set=? #:title size))
