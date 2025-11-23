#lang racket

(struct pqnode (val left right size))
(define (pqnode-smaller? n1 n2)
  (< (pqnode-size n1) (pqnode-size n2)))

(struct priority-queue (heap gt?))

(define ((on g f) . args)
  (apply g (map f args)))

(define (heap-push n x gt?)
  (match n
    [#f (pqnode x #f #f 1)]
    [(pqnode v #f #f s)
     (if (gt? x v)
         (pqnode x n #f (add1 s))
         (pqnode v (pqnode x #f #f 1) #f (add1 s)))]
    [(pqnode v l #f s)
     (cond [(gt? x v) (pqnode x l (pqnode v #f #f 1) (add1 s))]
           [else (pqnode v l (pqnode x #f #f 1) (add1 s))])]
    [(pqnode v #f r s)
     (cond [(gt? x v) (pqnode x (pqnode v #f #f 1) r (add1 s))]
           [else (pqnode v (pqnode x #f #f 1) r (add1 s))])]
    [(pqnode v l r s)
     (define-values (big small) (if (gt? x v) (values x v) (values v x)))
     (if (pqnode-smaller? r l)
         (pqnode big l (heap-push r small) (add1 s))
         (pqnode big (heap-push l small) r (add1 s)))]))

(define (heap-pop n)
  (match n
    [#f (error "empty heap")]
    [(pqnode v l #f s)
     (values v l)]
    [(pqnode v #f r s)
     (values v r)]
    [(pqnode v l r s)
     (if (pqnode-smaller? l r)
         (let-values ([(v2 r2) (heap-pop r)])
           (values v (pqnode v2 l r2 (sub1 s))))
         (let-values ([(v2 l2) (heap-pop l)])
           (values v (pqnode v2 l2 r (sub1 s)))))]))

(define (priority-queue-push pq x)
  (match pq
    [(priority-queue heap gt?) (priority-queue (heap-push heap x gt?) gt?)]))
(define (priority-queue-pop pq)
  (match pq
    [(priority-queue heap gt?)
     (define-values (v h-rest) (heap-pop heap))
     (values v (priority-queue h-rest gt?))]))

(define (hamming-distance s1 s2)
  (for/sum ([c1 (in-string s1)]
            [c2 (in-string s2)])
    (if (char=? c1 c2) 0 1)))
(define (string-near? s1 s2)
  (= 1 (hamming-distance s1 s2)))

; 给定 w2, 有多少个词 w1 可以到达w2
(define (words->graph words)
  (define g (make-hash))
  (for* ([w1 (in-list words)]
         [w2    (in-list words)]
         #:when (and (not (eq? w1 w2))
                     (string-near? w1 w2)))
    (hash-update! g w2 (λ (l) (cons w1 l)) '()))
  g)

(define ((searched-in? mem) x)
  (hash-ref mem x #f))

(define (depth-first-search-with-memorize
         #:ok? ok?
         #:spread spread
         #:heuristic heuristic
         #:start start)
  (define current-path-length #f)
  (define current-path '())
  (let loop ([current start]
             [depth 0]
             [current-trace (list start)])
    (cond
      [(ok? current)
       (printf "find ~a~%" current-trace)
       (set! current-path-length depth)
       (set! current-path current-trace)
       current-trace]
      [(and current-path-length
            (> (+ depth (heuristic current))
               current-path-length))
       '()]
      [else
       (define spread-list (sort (spread current) < #:key heuristic))
       (for ([w (in-list spread-list)])
         (loop w (add1 depth) (cons w current-trace)))]))
  current-path)



(define/contract (ladder-length beginWord endWord wordList)
  (-> string? string? (listof string?) exact-integer?)
  (if (not (member endWord wordList))
      0
      (let* ([wg (words->graph wordList)]
             [path (depth-first-search-with-memorize
                    #:ok?  (λ (w) (string-near? w beginWord))
                    #:spread  (λ (w) (hash-ref wg w '()))
                    #:heuristic  (λ (w) (add1 (hamming-distance w beginWord)))
                    #:start  endWord)])
        (if (empty? path) 0 (add1 (length path))))))

(require "util/checker.rkt")

(test-to-answer ladder-length '("hit" "cog" ("hot" "dot" "dog" "lot" "log" "cog")) 5)
(test-to-answer ladder-length '("hit" "cog" ("hot" "dot" "dog" "lot" "log")) 0)
