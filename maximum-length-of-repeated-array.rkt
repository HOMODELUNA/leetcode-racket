#lang racket
; <https://leetcode.cn/problems/maximum-length-of-repeated-subarray/description/
(define (tails-with-size lst n)
  (if (empty? lst) (list (cons '() 0)) (cons (cons lst n) (tails-with-size (cdr lst) (sub1 n)))))

(struct index-node (lst size) #:transparent)

; V -> listof index-node
(define (insert-with-order l v pred)
  (let-values ([(before after) (splitf-at l pred)])
    (append before (list v) after)))

(define (add-vn vs-n acc)
  (let ([vs (car vs-n)]
        [n (cdr vs-n)])
    (match vs
      ['() acc]
      [(cons v _)
       (hash-update
        acc
        v
        (λ (nodes)
          (insert-with-order nodes (index-node vs n) (λ (node) ((index-node-size node) . > . n))))
        '())])))

(define (match-to lst index-nodes)
  (let loop ([l '()]
             [n 0]
             [inodes index-nodes])
    (cond
      [(empty? inodes) (cons l n)]
      [((index-node-size (car inodes)) . < . n) (cons l n)]
      [else
       (match (take-common-prefix lst (index-node-lst (car inodes)))
         [(? pair? l1)
          (let ([n1 (length l1)])
            (if (n1 . > . n) (loop l1 n1 (cdr inodes)) (loop l n (cdr inodes))))]
         [_ #f])])))

(define (make-index-hash list-vs-n)
  (foldr add-vn (hash) list-vs-n))

(define/contract (find-length nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (define len1 (length nums1))
  (define len2 (length nums2))
  (define nums2-vs-n (tails-with-size nums2 len2))
  (define indexes (make-index-hash nums2-vs-n))
  (for/fold ([res '()]
             [res-len 0]
             #:result res-len)
            ([vs-n (in-list (tails-with-size nums1 len1))])
    #:break (< (cdr vs-n) res-len)
    #:break (empty? (car vs-n))
    (match (match-to (car vs-n) (hash-ref indexes (caar vs-n) '()))
      [(cons l n) (if (< res-len n) (values l n) (values res res-len))]
      [_ (values res res-len)])))

(require racket/generator)
(define (in-tails lst)
  (in-generator (let loop ([l lst])
                  (if (empty? l)
                      (yield l)
                      (begin
                        (yield l)
                        (loop (cdr l)))))))
(define (length-common-prefix l1 l2 [acc 0])
  (if (or (empty? l1) (empty? l2) (not (equal? (car l1) (car l2))))
      acc
      (length-common-prefix (cdr l1) (cdr l2) (add1 acc))))

(define/contract (violent nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (for*/fold ([res 0]
              [res-lst '()]
              #:result (begin ;(printf "[violent] res is ~a~%" res-lst)
                         res))
             ([l1 (in-tails nums1)]
              [l2 (in-tails nums2)])
    (let* ([l (take-common-prefix l1 l2)]
           [n (length l)])
      (if (n . > . res) (values n l) (values res res-lst)))))

(require "util/checker.rkt")

(test-to-answer find-length '((1 2 3 2 1) (3 2 1 4 7)) 3)
(test-to-answer violent '((1 2 3 2 1) (3 2 1 4 7)) 3)
(test-to-answer find-length '((0 0 0 0 0) (0 0 0 0 0)) 5)
(test-to-answer violent '((0 0 0 0 0) (0 0 0 0 0)) 5)

(define (gen-data size)
  (define (gen-list size)
    (build-list size (λ (x) (random 0 100))))
  (define (make-others common l-rest)
    (let* ([l-before (random 0 l-rest)]
           [l-after (- l-rest l-before)])
      (append (gen-list l-before) common (gen-list l-after))))
  (let* ([l-common (random 1 size)]
         [l-rest (- size l-common)]
         [commons (gen-list l-common)])
    (list (make-others commons l-rest) (make-others commons l-rest))))

(for ([size (make-list 100 10)])
  (test-to-standard find-length
                    violent
                    (gen-data size)
                    #:title size
                    #:detail #t
                    #:quiet #t
                    #:fail-fast #t))
(for ([size '(100 200 200 200 200 500 1000 1000)])
  (test-to-standard find-length violent (gen-data size) #:title size))
