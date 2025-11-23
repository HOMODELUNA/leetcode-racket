#lang racket
; https://leetcode.cn/problems/course-schedule/submissions/580871370/?envType=study-plan-v2&envId=top-100-liked
; 不可变哈希表操作起来太慢了,还是得用可变哈希
(require racket/hash)

(struct graph (nodes edges rev-edges) #:transparent)

(define (remove-vertex! g v)
  (match g
    [(graph ns es regs)
     (define former-froms (hash-ref es v set))
     (define former-tos (hash-ref regs v set))
     (for ([from (in-set former-froms)])
       (set-remove! (hash-ref regs from) v))
     (for ([to (in-set former-tos)])
       (set-remove! (hash-ref es to) v))
     (set-remove! ns v)
     g]))

(define (read-graph ns edges)
  (define es (make-hash))
  (define regs (make-hash))
  (for ([edge (in-list edges)])
    (match edge
      [(list from to)
       (hash-update! es to
                     (λ (froms) (set-add! froms from) froms)
                     mutable-set)
       (hash-update! regs from
                     (λ (tos) (set-add! tos to) tos)
                     mutable-set)]))
  (graph (for/mutable-set ([x (in-range ns)]) x)
         es regs))

(define/match (acyclic? g)
  [((graph nodes edges regs))
   ;(println g)
   (define (zero-indegree? n)
     (or (not (hash-has-key? edges n))
         (set-empty? (hash-ref edges n))))
   (define zero-indegree-node (for/first ([n (in-set nodes)]
                                          #:when (zero-indegree? n))
                                n))
   ;(printf "zd=~a~%" zero-indegree-nodes)
   (or (set-empty? nodes)
       (hash-empty? edges)
       (and zero-indegree-node
            (acyclic? (remove-vertex! g zero-indegree-node))))
   ])

(define/contract (can-finish numCourses prerequisites)
  (-> exact-integer? (listof (listof exact-integer?)) boolean?)
  (acyclic? (read-graph numCourses prerequisites)))

(require "util/checker.rkt")

(test-to-answer can-finish '(2 ((1 0))) #t)
(test-to-answer can-finish '(2 ((1 0) (0 1))) #f)
