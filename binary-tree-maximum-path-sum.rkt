#lang racket

; 2025.10.17
; https://leetcode.cn/problems/binary-tree-maximum-path-sum/description/?envType=study-plan-v2&envId=top-100-liked

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(struct summary (internal head))

(define/match (summarize-1 val child-node-summary)
  [(v (summary l-internal l-head))
   (define max-head (max v (+ l-head v)))
   (summary (max max-head l-internal) max-head)])

(define/match (summarize-2 val lchild-summary rchild-summary)
  [(v (summary l-internal l-head) (summary r-internal r-head))
   (define max-head (max v (+ l-head v) (+ r-head v)))
   (summary (max max-head l-internal r-internal (+ l-head r-head v)) max-head)])

(define/match (summarize n)
  [((tree-node v #f #f)) (summary v v)]
  [((tree-node v l #f)) (summarize-1 v (summarize l))]
  [((tree-node v #f r)) (summarize-1 v (summarize r))]
  [((tree-node v l r)) (summarize-2 v (summarize l) (summarize r))]
  [(#f) (error "unexpected #f node")])

(define/contract (max-path-sum root)
  (-> (or/c tree-node? #f) exact-integer?)
  (summary-internal (summarize root)))




(require "util/checker.rkt")

(define/match (list->segments lst [num 1])
  [('() n) '()]
  [(l n)
   #:when (< (length l) n)
   (list (append l (make-list (- n (length l)) #f)))]
  [(l n)
   (define-values (heads remains) (split-at lst num))
   (define valid-heads (filter values heads))
   (if (empty? remains)
       (list heads)
       (cons heads (list->segments remains (* 2 (length valid-heads)))))])

(define/match (not-null x)
  [((tree-node v _ _))
   #:when v
   x]
  [(_) #f])

(define/match (list->pairslist l)
  [((list x1 x2 xs ...)) (cons (cons x1 x2) (list->pairslist xs))]
  [(_) '()])

(define (seg-merge ls rs)
  (printf "ls = ~a, rs = ~a~%" ls rs)
  (if (not rs)
      (map (λ (l) (tree-node l #f #f)) ls)
      (let loop ([vs ls]
                 [r1r2s (list->pairslist rs)])
        (cond
          [(empty? vs) '()]
          [(empty? r1r2s) '()]
          [(not (car vs)) (cons #f (loop (cdr vs) r1r2s))]
          [else
           (define r1r2 (car r1r2s))
           (define r1 (not-null (car r1r2)))
           (define r2 (not-null (cdr r1r2)))
           (define n (tree-node (car vs) r1 r2))
           (cons n (loop (cdr vs) (cdr r1r2s)))]))))

(define (list->tree l)
  (define segments (list->segments l))
  (printf "segments = ~a~%" segments)

  (car (foldr seg-merge #f segments)))

(define (params . xs)
  (list (list->tree xs)))

(test-to-answer max-path-sum (params 1 2 3) 6)
(test-to-answer max-path-sum (params 1 -1 -1) 1)
; [-10,9,20,null,null,15,7]
(test-to-answer max-path-sum (params -10 9 20 #f #f 15 7) 42)
; [5,4,8,11,null,13,4,7,2,null,null,null,1]
(test-to-answer max-path-sum (params 5 4 8 11 #f 13 4 7 2 #f #f #f 1) 48)
