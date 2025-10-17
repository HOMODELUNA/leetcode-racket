#lang racket

; 2025.10.17
; https://leetcode.cn/problems/candy/submissions/671297514/?envType=study-plan-v2&envId=top-interview-150

(struct entry (lseqs rseqs num rating) #:transparent)

(define (resolve n v l r)
  (match/values (values l r)
                [(#f #f) (entry 1 1 1 v)]
                [((entry lls lrs ln lv) #f)
                 (if (< lv v)
                     (entry (add1 lls) 0 (add1 ln) v)
                     (entry 0 0 1 v))]
                [(#f (entry rls rrs rn rv))
                 (if (> v rv)
                     (entry 0 (add1 rrs) (add1 rn) v)
                     (entry 0 0 1 v))]
                [((entry lls lrs ln lv) (entry rls rrs rn rv))
                 (define vls (if (< lv v) (add1 lls) 0))
                 (define vrs (if (> v rv) (add1 rrs) 0))
                 (define vn (add1 (max vls vrs)))
                 (entry vls vrs vn v)]
                [(_ _) (error "invalid argumet: " l r)]))

(define (make-table ratings)
  (define len (vector-length ratings))
  (define table (make-vector len #f))
  (define (calculate-if-greater old-n n)
    (cond
      [(or (< n 0) (>= n len)) #f]
      [else
       (define v (vector-ref ratings old-n))
       (define e (vector-ref ratings n))
       (if (> v e) (calculate n)
           (entry 0 0 0 (vector-ref ratings n)))]))

  (define (calculate n)
    (cond
      [(or (< n 0) (>= n len)) #f]
      [else (or (vector-ref table n)
                (let ([v (vector-ref ratings n)]
                      [l (calculate-if-greater n (sub1 n))]
                      [r (calculate-if-greater n (add1 n))])
                  (define res (resolve n v l r))
                  ;(printf "resolve ~a ~a ~a ~a -> ~a~%" n v l r res)
                  (vector-set! table n res)
                  res))]))
  (for ([i (in-range len)])
    (calculate i))
  ; (for ([e (in-vector table)]
  ;       [i (in-naturals)])
  ;   (printf "table ~a = ~a~%" i e))
  table)

(define/contract (candy ratings)
  (-> (listof exact-integer?) exact-integer?)
  (define table (make-table (list->vector ratings)))
  (for/sum ([e (in-vector table)]) (entry-num e)))



(require "util/checker.rkt")
(test-to-answer candy '((1 0 2)) 5)
(test-to-answer candy '((1 2 2)) 4)
(test-to-answer candy '((1 2 3 1)) 7)
; [1,2,87,87,87,2,1]
(test-to-answer candy '((1 2 87 87 87 2 1)) 13)
