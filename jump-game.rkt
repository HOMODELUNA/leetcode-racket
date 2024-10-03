#lang racket
; https://leetcode.cn/problems/jump-game/submissions/569896802/?envType=study-plan-v2&envId=top-100-liked

(module tle racket
  (define/match (jump-results nums)
    [('()) '()]
    [((list _)) (list #t)]
    [((list 0 xs ...)) (cons #f (jump-results xs))]
    [((list x xs ...)) (let ([latter-results (jump-results xs)])
                         (cons (for/or ([n (in-range x)])
                                 (list-ref latter-results n))
                               latter-results))])

  (define/contract (can-jump nums)
    (-> (listof exact-integer?) boolean?)
    (car (jump-results nums)))
  (displayln (jump-results '(2 3 1 1 4)))
  (displayln (jump-results '(3 2 1 0 4)))
  )

(module tle2 racket
  (require racket/generator)

  (define (in-reverse-inclusive-range start end)
    (in-generator (let loop ([x end])
                    (when (x . >= . start)
                      (yield x)
                      (loop (sub1 x))))))

  (define (greedy-search nums start steps former-results)
    (for/or
        ([increment (in-reverse-inclusive-range 1 steps)]
         #:do [(define next-start (+ start increment))])
      (can-jump/vec nums next-start former-results)))


  ;TLE
  (define/contract (can-jump/vec nums start former-results)
    (-> (vectorof exact-integer?) exact-integer? hash?  boolean?)
    ;(printf "start=~a cache=~a~%" start former-results)
    (define steps (vector-ref nums start))
    (cond [(hash-has-key? former-results start)
           (hash-ref former-results start) ]
          [((+ start steps) . >= . (sub1 (vector-length nums)))
           (hash-set! former-results start #t)
           #t
           ]
          [(zero? steps)
           (hash-set! former-results start #f)
           #f
           ]
          [else (let ([ok?
                       (greedy-search nums start steps former-results)])
                  (hash-set! former-results start ok?)
                  ok?)]))

  (define/contract (can-jump nums)
    (-> (listof exact-integer?) boolean?)
    (can-jump/vec (list->vector nums) 0 (make-hasheq)))
  )
(module tle3 racket
  (define (can-jump-search-range nums targets candidates)
    (printf "targets=~a candidates=~a~%" targets candidates)
    (define (can-reach? start)
      (define step (vector-ref nums start))
      (for/or ([t (in-list targets)])
        (and (< start t) (<= t (+ start step)))))
    (match targets
      ['() '()]
      [(list 0 xs ...) targets]
      [targets (can-jump-search-range
                nums
                (filter can-reach? candidates)
                (filter-not (λ (n) (member n targets)) candidates))]))

  (define/contract (can-jump nums)
    (-> (listof exact-integer?) boolean?)
    (define nums-vec (list->vector nums))
    (define len (vector-length nums-vec))
    (define candidates (range 0 len))
    (define m-res (can-jump-search-range nums-vec (list (sub1 len)) candidates))
    (match m-res
      [(list 0 xs ...) #t]
      [_ #f])))


(define ((in-l-open-r-close-interval start end) t)
  (and (< start t) (<= t end)))

(define/contract (can-jump nums)
  (-> (listof exact-integer?) boolean?)
  (define len (length nums))
  (define results (for/foldr ([trues (list (sub1 len))])
                    ([step (in-list nums)]
                     [start (in-range len)])
                    (if (ormap (in-l-open-r-close-interval start (+ start step))
                               trues)
                        (cons start trues)
                        trues)))
  (zero? (car results)))


(require "util/checker.rkt")

(test-to-answer can-jump '((2 3 1 1 4)) #t)
(test-to-answer can-jump '((3 2 1 0 4)) #f)
(test-to-answer can-jump '((0)) #t)

