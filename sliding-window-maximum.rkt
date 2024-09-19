#lang racket
;     https://leetcode.cn/problems/sliding-window-maximum/description/

(struct elem (value basesize lsub rsub) #:transparent)
(define (elem-size e)
  (if (elem? e) (elem-basesize e) 0))
(define (min-max a b #:cmp [less? <])
  (if (less? a b) (values a b) (values b a)))
(define ((on g f) . args)
  (apply g (map f args)))

(define/match (elem-add e x less?)
  [((elem v s l r) _ _)
   (let-values ([(smaller bigger) (min-max x v #:cmp less?)])
     ;(printf "elem add ~a~%" x)
     (if ((< . on . elem-size) l r)
         (elem smaller (add1 s) (elem-add l bigger less?) r)
         (elem smaller (add1 s) l (elem-add r bigger less?))))]
  [(_ _ _)
   (begin
     (printf "  elem add ~a~%" x)
     (elem x 1 '() '()))])

(define/match (elem-shift-top e less?)
  [((struct elem (v 0 _ _)) _) (values v '())]
  [((elem v s '() r) _) (values v r)]
  [((elem v s l '()) _) (values v l)]
  [((elem v s l r) _)
   (if ((less? . on . elem-value) l r)
       (let-values ([(v1 remain) (elem-shift-top l less?)])
         (values v (elem v1 (sub1 s) remain r)))
       (let-values ([(v1 remain) (elem-shift-top r less?)])
         (values v (elem v1 (sub1 s) l remain))))]
  [(_ _) (error "not a heap or it is empty")])

(define/match (elem-remove-one-if e delete? less?)
  [((elem v s l r) _ _)
   (if (delete? v)
       (let-values ([(shifted remains) (elem-shift-top e less?)])
         ;(printf "removed ~a~%" shifted)
         (values remains #t))
       (let-values ([(l-res l-success?) (elem-remove-one-if l delete? less?)])
         (if l-success?
             (values (elem v (sub1 s) l-res r) #t)
             (let-values ([(r-res r-success?) (elem-remove-one-if r delete? less?)])
               (if r-success? (values (elem v (sub1 s) l r-res) #t) (values e #f))))))]
  [(_ _ _) (values e #f)])

(define (scanl f acc lst)
  (if (empty? lst) (list acc) (cons acc (scanl f (f acc (car lst)) (cdr lst)))))
(define (enumerate lst [start 0])
  (if (empty? lst) '() (cons (cons (car lst) start) (enumerate (cdr lst) (add1 start)))))
(define car-greater? (> . on . car))

(define (tails lst)
  (if (empty? lst) '() (cons lst (tails (cdr lst)))))
(define/match (at-least? k l)
  [(0 _) '()]
  [(_ '()) #f]
  [(_ (cons x xs)) (at-least? (sub1 k) xs)])

(define/contract (violent nums k)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (map (λ (l) (apply max l)) (map (λ (l) (take l k)) (filter (curry at-least? k) (tails nums)))))

(define/contract (make-heap nums k)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define ((outdated-since x) e)
    ((- (cdr x) (cdr e)) . >= . k))
  (let*-values ([(prepares rests) (split-at (enumerate nums) (sub1 k))]
                [(init-heap) (foldl (λ (x h) (elem-add h x car-greater?)) '() prepares)])
    ;(println rests)
    (let loop ([h init-heap]
               [ns rests])
      (if (empty? ns)
          '()
          (let* ([x (car ns)]
                 [new-h
                  (if ((elem-size h) . >= . k)
                      (let-values ([(h1 _) (elem-remove-one-if h (outdated-since x) car-greater?)])
                        (elem-add h1 x car-greater?))
                      (elem-add h x car-greater?))])
            ;(printf "heap has ~a~%"  new-h)
            ;(printf "result has ~a~%" (car (elem-value new-h)))
            (cons (car (elem-value new-h)) (loop new-h (cdr ns))))))))

(struct section (bounds subs value base))
(define (make-bounds b e)
  (if (b . > . e) (error "expected begin <= end, but " b e) (cons b e)))

(define/match (overlap? p1 p2)
  [((cons b1 e1) (cons b2 e2)) (not (or (b1 . >= . e2) (b2 . >= . e1)))])
(define/match (total-include? p1 p2)
  [((cons b1 e1) (cons b2 e2)) (and (b1 . <= . b2) (e1 . >= . e2))])

(define/match (bounds-intersection p1 p2)
  [((cons b1 e1) (cons b2 e2)) (make-bounds (min (max b1 b2) e1 e2) (max (min e1 e2) b1 b2))])
(define ((overlap-with b1) b2)
  (overlap? b1 b2))
(define ((section-overlap-with b1) sec)
  (overlap? (section-bounds sec) b1))

;(takef  f (dropf (negate f) ( sequence)))
(define (find-interval lst f)
  (takef (dropf lst (negate f)) f))
(define (in-bounds b)
  (in-range (car b) (cdr b)))
(define (direct base bounds)
  (for/fold ([acc -99999]) ([i (in-bounds bounds)])
    (max acc (vector-ref base i))))
(define/match (section-max s bounds)
  [((section b subs v base) (cons b1 e1))
   ;(printf "on calculate ~a intersect ~a ~%" b bounds)
   (cond
     ;(printf "  return cached ~a ~%" (section-value s))
     [(bounds . total-include? . b) (section-value s)]
     [(not (overlap? b bounds)) (error b " and " bounds " not overlap")]
     [(not (empty? subs))
      (for/fold ([acc -99999])
                ([subsection (in-list (find-interval subs
                                                     (section-overlap-with
                                                      (bounds-intersection b bounds))))])
        (max acc (section-max subsection bounds)))]
     [else (direct base (bounds-intersection b bounds))])])
(define (bounds-size b)
  (- (cdr b) (car b)))

(define (bounds-split b s)
  (let loop ([start (car b)]
             [end (cdr b)])
    (if (<= end (+ start s))
        (list (cons start end))
        (cons (cons start (+ s start)) (loop (+ s start) end)))))

(define (build-section base k bounds)
  ;(printf "on build ~a~%" bounds)
  (cond
    ;(printf "  direct build ~a value=~a~%" bounds (sequence-fold max -99999 (in-bounds bounds)))
    [(< (bounds-size bounds) (max 8 (sqrt k))) (section bounds '() (direct base bounds) base)]
    [else
     (let* ([subsize (exact-ceiling (/ (bounds-size bounds) 4))]
            [subs (map (λ (b) (build-section base k b)) (bounds-split bounds subsize))]
            [value (apply max (map section-value subs))])
       (section bounds subs value base))]))

(define/contract (max-sliding-window nums k)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (if (< k 8)
      (violent nums k)
      (let* ([nums-v (list->vector nums)]
             [n (vector-length nums-v)]
             [sect-tree (build-section nums-v k (cons 0 n))])
        (sequence->list (sequence-map (λ (x) (section-max sect-tree (cons x (+ x k))))
                                      (in-range 0 (- (+ n 1) k)))))))

(println (max-sliding-window '(1 3 -1 -3 5 3 6 7) 3))
(println (max-sliding-window '(1) 1))
(println (max-sliding-window '(1 3 1 2 0 5) 3))
(define (test size)
  (let* ([nums (build-list size (λ (x) (random -10000 10000)))]
         [k (random 1 size)]
         [ans (max-sliding-window '(1 3 1 2 0 5) 3)]
         [expected (violent '(1 3 1 2 0 5) 3)])
    (if (equal? ans expected) (println "OK  ") (println "ERR "))))
(test 10)
(test 100)
(test 1000)
(test 10000)
(test 20000)
(test 100000)
