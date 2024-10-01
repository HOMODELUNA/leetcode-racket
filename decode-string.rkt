#lang racket
; RE

(define ((pc/char c) l)
  (cond
    [(empty? l) '()]
    [(char=? (car l) c) (printf "pc/char matched ~a ~a~%" c l )
                        (list l)]
    [else '()]))
(define (pc/string str)
  (define chars (string->list str))
  (define/match (f lst current)
    [(x '()) (list (cons str x))]
    [('() _) '()]
    [((cons c1 c1s) (cons c cs)) (if (char=? c1 c) (f c1s cs) '())])
  (λ (l) (f l chars)))

(define ((map-car f) p)
  (cons (f (car p)) (cdr p)))

(define ((pc-map f p) l)
  (map (map-car f) (p l)))

(define (pc-ok l)
  (list (cons '() l)))

(define ((pc-cons p1 p2) l)
  (for*/list ([x-l1 (p1 l)]
              #:do [(define x (car x-l1))]
              [y-l2 (p2 (cdr x-l1))]
              #:do [(define y (car y-l2))
                    (define remain (cdr y-l2))])
    (cons (cons x y) remain)))

; (define ((pc-cons p1 p2) l)
;   (append-map (λ (x-l1) (let ([x (car x-l1)])
;                              (map (map-car (curry cons x))
;                                   (p2 (cdr x-l1)))))
;               (p1 l)))
(define (pc-apply f . ps)
  (define parse-to-list (foldr pc-cons pc-ok ps))
  (λ (l) (map (map-car (λ (results) (apply f results))) (parse-to-list l))))
(define (pc-first . ps)
  (if (empty? ps)
      (const '())
      (let ([p1 (car ps)]
            [p-rest (pc-first (cdr ps))])
        (λ (l) (let ([res1 (p1 l)]) (if (pair? res1) res1 (p-rest l)))))))

(define (pc/many ok?)
  (define (f l acc)
    (cond
      [(empty? l) (list (cons (reverse acc) '()))]
      [(ok? (car l)) (f (cdr l) (cons (car l) acc))]
      [else (printf "many: remains ~a~%" l)(list (cons (reverse acc) l))]))
  (λ (l) (f l '())))

(define ((pc/one ok?) l)
  (cond
    [(empty? l) '()]
    [(ok? (car l)) (list l)]
    [else '()]))

(define (pc/at-least-one ok?)
  (pc-cons (pc/one ok?) (pc/many ok?)))

(define pc/number (pc-map (compose1 string->number list->string) (pc/at-least-one char-numeric?)))
(define pc/lower-cases (pc-map list->string (pc/at-least-one char-lower-case?)))

(define (string-repeat s n)
  (define len (string-length s))
  (build-string (* n len) (λ (i) (string-ref s (modulo i len)))))
(define ((pc-repeat p) l)
  (let loop ([acc '()]
             [l l])
    (if (empty? l) (list (cons (reverse acc) '()))
        (let ([res1 (p l)])
          (append-map (λ (x-l1) (let ([x (car x-l1)]
                                      [l1 (cdr x-l1)])
                                  (loop (cons x acc) l1)))
                      res1)))))

(define (pc/section lst)
  (printf "pc/section ~a~%" lst)
  (define pc/repeater
    (pc-apply (λ (n c str c2) (string-repeat str n))
              pc/number
              (pc/char #\[)
              (pc-map (λ (strs) (apply string-append strs)) (pc-repeat pc/section))
              (pc/char #\])))
  (cond
    [(empty? lst) '()]
    [(char-lower-case? (car lst)) (pc/lower-cases lst)]
    [(char-numeric? (car lst)) (pc/repeater lst)]
    [else '()]))


(define pc/total (pc-map (λ (strs) (apply string-append strs)) (pc-repeat pc/section)))

(define/contract (decode-string s)
  (-> string? string?)
  (define parsed-res (pc/total (string->list s)))
  (when (empty? parsed-res)
    (error "parse error" s))
  (car (first parsed-res)))

(require "util/checker.rkt")

(test-to-answer (pc/char #\A) (list (list #\A #\B)) (list (list #\A #\B)))
(test-to-answer (pc/char #\A) (list (list #\C #\B)) '())
(test-to-answer (pc/string "abc") (list (string->list "abcd")) (list (cons "abc" (list #\d))))
(println ((pc/at-least-one char-numeric?) (string->list "123abc")))
(println (pc/number (string->list "123abc")))
(test-to-answer pc/section (list (string->list "abcd")) (list (cons "abcd" '())))
(test-to-answer pc/section (list (string->list "3[cd]")) (list (cons "cdcdcd" '())))
(for ([in '("3[a]2[bc]" "3[a2[c]]")]
      [out '("aaabcbc" "accaccacc")])
  (test-to-answer decode-string (list in) out))
