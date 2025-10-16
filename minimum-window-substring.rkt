#lang racket
; WA

(struct chentry (str start end chset) #:transparent)

(define/match (chentry-length ch)
  [((chentry _ start end _)) (- end start)])

(define/match (chentry-substring ch)
  [((chentry s start end _)) (substring s start end)])
(define/match (chentry-pair ch)
  [((chentry s start end _)) (cons start end)])

(define empty-chentry (chentry "" 0 0 (hash)))

(define (string->chset s)
  (for/fold ([acc (hash)])
            ([c (in-string s)])
    (hash-update acc c add1 0)))

(define (string->chentry s)
  (define l (string-length s))
  (define chs (string->chset s))
  (chentry s 0 l chs))

(define (chentry-extract-left ch [n 1])
  (if ((chentry-length ch) . < . n)
      (chentry "" 0 0 (hash))
      (let*([start (chentry-start ch)]
            [str (chentry-str ch)]
            [new-chset (for/fold  ([acc (chentry-chset ch)])
                                  ([k (in-range start (+ start n))])
                         (define c (string-ref str k))
                         (hash-update acc c sub1 1))])
        (chentry str (+ start n) (chentry-end ch) new-chset))))

(define (chentry-extract-right ch [n 1])
  (if ((chentry-length ch) . < . n)
      (chentry "" 0 0 (hash))
      (let*([end (chentry-end ch)]
            [str (chentry-str ch)]
            [new-chset (for/fold  ([acc (chentry-chset ch)])
                                  ([k (in-range (- end n) end)])
                         (define c (string-ref str k))
                         (hash-update acc c sub1 1))])
        (chentry str (chentry-start ch) (- end n) new-chset))))

(define (chentry-contains? che chs)
  (define s (chentry-chset che))
  (for/and ([(c n) (in-hash chs)])
    ((hash-ref s c 0) . >= . n)))

(define (find-while start f ok?)
  (let loop ([x start])
    (let ([next (f x)])
      (if (ok? next)
          (loop next)
          x))))

(define ((on g f) . args)
  (apply g (map f args)))

(define (my-min a b #:by f)
  (if ((<= . on . f) a b)
      a
      b))

(define/contract (min-window s t)
  (-> string? string? string?)
  (define init-che (string->chentry s))
  (define target-chs (string->chset t))
  (define former-results (make-hash))
  (define (ok? che)
    (define key (chentry-pair che))
    (printf "ok? ~a~%" che)
    (cond [(hash-has-key? former-results key)
           (hash-ref former-results key)]
          [(chentry-contains? che target-chs)
           (hash-set! former-results key #t)
           #t]
          [else (hash-set! former-results key #f)
                #f]))


  (define (apply-while f1 f2)
    (define mid (find-while init-che f1 ok?))
    (find-while mid f2 ok?))

  (if (not (ok? init-che))
      ""
      (chentry-substring (let loop ([current init-che])
                           (let ([try-right (chentry-extract-right current)]
                                 [try-left (chentry-extract-left current)]
                                 [try-lr (chentry-extract-left current)]
                                 [try-rl (chentry-extract-right current)])
                             (if (ok? try-left)
                                 (let ([try-mid (chentry-extract-right try-left)])
                                   (if (ok? try-mid)
                                       (loop try-mid)
                                       (let ([try-right (chentry-extract-right current)])
                                         (if (ok? try-right)
                                             (my-min (loop try-left) (loop try-right) #:by chentry-length)
                                             (loop try-left)))))
                                 (let ([try-right (chentry-extract-right current)])
                                   (if (ok? try-right)
                                       (loop try-right)
                                       current))))))))





(println (min-window "ADOBECODEBANC" "ABC"))
(println (min-window "a" "a"))
(println (min-window "a" "aa"))
(println (min-window "cabwefgewcwaefgcf" "cae")) ; cwae
