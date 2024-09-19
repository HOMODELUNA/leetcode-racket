#lang racket
;     https://leetcode.cn/problems/add-strings/
(define **int-of-0** (char->integer #\0))
(define (char->digit c)
  (- (char->integer c) **int-of-0**))
(define (digit->char c)
  (integer->char (+ c **int-of-0**)))

(define (string->digits str)
  (define N (string-length str))
  (build-list N (λ (i) (char->digit (string-ref str (- N 1 i))))))

(define (digits->string digits)
  (list->string (map digit->char (reverse digits))))

(define (add-one-digit . args)
  (quotient/remainder (apply + args) 10))

(define/match (add-digits digits1 digits2 [acc-outer 0])
  [('() res 0) res]
  [(res '() 0) res]
  [('() '() acc) (list acc)]
  [((cons d ds) '() acc)
   (let-values ([(new-acc out-d) (add-one-digit d acc)])
     (cons out-d (add-digits ds '() new-acc)))]
  [('() (cons d ds) acc)
   (let-values ([(new-acc out-d) (add-one-digit d acc)])
     (cons out-d (add-digits ds '() new-acc)))]
  [((cons d1 d1s) (cons d2 d2s) acc)
   (let-values ([(new-acc out-d) (add-one-digit d1 d2 acc)])
     (cons out-d (add-digits d1s d2s new-acc)))])

(define/contract (add-strings num1 num2)
  (-> string? string? string?)
  (digits->string (add-digits (string->digits num1) (string->digits num2))))

(define (test x y)
  (define x+y (number->string (+ x y)))
  (define actual (add-strings (number->string x) (number->string y)))
  (if (string=? actual x+y)
      (printf "OK  ~a + ~a = ~a~%" x y x+y)
      (printf "ERR ~a + ~a -> ~a != ~a ~%" x y actual x+y)))

(test 1 2)
(test 11 19)
(test 9999 31)
(test 999987 0)
(test 0 0)
(test 3 7)
