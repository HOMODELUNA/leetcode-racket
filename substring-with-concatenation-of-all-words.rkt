#lang racket

; 2025.10.18
; https://leetcode.cn/problems/substring-with-concatenation-of-all-words/submissions/671582215/?envType=study-plan-v2&envId=top-interview-150


(define (wp-length words)
  (for/sum ([(k v) (in-hash words)]) v))

(define (wp-add words w)
  (hash-update words w add1 (const 0)))

(define (wp-remove words w)
  (let ([h1 (hash-update words w sub1 (const 1))])
    (if (zero? (hash-ref h1 w 0))
        (hash-remove h1 w)
        h1)))

(define (cs->string cs b l)
  (list->string (for/list ([i (in-range b (+ b l))])
                  (vector-ref cs i))))

(define (list->wp words)
  (foldl (λ (w acc) (wp-add acc w)) (hash) words))

(define (slice-window->wp cs b e wl)
  (for/fold ([acc (hash)]) ([i (in-range b e wl)])
    (define w (cs->string cs i wl))
    (wp-add acc w)))

(define (wp-roll c-words cs i-begin i-end wlen)
  (define last-word (cs->string cs (- i-begin wlen) wlen))
  (define new-word (cs->string cs (- i-end wlen) wlen))
  (wp-add (wp-remove c-words last-word) new-word))

; 前条件: wp1 wp2 单词总数相等
(define (wp-same-frequency? wp1 wp2)
  (for/and ([(k v) (in-hash wp1)])
    (define v2 (hash-ref wp2 k 0))
    (= v v2)))

(define (slice-window cs words wlen i-begin i-end former-c-words)
  (cond [(> i-end  (vector-length cs)) empty-stream]
        [else (define c-words (if former-c-words
                                  (wp-roll former-c-words cs i-begin i-end wlen)
                                  (slice-window->wp cs i-begin i-end wlen)))
              (if (wp-same-frequency? c-words words)
                  (stream-cons #:eager i-begin (slice-window cs words wlen  (+ i-begin wlen) (+ i-end wlen) c-words))
                  (slice-window cs words wlen  (+ i-begin wlen) (+ i-end wlen) c-words))]))

(define (strm2 cs words wlen)
  (define cs-len (vector-length cs))
  (define total-words-len (* wlen (wp-length words)))
  (for/stream ([i-begin (in-range wlen)]
               #:do [(define i-end (+ i-begin total-words-len))]
               #:when (<= i-end cs-len)
               [i-result (in-stream (slice-window cs words wlen i-begin i-end #f))])
    i-result))

(define (string->vector s)
  (for/vector ([c (in-string s)])
    c))

(define/contract (find-substring s words)
  (-> string? (listof string?) (listof exact-integer?))
  (define wlen (string-length (car words)))
  (stream->list (strm2 (string->vector s) (list->wp words) wlen)))

(require "util/checker.rkt")

(test-to-answer find-substring '("barfoothefoobarman" ("foo" "bar")) (list 0 9))
(test-to-answer find-substring '("barfoofoothefoobarman" ("foo" "foo" "bar")) (list 0))
(test-to-answer find-substring '("wordgoodgoodgoodbestword" ("word" "good" "best" "word")) (list))
(test-to-answer find-substring '("barfoofoobarthefoobarman" ("bar" "foo" "the")) (list 6 9 12))
(test-to-answer find-substring '("wordgoodgoodgoodbestword" ("word" "good" "best" "good")) (list 8))
(test-to-answer find-substring (list (make-string 5000 #\a) (make-list 5000 "a")) (list 0))
