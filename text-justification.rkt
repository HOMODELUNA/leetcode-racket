#lang racket

; 2025.10.17
; https://leetcode.cn/problems/text-justification/submissions/671317880/?envType=study-plan-v2&envId=top-interview-150

(struct line (words letters data) #:transparent)

(define empty-line (line 0 0 '()))

(define (line-cons w l)
  (line (add1 (line-words l))
        (+ (string-length w)
           (line-letters l))
        (cons w (line-data l))))

(define/match (line-minlength l)
  [((line ws ls _)) (+ ws ls -1)])

(define (words-find-group words max-width)
  (let loop ([ln empty-line]
             [ws words])
    (cond [(empty? ws)  ln]
          [else
           (define new-ln (line-cons (car ws) ln))
           (if (> (line-minlength new-ln) max-width)
               (cons ln ws)
               (loop new-ln (cdr ws)))])))

(define (words->groups words max-width)
  (if (empty? words) words
      (match (words-find-group words max-width)
        [(? line? ln) (list ln)]
        [(cons ln ws) (cons ln (words->groups ws max-width))])))

(define (groups-output groups width)
  (match groups
    [(list final-line) (list (line-output-final final-line width))]
    [(cons ln lns) (cons (line-output ln width) (groups-output lns width))]
    ['() '()]))

(define (line-output-final ln width)
  (define data (line-data ln))
  (define text-part (string-join (reverse data)))
  (define len(string-length text-part))
  (if (= width len)
      text-part
      (string-append text-part (make-string (- width len) #\space))))

(define (line-output ln width)
  (define slots (sub1 (line-words ln)))
  (cond
    [(zero? slots) (line-output-final ln width)]
    [else
     (define total-spaces (- width (line-letters ln)))
     (define-values (spaces remains) (quotient/remainder total-spaces slots))
     (define small-space (make-string spaces #\space))
     (define big-space (make-string (add1 spaces) #\space))
     (define small-spaces (- slots remains))
     (define join-parts (let loop ([n 0]
                                   [ws (line-data ln)]
                                   [acc '()])
                          (cond [(empty? ws) acc]
                                [(< n small-spaces)
                                 (loop (add1 n) (cdr ws) (list* small-space (car ws) acc))]
                                [(< n slots)
                                 (loop (add1 n) (cdr ws) (list* big-space (car ws) acc))]
                                [else
                                 (loop n (cdr ws) (cons (car ws) acc))])))
     (apply string-append join-parts)]))


(define/contract (full-justify words maxWidth)
  (-> (listof string?) exact-integer? (listof string?))
  (define groups (words->groups words maxWidth))
  (displayln groups)
  (groups-output groups maxWidth))


(require "util/checker.rkt")
(test-to-answer full-justify
                '(("This" "is" "an" "example" "of" "text" "justification.") 16)
                (list "This    is    an"
                      "example  of text"
                      "justification.  "))
(test-to-answer full-justify
                '(("What" "must" "be" "acknowledgment" "shall" "be") 16)
                (list "What   must   be"
                      "acknowledgment  "
                      "shall be        "))
(test-to-answer full-justify
                '(("Science" "is" "what" "we" "understand" "well" "enough" "to" "explain" "to" "a" "computer." "Art" "is" "everything" "else" "we" "do") 20)
                (list "Science  is  what we"
                      "understand      well"
                      "enough to explain to"
                      "a  computer.  Art is"
                      "everything  else  we"
                      "do                  "))





