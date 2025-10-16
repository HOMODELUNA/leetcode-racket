#lang racket
;; https://leetcode.cn/problems/word-search/description/?envType=study-plan-v2&envId=top-100-liked

(struct pos (r c) #:transparent)

(define/match (pos+ p1 p2)
  [((pos r1 c1) (pos r2 c2)) (pos (+ r1 r2) (+ c1 c2))])

(define expands (list (pos 1 0) (pos 0 1) (pos -1 0) (pos 0 -1)))

(struct mat (rows cols data) #:transparent)

(define/match (mat-member? m p)
  [((mat r c d) (pos pr pc)) (and (>= pr 0) (>= pc 0) (< pr r) (< pc c))])

(define/match (mat-ref m p)
  [((mat r c d) (pos pr pc)) (vector-ref (vector-ref d pr) pc)])

(define/match (in-mat-poses m)
  [((mat rows cols d)) (in-stream (for*/stream ([r rows] [c cols]) (pos r c)))])

(define (listlist->mat ll)
  (define r (length ll))
  (define c (length (car ll)))
  (mat r c (list->vector (map list->vector ll))))

(define (neighbors m p)
  (for/list ([d (in-list expands)]
             #:do [(define n (pos+ p d))]
             #:when (mat-member? m n))
    ;(printf "~a -> ~a~%" p n)
    n))

(define (paths m word)
  (match word
    ['() empty-stream]
    [(list c) (for/stream ([p (in-mat-poses m)] #:when (char=? c (mat-ref m p))) (list p))]

    [(cons c cs)
     (for*/stream ([former-path (in-stream (paths m cs))]
                   #:do[(define p (car former-path))]
                   [n (in-list (neighbors m p))]
                   #:when (and (not (member n former-path))
                               (char=? c (mat-ref m n))))
       ;(printf "~v find ~v~%" word n)
       (cons n former-path))]))
(define (char-consistent? board word)
  (define board-chars (for*/set ([row (in-list board)]
                                 [c (in-list row)])
                        c))
  (define word-chars (list->set (string->list word)))
  (subset? word-chars board-chars))

(define (has-path? board word)
  (define m (listlist->mat board))
  (define chars (string->list word))
  (not (stream-empty? (paths m chars))))

(define/contract (exist board word)
  (-> (listof (listof char?)) string? boolean?)
  (and (char-consistent? board word)
       (has-path? board word)))

(require "util/checker.rkt")
(define (make-board ls)
  (map string->list ls))
(test-to-answer exist (list (make-board (list "ABCE" "SFCS" "ADEE")) "ABCCED") #t)
(test-to-answer exist (list (make-board (list "ABCE" "SFCS" "ADEE")) "SEE") #t)
(test-to-answer exist (list (make-board (list "ABCE" "SFCS" "ADEE")) "ABCB") #f)
