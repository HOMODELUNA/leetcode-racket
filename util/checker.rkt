#lang racket
(define (test-to-standard f
                          standard-f
                          data
                          #:title [title #f]
                          #:detail [detail #f]
                          #:quiet [quiet? #f]
                          #:fail-fast [fail-fast? #f]
                          #:compare-by [ok? equal?])
  (let* ([ans (apply f data)]
         [expected (apply standard-f data)])
    (cond
      [title
       (if (ok? ans expected)
           (unless quiet?
             (printf "OK  :~a~%" title))
           (begin
             (printf "ERR :~a~%" title)
             (when detail
               (printf "    ~a => ~a != ~a~%" data ans expected))
             (when fail-fast?
               (exit))))])))

(define (test-to-answer f args expected #:compare-by [ok? equal?])
  (let ([ans (apply f args)])
    (if (ok? ans expected)
        (printf "OK  :~a => ~a~%" args expected)
        (printf "ERR :~a => ~a != ~a~%" args ans expected))))

(define (gen-list size #:bounds [bounds (cons 0 10000)])
  (build-list size (λ (x) (random (car bounds) (cdr bounds)))))

(provide test-to-answer
         test-to-standard
         gen-list)

