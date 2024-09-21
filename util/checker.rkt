#lang racket
(define (test-to-standard f
                          standard-f
                          data
                          #:title [title #f]
                          #:detail [detail #f]
                          #:quiet [quiet? #f]
                          #:fail-fast [fail-fast? #f])
  (let* ([ans (apply f data)]
         [expected (apply standard-f data)])
    (cond
      [title
       (if (equal? ans expected)
           (unless quiet?
             (printf "OK  :~a~%" title))
           (begin
             (printf "ERR :~a~%" title)
             (when detail
               (printf "    ~a => ~a != ~a~%" data ans expected))
             (when fail-fast?
               (exit))))])))

(define (test-to-answer f args expected)
  (let ([ans (apply f args)])
    (if (equal? ans expected)
        (printf "OK  :~a => ~a~%" args expected)
        (printf "ERR :~a => ~a != ~a~%" args ans expected))))

(provide test-to-answer
         test-to-standard)
