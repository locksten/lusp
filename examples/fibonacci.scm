;; iterative fibonacci
(define (fib n)
  (let ((curr 1) (last 0))
    (for 0 n
         (lambda (x)
           (define tmp curr)
           (set! curr (+ last curr))
           (set! last tmp)))
    curr))

;; Example
(display "fibonacci-iter    0..9: ")
(for 0 9 (lambda (x) (begin (display (fib x))
                            (display #\ ))))
(newline)

;; recursive fibonacci
(define (fib-rec i)
  (if (zero? i) 0
    (if (= i 1) 1
      (+ (fib-rec (- i 1))
         (fib-rec (- i 2))))))

;; Example
(display "fibonacci-rec 0..9: ")
(for 0 9 (lambda (x) (begin (display (fib-rec x))
                            (display #\ ))))
(newline)
