(define (displayn . xs)
  (apply display xs) (newline))

(define (compose f g)
  (lambda (arg)
    (f (apply g arg))))

(define (list . xs) xs)

(define (curry func arg1)
  (lambda (arg)
    (apply func
           (cons arg1 (list arg)))))

(define (id x) x)
(define void (if #f '()))
(define (null? x) (eqv? x '()))
(define (not x) (if x #f #t))
(define (flip f) (lambda (a b) (f b a)))
(define (flip3 f) (lambda (a b c) (f c b a)))

(define zero?       (curry = 0))
(define positive?   (curry < 0))
(define negative?   (curry > 0))
(define (odd? x)    (= (modulo x 2) 1))
(define (even? x)   (= (modulo x 2) 0))
(define (square x)  (* x x))
(define (abs x)     (if (< x 0) (- x) x))
(define (sign x)    (round (/ x (abs x))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (foldr func end lst)
  (if (null? lst)
    end
    (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (unfold func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred))))

(define (append-two l m)
  (if (null? l) m
    (cons (car l) (append (cdr l) m))))

(define (append a . b)
  (foldl append-two a b))

;; improved curry
(define (curry func . curry-args)
  (lambda args
    (apply func (append curry-args args))))

(define list-tail
  (lambda (xs k)
    (if (zero? k)
      xs
      (list-tail (cdr xs) (- k 1)))))

(define (list-init xs k)
    (if (zero? k)
      '()
      (cons (car xs)
            (list-init (cdr xs) (- k 1)))))

(define (length lst)
  (foldl (lambda (x y) (+ x 1))
        0 lst))

(define (filter pred lst)
  (foldr
    (lambda (x y) (if (pred x) (cons x y) y))
    '() lst))

(define (reverse lst)
  (fold (flip cons) '() lst))

(define (max first . rest)
  (fold (lambda (old new)
          (if (> old new) old new))
        first rest))

(define (min first . rest)
  (fold (lambda (old new)
          (if (< old new) old new))
        first rest))

(define (distance x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

(define (force promise) (promise))

(define-macro delay
  (lambda (expr) `(lambda () ,expr)))

(define (list-ref l n)
  (if (zero? n)
    (car l)
    (list-ref (cdr l) (- n 1))))

(define (clamp a b x)
  (cond ((< x a) a)
        ((> x b) b)
        (else x)))

(define (member x xs)
  (if (null? xs)
    #f
    (if (eqv? x (car xs))
      xs
      (member x (cdr xs)))))

(define memq member)
(define memv member)

(define (cmd-line n)
  (if (< (length command-line) (+ n 1))
    (error "Insufficient number of command line arguments")
    (read (list-ref command-line n))))

(define current-working-dir (cmd-line 0))

(define (assoc x xs)
  (if (null? xs)
    #f
    (if (eqv? x (caar xs))
      (car xs)
      (assoc x (cdr xs)))))

(define assq assoc)
(define assv assoc)

; ;; Example
; (newline)
; (display "assoc ")
; (display (assoc 'b '((a 1)
;                      (b 2)
;                      (c 3))))


(define-macro when (lambda (test . branch)
                     (list 'if test
                           (cons 'begin branch))))

;; Example
; (newline)
; (when (= 2 2)
;   (display "when is true"))


(define for (lambda (start end func)
              (let ((index start))
                (if (> index end) void
                  (begin (func index) (for (+ index 1) end func))))))

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
; (newline)
; (display "fibonacci-iter 0..9: ")
; (for 0 9 (lambda (x) (begin (display (fib x))
;                             (display #\ ))))

;; recursive fibonacci
(define (fib-rec i)
  (if (zero? i) 0
    (if (= i 1) 1
      (+ (fib-rec (- i 1))
         (fib-rec (- i 2))))))

;; Example
; (newline)
; (display "fibonacci-rec 0..9: ")
; (for 0 9 (lambda (x) (begin (display (fib-rec x))
;                              (display #\ ))))

;; factorial
(define (factorial i)
  (if (zero? i) 1
    (if (= i 1) 1
      (* i (factorial (- i 1))))))

;; Example
; (newline)
; (display "factorial 5: ")
; (display (factorial 5))

;; multi-arg or
(define-macro or (lambda (x . xs)
                   (list 'if x #t
                         (if (null? xs)
                           #f
                           (cons 'or xs)))))

;; Example
; (newline)
; (display "or: ")
; (display (or (= 0 1)
;              (= 0 2)
;              (= 0 3)
;              (= 0 4)))

;; multi-arg and
(define-macro and (lambda (x . xs)
              (list 'if x
                    (if (null? xs)
                      #t
                      (cons 'and xs))
                    #f)))

;; Example
; (newline)
; (display "and: ")
; (display (and (= 1 1)
;               (= 2 2)
;               (= 3 3)
;               (= 0 4)))


(define-macro let*
  (lambda (bindings . body)
    (if (null? bindings)
      `(let () ,@body)
      `(let ,(list (car bindings))
         ,(append
            (list 'let* (cdr bindings))
            body)))))

;; Example
; (newline)
; (display "let*\n")
; (let* ((a 1)
;        (b (+ a 10))
;        (c (+ a b 100)))
;   (display a) (newline)
;   (display b) (newline)
;   (display c) (newline))


(define-macro cond
  (lambda (x . xs)
    (let* ((test (car x))
           (expr (if (null? (cdr x))
                   test
                   (if (and (symbol? (cadr x))
                            (eqv? "=>" (symbol->string (cadr x))))
                     (apply (eval (caddr x))
                            (list (eval test)))
                     (cadr x)))))
      (list 'if (and (symbol? test)
                     (eqv? (symbol->string test) "else"))
            expr
            (list 'if test
                  expr
                  (if (null? xs)
                    'void
                    (cons 'cond xs)))))))

; ;; Example
; (define (false-or-num x)
;   (if (> x 3) x #f))
; (newline)
; (display "cond: ")
; (display
;   (cond ((= 0 1) "ALPHA")
;         ((false-or-num 2) => square)
;         ((false-or-num 4) => square)
;         ((= 0 3) "BETA")
;         (else "ELSE")))


; (define-macro case
;   (lambda (key . clauses)
;     (if (null? clauses)
;       (error "non-exhaustive matches in case")
;       (let* ((datums (caar clauses))
;              (expr (cons 'begin
;                          (cdar clauses))))
;         (list 'if (and (symbol? datums)
;                        (eqv? (symbol->string datums) "else"))
;               expr
;               (list 'if
;                     (list 'member key (list 'quote datums))
;                     expr
;                     (if (null? clauses)
;                       (error "non-exhaustive matches in case")
;                       (cons 'case
;                             (cons key
;                                   (cdr clauses))))))))))

(define-macro case
  (lambda (key . clauses)
    (if (null? clauses)
      (error "non-exhaustive matches in case")
      (let* ((datums (caar clauses))
             (expr (cons 'begin
                         (cdar clauses))))
        (list 'if (and (symbol? datums)
                       (eqv? (symbol->string datums) "else"))
              expr
              (list 'if
                    (list 'member key (list 'quote datums))
                    expr
                    (if (null? clauses)
                      (error "non-exhaustive matches in case")
                      (cons 'case
                            (cons key
                                  (cdr clauses))))))))))

; ;; Example
; (newline)
; (display
;   (case (car '(c d))
;     ((a e i o u) 'vowel)
;     ((w y) 'semivowel)
;     (else 'consonant)))

(define (for-each f l)
  (cond ((not (null? l))
         (let ()
           (f (car l))
           (for-each f (cdr l))))))

; ;; Example
; (for-each
;   (lambda (x) (newline) (display x))
;   '(1 2 3))


(define (string . xs)
  (list->string xs))

(define (string-length xs)
  (length (string->list xs)))

(define (string->number xs)
  (let ((num (read xs)))
    (if (number? num)
      num
      #f)))

(define (string-append . xs)
  (list->string
    (apply append
           (map string->list xs))))

(define (string-ref xs k)
  (list-ref (string->list xs) k))

(define (sublist xs start end)
  (list-init (list-tail xs start) (- end start)))

(define (substring xs start end)
  (list->string (sublist (string->list xs) start end)))


(define (zip-with f as bs)
  (if (or (null? as)
          (null? bs))
    '()
    (cons (f (car as)
             (car bs))
          (zip-with f
                    (cdr as)
                    (cdr bs)))))

; ;; Example
; (newline)
; (display (zip-with + '(1 2 3) '(10 20 30)))


(define (zip as bs)
  (zip-with list as bs))

; ;; Example
; (newline)
; (displayn (zip '(1 2 3) '(a b c)))


(define (range a b)
  (if (= a b)
    '()
    (cons a
          (range (+ a 1) b))))


(define (split-on xs p)
  (if (null? xs)
    '(())
    (let ((split-on_cdr (split-on (cdr xs) p)))
      (if (p (car xs))
        (if (and (not (null? (cdr xs)))
                 (p (cadr xs)))
          split-on_cdr
          (cons '() split-on_cdr))
        (cons (cons (car xs)
                    (car split-on_cdr))
              (cdr split-on_cdr))))))


(define (split-string-on xs p)
  (map list->string
       (split-on (string->list xs) p)))

; ;; Example
; (newline)
; (displayn
;   (split-string-on "abc def ghi" char-whitespace?))


; (define-macro export
;     (lambda (. xs)
;       `(set! ,@xs)))

; (define-macro module
;     (lambda (declarations . body)
;       (append
;         (cons 'begin
;               (map (lambda (x) (list 'define x 'void)) declarations))
;         (list `(let () ,@body)))
;       ))


(define-macro export
    (lambda (. xs)
      `(set! ,@xs)))

(define-macro module
    (lambda (declarations . body)
      (append
        (cons 'begin
              (map
                (lambda (x) (list 'define x 'void))
                declarations))
        (list `(let () ,@body)))))

; ;; Example
; (module
;   (pow-4
;    pow-16)

; (define (helper-func x)
;   (* x x))

; (define (four x)
;   (helper-func (helper-func x)))

; (define (sixteen x)
;   (four (four x)))

; (export pow-4 four)
; (export pow-16 sixteen)
; )
