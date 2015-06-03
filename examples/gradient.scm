(define (get-gradient gradient f x y)
  (list-ref gradient
            (clamp 0 (- (length gradient) 1)
                   (round (* (length gradient)
                             (f x y))))))

(define (display2d width height f)
  (for 0 height (lambda (y)
                  (let ((line '()))
                    (for 0 width (lambda (x)
                                   (set! line (cons (f x y) line))))
                    (display (list->string line)))
                  (newline))))

(define (display-gradient gradient width height f)
  (display2d width height (lambda (x y)
                            (get-gradient gradient f x y))))

(define (radial-gradient x y start_x start_y radius)
  (/ (distance start_x start_y x y)
     radius))

(define (linear-gradient x y p1x p1y p2x p2y)
  (let ((sz2 (+ (expt (- p2x p1x) 2)
                (expt (- p2y p1y) 2))))
    (let ((vx (/ (- p2x p1x) sz2))
          (vy (/ (- p2y p1y) sz2)))
      (+ (* (- x p1x) vx)
         (* (- y p1y) vy)))))

(define help
"Usage: <height> <width> <type> <arguments>
types:
  radial     x y r
  linear     x y x2 y2\n")

(if (or (< (length command-line) 2)
        (member (list-ref command-line 1)
                '("-h" "--help")))
  (display help)
  (begin
    (define gradient '(#\space #\. #\, #\: #\; #\x #\X #\& #\@))
    (define height (cmd-line 1))
    (define width (cmd-line 2))
    (define type (cmd-line 3))

    (cond ((eqv? type 'radial)
           (let ((x1 (cmd-line 4))
                 (y1 (cmd-line 5))
                 (r  (cmd-line 6)))
             (display-gradient gradient width height
                               (lambda (x y)
                                 (radial-gradient x y x1 y1 r)))))
          ((eqv? type 'linear)
           (let ((x1 (cmd-line 4))
                 (y1 (cmd-line 5))
                 (x2 (cmd-line 6))
                 (y2 (cmd-line 7)))
             (display-gradient gradient width height
                               (lambda (x y)
                                 (linear-gradient x y x1 y1 x2 y2)))))
          (else (display "unrecognized type: ")
                (display type) (newline)))))
