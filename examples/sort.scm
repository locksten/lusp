(module
  (sort)

(define (split xs)
  (let ((half (quotient (length xs) 2)))
    (cons (list-tail xs half)
          (list-init xs half))))

(define (merge pred as bs)
  (cond
    ((null? as) bs)
    ((null? bs) as)
    ((pred (car as) (car bs)) (cons (car as)
                                    (merge pred (cdr as) bs)))
    (else (cons (car bs)
                (merge pred as (cdr bs))))))

(define (merge-sort pred xs)
  (cond
    ((null? xs) xs)
    ((= 1 (length xs)) xs)
    (else (merge pred
                 (merge-sort pred (car (split xs)))
                 (merge-sort pred (cdr (split xs)))))))

(export sort merge-sort))
