#lang racket

(define pow1
  (lambda (x y)
    (if (= y 0)
        1
        (* x (pow1 x (- y 1))))))

(define pow2
  (lambda (x)
    (lambda (y)
      (if (= y 0)
          1
          (* x ((pow2 x) (- y 1)))))))

(define (fact n)
  (if (= n 0)
      1
      ( * n (fact ( - n 1)))))

(define (sum xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          ( + (car xs) (sum (cdr xs)))
          ( + (sum (car xs)) (sum (cdr xs))))))

(define (sum1 xs)
  (cond [(null? xs) 0]
        [(number? xs) xs]
        [(list? xs) (+ (sum1 (car xs)) (sum1 (cdr xs)))]
        [#t 0]))


(define x (list 1 2 (list 3 4)))