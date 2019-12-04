#lang racket

(define (expand e)
  (match e
    [`(letrec ([,x ,init]) ,body)
     `((λ (,x) ,(expand body))
       (fix (λ (,x) ,(expand init))))]
    [`(let ([,x ,init]) ,body)
     `((λ (,x) ,(expand body))
       ,(expand init))]
    [`(cond [else ,e])
     (expand e)]
    [`(cond [,e1 ,e2] . ,rest)
     `(if ,(expand e1)
          ,(expand e2)
          ,(expand `(cond . ,rest)))]
    [`,e
     (if (list? e)
         (map expand e)
         e)]))