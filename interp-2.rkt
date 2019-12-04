#lang racket
;; Remove matching with cond

(define (extend-env x v env)
  `((,x . ,v) . ,env))

(define (apply-env env y)
  (cond
    [(equal? env '())
     (error y)]
    [else
     (let ([x (car (car env))])
       (let ([v (cdr (car env))])
         (let ([env (cdr env)])
           (if (equal? x y) v
               (apply-env env y)))))]))

(define (valof e env)
  (cond
    [(symbol? e)
     (apply-env env e)]
    [(equal? (car e) 'quote)
     (let ([d (car (cdr e))])
       d)]
    [(equal? (car e) 'error)
     (let ([e1 (car (cdr e))])
       (error (valof e1 env)))]
    [(equal? (car e) 'cons)
     (let ([e1 (car (cdr e))])
       (let ([e2 (car (cdr (cdr e)))])
         (cons (valof e1 env) (valof e2 env))))]
    [(equal? (car e) 'car)
     (let ([e1 (car (cdr e))])
       (car (valof e1 env)))]
    [(equal? (car e) 'cdr)
     (let ([e1 (car (cdr e))])
       (cdr (valof e1 env)))]
    [(equal? (car e) 'pair?)
     (let ([e1 (car (cdr e))])
       (pair? (valof e1 env)))]
    [(equal? (car e) 'symbol?)
     (let ([e1 (car (cdr e))])
       (symbol? (valof e1 env)))]
    [(equal? (car e) 'equal?)
     (let ([e1 (car (cdr e))])
       (let ([e2 (car (cdr (cdr e)))])
         (equal? (valof e1 env) (valof e2 env))))]
    [(equal? (car e) 'if)
     (let ([e1 (car (cdr e))])
       (let ([e2 (car (cdr (cdr e)))])
         (let ([e3 (car (cdr (cdr (cdr e))))])
           (if (valof e1 env) (valof e2 env) (valof e3 env)))))]
    [(equal? (car e) 'λ)
     (let ([x (car (car (cdr e)))])
       (let ([body (car (cdr (cdr e)))])
         (λ (arg)
           (valof body (extend-env x arg env)))))]
    [else
     (let ([rator (car e)])
       (let ([rand (car (cdr e))])
         ((valof rator env)
          (valof rand env))))]))

(define (eval e)
  (valof e '()))

(equal? (eval '((((λ (+) (+ +))
                  (λ (+)
                    (λ (n)
                      (λ (m)
                        (if (equal? n '())
                            m
                            (cons 's (((+ +) (cdr n)) m)))))))
                 '(s s))
                '(s s s)))
        '(s s s s s))