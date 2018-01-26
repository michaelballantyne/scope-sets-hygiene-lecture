#lang racket

(require (for-syntax syntax/parse))

(define-syntax (or stx)
  (syntax-parse stx
    [(or e1:expr e2:expr)
     #'(let ([tmp e1])
         (if tmp
             tmp
             e2))]))

(let ([tmp 5])
  (or #f tmp))
