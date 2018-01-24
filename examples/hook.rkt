#lang racket


(let ([x 5])
  (let-syntax ([m (lambda (stx) ... #'x ...)])
    (let ([x 6])
      (m ... x ...))))