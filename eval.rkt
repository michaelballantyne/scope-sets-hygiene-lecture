#lang racket

(require "syntax.rkt")

(provide eval-transformer)

(define (parse-and-lift stx ns)
  (match stx
    [(identifier symbol _) symbol]
    [(list (identifier 'syntax _) quoted)
     (define sym (gensym 'lit))
     (namespace-set-variable-value! sym quoted #t ns)
     sym]
    [(cons a d)
     (cons (parse-and-lift a ns)
           (parse-and-lift d ns))]
    ['() '()]
    [e e]))

(define (eval-transformer stx)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/match))

  (define parsed (parse-and-lift stx ns))
  (eval parsed ns))

(module+ test
  (require rackunit)

  (define proc
    (eval-transformer
     (datum->syntax (identifier 'foo (set))
                    `(lambda (stx)
                       #'(hello world)))))

  (check-equal?
   (proc 'blah)
   (list
    (identifier
     'hello
     (set))
    (identifier
     'world
     (set)))))
