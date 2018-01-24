#lang racket

(provide
 (struct-out identifier)
 (struct-out binding)
 datum->syntax
 syntax->datum)

(struct identifier (symbol scopes) #:transparent)
(struct scope (id) #:transparent)

(define new-scope
  (let ([counter 0])
    (lambda ()
      (set! counter (add1 counter))
      (scope counter))))

(define (sexpr-map f sexpr)
  (match sexpr
    [(cons a d)
     (cons (sexpr-map f a) (sexpr-map f d))]
    ['() '()]
    [e (f e)]))

(define (datum->syntax id datum)
  (define scopes (if id (identifier-scopes id) (set)))
  (sexpr-map (lambda (sym) (identifier sym scopes)) datum))

(define (syntax->datum stx)
  (sexpr-map identifier-symbol))

(define (add-scope scope stx)
  (sexpr-map
   (lambda (id)
     (match id
       [(identifier sym scopes)
        (identifier sym (set-add scopes scope))]))
   stx))

(define (flip-scope scope stx)
  (sexpr-map
   (lambda (id)
     (match id
       [(identifier sym scopes)
        (identifier sym (if (set-member? scopes scope)
                            (set-remove scopes scope)
                            (set-add scopes scope)))]))
   stx))

(struct binding (scopes symbol value))

(define (resolve id binding-store)
  (define ref-scopes (identifier-scopes id))
  (define ref-sym (identifier-symbol id))

  (for/fold ([best #f])
            ([next binding-store])
    (if (and (subset? (binding-scopes next) ref-scopes)
             (or (not best)
                 (subset? (binding-scopes best) (binding-scopes next))))
        next
        candidate)))