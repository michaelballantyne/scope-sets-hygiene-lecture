#lang racket

(provide
 (struct-out identifier)
 (struct-out binding)
 datum->syntax
 syntax->datum
 empty-scopes
 new-scope
 add-scope
 flip-scope
 resolve)

(define empty-scopes (set))

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
  (sexpr-map
   (lambda (datum)
     (if (symbol? datum)
         (identifier datum scopes)
         datum))
   datum))

(define (syntax->datum stx)
  (sexpr-map
   (lambda (stx)
     (if (identifier? stx)
         (identifier-symbol stx)
         stx))
   stx))

(define (add-scope scope stx)
  (sexpr-map
   (lambda (id)
     (match id
       [(identifier sym scopes)
        (identifier sym (set-add scopes scope))]
       [e e]))
   stx))

(define (flip-scope scope stx)
  (sexpr-map
   (lambda (id)
     (match id
       [(identifier sym scopes)
        (identifier sym (if (set-member? scopes scope)
                            (set-remove scopes scope)
                            (set-add scopes scope)))]
       [e e]))
   stx))

(struct binding (scopes symbol value) #:transparent)

(define (resolve id binding-store)  
  (define ref-scopes (identifier-scopes id))
  (define ref-sym (identifier-symbol id))
  
  (define best-binding
    (for/fold ([best #f])
              ([next binding-store])
      (if (and (equal? (binding-symbol next) ref-sym)
               (subset? (binding-scopes next) ref-scopes)
               (or (not best)
                   (subset? (binding-scopes best) (binding-scopes next))))
          next
          best)))
  
  (and best-binding
       (binding-value best-binding)))