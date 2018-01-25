#lang racket

(provide
 (struct-out identifier)
 datum->syntax
 syntax->datum
 (rename-out [scope new-scope])
 add-scope
 flip-scope)

(struct scope ())
(struct identifier (symbol scopes) #:transparent)

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

(define (sexpr-map f sexpr)
  (match sexpr
    [(cons a d)
     (cons (sexpr-map f a) (sexpr-map f d))]
    ['() '()]
    [e (f e)]))