
#lang racket 

(require
  "syntax.rkt"
  "eval.rkt")

(module+ test (require rackunit))

; Replaced STXTRANS with BINDING-STORE, which includes bindings for:
;  - primitive functions
;  - syntax transformers
;  - runtime variables

; [Listof Binding]
(define BINDING-STORE '())

; scopes is (Setof Scope)
; value is a BindingValue, which is one of Symbol or Procedure
(struct binding (scopes symbol value) #:transparent)

(define (add-binding! id val)
  (set! BINDING-STORE
        (cons (binding (identifier-scopes id)
                       (identifier-symbol id)
                       val)
              BINDING-STORE)))


(module+ test
  #;(expand-top 'a)
  
  #;(expand-top '(lambda (a)
                 (if (zero? a)
                     1
                     5))))






; Primitive bindings
#;(add-binding! (datum->syntax #f 'zero?) 'zero?)


(define (extend-syntax macro transformer-stx)
  (define proc (eval-transformer (datum->syntax #f transformer-stx)))
  (add-binding! (datum->syntax #f macro) proc))

;; EXAMPLE
;; (let ((variable S-expression1)) S-expression2)
;; ==>
;; ((lambda (variable) S-expression2) S-expression1)
(extend-syntax
 'let
 '(lambda (stx)
    (match stx
      [`(,_ ((,variable ,rhs-expression)) ,body-expression)
       `((,#'lambda (,variable) ,body-expression) ,rhs-expression)]
      [else (error 'let-transformer "not a let: ~e" stx)])))

;; -----------------------------------------------------------------------------
;; `expand-top`:
;; S-expression -> Expression
;; determine whether the S-expression belongs to Expression
;; EFFECT raise an exception when a sub-tree is invalid
;; EFFECT add bindings to the binding store

(module+ test
  ; These used to expand, but we check that all references are bound now,
  ; so instead we should get unbound identifier errors:
  ;  (check-equal? (expander 'a 'a))
  ;  (check-equal? (expander '(lambda (a) b)) '(lambda (a) b))
  (check-exn exn:fail? (lambda () (expand-top 'a)))
  (check-exn exn:fail? (lambda () (expand-top '(lambda (a) b))))

  ; The reference to `zero?` is OK because we added a binding for it to the store.
  (check-equal?
   (expand-top '((lambda (a) (if (zero? a) (lambda (x y) x) (lambda (x y) y))) 10))
   '((lambda (a1) (if (zero? a1) (lambda (x2 y3) x2) (lambda (x4 y5) y5))) 10))

  ; Our old extend-syntax `let` macro still works:
  (check-equal?
   (expand-top '(let ((a 10))
                  (if (zero? a) (lambda (x y) x) (lambda (x y) y))))
   '((lambda (a1) (if (zero? a1) (lambda (x2 y3) x2) (lambda (x4 y5) y5))) 10))

  (check-exn exn:fail? (lambda () (expand-top '(let ((a 10)) (lambda a a)))))

  (check-exn exn:fail? (lambda () (expand-top '(let ((10 a)) a))))
  
  (check-exn exn:fail?
             (lambda ()
               (expand-top '((lambda x) 10))))
  (check-exn exn:fail?
             (lambda ()
               (expand-top '((if 1 2 3 4) (lambda (x) x) (lambda (x) 42)))))

  ; But we can also define `let` via `let-syntax`:
  (check-equal?
   
   (expand-top
    '(let-syntax ([my-let (lambda (stx)
                            (match stx
                              [`(,_ ((,variable ,rhs-expression)) ,body-expression)
                               `((,#'lambda (,variable) ,body-expression) ,rhs-expression)]
                              [else (error 'let-transformer "not a let: ~e" stx)]))])
       (my-let ([x 6])
               x)))
   
   '((lambda (x1) x1) 6)

   )

  ;; ******************************************************************
  
  ; And a hygienic `or` macro with `let-syntax`:
  (check-equal?
   
   (expand-top
    '(let-syntax ([or (lambda (stx)
                        (match stx
                          [`(,_ ,e1 ,e2)
                           `(,#'let ([,#'tmp ,e1])
                                    (,#'if ,#'tmp
                                           ,#'tmp
                                           ,e2))]))])
       (let ([tmp 2])
         (or 1 tmp))))
   
   '((lambda (tmp1)
       ((lambda (tmp2)
          (if tmp2
              tmp2
              tmp1))
        1))
     2)

   ;; ******************************************************************

   ))

(define (expand-top sexpr)
  (set! GENSYM-COUNTER 0)
  (expander (datum->syntax #f sexpr)))

(define (expander s)
  (match s
    [(? identifier? id)
     (or (resolve id BINDING-STORE)
         (error 'expander "unbound identifier: ~a" id))]
    [(or (? number?) #true #false) s]
    [`(,(identifier 'if _) ,que ,thn ,els)
     `(if ,(expander que) ,(expander thn) ,(expander els))]
    [`(,(identifier 'lambda _) ,params ,body)
     (define scope (new-scope))
     (define new-params
       (for/list ([param (all-variables params)])
         (define new-sym (gensym (identifier-symbol param)))
         (add-binding! (add-scope scope param) new-sym)
         new-sym))
     `(lambda ,new-params
        ,(expander (add-scope scope body)))]
    [`(,(identifier 'let-syntax _) ([,macro ,transformer-stx]) ,body)
     (let ([scope (new-scope)])
       (add-binding! (add-scope scope macro) (eval-transformer transformer-stx))
       (expander (add-scope scope body)))]
    [(cons (? identifier? id) _)
     (match (resolve id BINDING-STORE)
       [#f (error 'expander "unbound identifier: ~a" id)]
       [(? procedure? transformer)
        (expander (apply-transformer transformer s))]
       [(? not-keyword?)
        (map expander s)])]
    [`(,(not (? identifier?)) ,args ...)
     (map expander s)]
    [else
     (error 'expander "not a valid Expression: ~e" s)]))

; Identifier, BindingStore -> one of #false or BindingValue
(define (resolve id binding-store)  
  (define ref-scopes (identifier-scopes id))
  (define ref-sym (identifier-symbol id))
  
  (define best-binding
    (for/fold ([best #f])
              ([next binding-store]
               #:when (and (equal? (binding-symbol next) ref-sym)
                           (subset? (binding-scopes next) ref-scopes))
               #:when (or (not best)
                          (subset? (binding-scopes best) (binding-scopes next))))
      next))
  
  (and best-binding
       (binding-value best-binding)))

; Transformer, Syntax -> Syntax
; where Transformer is Syntax -> Syntax
(define (apply-transformer transformer stx)
  (let* ([introduction-scope (new-scope)]
         [use-site-scope (new-scope)]
         [argument-stx
          (add-scope introduction-scope
                     (add-scope use-site-scope
                                stx))]
         [transformer-result (transformer argument-stx)])
    (flip-scope introduction-scope transformer-result)))

(define GENSYM-COUNTER 0)

(define (gensym symbol)
  (set! GENSYM-COUNTER (add1 GENSYM-COUNTER))
  (string->symbol (format "~a~a" symbol GENSYM-COUNTER)))

;; S-expression -> [Listof Symbol]
(define (all-variables s)
  (if (andmap identifier? s) s (error 'expander "not a valid Parameter List: ~e" s)))

(define KEYWORDS '(if lambda let-syntax))

(define (not-keyword? x)
  (not (member x KEYWORDS)))