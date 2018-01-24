#lang racket 

(require
  "syntax.rkt"
  "eval.rkt")

(module+ test (require rackunit))

; List of binding structs
(define BINDING-STORE '())

(define (add-binding! id val)
  (set! BINDING-STORE
        (cons (binding (identifier-scopes id)
                       (identifier-symbol id)
                       val)
              BINDING-STORE)))

; Core form bindings
(add-binding! (datum->syntax #f 'if) 'if)
(add-binding! (datum->syntax #f 'lambda) 'lambda)
(add-binding! (datum->syntax #f 'let-syntax) 'let-syntax)

;; *************************************************
(define (extend-syntax macro transformer-stx)
  (define proc (meta-eval (datum->syntax #f transformer-stx)))
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

;; *************************************************

;; -----------------------------------------------------------------------------

;; S-expression -> Expression
;; determine whether the S-expression belongs to Expression
;; EFFECT raise an exception when a sub-tree is invalid 

#;(module+ test
    (check-equal? (expander 'a) 'a)
    (check-equal? (expander '(lambda (a) b)) '(lambda (a) b))
    (check-equal?
     (expander '((lambda (a) (if (zero? a) (lambda (x y) x) (lambda (x y) y))) 10))
     '((lambda (a) (if (zero? a) (lambda (x y) x) (lambda (x y) y))) 10))

    ;; ******************************************************************
    (check-equal?
     (expander '(let ((a  10))
                  (if (zero? a) (lambda (x y) x) (lambda (x y) y))))
     '((lambda (a) (if (zero? a) (lambda (x y) x) (lambda (x y) y))) 10))

    (check-exn exn:fail? (lambda () (expander '(let ((a 10)) (lambda a a)))))

    (check-exn exn:fail? (lambda () (expander '(let ((10 a)) a))))
    ;; ******************************************************************

    (check-exn exn:fail?
               (lambda ()
                 (expander '((lambda x) 10))))
    (check-exn exn:fail?
               (lambda ()
                 (expander '((if 1 2 3 4) (lambda (x) x) (lambda (x) 42))))))

(define GENSYM-COUNTER 0)

(define (gensym symbol)
  (set! GENSYM-COUNTER (add1 GENSYM-COUNTER))
  (string->symbol (format "~a~a" symbol GENSYM-COUNTER)))

(define (expand-top sexpr)
  (set! GENSYM-COUNTER 0)
  (expander (datum->syntax #f sexpr)))

(define (apply-transformer transformer stx)
  (define introduction-scope (new-scope))
  (define use-site-scope (new-scope))

  (define argument-stx
    (add-scope introduction-scope
               (add-scope use-site-scope
                          stx)))

  (define transformer-result (transformer argument-stx))
  
  (flip-scope introduction-scope transformer-result))

(define (expander s)
  (match s
    [(? identifier? id)
     (or (resolve id BINDING-STORE)
         (error 'expander "unbound identifier: ~a" id))]
    [(? number?) s]
    [(cons (? identifier? id) _)
     (match* ((resolve id BINDING-STORE) s)
       [(#f _) (error 'expander "unbound identifier: ~a" id)]
       [('if `(,_ ,que ,thn ,els))
        `(if ,(expander que) ,(expander thn) ,(expander els))]
       [('lambda `(,_ ,params ,body))
        (define scope (new-scope))
        (define new-params
          (for/list ([param (all-variables params)])
            (define new-sym (gensym (identifier-symbol param)))
            (add-binding! (add-scope scope param) new-sym)
            new-sym))
        `(lambda ,new-params
           ,(expander (add-scope scope body)))]
       [('let-syntax `(,_ ([,macro ,transformer-stx]) ,body))
        (define scope (new-scope))
        (add-binding! (add-scope scope macro) (meta-eval transformer-stx))
        (expander (add-scope scope body))]
       [((? procedure? transformer) s)
        (expander (apply-transformer transformer s))]
       [((? symbol?) _)
        (map expander s)])]
    [`(,(not (? identifier?)) ,args ...)
     (map expander s)]
    [else
     (error 'expander "not a valid Expression: ~e" s)]))

;; S-expression -> [Listof Symbol]
(define (all-variables s)
  (if (andmap identifier? s) s (error 'expander "not a valid Parameter List: ~e" s)))

(module+ test
  ; core form expansions
  (check-equal?
   (expand-top '(lambda (x) (if x 1 2)))
   '(lambda (x1) (if x1 1 2)))

  ; extend-syntax `let` expansion
  (check-equal?
   (expand-top '(let ([x 5]) x))
   '((lambda (x1) x1) 5))

  ; let-syntax `let` expansion
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

  ; let-syntax hygienic `or` macro
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

   ))