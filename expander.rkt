#lang racket 

(module+ test (require rackunit))

#|
S-expression is one of: 
 -- Symbol 
 -- Number 
 -- [Listof S-expression]

Expression is one of: 
 -- Variable ~~ Symbol 
 -- Number 
 -- (list 'if Expression Expression Expression)
 -- (list 'lambda [Listof Variable] Expression) 
 -- (cons Expression [Listof Expression])
 -- (cons STXWORD [Listof S-expression])
|#

(define STXTRANS '())
(define (STXWORD) (map first STXTRANS))

;; *************************************************
(define (extend-syntax macro transformer)
  (set! STXTRANS
        (cons (list macro transformer)
              STXTRANS)))

;; EXAMPLE
;; (let ((variable S-expression1)) S-expression2)
;; ==>
;; ((lambda (variable) S-expression2) S-expression1)
(define (let-transformer stx)
  (match stx
    [`(let ((,variable ,rhs-expression)) ,body-expression)
     `((lambda (,variable) ,body-expression) ,rhs-expression)]
    [else (error 'let-transformer "not a let: ~e" stx)]))

(extend-syntax 'let let-transformer)

;; *************************************************

;; -----------------------------------------------------------------------------

(define KEYWORDS '(if lambda))

;; S-expression -> Expression
;; determine whether the S-expression belongs to Expression
;; EFFECT raise an exception when a sub-tree is invalid 

(module+ test
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

(define (expander s)
  (match s
    [(? symbol?) s]
    [(? number?) s]
    [`(if ,que ,thn ,els)
     `(if ,(expander que) ,(expander thn) ,(expander els))]
    [`(lambda ,paras ,body)
     `(lambda ,(all-variables paras) ,(expander body))]
    [`(,(and (? not-keyword) fun)  ,args ...)
     `(,(expander fun) ,@(map expander args))]
    [`(,(and (? stx-word) macro) ,forms ...)
     (expander ((retrieve-transformer macro) s))]
    [else
     (error 'expander "not a valid Expression: ~e" s)]))

;; S-expression -> [Listof Symbol]
(define (all-variables s)
  (if (andmap symbol? s) s (error 'expander "not a valid Parameter List: ~e" s)))

;; S-expression -> Boolean 
(define (not-keyword x)
  (not (or (member x (STXWORD)) (member x KEYWORDS))))

;; S-expression -> Boolean
(define (stx-word s)
  (member s (STXWORD)))

;; Symbol -> [S-expression -> S-expression]
;; ASSUME (stx-word? s) holds 
(define (retrieve-transformer s)
  (second (assq s STXTRANS)))