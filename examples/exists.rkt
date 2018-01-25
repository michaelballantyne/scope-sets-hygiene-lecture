#lang racket

(require (for-syntax syntax/parse))

(provide ∃!/s)

(define (∃!* . args)
  ;; [Listof Any] Any -> Any
  ;; SEEN have we seen any non-#false value
  (define (aux args seen)
    (cond
      [(empty? args) seen]
      [else (define temp {(first args)})
            (if (and seen temp) #false (aux (rest args) (or temp seen)))]))
  ;; -- IN --
  (aux args0 #false))

(define-syntax (∃!/s stx)
  (syntax-parse stx
    [(_ e1:expr ...) #'(∃!* (lambda () e1) ...)]))
