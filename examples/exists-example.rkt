#lang racket

(require "exists.rkt"
         rackunit)

; Expected result:
(define ∃!* 5)

; Test:
(check-equal?
  (∃!/s #false 5 #false)
  ∃!*)
