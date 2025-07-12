#lang rosette/safe
(require "src/hijack/hijack.rkt"    ; overrides supported operations
         "src/solver/solve.rkt")    ; enables solver functionality

(define (add-numbers a b)
  (+ a b))

(define (test-struct)
  (struct point (x y))
  (point 1 2))

; Solve for types
(solve (list add-numbers test-struct))    ; solve form: (solve (list function1 function2 ...))