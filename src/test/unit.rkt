#lang rosette/safe

;; =====
;; unit.rkt
;;
;; Various unit tests for the symbolic type inference engine.
;; REPL: (solve (list test ...))
;; =====

(require "../hijack/hijack.rkt"
         "../solver/solve.rkt")

; int-op assertions
; set! assertions
(define (test a b c d)
  (set! a (= (+ b d) c)))

; non-void return
(define (test2 b c d)
  (= (+ b d) c))

; local definitions
(define (test3)
  (define a 123))

; returned local
(define (test4)
  (define a 123)
  a)

; literal support
(define (test5 a b)
  (+ a 2 b))

; literal support
(define (test6)
  (if #f 1 2))

; struct support
(struct posn (x y))
(define (test7)
  (posn 1 2))

; struct morphing
(define (test8 a)
  (posn-x a))

; nested structs
(define (test9 a)
  (posn-y (posn-x a)))

; recursion detection
(define (temp n)
  (sum-to (- n 1)))
(define (sum-to n)
  (if (= n 0)
      0
      (+ n (temp n))))

; function argument
(define (test10 a)
  (a 1 2 #t))

; nested functions
(define (test11 a b)
  ((posn-x a) (b 1 #f)))

; lambda functions
(define (test12 a)
  (set! a (lambda (x) (+ 1 x)))
  (a 2))

(define (double x)
  (+ x x))
; Inferred type: (integer -> integer)
(struct person (name age))
(define (get-age p)
  (person-age p))
; get-age: ((Structof [name : any] [age : any]) -> any)
(define (apply-twice f x)
  (f (f (add1 x))))
; Inferred type: ((integer -> integer) integer -> integer)
(solve (list double get-age apply-twice))