#lang racket

;; =====
;; root.rkt
;;
;; Provides utility functions to manage root symbolics and track them for future type resolution.
;; =====

(require (prefix-in rosette:: rosette/safe)
         "./value.rkt"
         "./primitive.rkt")
(provide roots
         make-root!
         add-root!
         clear-roots!)

;; Track roots for future lookup during type resolution
(define roots (make-hash))

;; Used to create the roots of all type trees built during type inference
(define (make-root! name)
  (hash-set! roots
             name
             (box (value 'Primitive (make-fresh-sym!))))
  (hash-ref roots name))

(define (add-root! name val)
  (unless (is-value? val)
    (error "add-root!: val must be a value struct"))
  (hash-set! roots
             name
             val))

(define (clear-roots!)
  (hash-clear! roots))