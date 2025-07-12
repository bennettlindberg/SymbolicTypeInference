#lang racket

;; =====
;; primitive.rkt
;;
;; Provides a utility function for creating Rosette free symbolics and tracks them for future
;; type resolution.
;; =====

(require (prefix-in rosette:: rosette/safe))
(provide guard-ht
         make-fresh-sym!)

;; Track guards for future lookup during type resolution
(define guard-ht (make-hash))

(define (make-fresh-sym-int!)
  (rosette::define-symbolic* x rosette::integer?)
  x)

(define (make-fresh-sym-bool!)
  (rosette::define-symbolic* b rosette::boolean?)
  b)

;; Fresh symbolics are unions of the form `(if guard int bool)`
;; If Rosette assertions mandate that the guard is true, then the symbolic is an integer,
;; otherwise it is a boolean.
(define (make-fresh-sym!)
  (define guard (make-fresh-sym-bool!))
  (define int (make-fresh-sym-int!))
  (define bool (make-fresh-sym-bool!))

  (define new-sym (rosette::if guard int bool))

  (hash-set! guard-ht new-sym guard)

  new-sym)