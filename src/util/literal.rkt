#lang racket

;; =====
;; literal.rkt
;; 
;; Provides utility functions to convert Racket values to Rosette value structs and free symbolics.
;; =====

(require (prefix-in rosette:: rosette/safe)
         "./value.rkt"
         "./primitive.rkt")
(provide any->value
         any->free-sym)

;; Convert any Racket value to a Rosette value struct
;; Safely handles pre-existing value structs
(define (any->value val)
  (if (box? val)
      (if (is-value? val)
          val
          (error "any->value: encountered boxed value that is not a value struct:" val))
      (cond
        [(procedure? val) (error "any->value: TODO implement proc literals in define")]
        [(or (integer? val) (boolean? val) (rosette::symbolic? val)) (begin (define lit-sym (make-fresh-sym!))
                                                                            (rosette::assert (rosette::equal? lit-sym val))
                                                                            (make-new-primitive! lit-sym))]
        [else (make-new-primitive!)])))

;; Convert any Racket value to a Rosette free symbolic
;; Safely handles pre-existing free symbolics inside of primitive values
(define (any->free-sym val)
  (if (box? val)
      (if (is-value? val)
          (if (is-type? 'Primitive val)
              (get-free-sym val)
              (if (is-type? 'Hole val)
                  (get-free-sym (make-new-primitive!))
                  (error "any->free-sym: encountered a non-Primitive value struct:" (get-type val))))
          (error "any->free-sym: encountered boxed value that is not a value struct:" val))
      (cond
        [(or (integer? val) (boolean? val) (rosette::symbolic? val)) val]
        [else (error "any->free-sym: encountered a non-integer, non-boolean concrete value:" val)])))