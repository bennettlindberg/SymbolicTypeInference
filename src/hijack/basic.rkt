#lang racket

;; =====
;; basic.rkt
;; 
;; Overrides Racket's simplest syntax to provide a custom integration with Rosette's symbolic
;; evaluation. 
;; =====

(require (prefix-in rosette:: rosette/safe)
         "../util/primitive.rkt"
         "../util/value.rkt"
         "../util/literal.rkt")
(require (for-syntax racket/syntax))
(provide hijack::if
         hijack::set!
         hijack::+
         hijack::add1
         hijack::-
         hijack::sub1
         hijack::*
         hijack::modulo
         hijack::=)

;; If-else expression
(define (hijack::if cond then else)
  (rosette::if (any->free-sym cond)
               then
               else))

;; Set! expression
(define (hijack::set! val expr)
  (when (not (is-value? val))
    (error "set!: expects val to be a value struct"))

  (set-box! val (unbox (any->value expr)))
  (make-new-void))

;; Arithmetic operations
(define (hijack::+ . args)
  (when (empty? args)
    (error "+: expects at least one argument"))

  (define result-sym (make-fresh-sym!))
  (rosette::assert (rosette::equal? result-sym
                                    (apply rosette::+ (map (lambda (arg) (any->free-sym arg))
                                                           args))))
  (make-new-primitive! result-sym))

(define (hijack::add1 val)
  (hijack::+ val 1))

(define (hijack::- . args)
  (when (empty? args)
    (error "-: expects at least one argument"))

  (define result-sym (make-fresh-sym!))
  (rosette::assert (rosette::equal? result-sym
                                    (apply rosette::- (map (lambda (arg) (any->free-sym arg))
                                                           args))))
  (make-new-primitive! result-sym))

(define (hijack::sub1 val)
  (hijack::- val 1))

(define (hijack::* . args)
  (when (empty? args)
    (error "*: expects at least one argument"))

  (define result-sym (make-fresh-sym!))
  (rosette::assert (rosette::equal? result-sym
                                    (apply rosette::* (map (lambda (arg) (any->free-sym arg))
                                                           args))))
  (make-new-primitive! result-sym))

(define (hijack::modulo n m)
  (define result-sym (make-fresh-sym!))
  (rosette::assert (rosette::equal? result-sym
                                    (rosette::modulo (any->free-sym n)
                                                     (any->free-sym m))))
  (make-new-primitive! result-sym))

(define (hijack::= . args)
  (when (empty? args)
    (error "=: expects at least one argument"))

  (define result-sym (make-fresh-sym!))
  (rosette::assert (rosette::equal? result-sym
                                    (apply rosette::= (map (lambda (arg) (any->free-sym arg))
                                                           args))))
  (make-new-primitive! result-sym))
