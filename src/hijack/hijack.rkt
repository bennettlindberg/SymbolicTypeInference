#lang racket

;; =====
;; hijack.rkt
;; 
;; Overrides the default Racket syntax to provide a custom integration with Rosette's symbolic
;; evaluation. Compiles all hijack modules into a single module for easier use.
;; =====

(require (only-in "./call.rkt" hijack::#%app hijack::apply)
         (only-in "./define.rkt" hijack::lambda hijack::define)
         (only-in "./struct.rkt" hijack::struct)
         (only-in "./basic.rkt" hijack::if hijack::set! hijack::+ hijack::add1
                  hijack::- hijack::sub1 hijack::* hijack::modulo hijack::=))
(provide (rename-out [hijack::struct    struct]
                     [hijack::#%app     #%app]
                     [hijack::apply     apply]
                     [hijack::lambda    lambda]
                     [hijack::define    define]
                     [hijack::if        if]
                     [hijack::set!      set!]
                     [hijack::+         +]
                     [hijack::add1      add1]
                     [hijack::-         -]
                     [hijack::sub1      sub1]
                     [hijack::*         *]
                     [hijack::modulo    modulo]
                     [hijack::=         =]))
