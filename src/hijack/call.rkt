#lang racket

;; =====
;; call.rkt
;; 
;; Overrides Racket's function call syntax to provide a custom integration with Rosette's symbolic
;; evaluation.
;; =====

(require (prefix-in rosette:: rosette/safe)
         "../util/value.rkt"
         "../util/literal.rkt")
(require (for-syntax racket/syntax))
(provide hijack::#%app
         hijack::apply)

;; Function call -- `(...)` syntax
(define-syntax (hijack::#%app stx)
  (syntax-case stx ()
    [(_ func args ...)
     #`(cond
         [(procedure? func) (#%plain-app func args ...)]
         [(and (is-value? func) (is-type? 'Function func)) (begin (unless (= (length (get-sorted-param-list func)) #,(length (syntax->list #'(args ...))))
                                                                    (raise-syntax-error #f "#%app: expects func to receive the same number of arguments as passed"))
                                                                  (for ([arg-val (list args ...)]
                                                                        [param-box (get-sorted-param-list func)])
                                                                    (value-unify! param-box (any->value arg-val)))
                                                                  (define result (if (get-raw-proc func)
                                                                                     (apply (get-raw-proc func) (get-sorted-param-list func))
                                                                                     (get-return-box func)))
                                                                  (value-unify! (get-return-box func) (any->value result))
                                                                  (any->value result))]
         [(is-value? func) (begin (func-morph! func #,(length (syntax->list #'(args ...))))
                                  (for ([arg-val (list args ...)]
                                        [param-box (get-sorted-param-list func)])
                                    (value-unify! param-box (any->value arg-val)))
                                  (get-return-box func))]
         [else (raise-syntax-error #f "#%app: expects func to be a procedure or value struct")])]))

;; Function call -- `apply` syntax
(define-syntax (hijack::apply stx)
  (syntax-case stx ()
    [(_ func args)
     #`(cond
         [(procedure? func) (apply func args)]
         [(and (is-value? func) (is-type? 'Function func)) (begin (unless (= (length (get-sorted-param-list func)) (length args))
                                                                    (raise-syntax-error #f "apply: expects func to receive the same number of arguments as passed"))
                                                                  (for ([arg-val args]
                                                                        [param-box (get-sorted-param-list func)])
                                                                    (value-unify! param-box (any->value arg-val)))
                                                                  (define result (if (get-raw-proc func)
                                                                                     (apply (get-raw-proc func) (get-sorted-param-list func))
                                                                                     (get-return-box func)))
                                                                  (value-unify! (get-return-box func) (any->value result))
                                                                  (any->value result))]
         [(is-value? func) (begin (func-morph! func (length args))
                                  (for ([arg-val args]
                                        [param-box (get-sorted-param-list func)])
                                    (value-unify! param-box (any->value arg-val)))
                                  (get-return-box func))]
         [else (raise-syntax-error #f "#apply: expects func to be a procedure or value struct")])]))
