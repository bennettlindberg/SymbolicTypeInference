#lang racket

;; =====
;; struct.rkt
;;
;; Overrides Racket's struct syntax to provide a custom integration with Rosette's symbolic
;; evaluation.
;; =====

(require (for-syntax racket/syntax)
         "../util/value.rkt"
         "../util/literal.rkt")
(provide hijack::struct)

(define-syntax (hijack::struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (for-each (lambda (x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (syntax->list #'(fields ...)))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)]
                   [(field ...) (syntax->list #'(fields ...))]
                   [(field-param ...) (generate-temporaries #'(fields ...))])
       #`(begin
           ;; Constructor
           (define (id field-param ...)
             (define fields-ht (make-hash))
             #,@(for/list ([field-id (syntax->list #'(fields ...))]
                           [param-id (syntax->list #'(field-param ...))])
                  #`(hash-set! fields-ht (symbol->string '#,field-id) (any->value #,param-id)))
             (box (value 'Struct fields-ht)))

           ;; Predicate
           (define (pred-id v)
             (and (is-value? v)
                  (is-type? 'Struct v)
                  (andmap (lambda (f) (has-field? (symbol->string f) v))
                          '(field ...))))

           ;; Field assessors
           #,@(for/list ([field-id (syntax->list #'(fields ...))])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id field-id)]
                              [field (syntax-e field-id)])
                  #`(define (acc-id v)
                      ;; (unless (pred-id v)
                      ;;   (raise-argument-error 'acc-id "not a ~a struct" v))
                      (struct-morph! (map symbol->string (map syntax->datum (syntax->list #'(fields ...)))) v)
                      (hash-ref (value-fields (unbox v)) (symbol->string 'field)))))))]))
