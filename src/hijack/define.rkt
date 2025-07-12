#lang racket

;; =====
;; define.rkt
;; 
;; Overrides Racket's lambda, function, and variable definition syntax to provide a custom
;; integration with Rosette's symbolic evaluation.
;; =====

(require (prefix-in rosette:: rosette/safe)
         "../util/value.rkt"
         "../util/root.rkt"
         "../util/literal.rkt")
(require (for-syntax racket/syntax))
(provide hijack::lambda
         hijack::define)

;; Lambda function
(define-syntax (hijack::lambda stx)
  (syntax-case stx ()
    [(_ (param ...) body ...)
     #'(let ([fields (make-hash)])
         (hash-set! fields "raw-proc" (lambda (param ...)
                                        (define (helper) body ...)
                                        (if (is-recursive-call? (syntax-e #'id))
                                            (make-new-primitive!)
                                            (with-continuation-mark 'current-function (syntax-e #'id)
                                              (helper)))))
         (hash-set! fields "return" (make-new-void))
         (for ([i (in-range (length (syntax->list #'(param ...))))])
           (hash-set! fields (format "param$~a" i) (make-new-primitive!)))
         (box (value 'Function fields)))]))

;; Define (variable and function)
(define-syntax (hijack::define stx)
  (syntax-case stx ()
    ;; Function definition form: (define (id param ...) body ...)
    [(_ (id param ...) body ...)
     #'(begin (define fields (make-hash))
              (hash-set! fields "raw-proc" (lambda (param ...)
                                             (define (helper) body ...)
                                             (if (is-recursive-call? (syntax-e #'id))
                                                 (if (is-bypass-active?)
                                                     (make-new-hole)
                                                     (let ([found-type (with-continuation-mark 'bypass-recursion #t
                                                                         (helper))])
                                                       (if (was-type-already-found? found-type)
                                                           (union-sensitive-replace-holes found-type)
                                                           (with-continuation-mark 'recursive-type-history found-type
                                                             (helper)))))
                                                 (with-continuation-mark 'current-function (syntax-e #'id)
                                                   (helper)))))
              (hash-set! fields "return" (make-new-void))
              (for ([i (in-range (length (syntax->list #'(param ...))))])
                (hash-set! fields (format "param$~a" i) (make-new-primitive!)))
              (rosette::define id (box (value 'Function fields)))
              (make-new-void))]

    ;; Variable definition form: (define name value)
    [(_ name value)
     (with-syntax ([name-str (symbol->string (syntax-e #'name))])
       #'(begin
           (define temp (make-root! name-str))
           (set-box! temp (unbox (any->value value)))
           (rosette::define name temp)
           (make-new-void)))]))

(define (is-recursive-call? fun-sym)
  (define cur-fun-list (continuation-mark-set->list (current-continuation-marks) 'current-function))
  (member fun-sym cur-fun-list equal?))

(define (is-bypass-active?)
  (define cur-bypass-list (continuation-mark-set->list (current-continuation-marks) 'bypass-recursion))
  (not (empty? cur-bypass-list)))

(define (was-type-already-found? newly-found)
  (define previously-found-types (continuation-mark-set->list (current-continuation-marks) 'recursive-type-history))
  (ormap (lambda (prev-found)
           (union-sensitive-value=? newly-found prev-found))
         previously-found-types))

(define (union-sensitive-value=? newly-found prev-found)
  (cond [(rosette::union? newly-found) (andmap (lambda (candidate-cons)
                                                 (union-sensitive-value=? (cdr candidate-cons) prev-found))
                                               (rosette::union-contents newly-found))]
        [(rosette::union? prev-found) (ormap (lambda (candidate-cons)
                                               (union-sensitive-value=? newly-found (cdr candidate-cons)))
                                             (rosette::union-contents prev-found))]
        [else (value=? (any->value newly-found)
                       (any->value prev-found))]))

(define (union-sensitive-replace-holes val)
  (cond [(rosette::union? val) (begin (for ([candidate-cons (rosette::union-contents val)])
                                        (union-sensitive-replace-holes (cdr candidate-cons)))
                                      val)]
        [else (let ([morphed-val (any->value val)])
                     (set-box! morphed-val (replace-holes morphed-val))
                     morphed-val)]))
