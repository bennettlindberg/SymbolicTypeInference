#lang racket

;; =====
;; value.rkt
;;
;; Provides the definition of value structs and utility functions to manipulate them.
;; =====

(require (prefix-in rosette:: rosette/safe)
         "./primitive.rkt")
(provide value
         value-type
         value-fields
         print-value
         is-value?
         is-type?
         get-type
         get-fields
         has-field?
         get-field
         add-field!
         struct-morph!
         get-free-sym
         func-morph!
         get-raw-proc
         get-sorted-param-list
         get-return-box
         get-local-keys
         value-unify!
         value=?
         replace-holes
         make-new-primitive!
         make-new-void
         make-new-hole)

;; Value structs are of the form `(value type fields)`
;; Values begin as 'Primitive with a fresh symbolic, and can morph into other types. Morphing grows
;; the type trees of the root symbolics.
;; Types:
;; - 'Primitive: an integer, boolean, or a free symbolic
;; - 'Struct: a struct with named fields
;; - 'Function: a function with parameters and a return type
;; - 'Void: no fields, used for void functions
;; - 'Hole: a placeholder for unbound types
;; Fields:
;; - 'Primitive: a free symbolic
;; - 'Struct: a hash table mapping field names to values
;; - 'Function: a hash table with keys "param$<n>", "return", "raw-proc", and possibly others for local variables
;; - 'Void: no fields
;; - 'Hole: no fields
(struct value (type fields))

(define (print-value val)
  (print (format "(value (~a ~a)"
                 (value-type (unbox val))
                 (value-fields (unbox val)))))

(define (is-value? val)
  (if (not (box? val))
      #f
      (value? (unbox val))))

(define (is-type? type val)
  (when (not (is-value? val))
    (error "is-type?: val must a be value struct"))

  (equal? type (value-type (unbox val))))

(define (get-type val)
  (when (not (is-value? val))
    (error "get-type: val must a be value struct"))

  (value-type (unbox val)))

(define (get-fields val)
  (when (not (is-value? val))
    (error "get-fields: val must a be value struct"))

  (value-fields (unbox val)))

(define (has-field? field val)
  (when (not (is-value? val))
    (error "has-field?: val must be a value struct"))
  (when (not (or (is-type? 'Struct val) (is-type? 'Function val)))
    (error "has-field?: val must be a 'Struct or 'Function"))

  (hash-has-key? (value-fields (unbox val)) field))

(define (get-field field val)
  (when (not (is-value? val))
    (error "get-field: val must be a value struct"))
  (when (not (or (is-type? 'Struct val) (is-type? 'Function val)))
    (error "get-field: val must be a 'Struct or 'Function"))

  (hash-ref (value-fields (unbox val)) field 'NotFound))

(define (add-field! new-field val)
  (when (not (is-value? val))
    (error "add-field!: val must be a value struct"))
  (when (not (or (is-type? 'Struct val) (is-type? 'Function val)))
    (error "add-field!: val must be a 'Struct or 'Function"))
  (when (hash-has-key? (value-fields (unbox val)) new-field)
    (error "add-field!: new-field must not already be present in val's fields"))

  (hash-set! (value-fields (unbox val))
             new-field
             (make-new-primitive!))
  (hash-ref (value-fields (unbox val)) new-field))

;; Morph into a struct with the given fields
(define (struct-morph! fields val)
  (when (not (is-value? val))
    (error "struct-morph!: val must be a value struct"))

  (define fields-copy (value-fields (unbox val)))
  (unless (hash? fields-copy) ; in case we are morphing from 'Primitive
    (set! fields-copy (make-hash)))

  (for-each (lambda (field)
              (unless (hash-ref fields-copy field #f)
                (hash-set! fields-copy field (make-new-primitive!))))
            fields)

  (set-box! val (value 'Struct fields-copy)))

(define (get-free-sym val)
  (when (not (is-value? val))
    (error "get-free-sym: val must be a value struct"))
  (when (not (is-type? 'Primitive val))
    (error "get-free-sym: val must be a 'Primitive"))

  (value-fields (unbox val)))

;; Morph into a function with the correct number of parameters and a return type
(define (func-morph! val num-params)
  (when (not (is-value? val))
    (error "func-morph!: val must be a value struct"))

  (define fields-copy (value-fields (unbox val)))
  (unless (hash? fields-copy) ; in case we are morphing from 'Primitive
    (set! fields-copy (make-hash)))

  (for ([key (hash-keys fields-copy)])
    (when (and (not (string-contains? "param$"))
               (not (string-contains? "return"))
               (not (string-contains? "raw-proc")))
      (hash-remove! fields-copy key)))

  (for ([i (in-range num-params)])
    (unless (hash-ref fields-copy (format "param$~a" i) #f)
      (hash-set! fields-copy (format "param$~a" i) (make-new-primitive!))))
  (unless (hash-ref fields-copy "return" #f)
    (hash-set! fields-copy "return" (make-new-primitive!)))
  (unless (hash-ref fields-copy "raw-proc" #f)
    (hash-set! fields-copy "raw-proc" #f))

  (set-box! val (value 'Function fields-copy)))

(define (get-raw-proc val)
  (when (not (is-value? val))
    (error "get-raw-proc: val must be a value struct"))
  (when (not (is-type? 'Function val))
    (error "get-raw-proc: val must be a 'Function"))

  (hash-ref (value-fields (unbox val)) "raw-proc"))

(define (get-sorted-param-list val)
  (when (not (is-value? val))
    (error "get-sorted-param-list: val must be a value struct"))
  (when (not (is-type? 'Function val))
    (error "get-sorted-param-list: val must be a 'Function"))

  (map (lambda (key) (hash-ref (value-fields (unbox val)) key))
       (sort (filter (lambda (key) (string-contains? key "param$"))
                     (hash-keys (value-fields (unbox val))))
             string<?)))

(define (get-return-box val)
  (when (not (is-value? val))
    (error "get-return-box: val must be a value struct"))
  (when (not (is-type? 'Function val))
    (error "get-return-box: val must be a 'Function"))

  (hash-ref (value-fields (unbox val)) "return"))

(define (get-local-keys val)
  (when (not (is-value? val))
    (error "get-local-keys: val must be a value struct"))
  (when (not (is-type? 'Function val))
    (error "get-local-keys: val must be a 'Function"))

  (sort (filter (lambda (key) (and (not (string-contains? key "param$"))
                                   (not (string-contains? key "return"))))
                (hash-keys (value-fields (unbox val))))
        string<?))

;; Unify two values by replacing the old value with the new one. This is used when new type
;; information is learned during program execution.
;; ! TODO: Resolve unification conflicts without breaking the inference engine!
(define (value-unify! old-val new-val)
  (when (not (is-value? old-val))
    (error "value-unify!: old-val must be a value struct"))
  (when (not (is-value? new-val))
    (error "value-unify!: new-val must be a value struct"))

  (set-box! old-val (unbox new-val)))

;   (if (and (is-type? 'Primitive old-val)
;            (is-type? 'Primitive new-val))
;       (rosette::assert (rosette::equal? (get-free-sym old-val)
;                                         (get-free-sym new-val)))
;   (if (and (is-type? 'Primitive new-val)
;            (not (is-type? 'Primitive old-val))
;            (not (is-type? 'Void old-val)))
;       (println (format "[WARN] value-unify!: old-val is ~a and new-val is ~a"
;                        (get-type old-val)
;                        (get-type new-val)))
;       (set-box! old-val (unbox new-val)))))

;   (cond [(and (is-type? 'Primitive old-val)
;               (is-type? 'Primitive new-val))
;          (rosette::assert (rosette::equal? (get-free-sym old-val)
;                                            (get-free-sym new-val)))]
;         [(and (is-type? 'Void old-val)
;               (not (is-type? 'Void new-val)))
;          (set-box! old-val (unbox new-val))]
;         [(and (is-type? 'Primitive old-val)
;               (not (is-type? 'Primitive new-val)))
;          (set-box! old-val (unbox new-val))]
;         [(equal? (get-type old-val)
;                  (get-type new-val))
;          (set-box! old-val (unbox new-val))]
;         [else (println (format "[WARN] value-unify!: old-val is ~a and new-val is ~a"
;                                (get-type old-val)
;                                (get-type new-val)))]))

;; Deeply check if two values are equal, considering their types and fields
(define (value=? val-a val-b)
  (when (not (is-value? val-a))
    (error "value=?: val-a must be a value struct"))
  (when (not (is-value? val-b))
    (error "value=?: val-b must be a value struct"))

  (if (equal? (get-type val-a) (get-type val-b))
      (case (get-type val-a)
        ['Hole #t]
        ['Void #t]
        ['Primitive #t]
        [else (equal-field-values? (sort (hash-keys (get-fields val-a)) string<?)
                                   (sort (hash-keys (get-fields val-b)) string<?)
                                   val-a
                                   val-b)])
      #f))

(define (equal-field-values? lst-a lst-b val-a val-b)
  (if (and (empty? lst-a) (empty? lst-b))
      #t
      (if (or (empty? lst-a) (empty? lst-b))
          #f
          (and (value=? (get-field (first lst-a) val-a)
                        (get-field (first lst-b) val-b))
               (equal-field-values? (rest lst-a) (rest lst-b) val-a val-b)))))

;; Replace all 'Hole values in a value with new 'Primitive values
(define (replace-holes val)
  (when (not (is-value? val))
    (error "replace-holes: val must be a value struct"))

  (case (get-type val)
    ['Hole (make-new-primitive!)]
    ['Void val]
    ['Primitive val]
    [else (define new-fields (make-hash))
          (for ([field (hash-keys (get-fields val))])
            (hash-set! new-fields field (replace-holes (hash-ref (get-fields val) field))))
          (value (get-type val) new-fields)]))

(define make-new-primitive!
  (case-lambda [()      (box (value 'Primitive (make-fresh-sym!)))]
               [(sym)   (box (value 'Primitive sym))]))

(define (make-new-void)
  (box (value 'Void (void))))

(define (make-new-hole)
  (box (value 'Hole (void))))
