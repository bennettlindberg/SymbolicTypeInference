#lang racket

;; =====
;; solve.rkt
;;
;; Provides the main function to solve the type inference problem by applying functions and checking
;; assertion satisfiability using Rosette.
;; =====

(require (prefix-in rosette:: rosette/safe)
         "../util/root.rkt"
         "../util/value.rkt"
         "../util/primitive.rkt"
         (only-in "../hijack/hijack.rkt" [apply hijack::apply]))
(provide solve)

(define (strlst->str strlst)
  (define (helper lst accum)
    (if (empty? lst)
        accum
        (helper (rest lst) (string-append accum " " (first lst)))))
  (if (empty? strlst) ""
      (substring (helper strlst "") 1)))

(define (get-assignment sym raw-ht)
  (match (hash-ref raw-ht (hash-ref guard-ht sym 'NotFound) 'NotFound)
    ['NotFound  "any"]
    [#t         "integer"]
    [#f         "boolean"]))

;; Helper for converting a value to its type string representation
(define (get-type-str root-val raw-ht)
  (match (get-type root-val)
    ['Primitive      (get-assignment (get-free-sym root-val) raw-ht)]
    ['Struct         (string-append "(Structof "
                                    (strlst->str (for/list ([field (sort (hash-keys (get-fields root-val)) string<?)])
                                                   (string-append "["
                                                                  field
                                                                  " : "
                                                                  (get-type-str (hash-ref (get-fields root-val) field) raw-ht)
                                                                  "]")))
                                    ")")]
    ['Function       (begin (define params (strlst->str (for/list ([param-box (get-sorted-param-list root-val)])
                                                          (get-type-str param-box raw-ht))))
                            (define return (get-type-str (get-return-box root-val) raw-ht))
                            ;; (define locals (strlst->str (for/list ([loc-key (get-local-keys root-val)])
                            ;;                               (string-append "["
                            ;;                                              loc-key
                            ;;                                              " : "
                            ;;                                              (get-type-str (hash-ref (get-fields root-val) loc-key) raw-ht)
                            ;;                                              "]"))))
                            ;; (format "(~a -> ~a)\n\tLocal variables: ~a" params return (if (equal? locals "")
                            ;;                                                               "none"
                            ;;                                                               locals)))]
                            (format "(~a -> ~a)" params return))]
    ['Void           "void"]
    ['Hole           "hole"]))

;; Helper for printing types of all roots (type trees)
(define (print-types raw-ht)
  (for ([root (hash-keys roots)])
    (define root-val (hash-ref roots root))
    (define type-str (get-type-str root-val raw-ht))
    (printf "[~a : ~a]\n" root type-str)))

;; Helper for printing the function signature of the top-level solved function
(define (print-root-fun raw-ht)
  (define params (strlst->str (for/list ([root (sort (filter (lambda (x) (and (>= (string-length x) 6) (equal? "param$" (substring x 0 6))))
                                                             (hash-keys roots))
                                                     string<?)])
                                (get-type-str (hash-ref roots root) raw-ht))))
  (define return (if (hash-ref roots "return$" #f)
                     (get-type-str (hash-ref roots "return$") raw-ht)
                     "void"))
  (define locals (strlst->str (for/list ([root (sort (filter (lambda (x) (and (or (< (string-length x) 6) (not (equal? "param$" (substring x 0 6))))
                                                                              (or (< (string-length x) 7) (not (equal? "return$" (substring x 0 7))))))
                                                             (hash-keys roots))
                                                     string<?)])
                                (string-append "["
                                               root
                                               " : "
                                               (get-type-str (hash-ref roots root) raw-ht)
                                               "]"))))
  (format "(~a -> ~a)\n\tLocal variables: ~a" params return (if (equal? locals "")
                                                                "none"
                                                                locals)))

;; Main function to solve the type inference problem
(define (solve proc-lst)
  (rosette::clear-vc!)

  (for ([proc proc-lst])
    (hijack::apply proc
                   (for/list ([n (in-range (length (get-sorted-param-list proc)))])
                     (make-new-primitive!))))

  (define solved (rosette::solve void))
  (if (rosette::sat? solved)
      (for ([proc proc-lst])
        (printf "~a: ~a\n" proc (get-type-str proc (rosette::model solved))))
      (printf "solve: could not find a satisfying typing of the program")))