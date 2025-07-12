#lang rosette/safe

;; =====
;; sieve.rkt
;;
;; Implementation of the Sieve of Eratosthenes using streams.
;; Reference: https://github.com/bennn/gtp-benchmarks/tree/master/benchmarks/sieve
;; =====

(require "../hijack/hijack.rkt"
         "../solver/solve.rkt"
         "../util/value.rkt")

;; ===== streams.rkt =====

;; A stream is a cons of a value and a thunk that computes the next value when applied
(struct stream (first rest))

(define (make-stream hd thunk)
  (stream hd thunk))

;; `stream-get st i` Get the `i`-th element from the stream `st`
(define (stream-get st i)
  (define hd (stream-first st))
  (define tl ((stream-rest st)))
  (cond [(= i 0) hd]
        [else (stream-get tl (sub1 i))]))

;; `stream-take st n` Collect the first `n` elements of the stream `st`.
(define (stream-take st n)
  (cond [(= n 0) '()]
        [else (cons (stream-first st) (stream-take ((stream-rest st)) (sub1 n)))]))

;; ===== main.rkt =====

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(define (count-from n)
  (make-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(define (sift n st)
  (define hd (stream-first st))
  (define tl ((stream-rest st)))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

;; `sieve st` Sieve of Eratosthenes
(define (sieve st)
  (define hd (stream-first st))
  (define tl ((stream-rest st)))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

;; stream of prime numbers
(define primes (sieve (count-from 2)))

;; returns the `i`-th prime number
(define (main ith-prime)
  (stream-get primes ith-prime))
