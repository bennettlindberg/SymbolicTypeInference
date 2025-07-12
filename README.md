# Symbolic Type Inference Engine

*An automated type inference engine for Racket programs powered by symbolic evaluation and SMT solvers.*

### Contributors
- Bennett Lindberg
- [Andrew Li](https://github.com/andrlime)
- [Christos Dimoulas](https://users.cs.northwestern.edu/~chrdimo/)

## Overview

When developing software projects, software engineers often initially select dynamically-typed languages for rapid prototyping. However, developers often regret this decision as project sizes increase, deciding later that they need static types for improved type safety, maintainability, and documentation. Unfortunately, switching from dynamic to static typing requires significant effort, as **manual type annotation is time-intensive and error-prone**.

This project demonstrates a novel approach to **automatically generating precise types for larger-scale programs** using symbolic evaluation and SMT solvers. Our solution models types as trees with interior nodes representing concrete compound types (structs, functions) and leaves representing symbolic primitive types (integers, booleans). By observing usage behavior during program execution, we deduce type structure without requiring concrete inputs.

This research addresses the existing gap between scalable-but-imprecise and precise-but-limited approaches to automated type inference. By combining symbolic evaluation with SMT solving, we achieve semi-precision for supported constructs while maintaining the ability to reason about infinite value spaces without concrete inputs. However, we do not handle all language features, all possible types, or recursive programs.

## Key Features

- **Symbolic Evaluation**: Uses Racket's Rosette framework for symbolic program execution
- **No Concrete Inputs Required**: Explores all execution paths symbolically, allowing type inference without needing specific input values
- **Precise Type Inference**: Generates specific types for supported language constructs under many circumstances
- **Compound Type Support**: Handles complex data structures like structs and higher-order functions
- **SMT Solver Integration**: Leverages Z3 for constraint satisfaction when determining concrete types

## Usage

1. Install Racket and Rosette:
   - Follow the [Racket installation guide](https://racket-lang.org/download/).
   - Install Rosette via Racket's package manager: `raco pkg install rosette`.

2. Write your Racket program using the hijacked syntax:

```racket
#lang rosette/safe
(require "src/hijack/hijack.rkt"    ; overrides supported operations
         "src/solver/solve.rkt")    ; enables solver functionality

(define (add-numbers a b)
  (+ a b))

(define (test-struct)
  (struct point (x y))
  (point 1 2))

; Solve for types
(solve (list add-numbers test-struct))    ; solve form: (solve (list function1 function2 ...))
```

3. Run the program with the call to `solve` to get inferred types:

```bash
racket usage-example.rkt

# Output example:
# add-numbers: (integer integer -> integer)
# test-struct: (-> (Structof [x : integer] [y : integer]))
```

## Supported Language Features

### Types
- Primitive types: `integer`, `boolean`
- Compound types: `struct`, `function`
- Special types: `void`

### Operations
- Arithmetic: `+`, `-`, `*`, `add1`, `sub1`, `modulo`
- Comparison: `=`
- Control flow: `if`, `set!`
- Functions: `lambda`, `define`, function calls
- Structs: constructors, field accessors
- Higher-order functions

## Implementation Details

### Value Representation
Values are represented as boxed structs with type information:
- `'Primitive`: Contains a symbolic union of integer/boolean
- `'Struct`: Hash table mapping field names to values  
- `'Function`: Hash table with parameters, return type, and raw procedure
- `'Void`: No fields, represents void functions
- `'Hole`: Placeholder for unbound types

### Type Tree Evolution
Types start as primitive symbolics and morph into compound types based on usage:

```racket
; Initially: (value 'Primitive symbolic-union)
; After struct access: (value 'Struct field-hash)
; After function call: (value 'Function param-return-hash)
```

### Symbolic Constraint Generation
Primitive types use symbolic unions that Rosette can reason about:

```racket
(define fresh-sym (if guard integer-sym boolean-sym))
```

The SMT solver determines concrete types by satisfying generated constraints on each guard's value.

## Examples

### Basic Function Type Inference
```racket
(define (double x)
  (+ x x))
; Inferred type: (integer -> integer)
```

### Struct Type Inference
```racket
(struct person (name age))
(define (get-age p)
  (person-age p))
; get-age: ((Structof [name : any] [age : any]) -> any)
```

### Higher-Order Functions
```racket
(define (apply-twice f x)
  (f (f (add1 x))))
; Inferred type: ((any -> any) integer -> any)
```

## Testing

Run unit tests:
```racket
(require "src/test/unit.rkt")
(solve (list test test2 test3 ...))
```

Benchmark on prime sieve:
```racket
(require "src/test/sieve.rkt") 
(solve (list main count-from sieve sift))
```

## Limitations

- **Recursion**: Cannot precisely handle recursive functions due to symbolic divergence
- **Limited Primitives**: Only supports integers and booleans
- **No Loops**: Does not support iterative constructs
- **No Union Types**: Cannot handle variables used with different types

## Future Work

- **Extended Language Support**: Loops, lists, more primitive types
- **Recursion Handling**: Techniques to avoid symbolic divergence
- **Union Types**: Support for variables with multiple possible types
- **LLM Integration**: Combine with large language models for context-aware type inference

## Dependencies

- [Racket](https://racket-lang.org/) programming language
- [Rosette](https://docs.racket-lang.org/rosette-guide/) symbolic evaluation framework
- Z3 SMT solver (included with Rosette)

## Architecture

```
src/
├── hijack/           # Racket syntax overrides for symbolic evaluation
│   ├── basic.rkt     # Basic operations (if, arithmetic, etc.)
│   ├── call.rkt      # Function calls and application
│   ├── define.rkt    # Variable and function definitions
│   ├── struct.rkt    # Struct definitions and operations
│   └── hijack.rkt    # Main hijack module
├── solver/
│   └── solve.rkt     # SMT solver integration and type resolution
├── util/
│   ├── value.rkt     # Value representation and manipulation
│   ├── root.rkt      # Root symbol management
│   ├── primitive.rkt # Symbolic primitive creation
│   └── literal.rkt   # Literal value conversion
└── test/
    ├── unit.rkt      # Unit tests
    └── sieve.rkt     # Prime sieve benchmark
```
