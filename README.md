# Eior
A compiler for Eior which is stack-base and just like scheme. This compiler can tranfer source  code to s-exprssion assembly code, and then the VM would execute it.

# Usage

```
cd core/
./run [filename]
```

# stack & register

```
;; the stack' structures
;; (top)
;; | arg1
;; | ...
;; | argn
;; | the next expression   -> x
;; | the current frame    -> f
;; | the current closure   -> c
;; (bottom)
```

```
;;register =>
;; a => accumulator
;; x => next expression
;; f => current call frame
;; c => current closure
;; s => current stack
```

# support

```scheme
((let) (letrec) (if) (lambda) (quote) (set!) (call/cc) (begin))
```

```scheme
((boolean?) (null?) (pair?) (number?) (char?) (string?) (symbol?) (car) (cdr) (list?) (not) (eval) (length) (display) (reverse) (zero?) (positive?) (negative?) (even?) (odd?) (log) (sin) (cos) (tan) (string->number) (number->string) (integer->char) (char->integer) (string-length))
```

```scheme
((cons) (eq?) (+) (equal?) (-) (*) (/) (=) (>) (<) (>=) (<=) (and) (or) (eqv?) (memq) (memv) (member) (assq) (assv) (assoc) (mod) (char=?) (char<?) (char>?) (char<=?) (char>=?) (string<?) (string>?) (string=?) (string<=?) (string>=?) (string-append))
```

# example

* list-ref

```scheme
(letrec ([list-ref
	  (lambda (lst x)
	    (if (= x 0)
			(car lst)
			(list-ref (cdr lst) (- x 1))))])
  (list-ref '(a b c d) 1))
```

* fibonacci

```scheme
(letrec ([fib-iter (lambda (a b count)
		     (if (= count 0)
			 b
			 (fib-iter (+ a b) a (- count 1))))])
  (let ([fib (lambda (n)
	       (fib-iter 1 0 n))])
    (fib 100)))
```

# reference

* Three Implementation Models for Scheme - R. Kent Dybvig
* Structure and Interpretation of Computer Programs - Abelson, H. and Sussman, G
* Essentials of Programming Languages - Daniel P. Friedman Mitchell Wand