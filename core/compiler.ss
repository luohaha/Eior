;;instructions :
;;(halt)
;;(refer-local n x)  =>  load the nth argument in current call frame
;;(refer-free n x)  =>  load the nth free argument in closure
;;(indirect x)
;;(constant obj x)
;;(close n body x) =>
;;(box n x)
;;(test then else)
;;(assign-local var x)
;;(assign-free var x)
;;(conti x)
;;(nuate stack x)
;;(frame x ret)
;;(argument x) => push the argument in acc on stack
;;(apply)
;;(return n)
;;(shift n m x)
;;(primitive-1 proc x)
;;(primitive-2 proc x)

;; the stack' structures
;; (top)
;; | arg1
;; | ...
;; | argn
;; | the next expression   -> x
;; | the current frame    -> f
;; | the current closure   -> c
;; (bottom)

;;register =>
;; a => accumulator
;; x => next expression
;; f => current call frame
;; c => current closure
;; s => current stack

(load "utils.ss")
(load "stack.ss")
(load "set.ss")

(define continuation
  (lambda (s)
    (closure `(refer-local ,0 (nuate ,(save-stack s) (return ,0))) 0 '())))

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

(define find-assignments
  (lambda (x v)
    (cond [(assq x primitive-1) '()]
	  [(assq x primitive-2) '()]
          [(symbol? x) '()]
	  [(pair? x)
	   (record-case
	    x
	    [quote (obj) '()]
	    [begin (b1 . b2) (find-assignments (cons b1 b2) v)]
	    [lambda (vars . body) (find-assignments body (set-minus v vars))]
	    [if (test then else)
		(set-union (find-assignments test v)
			   (set-union (find-assignments then v)
				      (find-assignments else v)))]
	    [set! (var x) (set-union (find-assignments x v)
				     (if (set-member? var v)
					 (list var)
					 '()))]
	    [call/cc (exp) (find-assignments exp v)]
	    [else (let loop ([x x])
		    (if (null? x)
			'()
			(set-union (find-assignments (car x) v)
				   (loop (cdr x)))))]
	    )]
	  [else '()])))

(define find-free
  (lambda (x b)
    (cond [(assq x primitive-1) '()]
	  [(assq x primitive-2) '()]
          [(symbol? x) (if (set-member? x b) '() (list x))]
	  [(pair? x)
	   (record-case
	    x
	    [quote (obj) '()]
	    [begin (b1 . b2) (find-free (cons b1 b2) b)]
	    [lambda (vars . body) (find-free body (set-union vars b))]
	    [if (test then else)
		(set-union (find-free test b)
			   (set-union (find-free then b)
				      (find-free else b)))]
	    [set! (var x) (set-union (find-free var b)
				     (find-free x b))]
	    [call/cc (clo) (find-free clo b)]
	    [else (let loop ([x x])
		    (if (null? x)
			'()
			(set-union (find-free (car x) b)
				   (loop (cdr x)))))])]
	  [else '()])))

(define compile-lookup
  (lambda (x e return-local return-free)
    (let loop-local ([locals (car e)] [n 0])
      (if (null? locals)
	  (let loop-free ([frees (cdr e)] [m 0])
	    (if (eq? (car frees) x)
		(return-free m)
		(loop-free (cdr frees) (+ m 1))))
	  (if (eq? (car locals) x)
	      (return-local n)
	      (loop-local (cdr locals) (+ n 1)))))))

(define compile-refer
  (lambda (x e next)
    (compile-lookup x e (lambda (n) `(refer-local ,n ,next))
		    (lambda (n) `(refer-free ,n ,next)))))

(define collect-free
  (lambda (vars e next)
    (if (null? vars)
	next
	(collect-free (cdr vars) e (compile-refer (car vars)
						  e
						  `(argument ,next))))))

(define make-boxs
  (lambda (sets vars next)
    (let loop ([vars vars] [n 0])
      (if (null? vars)
	  next
	  (if (set-member? (car vars) sets)
	      `(box ,n ,(loop (cdr vars) (+ n 1)))
	      (loop (cdr vars) (+ n 1)))))))

(define compile-multi
  (lambda (lst e s next)
    (define (loop lst)
      (if (null? lst)
	  next
	  (compile (car lst) e s (loop (cdr lst)))))
    (loop lst)))

;;primitive procedure which need only one argument
(define primitive-1
  (list (list 'boolean?) (list 'null?) (list 'pair?)
	(list 'number?) (list 'char?) (list 'string?)
	(list 'symbol?) (list 'car) (list 'cdr)
	(list 'list?) (list 'not) (list 'eval)
	(list 'length) (list 'display) (list 'reverse)))

;;primitive procedure which need two arguments
(define primitive-2
  (list (list 'cons) (list 'eq?) (list '+) (list 'equal?)
	(list '-) (list '*) (list '/) (list '=)
	(list 'and) (list 'or) (list 'eqv?) (list 'memq)
	(list 'memv) (list 'member) (list 'assq) (list 'assv)
	(list 'assoc)))

(define compile
  (lambda (x e s next)
    ;;(debug-line x)
    (cond
     [(assq x primitive-1) => (lambda (v)
				`(close ,0 (primitive-1 ,@v (return ,1)) ,next))]
     [(assq x primitive-2) => (lambda (v)
				`(close ,0 (primitive-2 ,@v (return ,2)) ,next))]
     [(symbol? x) (compile-refer x e
				 (if (set-member? x s)
				     `(indirect ,next)
				     next))]
     [(pair? x)
      (record-case
       x
       [quote (obj) `(constant ,obj ,next)]
       [begin (b1 . b2)
	      (compile-multi (cons b1 b2) e s next)]
       [lambda (vars . body)
	 (let ([free (find-free body vars)]
	       [sets (find-assignments body vars)])
	   (collect-free free e
			 `(close
			   ,(length free)
			   ,(make-boxs sets vars
				       (compile-multi body
						(cons vars free)
						(set-union sets
							   (set-intersect free s))
						`(return ,(length vars))))
			   ,next)))]
       [if (test then else)
	   (let ([after-then (compile then e s next)]
		 [after-else (compile else e s next)])
	     (compile test e s `(test ,after-then, after-else)))]
       [set! (var x) (compile-lookup var e
				     (lambda (n) (compile x e s `(assign-local ,n ,next)))
				     (lambda (n) (compile x e s `(assign-free ,n ,next))))]
       [call/cc (x)
		(let ((c `(conti (argument ,(compile x e s
						     (if (tail? next)
							 `(shift ,1 ,(cadr next) (apply))
							 '(apply)))))))
		  (if (tail? next)
		      c
		      `(frame ,next ,c)))]
       [else
	(let loop ([args (cdr x)] [c (compile (car x) e s (if (tail? next)
							      `(shift ,(length (cdr x)) ,(cadr next) (apply))
							      '(apply)))])
	  (if (null? args)
	      (if (tail? next)
		  c
		  `(frame ,next ,c))
	      (loop (cdr args)
		    (compile (car args)
			     e
			     s
			     `(argument ,c)))))])]
     [else `(constant ,x ,next)])))

(define closure
  (lambda (body n s)
    (let ([v (make-vector (+ n 1))])
      (vector-set! v 0 body)
      (let loop ([i 0])
	(if (= i n)
	    v
	    (begin (vector-set! v (+ i 1) (index s i))
		   (loop (+ i 1))))))))

(define closure-index
  (lambda (clo n)
    (vector-ref clo n)))

(define (box x)
  (list x))

(define (unbox x)
  (car x))

(define (shift-arg n m s)
  (let loop ([n (- n 1)])
    (if (< n 0)
	(- s m)
	(begin (index-set! s (+ n m) (index s n))
	       (loop (- n 1))))))

(define VM
  (lambda (a x f c s)
    ;;(debug-line x)
    (record-case
     x
     [halt () a]
     [primitive-2 (proc x) (let ([n (index f 0)]
				 [m (index f 1)])
			     (VM ((eval proc) n m) x f c s))]
     [primitive-1 (proc x) (let ([n (index f 0)])
			     (VM ((eval proc) n) x f c s))]
     [refer-local (n x) (VM (index f n) x f c s)]
     [refer-free (n x) (VM (closure-index c (+ n 1)) x f c s)]
     [indirect (x) (VM (unbox a) x f c s)]
     [constant (obj x) (VM obj x f c s)]
     [close (n body x) (VM (closure body n s) x f c (- s n))]
     [box (n x) (index-set! s n (box (index s n)))
	  (VM a x f c s)]
     [test (then else) (VM a (if a then else) f c s)]
     [assign-local (n x) (set-car! (index f n) a)
		   (VM a x f c s)]
     [assign-free (n x) (set-car! (closure-index c (+ n 1)) a)
		  (VM a x f c s)]
     [conti (x) (VM (continuation s) x f c s)]
     [nuate (stack x) (VM a x f c (restore-stack stack))]
     [frame (ret x) (VM a x f c (push ret (push f (push c s))))]
     [argument (x) (VM a x f c (push a s))]
     [shift (n m x) (VM a x f c (shift-arg n m s))]
     [apply () (VM a (closure-index a 0) s a s)]
     [return (n)
	     (let ([s (- s n)])
	       (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]
     [else (display "syntax error\n")])))

(define tagged-list?
  (lambda (exp tag)
    (if (eq? (car exp) tag) #t #f)))

(define Y
  '(lambda (X)
    ((lambda (proc)
       (X (lambda (arg) ((proc proc) arg))))
     (lambda (proc)
       (X (lambda (arg) ((proc proc) arg)))))))

(define gen-Y-exp
  (lambda (vars vals)
    (define (gen var val)
      `(,Y (lambda (,var) ,(pre-compile val))))
    (map gen vars vals)))

(define pre-compile
  (lambda (x)
    (cond [(not (pair? x)) x]
          [(tagged-list? x 'let)
	   `((lambda (,@(map car (cadr x))) ,@(pre-compile (cddr x))) ,@(pre-compile (map cadr (cadr x))))]
	  [(tagged-list? x 'letrec)
	   `((lambda (,@(map car (cadr x)))
	       ,@(pre-compile (cddr x)))
	     ,@(gen-Y-exp (map car (cadr x)) (map cadr (cadr x))))]
	  [(tagged-list? x 'list)
	   (if (= 2 (length x))
	       `(cons ,(cadr x) ())
	       `(cons ,(cadr x) ,(pre-compile `(list ,@(cddr x)))))]
	  [(pair? x) (map pre-compile x)]
	  [else (display "syntax error!\n")])))

(define evaluate
  (lambda (x)
    (VM '() (compile (pre-compile x) '() '() '(halt)) 0 '() 0)))

(display (evaluate '(letrec ([list-ref
			      (lambda (lst n)
				(if (= n 0)
				    (car lst)
				    (list-ref (cdr lst) (- n 1))))])
		      (list-ref '(1 2 3 4) 1))))
