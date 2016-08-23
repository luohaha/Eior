;;primitive procedure which need only one argument
(define primitive-1
  (list (list 'boolean?) (list 'null?) (list 'pair?)
	(list 'number?) (list 'char?) (list 'string?)
	(list 'symbol?) (list 'car) (list 'cdr)
	(list 'list?) (list 'not) (list 'eval)
	(list 'length) (list 'display) (list 'reverse)
	(list 'zero?) (list 'positive?) (list 'negative?)
	(list 'even?) (list 'odd?) (list 'log)
	(list 'sin) (list 'cos) (list 'tan)
	(list 'string->number) (list 'number->string)
	(list 'integer->char) (list 'char->integer)
	(list 'string-length)))

;;primitive procedure which need two arguments
(define primitive-2
  (list (list 'cons) (list 'eq?) (list '+) (list 'equal?)
	(list '-) (list '*) (list '/) (list '=)
	(list '>) (list '<) (list '>=) (list '<=)
	(list 'and) (list 'or) (list 'eqv?) (list 'memq)
	(list 'memv) (list 'member) (list 'assq) (list 'assv)
	(list 'assoc) (list 'mod) (list 'char=?) (list 'char<?)
	(list 'char>?) (list 'char<=?) (list 'char>=?)
	(list 'string<?) (list 'string>?) (list 'string=?)
	(list 'string<=?) (list 'string>=?) (list 'string-append)))
