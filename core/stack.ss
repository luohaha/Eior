
(define stack (make-vector 1000))

(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))

(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))

(define index-set!
  (lambda (s i x)
    (vector-set! stack (- (- s i) 1) x)))

(define save-stack
  (lambda (s)
    (let ([v (make-vector s)])
      (let copy ([i 0])
	(if (= i s)
	    v
	    (begin (vector-set! v i (vector-ref stack i))
		   (copy (+ i 1))))))))

(define restore-stack
  (lambda (v)
    (let ([s (vector-length v)])
      (let copy ([i 0])
	(if (= i s)
	    s
	    (begin (vector-set! stack i (vector-ref v i))
		   (copy (+ i 1))))))))


