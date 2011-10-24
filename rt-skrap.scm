(define (g-Seq-body gens inputs connections first-inputs generator-names generators kont)
  (fix-defines
   (define outputs (cl-car connections))
   (define gen (cl-car gens))
   (define generator (and gen (make-generator gen inputs outputs)))
   (define generator-name (rt-gensym "generator"))
   `(  ,generator-name
       ,@(gen-input-args inputs)
       ,(if (null? (cdr gens))
	    'kont
	    `(lambda (,@(gen-input-args outputs))
	       ,(g-Seq-0 (cdr gens) outputs (cdr connections) first-inputs
			 generator-name
			 generator
			 kont))))))

(define (g-Seq code inputs outputs)
  (define connections (g-Seq-get-connections code inputs outputs))
  (g-Seq-body (cdr code) 
	      (car connections)
	      (cdr connections)
	      (car connections)
	      '()
	      '()
	      (lambda (generator-names generators body)
		`(let ,(map list generator-names generators)
		   (lambda (,@(gen-input-args first-inputs) kont)
		     ,body)))))



(define (g-Seq-old code inputs outputs)
  (let ((gens (cdr code))
	(connections (g-Seq-get-connections code inputs outputs))) ;; Also checks legal status
    (fix-defines
     (define generator-names (map (lambda (n) (rt-gensym "generator")) (iota (length gens))))
     (define prev-outputs (generator-inputs (car gens) inputs))
     (define generators '())
     
     (define body (let loop ((generator-names generator-names)
			     (gens gens)
			     (prev-outputs prev-outputs))
		    (push! (make-generator (car gens) prev-outputs 1) generators)
		    `(  ,(car generator-names)             ;; generator
			,@(gen-input-args prev-outputs)    ;; arguments
			,(if (null? (cdr generator-names)) ;; the continuation
			     'kont
			     (let ((next-outputs (generator-outputs (car generators) outputs)))
			       `(lambda (,@(gen-input-args next-outputs))
				  ,(loop (cdr generator-names)
					 (cdr gens)
					 next-outputs)))))))
     
     `(let ,(map list generator-names (reverse! generators))
	(lambda (,@(gen-input-args prev-outputs) kont)
	  ,body)))))


