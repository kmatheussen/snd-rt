

(define (capitol-first-old? sym)
  (memq (string->symbol (string (car (string->list (symbol->string sym)))))
        '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))

(define (capitol-first? sym)
  (let loop ((chars (string->list (symbol->string sym))))
    (if (null? chars)
	#f
	(let ((sym (string->symbol (string (car chars)))))
	  (cond ((memq sym '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
		 #t)
		((memq sym '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
		 #f)
		(else
		 (loop (cdr chars))))))))
	  
#!
(capitol-first? '-ZbZA50)
(capitol-first? '-zbZA50)
(capitol-first? '-)
(take '(2 3 4) 2)
(take '(2 3 4) 2)
!#

(define (gen-input-args n)
  (map (lambda (n)
	 (string->symbol (string-append "input" (number->string n))))
       (iota n)))

#!
(gen-input-args 0)
!#

(delafina (gen-ret-args n :key (start-val 0))
  (map (lambda (n)
	 (string->symbol (string-append "ret" (number->string (+ start-val n)))))
       (iota n)))


(define stalin-generators-inputs (make-hash-table 19))
(define stalin-generators-outputs (make-hash-table 19))
;;(define stalin-generators-legal (make-hash-table 19))
(define stalin-generators-transform (make-hash-table 19))

(define (stalin-add-generator name transform-func input-func output-func)
  (hashq-set! stalin-generators-transform name transform-func)
  (hashq-set! stalin-generators-inputs name input-func)
  (hashq-set! stalin-generators-outputs name output-func)
  ;;(hashq-set! stalin-generators-legal name legal-func)
  )

  
#!
(stalin-add-generator 'Seq g-Seq g-Seq-inputs g-Seq-outputs)
!#

(define (generator-name gen)
  (if (pair? gen)
      (generator-name (car gen))
      gen))

(define (is-generator? gen)
  (or (and (not (pair? gen))
	   (symbol? gen)
	   (capitol-first? gen))
      (and (pair? gen)
	   (symbol? (car gen))
	   (hashq-ref stalin-generators-transform (car gen)))))

(define (find-generator-from-stalin-func gen)
  (take-while (lambda (expr)
                (not (memv expr stalin-reserved-keywords)))
              (last (nth-cdr 2 (nth 2 (get-stalin-func (generator-name gen)))))))
#!
(define-stalin (Stereo-pan2 c)
  (define gakk1 9)
  (define gakk2 10)
  (Seq b)
  :where b 100
  :where c 2000)
(find-generator-from-stalin-func '(Stereo-pan))
!#

(define (generator-inputs gen def-inputs)
  (cond ((hashq-ref stalin-generators-inputs (generator-name gen))
	 ((hashq-ref stalin-generators-inputs (generator-name gen)) gen def-inputs))
	((and (is-generator? (generator-name gen))
	      (get-stalin-func (generator-name gen)))
	 (generator-inputs (find-generator-from-stalin-func gen)
			   def-inputs))
	((and (symbol? gen)
	      (capitol-first? gen))
	 def-inputs)
	;;#f)
	((not (pair? gen))
	 0)
	(else
	 def-inputs)))

(define (generator-outputs gen def-outputs)
  (cond ((hashq-ref stalin-generators-outputs (generator-name gen))
         (c-display "hepp1" gen (generator-name gen))
	 (or ((hashq-ref stalin-generators-outputs (generator-name gen)) gen def-outputs)
	     def-outputs))
	((and (is-generator? (generator-name gen))
	      (get-stalin-func (generator-name gen)))
         (c-display "hepp2" gen)
         (c-display (find-generator-from-stalin-func gen))
	 (generator-outputs (find-generator-from-stalin-func gen)
			    def-outputs))
	((and (symbol? gen)
	      (capitol-first? gen))
         (c-display "hepp3")
	 def-outputs)
	((not (pair? gen))
	 1)
	(else
	 1)))

#!
(generator-outputs '(Seq 2 3) 1)
(generator-outputs '(Stereo-pan) 1)
(generator-inputs '(Seq 2 3) 0)
(generator-inputs '(Seq (Incrementer 1)) 1)
!#

(delafina (make-generator gen def-inputs def-outputs)
  (cond (#f #f)

	((hashq-ref stalin-generators-transform (generator-name gen))
	 (  (hashq-ref stalin-generators-transform (generator-name gen))
	    gen
	    def-inputs
	    def-outputs))

	((is-generator? gen)
	 gen)
	;;(make-generator (generator-transform gen :inputs inputs :outputs outputs)))

	((is-generator? (generator-name gen))
	 gen)

	((not (pair? gen)) ;; constant
         (let ((kont (rt-gensym "kont")))
           `(lambda (,kont)
              (,kont ,gen))))

        (else
         (let* ((inputs (gen-input-args def-inputs))
                (kont (rt-gensym "kont"))
                (args (map (lambda (arg)
			     (if (pair? arg)
				 (list #t (rt-gensym "arg") (if (eq? 'autovar_ (car arg))
								(nth 2 arg)
								arg))
				 (list #f arg)))
			   (stalin-macroexpand `(,@gen ,@inputs))))) ;; autovar?, autovar_?, ...
           `(let ,(map cdr (%filter car args)) ;non-direct-args
              (lambda (,@inputs ,kont)
                (,kont ,(map cadr args))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Seq generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Seq-inputs code inputs)
  (generator-inputs (nth 1 code) inputs))

(define (g-Seq-outputs code outputs)
  (generator-outputs (last code) outputs))


;; Returns a list of connections.
;; Example:
;; [(Seq   (Par 2 3) (* 2) (* 3) ) 1 1]
;;    -> (0         2     1     1)
;; As usual, the 'inputs' and 'outputs' arguments are
;; only default values.
(define (g-Seq-connections-0 code gens n prev-outputs final-outputs)
  (fix-defines
   (define gen (cl-car gens))
   (define curr-inputs (and gen (generator-inputs gen prev-outputs)))
   (define next-gen (cl-cadr gens))
   ;;(c-display n "gen" gen next-gen)
   (cond ((not gen)
	  (list prev-outputs))
	 ((and (not (= prev-outputs curr-inputs))
	       (> n 0))
	  (c-display "Error in Seq:" code ": Mismatch in number of inputs and outputs."
		     "\n\tExpression" n "produces" prev-outputs "signals,"
		     "\n\twhile Expression" (1+ n) "expects" curr-inputs "signals.")
	  (throw 'compilation-error))
	 (else
	  (cons curr-inputs
		(g-Seq-connections-0 code
				     (cdr gens)
				     (1+ n)
				     (generator-outputs gen (if next-gen
								(generator-inputs next-gen 1)
								final-outputs))
				     final-outputs))))))
(define (g-Seq-connections code inputs outputs)
  (g-Seq-connections-0 code (cdr code) 0 inputs outputs))

#!
(pretty-print (stalin-macroexpand '(Seq (In (softsynth))
					(Par (* 2)
					     (* 3)))))

(g-Seq-connections '(Seq (In (hepp))
			 (Par (+ 8)
			      (+ 9))
			 (Par (* 2)
			      (* 3)))
		   0 1)

(g-Seq-connections '(Seq (* 2))
		   9 10)

(g-Seq-connections '(Seq (Par 2 3 9 8 8)
			 (* 2) 
			 (* 1)
			 (Split Identity
				Identity
				Identity
				Identity)
			 (Par (* 2) (* 3) (+ 9))
			 (Par (* 4)
			      (* 5))
			 (* 1))
		   9 10)
!#


#!
Qi: (approx)
(define g-Seq
  Code         Inputs Outputs -> (g-Seq-0 Code [] Inputs Outputs [Lambda [(gen-input-args Outputs) kont]])
  []           Inputs Outputs -> [])
(define g-Seq-0
  [Gen | Rest] Generators Inputs Outputs Code -> (g-Seq-0 Rest
					            [Generators | [(rt-gensym "generator") (make-generator Gen Inputs Outputs)]]
						    Outputs
						    (get-Inputs Gen Outputs)
						    [lambda (gen-input-args Outputs)])
  []           Generators Inputs Outputs Code -> [let Generators Code])
!#

(define (g-Seq-generators gens connections)
  (if (null? gens)
      '()
      (cons (make-generator (car gens) (car connections) (cadr connections))
	    (g-Seq-generators (cdr gens) (cdr connections)))))

(define (g-Seq-body generator-names generators connections)
  `( ,(car generator-names)
     ,@(gen-input-args (car connections))
     ,(if (null? (cdr generator-names))
	  'kont
	  `(lambda (,@(gen-input-args (cadr connections)))
	     ,(g-Seq-body (cdr generator-names)
			  (cdr generators)
			  (cdr connections))))))

(define (g-Seq code inputs outputs)
  (fix-defines
   (define gens            (cdr code))
   (define connections     (g-Seq-connections code inputs outputs))
   (define generators      (g-Seq-generators gens connections))
   (define generator-names (map (lambda (g) (rt-gensym "generator")) generators))
   (define body            (g-Seq-body generator-names generators connections))
   `(let ,(map list generator-names generators)
      (lambda (,@(gen-input-args (car connections)) kont)
	,body))))


(stalin-add-generator 'Seq g-Seq g-Seq-inputs g-Seq-outputs) ;; g-Seq-check-legal)

(define-stalin-macro (Seq . gens)
  (g-Seq `(Seq ,@gens) 1 1))


#!
(g-Seq '(Seq (In (softsynth))
	     (Par (* 2)
		  (* 3)))
       1 1)

(pretty-print (g-Seq '(Seq (* -1 89)) 3 3))
(pretty-print (g-Seq '(Seq (* -1 89)
			   (Prod Identity
                                 Identity)
			   )
		     3 3))
(is-generator? '(Identity))
(hashq-ref stalin-generators-transform (generator-name '(Identity)))

(pretty-print (stalin-macroexpand '(Seq (* -1))))
(pretty-print (stalin-macroexpand '(Seq 2 
					(* -1 (a)))))
->
(let ((generator0 (lambda (input0 kont0)
                    (kont0 (* -1 input0)))))
  generator0)

(pretty-print (stalin-macroexpand '(Seq (* -1 (ha))
					;;(Seq 2 3)
					(* -2))))
->
(let ((generator0 (lambda (input0 kont0)
                    (kont0 (* -1 input0)))))
  (lambda (input1 kont1)
    (generator0 input1 (lambda (result0)
                         (kont1 (* -2 result0))))))
->
(lambda (input1 kont1)
  ((lambda (input0 kont0)
     (kont0 (* -1 input0)))
   input1 (lambda (result0)
            (kont1 (* -2 result0)))))


(pretty-print (g-Seq '(Seq (* (gapp))
			   (* -2))))
->
(let ((generator0 (let ((arg0 (gapp)))
                    (lambda (input0 kont0)
                      (kont0 (* arg0 input0))))))
  (lambda (input1 kont1)
    (generator0 input1 (lambda (result0)
                         (kont1 (* -1 result0))))))
->
(let ((arg0 (gapp)))
  (lambda (input1 kont1)
    ((lambda (input0 kont0)
       (kont0 (* arg0 input0)))
     input1 (lambda (result0)
              (kont1 (* -1 result0))))))

(pretty-print (g-Seq '(Seq (* (gapp))
			   (* (hupp))
			   (* -2))))
->
(let ((generator0 (let ((arg0 (gapp)))
                    (lambda (input0 kont0)
                      (kont0 (* arg0 input0)))))
      (generator1 (let ((arg1 (hupp)))
                    (lambda (input1 kont1)
                      (kont1 (* arg1 input1))))))
  (lambda (input2 kont2)
    (generator0 input2 (lambda (result0)
                         (generator1 result0 (lambda (result1)
                                               (kont2 result1)))))))
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Par generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Par-inputs code inputs)
  (fold + 0 (map (lambda (gen)
		   (generator-inputs gen (min inputs 1)))
		 (cdr code))))

(define (g-Par-outputs code outputs)
  (fold + 0 (map (lambda (gen)
		   (generator-outputs gen 1))
		 (cdr code))))

(define (g-Par-check-legal code def-inputs def-outputs errortext)
  #t)

#!
(define (g-Par-check-legal code def-inputs def-outputs errortext)
  (define inputs (generator-inputs (nth 1 code) def-inputs))
  (c-for-each (lambda (n gen)
		(when (not (= inputs (generator-inputs gen inputs)))
		  (c-display "Error in " errortext ":" code ": Mismatch in number of inputs."
			     "\n\t      Expression 1 expects" inputs "signals,"
			     "\n\twhile Expression" (+ 1 n) "expects" (generator-inputs gen inputs) "signals.")
		  (throw 'compilation-error)))
	      (nth-cdr 2 code)))
!#

#!
(g-Par-check-legal '(Par (+ 2) 9) "Par")
(g-Par-check-legal '(Par 2 3 (* 2)) "Par")
(g-Par-outputs '(Par 2 3 (* 2) (Ser 9 8 4 1 az)))
!#

(define (g-Par code def-inputs def-outputs)
  ;;(c-display "g-Par" code def-inputs def-outputs)
  (g-Par-check-legal code def-inputs def-outputs "Par")
  (let ((gens (cdr code)))    
    (define generators (map (lambda (gen)
                              (make-generator gen def-inputs 1))
                            gens))
    (define generator-names (map (lambda (n) (rt-gensym "generator")) (iota (length gens))))
    (define input-args (gen-input-args (g-Par-inputs code def-inputs)))
    ;;(c-display "a" generator-names)
    ;;(c-display "b" generators)
    `(let ,(map list generator-names generators)
       (lambda (,@input-args kont)
	 ,(let loop ((generator-names generator-names)
		     (gens gens)
		     (all-input-args input-args)
		     (ret-args '()))
	    ;;(c-display "all-input-args" all-input-args );(generator-inputs (car gens) (min def-inputs 1)))
	    (if (null? generator-names)
		`(kont ,@ret-args)
		(let* ((gen-inputs (generator-inputs (car gens) (min def-inputs 1)))
		       (gen-outputs (generator-outputs (car gens) 1))
		       (input-args (take all-input-args gen-inputs)))
		  `(,(car generator-names) ,@input-args
				           ,(let ((output-args (gen-ret-args gen-outputs :start-val (length ret-args))))
					      `(lambda (,@output-args)
						 ,(loop (cdr generator-names)
							(cdr gens)
							(nth-cdr gen-inputs all-input-args)
							(append ret-args output-args))))))))))))

#!
(pretty-print (g-Par '(Par (* 2) (* 3)) 2 2))

(pretty-print (g-Par '(Par 2 3) 9 10))

(pretty-print (g-Par '(Par (+ 2) 
			   (Seq (* 4) (+ 5))
			   (Par 9 3 2)
			   (* 3))
                     2 10))
(Par 2 3)
->
(let ((generator0 (lambda (kont)
		    (kont 2)))
      (generator1 (lambda (kont)
		    (kont 3))))
  (lambda (kont)
    (generator0 (lambda (ret0)
		  (generator1 (lambda (ret1)
				(kont ret0 ret1)))))))
!#

(stalin-add-generator 'Par g-Par g-Par-inputs g-Par-outputs) ;; g-Par-check-legal)

(define-stalin-macro (Par . gens)
  (g-Par `(Par ,@gens) 1 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Split generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Split-inputs code def-inputs)
  (generator-inputs (nth 1 code) def-inputs))

(define (g-Split-def-split-outputs code def-outputs)
  (fix-defines
   (define splitters (nth-cdr 2 code))
   (define num-splitters (length splitters))
   (define split-outputs (if (= 0 (modulo def-outputs num-splitters))
			     (/ def-outputs num-splitters)
			     1))))

(define (g-Split-outputs code def-outputs)
  (fix-defines
   (define splitters (nth-cdr 2 code))
   (define split-outputs (g-Split-def-split-outputs code def-outputs))
   (fold + 0 (map (lambda (gen)
		    (generator-outputs gen split-outputs))
		  splitters))))

(define (g-Split-check-legal code def-inputs def-outputs errortext)
  (define internal-channels (generator-outputs (nth 1 code) 1))
  (c-for-each (lambda (n gen)
		(when (not (= internal-channels (generator-inputs gen internal-channels)))
		  (c-display "Error in " errortext ":" code ": Mismatch in number of inputs."
			     "\n\t      Expression 1 produces" internal-channels "signal(s),"
			     "\n\twhile Expression" (+ 2 n) "expects" (generator-inputs gen internal-channels) "signal(s).")
		  (throw 'compilation-error)))
	      (nth-cdr 2 code)))

#!
(g-Split-check-legal '(Split 2 (* 9) (- 2) 4) 1 1 "Split")
(g-Split-check-legal '(Split 2 3 (* 2)) "Split")
(g-Split-outputs '(Split 2 3 (* 2) (Ser 9 8 4 1 az)))
(g-Split-inputs '(Split (Par (* 2) (* 3)) 3 (* 2) (Ser 9 8 4 1 az)))
!#


(define (g-Split code def-inputs def-outputs)
  (g-Split-check-legal code def-inputs def-outputs "Split")
  (fix-defines
   (define gens (cdr code))
   (define num-inputs (g-Split-inputs code def-inputs))
   (define receiver-outputs (generator-outputs (nth 1 code) 1))
   (define split-def-outputs (g-Split-def-split-outputs code def-outputs))
   (define receiver-gen (make-generator (car gens) def-inputs 1))
   (define splitters (cdr gens))
   (define splitter-gens (map (lambda (gen)
				(make-generator gen receiver-outputs split-def-outputs))
			      splitters))
   (define generator-names (map (lambda (n) (rt-gensym "generator")) (iota (length gens))))    
   (define input-args (gen-input-args num-inputs))
   (define receiver-input-args (gen-input-args receiver-outputs))
   ;;(c-display "a" generator-names)
   ;;(c-display "b" generators)
   `(let ,(map list generator-names (cons receiver-gen splitter-gens))
      (lambda (,@input-args kont)
	(,(car generator-names) ,@input-args
    	                        (lambda (,@receiver-input-args)
				  ,(let loop ((generator-names (cdr generator-names))
					      (generators splitter-gens)
					      (ret-args '()))
				     (if (null? generator-names)
					 `(kont ,@ret-args)
					 `(   ,(car generator-names) ,@receiver-input-args
					      ,(let ((output-args (gen-ret-args (generator-outputs (car generators) split-def-outputs) :start-val (length ret-args))))
						 `(lambda (,@output-args)
						    ,(loop (cdr generator-names)
							   (cdr generators)
							   (append ret-args output-args)))))))))))))


				

#!
(pretty-print (g-Split '(Split Identity
			       (+ 3)
			       (* 5))
		       1 1))
(pretty-print (g-Split '(Split (oscil :freq 400) ;(Seq (oscil :freq 400))
			       Identity
			       Identity)
		       0 1))

!#

(stalin-add-generator 'Split g-Split g-Split-inputs g-Split-outputs) ;; g-Split-check-legal)

(define-stalin-macro (Split . gens)
  (g-Split `(Split ,@gens) 1 1))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Merge generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Merge-inputs code)
  (fold + 0 (map generator-inputs (butlast (cdr code) 1))))

(define (g-Merge-outputs code)
  (generator-outputs (last code)))

(define (g-Merge-check-legal code errortext)
  (define internal-channels (generator-inputs (last code)))
  (when (not (= 0 (modulo (fold + 0 (map generator-outputs (butlast (cdr code) 1)))
			  internal-channels)))
    (c-display "Error in " errortext ":" code ": Mismatch in number of outputs from generators to inputs of the last generator.")
    (c-display "  (" (fold + 0 (map generator-outputs (butlast (cdr code) 1)))
	       "and" internal-channels
	       "are not a multiply of k!)")
    (throw 'compilation-error))    
  (c-for-each (lambda (n gen)
		(when (not (= (generator-outputs gen) internal-channels))
		  (c-display "Error in " errortext ":" code ": Mismatch in number of outputs."
			     "\n\t      The last expression expects" internal-channels "signal(s),"
			     "\n\twhile Expression" (+ 1 n) "(\"" gen "\") produces" (generator-outputs gen) "signal(s).")
		  (throw 'compilation-error)))
	      (butlast (nth-cdr 1 code) 1)))

#!
(g-Merge-check-legal '(Merge (+ 1)
			     (+ 2)
			     (+ 6)
			     (+ 7)
			     (Par (+ 3)
				  ;;(+ 4)
				  (+ 5)))
		     "Merge")
(g-Merge-check-legal '(Merge (+ 1)
			     (Par 2)
			     (- 2)
			     (/ 9)
			     (Split Identity
				    (* 9)
				    (* 3)))
		     "Merge")
(g-Merge-check-legal '(Merge 2 (* 9) (- 2) 4) "Merge")
(g-Merge-check-legal '(Merge 2 3 (* 2)) "Merge")
(g-Merge-outputs '(Merge 2 3 (* 2) (Ser 9 8 4 1 az)))
(g-Merge-inputs '(Merge (Par (* 2) (* 3)) 3 (* 2) (Ser 9 8 4 1 az)))
!#


(define (g-Merge code)
  (g-Merge-check-legal code "Merge")
  (let ((gens (cdr code)))    

    (let* ((generators (map make-generator (butlast gens 1)))
	   (num-generators (length generators))
	   (generator-names (map (lambda (n) (rt-gensym "generator")) (iota num-generators)))
	   
	   (final-generator (make-generator (last gens)))
	   (final-generator-name (rt-gensym "final-generator"))
	   
	   (internal-channels (generator-inputs (last gens)))
	   (das-ret-args (gen-ret-args (* internal-channels num-generators)))
	   (input-channels (g-Merge-inputs code))
	   (input-args (gen-input-args input-channels)))

      ;;(c-display "a" generator-names)
      ;;(c-display "b" generators)
      `(let ,(map list
		  (append generator-names (list final-generator-name))
		  (append generators (list final-generator)))
	 (lambda (,@input-args kont)
	   ,(let loop ((generator-names generator-names)
		       (generators generators)
		       (gens gens)
		       (ret-args das-ret-args)
		       (input-args input-args))
	      ;;(c-display ret-args internal-channels generator-names (null? generator-names))
	      ;;(c-display input-args (generator-inputs (car gens)) "hepp")
	      (if (null? generator-names)                                       ;; das-ret-args ->
		  `(,final-generator-name ,@(let loop ((ret-args das-ret-args)) ;; This is not quite correct. Fix! (a b c d -> [a b] [c d] instead of [a c] [ b d])
					      (if (null? ret-args)
						  '()
						  (let ((num-signals (/ (length das-ret-args) internal-channels)))
						    (cons (if (= 1 num-signals)
							      (car ret-args)
							      `(+ ,@(take ret-args num-signals)))
							  (loop (drop ret-args num-signals))))))
					  kont)
		  (let ((input-channels (generator-inputs (car gens))))
		    ;;(c-display "input-channels: " input-channels)
		    ;;(c-display "input-args:" input-args)
		    ;;(c-display "ret-args: " ret-args)
		    ;;(c-display "internal-channels" internal-channels)
		    `(,(car generator-names) ,@(take input-args input-channels)
		      (lambda (,@(take ret-args internal-channels))
			,(loop (cdr generator-names)
			       (cdr generators)
			       (cdr gens)
			       (drop ret-args internal-channels)
			       (drop input-args input-channels))))))))))))
#!
(pretty-print (g-Merge '(Merge (+ 3)
			       (* 5)
			       Identity)))
->
(let ((generator0 (lambda (input kont)
		    (kont (+ 3 kont))))
      (generator1 (lambda (input kont)
		    (kont (* 5 kont))))
      (generator2 Identity))
  (lambda (input0 input1 kont)
    (generator0 input0 (lambda (ret0)
			 (generator1 input1 (lambda (ret1)
					      (generator2 (+ ret0 ret1) kont)))))))
!#

(stalin-add-generator 'Merge g-Merge g-Merge-inputs g-Merge-outputs) ;; g-Merge-check-legal)

(define-stalin-macro (Merge . gens)
  (g-Merge `(Merge ,@gens)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Identity generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Identity-inputs code inputs)
  1)

(define (g-Identity-outputs code outputs)
  1)

#!
(pretty-print (g-Split '(Split Identity
			       (+ 3)
			       (* 5))))
!#

(define (g-Identity def-inputs def-outputs code)
  '(lambda (val kont)
     (kont val)))

(stalin-add-generator 'Identity g-Identity g-Identity-inputs g-Identity-outputs) ;; g-Identity-check-legal)

#!
(Define-stalin (Identity input)
  input)
!#

#!
(pretty-print (stalin-macroexpand '(Sum Identity Identity)))
(pretty-print (stalin-macroexpand '(Identity)))
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Cut generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Cut-inputs code inputs)
  1)

(define (g-Cut-outputs code outputs)
  0)

#!
(pretty-print (g-Split '(Split 50
			       Cut
			       Cut)))
!#

(stalin-add-generator 'Cut #f g-Cut-inputs g-Cut-outputs) ;; g-Cut-check-legal)

(Define-stalin (Cut input)
  )

#!
(pretty-print (stalin-macroexpand '(Sum Cut Cut)))
(pretty-print (stalin-macroexpand '(Split (Par Identity
					       Identity)
					  (Par Cut
					       Identity)
					  (Par Identity
					       Cut))))
(define (Cut input kont)
  (kont))
(define (Identity input kont)
  (kont input))
( (primitive-eval (stalin-macroexpand '(Split (Par Identity
						   Identity)
					      (Par Cut
						   Identity)
					      (Par Identity
						   Cut))))
  2 3
  c-display)
( (primitive-eval (stalin-macroexpand '(Split Identity
					       Cut
					       Identity
					       Identity
					       Cut)))
  3
  c-display)
->
  3 3


( (let ((func (primitive-eval (stalin-macroexpand '(Buffer 1
							   (Lambda (input old-val)
								   old-val
								   (+ input old-val)))))))
    (func 2 c-display)
    (func 3 c-display)
    (func 4 c-display)
    (func 5 c-display)
    (func 6 c-display)))

!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Sum generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Sum-inputs code inputs)
  (generator-inputs (nth 1 code) inputs))

(define (g-Sum-outputs code outputs)
  1)

(define (g-Sum-check-legal code def-inputs def-outputs errortext)
  (define inputs (generator-inputs (nth 1 code) (min def-inputs 1)))
  (c-for-each (lambda (n gen)
		(when (not (= inputs (generator-inputs gen inputs)))
		  (c-display "Error in " errortext ":" code ": Mismatch in number of inputs."
			     "\n\t      Expression 1 expects" inputs "signals,"
			     "\n\twhile Expression" (1+ n) "expects" (generator-inputs gen inputs) "signals.")
		  (throw 'compilation-error))
		(when (not (= 1 (generator-outputs gen 1)))
		  (c-display "Error in " errortext ":" code ": Mismatch in number of outputs."
			     "\n\t Expression" (1+ n) "has" (generator-outputs gen 1) "output signals,"
			     "\n\t expected 1.")
		  (throw 'compilation-error)))
	      (nth-cdr 1 code)))


(delafina (g-Sum code def-inputs def-outputs :key (operator '+))
  ;;(c-display "Sum" code def-inputs def-outputs operator)
  (g-Sum-check-legal code def-inputs def-outputs "Sum")
  (let ((gens (cdr code)))    
    (define generators (map (lambda (gen)
                              (make-generator gen def-inputs 1))
                            gens))
    (define generator-names (map (lambda (n) (rt-gensym "generator")) (iota (length gens))))
    (define input-args (gen-input-args (g-Sum-inputs code def-inputs)))
    ;;(c-display "a" generator-names)
    ;;(c-display "b" generators)
    `(let ,(map list generator-names generators)
       (lambda (,@input-args kont)
	 ,(let loop ((generator-names generator-names)
		     (generators generators)
		     (ret-args '()))
	    (if (null? generator-names)
		`(kont (,operator ,@ret-args))
		`(,(car generator-names) ,@input-args
		  ,(let ((output-args (gen-ret-args (generator-outputs (car generators) 1) :start-val (length ret-args))))
		     `(lambda (,@output-args)
			,(loop (cdr generator-names)
			       (cdr generators)
			       (append ret-args output-args)))))))))))
#!
(pretty-print (g-Sum '(Sum Identity
			   (+ 3)
			   (Split Identity
				  (* 2))
			   (* 5))
		     2 3))
(pretty-print (stalin-macroexpand '(Sum (* clean)
					(Seq Fx
					     (* wet)))))
(pretty-print (g-Sum '(Sum (* clean)
			   (Seq Fx
				(* wet)))
		     2 3))
!#

(stalin-add-generator 'Sum g-Sum g-Sum-inputs g-Sum-outputs) ;; g-Sum-check-legal)

(define-stalin-macro (Sum . gens)
  (g-Sum `(Sum ,@gens) 1 1))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Prod generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define g-Prod-inputs g-Sum-inputs)
(define g-Prod-outputs g-Sum-outputs)

(define (g-Prod-check-legal code def-inputs def-outputs errortext)
  (g-Sum-check-legal code def-inputs def-outputs errortext))

(define (g-Prod code def-inputs def-outputs)
  (g-Prod-check-legal code def-inputs def-outputs "Prod")
  (g-Sum code def-inputs def-outputs :operator '*))

#!
(pretty-print (stalin-macroexpand '(Prod Identity
					 (+ 3)
					 (Split Identity
						(* 2))
					 (* 5))))
!#

(stalin-add-generator 'Prod g-Prod g-Prod-inputs g-Prod-outputs) ;; g-Prod-check-legal)

(define-stalin-macro (Prod . gens)
  (g-Prod `(Prod ,@gens)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Lambda generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Lambda-inputs code def-inputs)
  (length (nth 1 code)))

(define (g-Lambda-outputs code def-outputs)
  (length (nth-cdr 2 code)))

(define (g-Lambda-check-legal code errortext)
  #t)

(define (g-Lambda code def-inputs def-outputs)
  (g-Lambda-check-legal code "Lambda")
  `(lambda (,@(nth 1 code) kont)
     (kont ,@(nth-cdr 2 code))))

#!
(pretty-print (g-Lambda '(Lambda (a b)
			    (+ 2 3)
			    100
			    )))
(map (lambda (f)
       (f '(Lambda (a b c d e f)
		   (+ 2 3)
		   100
		   9
		   )))
     (list g-Lambda-inputs g-Lambda-outputs g-Lambda))
!#

(stalin-add-generator 'Lambda g-Lambda g-Lambda-inputs g-Lambda-outputs) ;; g-Lambda-check-legal)

(define-stalin-macro (Lambda . gens)
  (g-Lambda `(Lambda ,@gens) 1 1))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Buffer generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Buffer-inputs code def-inputs)
  1)

(define (g-Buffer-outputs code def-outputs)
  1)

(define (g-Buffer-check-legal code errortext def-inputs def-outputs)
  (when (not (= 2 (generator-inputs (nth 2 code) 2)))
    (c-display "Error in " errortext ":" code ": Expected 2 inputs for expression, found" (generator-inputs (nth 2 code) 2))
    (throw 'compilation-error))
  (when (not (= 2 (generator-outputs (nth 2 code) 2)))
    (c-display "Error in " errortext ":" code ": Expected 2 outputs for expression, found" (generator-outputs (nth 2 code) 2))
    (throw 'compilation-error)))

(define (g-Buffer code def-inputs def-outputs)
  (g-Buffer-check-legal code "Buffer" def-inputs def-outputs)
  `(let ((buffer (make-vector ,(nth 1 code) 0.0))
	 (generator ,(make-generator (nth 2 code) 2 2))
	 (size ,(nth 1 code))
	 (n 0))
     (lambda (input kont)
       (generator input 
		  (vector-ref buffer n)
		  (lambda (new-val return-val)
		    (vector-set! buffer n new-val)
		    (set! n (1+ n))
		    (if (= size n)
			(set! n 0))
		    (kont return-val))))))

#!
(pretty-print (stalin-macroexpand '(Buffer 1024
					   (Lambda (input old-val)
						   (+ input 1)
						   old-val))))
(map (lambda (f)
       (f '(Buffer (a b c d e f)
		   (+ 2 3)
		   100
		   9
		   )))
     (list g-Buffer-inputs g-Buffer-outputs))


(pretty-print (stalin-macroexpand '(Buffer bufsize
					   (Lambda (input old-val)
					     (begin
					       (set! filterstore (+ (* output damp2) (* filterstore damp1)))
					       (+ input (* filterstore feedback)))
					     old-val))))

(define (Comb damp1 damp2 feedback bufsize)
  (define filterstore 0.0)
  (Buffer bufsize
	  (Lambda (input old-val)
		  (begin
		    (set! filterstore (+ (* output damp2) (* filterstore damp1)))
		    (+ input (* filterstore feedback)))
		  old-val)))

(define (Allpass feedback bufsize)
  (Buffer bufsize
          (Lambda (input old-val)
             (+ input (* old-val feedback))
             (- old-val input))))

!#

(stalin-add-generator 'Buffer g-Buffer g-Buffer-inputs g-Buffer-outputs) ;; g-Buffer-check-legal)

(define-stalin-macro (Buffer . gens)
  (g-Buffer `(Buffer ,@gens) 1 1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Counter generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-Counter-inputs code def-inputs)
  0)

(define (g-Counter-outputs code def-outputs)
  1)

(define (g-Counter-check-legal code errortext)
  (when (not (integer? (nth 1 code)))
    (c-display "Error in " errortext ":" code ": Argument is not an integer")
    (throw 'compilation-error)))

(define (g-Counter code def-inputs def-outputs)
  (g-Counter-check-legal code "Counter")  
  `(let ((n 0)
	 (size ,(nth 1 code)))
     (lambda (kont)
       (let ((prev n))
	 (set! n (1+ n))
	 (if (= n size)
	     (set! n 0))
	 (kont prev)))))

(define-stalin-macro (Counter . gens)
  (g-Counter `(Counter ,@gens) 0 1))


#!
(pretty-print (stalin-macroexpand '(Counter 16)))
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Read-table generator (similar to rdtable in faust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!
rdtable(a,b)
a = table size      - an integer.
b = the table       - a GUILE function taking as argument the index, returning the table value at that index.
                      The table is calculated during compilation.

INPUT: the index i
OUTPUT: table[i]

rdtable(tablesize, 20);
!#


(define (g-Read-table-inputs code inputs)
  1)

(define (g-Read-table-outputs code outputs)
  1)

(define (g-Read-table-check-legal code def-inputs def-outputs errortext)
  (define gen (cl-nth 3 code))
  (when (not (= 3 (length code)))
    (c-display "Error in " errortext ":" code ": Wrong number of arguments. Expected 2, found" (1- (length code)) ".")
    (throw 'compilation-error))
  (when (not (integer? (nth 1 code)))
    (c-display "Error in " errortext ":" code ": First argument is not an integer.")
    (throw 'compilation-error)))

(define (g-Read-table code def-inputs def-outputs)
  (g-Read-table-check-legal code def-inputs def-outputs "Read-table")
  (fix-defines
   (define size (nth 1 code))
   (define table-func (nth 2 code))
   (define vector-name (rt-gensym "table-vector"))
   (push! `(define ,vector-name (let* ((size ,size)
				       (vector (make-vector ,size))
				       (table-func ,table-func));(local-eval table-func *rt-local-stalin-code-environment*)))
				  (let loop ((i 0))
				    (if (< i size)
					(begin
					  (vector-set! vector i (table-func i size))
					  (loop (+ i 1)))))
				  vector))
	  stalin-extra-init-code)
   (define vector (let* ((size size)
			 (vector (make-vector size))
			 (table-func (local-eval table-func *rt-local-stalin-code-environment*)))
		    (let loop ((i 0))
		      (when (< i size)
			(vector-set! vector i (table-func i size))
			(loop (1+ i))))
		    vector))
   `(lambda (index kont)
      (kont (vector-ref ,vector-name index)))))

(define-stalin-macro (Read-table . gens)
  (g-Read-table `(Read-table ,@gens) 0 1))

#!
(pretty-print (stalin-macroexpand '(Read-table 1024
					       (lambda (i size)
						 (sin (/ i size))))))
->
(let ((vector ,(let* ((size 16)
		      (vector (make-vector size))
		      (table-func (lambda (i size)
				    (sin (/ i size)))))
		 (let loop ((i 0))
		   (when (< i size)
		     (vector-set! vector i (table-func i))
		     (loop (1+ i))))
		 vector))
      (index-generator (Counter 16)))
  (lambda (kont)
    (index-generator (lambda (index)
		       (kont (vector-ref vector index))))))


(pretty-print (stalin-macroexpand '(Seq (Counter 16)
					(Read-table 16
						    (lambda (i size)
						      (sin (/ i size)))))))

!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Sin generator 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-stalin (Sin)
  (Read-table 1024
	      (lambda (index size)
		(sin (/ index size)))))

(define-stalin (Sine-waveform)
  (Read-table 1024
	      (lambda (index size)
		(sin (/ (* index 2.0 3.14159265358979)
			size)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Incrementer generator 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-stalin (Incrementer how-much)
  (define val 0.0)
  (Lambda ()
    (let ((ret val))
      (inc! val how-much)
      ret)))

#!
(let ((gen '(Incrementer 50)))
  (c-display (nth 2 (nth 2 (get-stalin-func (generator-name gen)))))
  (last (take-while (lambda (expr)
		      (not (memv expr stalin-reserved-keywords)))
		    (nth-cdr 2 (nth 2 (get-stalin-func (generator-name gen)))))))

!#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Osc generator (similar to rdtable in faust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-stalin (Osc freq)
  (Seq (Incrementer (/ freq (mus-srate)))
       (Lambda (val)
	       (- val (floor val)))
       (* 1024)
       (inexact->exact)
       (Sine-waveform)
       (* 0.5)))


#!
(<rt-stalin>
 (gen-sound
  (Osc 220)))
(<rt-stalin>
 (gen-sound
  (oscil :freq 440)))
(is-stalin-defined? 'rt_gen_table-vectorb35)
(get-stalin-func 'rt_gen_table-vectorb35)

(pretty-print (stalin-macroexpand '(Osc 440)))

->
(Seq (Counter 1024)
     (Read-table 1024
		 (let ((phase 0)
		       (phase-inc (hz->radians 440)))
		   (lambda (i size)
		     (let ((ret (sin phase)))
		       (set! phase (+ phase phase-inc))
		       ret)))))
(hz->radians 440)
(radians->hz (hz->radians 440))
(radians->hz (/ (c-integer (* (* 16 1024) 0.0626893748994917)) (* 16 1024)))
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; In generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (g-In-inputs code inputs)
  0)

(define* (g-In-outputs code outputs)
  outputs)

(define (g-In-check-legal code errortext)
  #t)

(define (g-In code inputs outputs)
  (define kont (rt-gensym "kont"))
  (g-In-check-legal code "In")
  ;;(c-display "inputs/outputs" inputs outputs)
  (if (= 1 outputs)
      `(lambda (,kont)
	 (,kont (in ,(cadr code))))
      `(lambda (,kont)
	 (in ,(cadr code) ,outputs ,kont))))

(stalin-add-generator 'In g-In g-In-inputs g-In-outputs) ;; g-Buffer-check-legal)

(define-stalin-macro (In . gens)
  (g-In `(In ,@gens) 0 1))

#!
(pretty-print (stalin-macroexpand '(In (softsynth))))
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; gen-sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-stalin-macro (gen-sound :rest rest :allow-other-keys)
  (fix-defines
   (define options (c-butlast rest))
   (define gen (last rest))
   (define inputs (generator-inputs gen 0))
   (c-display "gen:" gen)
   (define outputs (generator-outputs gen 1))
   (c-display "gen2:" gen)
   (if (not (= 0 inputs))
       (begin
	 (c-display "Error in gen-sound:" `(gen-sound ,gen) ":"
		    "\n\tGenerator used by 'gen-sound' can not take input signals. (found" inputs ")")
	 (throw 'compilation-error))
       
       (let ((ret-args (gen-ret-args outputs)))
	 `(let ((func ,(make-generator gen 0 1)))
	    (sound ,@options
	      (func (lambda ,ret-args
		      ,@(let loop ((n 0)
				   (args ret-args))
			  (if (null? args)
			      '()
			      (cons `(out ,n ,(car args))
				    (loop (1+ n)
					  (cdr args)))))))))))))
#!
(define-stalin (Reverb)
  (Seq (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (Sum (comb :scaler 0.742 :size 9601)
	    (comb :scaler 0.733 :size 10007)
	    (comb :scaler 0.715 :size 10799)
	    (comb :scaler 0.697 :size 11597))))

(pretty-print (stalin-macroexpand '(gen-sound (Seq (oscil :freq 200)
                                                   ;;(* 0.2)
                                                   (Reverb)))))
(modulo 2 2)

(<rt-stalin>
 (sound
   (out 0 (random 0.5))))

(<rt-stalin>
 (sound
   (let ((val (oscil :freq 400)))
     (out 0 val)
     (out 1 val))))

(<rt-stalin>
 (sound
   (let ((val (oscil :freq 400)))
     (out val))))

(<rt-stalin>
 (sound
   (out 0.5)))

(<rt-stalin>
 (sound
   (out (oscil :freq 400))))

(pretty-print (stalin-macroexpand '(out (oscil :freq 400))))
(pretty-print (stalin-macroexpand '(let ((val (oscil :freq 400)))
				     (out val))))

(<rt-stalin>
 (gen-sound (Seq (oscil :freq 200)
		 (Split Identity
			(Seq (* 0.01)
			     (Reverb))
			(* 0.4)))))

(pretty-print (stalin-macroexpand '(gen-sound (Par 2 3))))
(pretty-print (stalin-macroexpand '(gen-sound (Lambda ()
						      (random 1.0)))))
(pretty-print (stalin-macroexpand '(gen-sound
				      (Par (* 2 )
					   (+ 3)))))
->
(let ((func <generator>))
  (sound
    (func (lambda (...outs)
	    (out <n> outs<n>)
	    ...))))


(pretty-print (stalin-macroexpand '(Seq (all-pass :feedback -0.7 :feedforward 0.7 vb))))
 :include-autovars #f))

(pretty-print (stalin-macroexpand '(Seq (all-pass :feedback -0.7 :feedforward 0.7)
					  (all-pass :feedback -0.7 :feedforward 0.7)
					  (all-pass :feedback -0.7 :feedforward 0.7)
					  (all-pass :feedback -0.7 :feedforward 0.7)
					  (Sum (comb :scaler 0.742 :size 9601)
					       (comb :scaler 0.733 :size 10007)
					       (comb :scaler 0.715 :size 10799)
					       (comb :scaler 0.697 :size 11597))
					  (delay :size (* .013 (mus-srate)))))))

(define-macro (make-freeverb)
  (stalin-macroexpand '(Seq (all-pass :feedback -0.7 :feedforward 0.7)
			    (all-pass :feedback -0.7 :feedforward 0.7)
			    (all-pass :feedback -0.7 :feedforward 0.7)
			    (all-pass :feedback -0.7 :feedforward 0.7)
			    (Sum (comb :scaler 0.742 :size 9601)
				 (comb :scaler 0.733 :size 10007)
				 (comb :scaler 0.715 :size 10799)
				 (comb :scaler 0.697 :size 11597))
			    (delay :size (* .013 (mus-srate))))))

(define freeverb (make-freeverb))
(freeverb 0.5 c-display)



(define-stalin (Reverb)
  (Seq (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (Sum (comb :scaler 0.742 :size 9601)
	    (comb :scaler 0.733 :size 10007)
	    (comb :scaler 0.715 :size 10799)
	    (comb :scaler 0.697 :size 11597))
       (delay :size (* .013 (mus-srate)))))

(define-stalin (Stereo-pan c)
  (Split Identity
	 (* left)
	 (* right)
    :where left  (* sqrt2/2 (+ (cos angle) (sin angle)))
    :where right (* sqrt2/2 (- (cos angle) (sin angle)))
    :where angle (- pi/4 (* c pi/2))
    :where sqrt2/2 (/ (sqrt 2) 2)))
  
(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on       
      (define tone
	(gen-sound
	  (Seq (oscil :freq (midi->hz (midi-note)))
	       (* (midi-vol))
	       (Stereo-pan (/ (midi-note) 127)))))
      (spawn
	(wait-midi :command note-off :note (midi-note)
	  (stop tone))))))

(define-stalin (Fx-ctrl clean wet Fx)
  (Sum (* clean)
       (Seq Fx
	    (* wet))))

(<rt-stalin>
  (gen-sound
    (Seq (In (softsynth))
	 (Par (Fx-ctrl 0.5 0.09 (Reverb))
	      (Fx-ctrl 0.5 0.09 (Reverb))))))


!#

