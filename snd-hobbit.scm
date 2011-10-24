
;; Quick way to use the hobbit guile compilator inside Snd.
;;
;; Example: (hobbit-compile (display "I am compiled")(newline))
;; The hobbit-compile macro also works around some bugs in hobbit.
;; 
;; The compiled code may infact run a lot slower compaired to when being interpreted. (but usually not)
;; For simple mathematical computations, code seems to run between 1.5 and 80 times faster than when being interpreted.
;; The simplest kind of fibonacci function runs up to 80 times faster when compiled:
;;(define (fib n)
;;  (cond ((%= n 0) 0)
;;	  ((%= n 1) 1)
;;	  (else (%+ (fib (%- n 1))
;;		    (fib (%- n 2))))))
;;
;; Some code also runs a lot faster if the variable "hobbit-add-numargs-property" is set to #f.
;; However, "hobbit-add-numargs-property" must be set to #t for Snd to know the number of arguments
;; a function inside a compiled closure takes. This is necesarry when making callbacks to any of the built-
;; in functions in Snd.
;;
;; Hobbit is a Scheme to C compiler, originally written for SCM by Tanel Tammet (tammet@cs.chalmers.se)
;; The Guile port is being maintained by Bernard Urban (Bernard.Urban@meteo.fr)
;; Hobbit can be downloaded from http://fix.me
;;
;; First version of this file: 29.7.2004. -Kjetil S. Matheussen.


(use-modules (srfi srfi-1))

(provide 'snd-snd-hobbit.scm)


(define hobbit-is-here (= 0 (system "hob >/dev/null 2>/dev/null")))
(define hobbit-clean-up #f)
(define hobbit-verbose #t)
(define hobbit-dont-fix #f)
(define hobbit-add-numargs-property #t)


;; Load the defmacroexand and generic-write slib modules.
(if hobbit-is-here
    (let ((tempfile (tmpnam)))
      (system (string-append "echo '(define hobbit-path \"' >" tempfile 
			     ";hob --info >>" tempfile
			     ";echo '\")' >>" tempfile
			     ";echo '(set! \%load-path (cons (substring hobbit-path 1 (- (string-length hobbit-path) 1)) \%load-path))' >>" tempfile
			     ";echo '(use-modules (slib defmacroexpand) (slib generic-write))' >>" tempfile))
      (load tempfile)
      (delete-file tempfile)))




;; Hobbit-only functions, used when hobbit is not available. (Should be optimized a lot, both because of speed and compatibility)
(define %eqv? eqv?)
(define %zero? zero?)
(define %negative? negative?)
(define %positive? positive?)
(define %number? number?)
(define %= =)
(define %< <)
(define %> >)
(define %<= <=)
(define %>= >=)
(define %+ +)
(define %- -)
(define %* *)
(define %/ /)



;; Workaround for functions hobbit have problems with. Hobbit tries to optimize these, but produce illegal code.
(define hobbit-logior logior)
(define hobbit-logand logand)
(define hobbit-logxor logxor)
(define hobbit-string-ci<? string-ci<?)

(define-macro (logior . args)
  `(hobbit-logior ,@args))
(define-macro (logand . args)
  `(hobbit-logand ,@args))
(define-macro (logxor . args)
  `(hobbit-logxor ,@args))
(define-macro (string-ci<? . args)
  `(hobbit-string-ci<? ,@args))




;; Workaround (which is actually also a very ugly hack) for a serious problem with variable reference in Hobbit.
;; When a variable is previously referenced inside a lambda function, and the variable
;; is set! on the current level, the reference in the lambda function just use the original
;; value. However when replacing (set! a b) with (apply (lambda () (set! a b) ) '()),
;; things work. Unfortunately, that replacement makes some code go really slow.
;; so the function tries to be smart (which it also seems to manage) and only replace when necesarry.
;; 
;; The function assumes that all (define (a b c) ...) are replaced with (define a (lambda (b c) ...))
;; before calling, and that all non-toplevel defines are replaced with letrec-code.
;;
;; The function also renames all variables, so that they are all unique. The reason is that
;; code like the following won't compile with hobbit:
;;
;;(define func1
;;  (let ((u 5))
;;    (lambda (y)
;;      (lambda (x)
;;	#t))))
;;(let ((x #f))
;;  #t)
;;
;; The following will though:
;;
;;(define func1
;;  (let ((u 5))
;;    (lambda (y)
;;      (lambda (x)
;;	#t))))
;;(let ((x2 #f))
;;  #t)

(define (hobbit-fix-vars term varlist)
  (define newlambda (cons 'hf_newlambda__ #t))
  
  (define get-varname car)
  (define get-newname cadr)
  (define get-numaccess caddr)
  (define (set-numaccess! al addornull)
    (set-car! (cddr al) (if (number? addornull)
			    addornull
			    (addornull (get-numaccess al)))))

  (define (make-nameelement name)
    (list name (gensym (string-append "hf_" (symbol->string name) "_")) 0))

  (define (add-varlist! nameelement)
    (set! varlist (cons nameelement varlist)))


  (if (not varlist)
      (set! varlist (list newlambda)))



  (cond 


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; '()
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((null? term) term)


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; A variable
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((symbol? term)
    (call-with-values (lambda () (break (lambda (a) (eq? a newlambda))
					varlist))
      (lambda (local nonlocal)
	(let ((a (assq term local)))
	  (if a
	      (get-newname a)
	      (let ((a (assq term nonlocal)))
		(if a
		    (begin
		      (set-numaccess! a 1+)
		      (get-newname a))
		    term)))))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; char/string/number/vector/etc.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((not (pair? term)) term)
   

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; '(...)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((eq? 'quote (car term)) term)


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; (lambda ...)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((eq? 'lambda (car term))
    (add-varlist! newlambda)
    (letrec ((namelist '())
	     (varlistadd (lambda (r)
			   (cond ((pair? r)
				  (set! namelist (cons (make-nameelement (car r)) namelist))
				  (add-varlist! (car namelist))
				  (varlistadd (cdr r)))
				 ((symbol? r)
				  (set! namelist (cons (make-nameelement r) namelist))
				  (add-varlist! (car namelist)))))))
      (varlistadd (cadr term))
      (append (list 'lambda)
	      (list (cond ((null? (cadr term)) '())
			  ((not (pair? (cadr term))) (get-newname (car namelist)))
			  ((dotted-list? (cadr term))
			   (letrec ((undotted (map get-newname (reverse (cdr namelist))))
				    (set-last! (lambda (x)
						 (if (null? (cdr x))
						     (set-cdr! x (get-newname (car namelist)))
						     (set-last! (cdr x))))))
			     (set-last! undotted)
			     undotted))
			  (else
			   (map get-newname (reverse namelist)))))
	      (hobbit-fix-vars (cddr term) varlist))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; (let ...)/(let* ...)/(letrec ...)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((or (eq? 'let (car term))
	(eq? 'let* (car term))
	(eq? 'letrec (car term)))
    (let* ((type (car term))
	   (vardecls (cadr term))
	   (body (cddr term))
	   (namelist (map (lambda (t) (make-nameelement (car t)))
			  vardecls)))
      (if (eq? 'letrec type)
	  (for-each (lambda (t)
		      (add-varlist! t))
		    namelist))
      (let ((newvardecls (map-in-order (lambda (t nameelement)
					 (let ((ret (list (get-newname nameelement)
							  (hobbit-fix-vars (cadr t) varlist))))
					   (if (eq? 'let* type)
					       (add-varlist! nameelement))
					   ret))
				       vardecls
				       namelist)))
	(if (eq? 'let type)
	    (for-each (lambda (t)
			(add-varlist! t))
		      namelist))
	(append (list type)
		(list newvardecls)
		(hobbit-fix-vars body varlist)))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; (set! ...)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((eq? 'set! (car term))
    (call-with-values (lambda () (break (lambda (a) (eq? a newlambda))
					varlist))
      (lambda (local nonlocal)
	(let ((a (assq (cadr term) local)))
	  (if (and a
		   (> (get-numaccess a) 0))
	      (begin
		(set-numaccess! a 0)
		(list 'apply
		      (list 'lambda '() 
			    (list 'set!
				  (get-newname a)
				  (hobbit-fix-vars (caddr term) varlist)))
		      '()))
	      (map-in-order (lambda (t) (hobbit-fix-vars t varlist))
			    term))))))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; (...)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (else
    (map-in-order (lambda (t) (hobbit-fix-vars t varlist))
		  term))))




;; (define (a b) ..) must be replaced with (define a (lambda b) ...) before calling.
(define (hobbit-fix-various term)
  (cond 
   ((not (pair? term)) term)
   ((null? term) term)
   ((eq? 'quote (car term)) term)
   
   ;; Hobbit doesn't understand comment strings for functions. (lambda (b) "returns argument" b) -> (lambda (b) b)
   ((and (eq? 'lambda (car term))
	 ;;(not (null? (cddr term)))
	 (string? (caddr term))
	 (not (null? (cdddr term))))
    (hobbit-fix-various (append (list (car term)
				      (cadr term))
				(cdddr term))))
   
   ;; Snd often need to know the number of arguments of a function in some way. This code does that,
   ;; and shouldn't slow down much. (I hope). Code should not behave wrongly if hobbit-add-numargs-property
   ;; is to #f, snd should instead give a clear error-message if it can't find the required number of arguments.
   ((and hobbit-add-numargs-property
	 (eq? 'lambda (car term))
	 (list? (cadr term)))
    (list 'let
	  (list (list 'youbetternotusethisvariablename (append (list 'lambda)
							       (list (cadr term))
							       (hobbit-fix-various (cddr term)))))
	  (list 'set-procedure-property! 'youbetternotusethisvariablename ''hobbit-numargs (if (list? (cadr term))
											       (length (cadr term))
											       -1))
	  'youbetternotusethisvariablename))
	  
   ((eq? 'lambda (car term))
    (append (list 'lambda (cadr term))
	    (map hobbit-fix-various (cddr term))))

   (else
    (map hobbit-fix-various term))))
  



;; Replace local defines with letrecs.
;; All (define (a b) ...) must be replaced with (define a (lambda (b) ...)) before calling.
(define (hobbit-fix-replace-define-with-letrec term)
  (cond 
   ((not (pair? term)) term)
   ((null? term) term)
   ((eq? 'quote (car term)) term)
   ((eq? 'lambda (car term))
    (append (list 'lambda (cadr term))
	    (map hobbit-fix-replace-define-with-letrec (cddr term))))
   (else
    (map hobbit-fix-replace-define-with-letrec
	 (call-with-values (lambda () (break (lambda (t) (and (pair? t)
							      (eq? 'define (car t))))
					     term))
	   (lambda (beforedefines definestart)
	     (if (null? definestart)
		 term
		 (call-with-values (lambda () (span (lambda (t) (and (pair? t)
								     (eq? 'define (car t))))
						    definestart))
		   (lambda (defines afterdefines)
		     (append beforedefines
			     (list (append (list 'letrec
						 (map-in-order (lambda (t) (list (cadr t) (caddr t)))
							       defines))
					   afterdefines))))))))))))

  

   
(define (hobbit-fix-define-and-setter term)
  (cond 
   ((not (pair? term)) term)
   ((null? term) term)
   ((eq? 'quote (car term)) term)

   ;;(define (a b c) d e) -> (define a (lambda b c) d e)
   ((and (eq? 'define (car term))
	 (pair? (cadr term)))
    (list 'define
	  (caadr term)
	  (append (list 'lambda)
		  (list (cdadr term))
		  (map hobbit-fix-define-and-setter (cddr term)))))
   
   ;; Hobbit doesn't understand setters. This is a workaround.
   ;; (set! (a 2) 3) -> ((setter a) 2 3)
   ((and (eq? 'set! (car term))
	 (list? (cadr term)))
    (hobbit-fix-define-and-setter (append (cons (list 'setter (caadr term))
						(cdadr term))
					  (list (caddr term)))))
   
   ((eq? 'lambda (car term))
    (append (list 'lambda (cadr term))
	    (map hobbit-fix-define-and-setter (cddr term))))
   (else
    (map hobbit-fix-define-and-setter term))))





(define (hobbit-fix term)
  (if hobbit-dont-fix
      term
      (hobbit-fix-vars
       (hobbit-fix-various
	(hobbit-fix-replace-define-with-letrec
	 (hobbit-fix-define-and-setter
	  term)))
       #f)))




(define-macro (hobbit-compile . stuff)
  (let* ((dirname (tmpnam))
	 (sourcefile (begin (system (string-append "mkdir " dirname))
			    (string-append dirname "/code.scm")))
	 (fd (open-file sourcefile "w"))
	 (dontcompile (eq? '#f (car stuff))))
    (for-each (lambda (block)
		(generic-write (hobbit-fix
				(defmacro:expand*
				  block))
			       #f
			       60
			       (lambda (x) (display x fd))))
	      (if (or (eq? '#t (car stuff)) dontcompile)
		  (cdr stuff)
		  stuff))
    (close fd)
    (if (or dontcompile (not hobbit-is-here))
	(load sourcefile)
	(begin
	  (if hobbit-verbose
	      (begin
		(display "compiling ")
		(display sourcefile)
		(display "...")
		(newline)))
	  (system (string-append "cd " dirname ";hob --only-c code.scm;gcc -O3 -shared -o code.so code.c -I`hob --info`"))
	  (if hobbit-verbose
	      (begin
		(display "Done.")
		(newline)))
	  (dynamic-call "scm_init_code" (dynamic-link (string-append dirname "/code.so")))
	  ))
    (if hobbit-clean-up
	(system (string-append "rm -fr " dirname)))))

(define-macro (hobbit-compile2 . stuff)
  (let ((dirname "/tmp/fileV1mNab"))
    (dynamic-call "scm_init_code" (dynamic-link (string-append dirname "/code.so")))))

(define-macro (define-compiled def . body)
  `(hobbit-compile
    (define ,def
      ,@body)))
