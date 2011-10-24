#!

eval-c.scm
-Kjetil S. Matheussen/Notam, 2005

eval-c.scm is developed with support from Notam/Oslo:
http://www.notam02.no


This is code to evaluate prefix-notated C-code on the fly with guile.
See gui.scm, ladspa.scm, rt-compile.scm, rt-engine.scm and snd_conffile.scm
for examples of use.





EVAL-C
------

eval-c takes as arguments lisp- and C-like blocks, generates c-code from it, and runs it.

Some reasons to use eval-c:

* Easy integration of c-code from within lisp.
  Mix C and Lisp in the same source without making it look strange. (hopefully)
* Use lisp-macros to generate c-code. (There is a special macro function
  called "define-c-macro" that works with eval-c). eval-c macros are more powerful
  than lisp macros since both strings and s-expressions can be manipulated to generate
  new code. See various examples.
* Generate/compile/link/run c-code on the fly.
* Some people think prefix notation is nice.
* Speed. C is faster than guile.
* Hides guile-semantic to access C-code from guile. Less need to read the guile manual.
* Global functions does not need to be defined at the top-level. (that is a good thing,
  right?)
* Special support for many strange things, for examples shared structures and classes.


Examples.
--------

The simplest fibonacci function:

(define-c (<int> fib (<int> n))
  (if (< n 2)
      (return n)
      (return (+ (fib (- n 1))
		 (fib (- n 2)))))))

(define-c fib <int> ((<int> n))
  (if (< n 2)
      (return n)
      (return (+ (fib (- n 1))
		 (fib (- n 2)))))))


The define-c macro will produce the following code:

(eval-c ""
	(public
	 (<int> fib (lambda ((<int> n))
		      (if (< n 2)
			  (return n)
			  (return (+ (fib (- n 1))
				     (fib (- n 2)))))))))


The "public" macro will change the code so that it looks something like this:

(eval-c ""
	(<int> fib (lambda ((<int> n))
		     (if (< n 2)
			 (return n)
			 (return (+ (fib (- n 1))
				    (fib (- n 2)))))))
	(<SCM> fib_eval_c_helper (lambda ((<SCM> n))
					  (return (MAKE_INTEGER (fib (GET_INTEGER n))))))
	(run-now
	 (scm_c_define_gsubr (string "fib") 1 0 0 fib_eval_c_helper)))


And after running the "lambda", "if", "<", "+", "-", "run-now" and "string" macros (most of eval-c is defined
as macros), eval-c will produce and run the following c-code:

static int fib (int n){
  if ((n < 2))
    return (n);
  else
    return ((fib ((n - 1)) + fib ((n - 2))));
}
static SCM fib_eval_c_helper (SCM n){
  return (MAKE_INTEGER (fib (GET_INTEGER (n))));
}
static void run_now_1 (){
  scm_c_define_gsubr ("fib", 1, 0, 0, fib_eval_c_helper);
}


The first function is the fibonacci generator, and the
second function is the guile-wrapper. (GET_INTEGER and
MAKE_INTEGER are just simple C macros.)
"run_now"-functions are run once when the file is loaded.

From guile you have now a function called "fib" which takes
one argument.



Hello world looks like this:

(eval-c ""
	(run-now
	 (printf (string "Hello world!\\n"))))




First argument to eval c is a string with compiling/linking options.
Usually just "", but can be "-lsnd" or something if needed.


!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 optargs)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (ice-9 rdelim)
	     (ice-9 pretty-print))


(provide 'snd-eval-c.scm)


;; A bit more stack required
(debug-set! stack 2000000)


(if (not (provided? 'snd-oo.scm))
    (load-from-path "oo.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Public variables ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *eval-c-do-cache* #t)
(define eval-c-verbose #t)
(define eval-c-very-verbose #f)
(define eval-c-cleanup #f)
(define eval-c-lazy-cleanup #t)
(define eval-c-string-is-pointer #f)

(if (not (defined? '*eval-c-compiler*))
    (primitive-eval '(define *eval-c-compiler* "gcc")))
(if (not (defined? '*eval-c-CFLAGS*))
    (primitive-eval '(define *eval-c-CFLAGS* "")))


(if (not (defined? 'snd-header-files-path))
    (let ((path #f))
      (for-each (lambda (l-path)
		  (if (not path)
		      (if (access? (string-append l-path "/clm.h") R_OK)
			  (set! path l-path))))
		%load-path)
      (if path
	  (define-toplevel 'snd-header-files-path path)
	  (begin
	    (c-display "Error! Header files for SND not found. Try setting snd-header-files-path.")
	    (catch 'header-files-path-not-found)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Various functions ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (c-tosymbol something)
  (cond ((string? something) (string->symbol something))
	((symbol? something) something)
	(else
	 (c-display "Error in eval-c.scm/c-tosymbol. Not able to handle " something ".")
	 (thisisnotafunctionhopefully))))

(define (c-tostring something)
  (cond ((string? something) something)
	((number? something) (number->string something))
	((symbol? something) (symbol->string something))
	((char? something) (string something))
	(else
	 (c-display "Error in eval-c.scm/c-tostring. Not able to handle " something ".")
	 (thisisnotafunctionhopefully))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *eval-macro-prefix* #f)

(define eval-c-macro-prefix 'eval-c-macro-)
(define eval-faust-macro-prefix 'eval-faust-macro-)

(define eval-c-run-nows '())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Type Handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define eval-c-void-types (list "void"))
(define eval-c-string-types (list "char *" "char*" "gchar*" "char *"))
(define eval-c-int-types (list "int" "long" "short" "char" "gint" "size_t"))
(define eval-c-int64-types (list "long long" "int64" "int64_t" "scm_t_int64"))
(define eval-c-float-types (list "float"))
(define eval-c-double-types (list "double"))


;; etype: '<static-int>
;; ctype: "static int"

(define (eval-c-isetype? type)
  (symbol? type))

(define (eval-c-isctype? type)
  (string? type))

(define (eval-c-ctype->etype das-type)
  (string->symbol (<-> "<" 
		       (list->string (map (lambda (c)
					    (if (eq? #\space c)
						#\-
						c))
					  (string->list das-type)))
		       ">")))


(define (eval-c-etype->ctype das-type)
  (let ((type (string->list (symbol->string das-type)))
	(ret '()))
    (if (eq? #\< (car type))
	(set! type (cdr type)))
    (if (eq? #\> (last type))
	(set! type (c-butlast type)))
    (list->string (map (lambda (c)
			 (if (eq? #\- c)
			     #\space
			     c))
		       type))))

#!
(eval-c-etype->ctype '<int>)
!#

(define (eval-c-etype type)
  (if (eval-c-isetype? type)
      type
      (eval-c-ctype->etype type)))

(define (eval-c-ctype type)
  (if (eval-c-isctype? type)
      type
      (eval-c-etype->ctype type)))

(define (eval-c-returnsametype orgtype das-ret)
  (if (and (eval-c-isetype? orgtype)
	   (eval-c-isctype? das-ret))
      (eval-c-ctype->etype das-ret)
      (if (and (eval-c-isctype? orgtype)
	       (eval-c-isetype? das-ret))
	  (eval-c-etype->ctype das-ret)
	  das-ret)))


(define (eval-c-strip-qualifiers das-type)
  (let* ((type-split (remove (lambda (s) (or (string=? "" s))) (string-split (eval-c-ctype das-type) #\space)))
	 (type (string-trim-right (apply <-> (map (lambda (t) (<-> t " ")) type-split))))
	 (type-first (car type-split)))
    (while (or (string=? type-first "unsigned")
	       (string=? type-first "signed")
	       (string=? type-first "static")
	       (string=? type-first "volatile")
	       (string=? type-first "const"))
	   (set! type-split (cdr type-split))
	   (set! type (string-trim-right (apply <-> (map (lambda (t) (<-> t " ")) type-split))))
	   (set! type-first (car type-split)))
    (eval-c-returnsametype das-type type)))

#!
(eval-c-strip-qualifiers "unsigned static   int")
(eval-c-strip-qualifiers '<unsigned-static-int>)
!#

(define (eval-c-get-propertype das-type)
  (if (pair? das-type)
      " void* " ;; A function
      (<-> (eval-c-etype->ctype das-type) " ")))

#!
(eval-c-get-propertype '<int-wefwe>)
!#

(define (eval-c-to-scm das-type)
  (let ((type (eval-c-strip-qualifiers das-type)))
    (cond ((member type eval-c-void-types) "UNSPECIFIED")
	  ((and (not eval-c-string-is-pointer) (member type eval-c-string-types) "STRING"))
	  ((member type eval-c-int-types) "INTEGER")
	  ((member type eval-c-int64-types) "INT64")
	  ((member type eval-c-float-types) "FLOAT")
	  ((member type eval-c-double-types) "DOUBLE")
	  ((string=? type "SCM") "SCM")
	  ((string=? type "jmp_buf") "JMP_BUF")
	  (else
	   "POINTER"))))

(define (eval-c-add-void-type type)
  (set! eval-c-void-types (cons type eval-c-void-types)))
(define (eval-c-add-string-type type)
  (set! eval-c-string-types (cons type eval-c-string-types)))
(define (eval-c-add-int-type type)
  (set! eval-c-int-types (cons type eval-c-int-types)))
(define (eval-c-add-int64-type type)
  (set! eval-c-int64-types (cons type eval-c-int64-types)))
(define (eval-c-add-float-type type)
  (set! eval-c-float-types (cons type eval-c-float-types)))
(define (eval-c-add-double-type type)
  (set! eval-c-double-types (cons type eval-c-double-types)))


(define (eval-c-get-known-type type)
  (cadr (member (eval-c-to-scm (eval-c-etype->ctype type))
		`("UNSPECIFIED" <void>
		  "STRING" <char-*>
		  "INTEGER" <int>
		  "INT64"   <int64>
		  "FLOAT" <float>
		  "DOUBLE" <double>
		  "SCM" <SCM>
		  "JMP_BUF" <jmp_buf>
		  "POINTER" <void-*>))))

#!
(eval-c-to-scm "        static  const unsigned char   *")
(eval-c-to-scm '<static-const-char-*>)
(eval-c-to-scm "int")
(eval-c-get-known-type '<static-const-char-*>)
!#

(define (eval-c-get-*type etype)
  (let ((ctype (string-trim-both (eval-c-etype->ctype etype))))
    (if (char=? #\* (last (string->list ctype)))
	(eval-c-ctype->etype (string-trim-both (list->string (c-butlast (string->list ctype)))))
	etype)))

#!
(eval-c-get-*type '<--int-*->)
(eval-c-etype->ctype '<char-*>)
!#

(define (eval-c-cify-var das-var)
  (if (and (not (string? das-var))
	   (not (symbol? das-var)))
      das-var
      (if (or (and (string? das-var)
		   (string=? "-" (string-trim-both das-var)))
	      (and (symbol? das-var)
		   (string=? "-" (string-trim-both (symbol->string das-var)))))
	  (if (symbol? das-var)
	      '-
	      "-")
	  (let* ((var (if (string? das-var)
			  das-var
			  (symbol->string das-var)))
		 (varlist (string->list var))
		 (firsthit (member #\- varlist)))
	    (if (and firsthit
		     (or (null? (cdr firsthit))
			 (not (equal? #\> (cadr firsthit)))))
		(let ((ret (apply <-> (map (lambda (c) (cond ((equal? #\- c)
							      "_minus_")
							     ((equal? #\> c)
							      "_greaterthan_")
							     ((equal? #\< c)
							      "_lessthan_")
							     ((equal? #\? c)
							      "_questionmark_")
							     ((equal? #\! c)
							      "_exclamationmark_")
							     ((equal? #\+ c)
							      "_plus_")
							     ((equal? #\/ c)
							      "_divide_")
							     ((equal? #\* c)
							      "_multiply_")
							     (else
							      (string c))))
					   varlist))))
		  (if (symbol? das-var)
		      (string->symbol ret)
		      ret))
		das-var)))))
      
#!
(eval-c-cify-var '(aiai))
(eval-c-cify-var 'c-dlsym)
!#

(define (eval-c-symbol-is-type s)
  (and (not (or (eq? s '<->')
		(eq? s '<)
		(eq? s '>)))
       (let ((aslist (string->list (symbol->string s))))
	 (and (eq? #\<
		   (car aslist))
	      (eq? #\>
		   (last aslist))))))


(define *eval-c-public-funcdecls* (make-hash-table 219))

(define (eval-c-put-public-funcdecl name decl)
  (hashq-set! *eval-c-public-funcdecls* name decl))

(define (eval-c-get-public-funcdecl name)
  (hashq-ref *eval-c-public-funcdecls* name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-structlist '())

(define (eval-c-add-struct name varlist)
  (set! eval-c-structlist (cons (cons (c-tosymbol name) varlist)
				eval-c-structlist)))

(define (eval-c-get-struct name)
  (let ((struct (assq (c-tosymbol name) eval-c-structlist)))
    (if struct
	(cdr struct)
	#f)))

(define (eval-c-get-structlisttype struct name)
  (let ((elem (assq (c-tosymbol name) struct)))
    (if elem
	(cadr elem)
	(c-display "Eval-c.scm/eval-c-get-structlisttype. Error. " name " not found in structure " struct "."))))

(define (eval-c-get-structlistelem struct name)
  (let ((elem (assq (c-tosymbol name) struct)))
    (if elem
	(cdr elem)
	(c-display "Eval-c.scm/eval-c-get-structlistelem. Error. " name " not found in structure " struct "."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-classlist '())

(define eval-c-privatevars  cadr)
(define eval-c-publicvars   caddr)
(define eval-c-privatefuncs cadddr)
(define eval-c-publicfuncs  (lambda (x) (cadddr (cdr x))))
(define eval-c-destructors  (lambda (x) (cadddr (cddr x))))
(define eval-c-constructors (lambda (x) (cadddr (cdddr x))))
(define eval-c-privatevars  (lambda (x) (cadddr (cdddr (cdr x)))))

(define (eval-c-addclass name
			 privatevars
			 publicvars
			 privatefuncs
			 publicfuncs
			 destructors
			 constructors)
  (set! eval-c-classlist (cons (list name privatevars privatefuncs publicfuncs destructors constructors)
			       eval-c-classlist)))


(define (eval-c-get-cmethodname methodname classname publicorprivate)
  (c-tosymbol (<-> (c-tostring classname)
		   "_split_"
		   (c-tostring methodname)
		   "_split_"
		   publicorprivate)))

#!
(eval-c-get-cmethodname 'aiai 'Gakk "private")
!#

#!
;; To complicated. Perhaps later.
(define (eval-c-get-cmethodname methodname classtype rettype args)
  (define (gettype das-type)
    (let ((type (string->list (symbol->string das-type)))
	  (ret '()))
      (if (eq? #\< (car type))
	  (set! type (cdr type)))
      (if (eq? #\> (last type))
	  (set! type (c-butlast type)))
      (list->string type)))

  (<-> (gettype classtype)
       "_split_"
       (gettype rettype)
       "_split_"
       methodname
       (apply <-> (map (lambda (x) (<-> "_split_" (gettype (car x))))
		       args))))
!#

#!
(eval-c-get-cmethodname "c-scale" '<Gakk_class> '<int> '((<struct-int> a)(<int> b)))
->
"Gakk_class" "_split_" "int" "_split_" "c-scale" "_split_" "struct-int" "_split_" "int"
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Getting the type of a variable ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-typelistlist '())
(define eval-c-typelist '())

(define (eval-c-puttype name type)
  (set! eval-c-typelist (cons (cons (c-tosymbol name) type) eval-c-typelist)))


(define (eval-c-get-structlist name)
  (let ((ret '())
	(temp '())
	(alist (string->list (c-tostring name))))
    (define (checkit)
      (define (get-tempstring)
	(set! ret (cons (string->symbol (list->string (reverse! temp))) ret))
	(set! temp '()))
      (if (eq? #\. (car alist))
	  (get-tempstring)
	  (if (eq? #\- (car alist))
	      (begin
		(set! alist (cdr alist))
		(get-tempstring))
	      (set! temp (cons (car alist) temp))))
      (set! alist (cdr alist))
      (if (not (null? alist))
	  (checkit)
	  (get-tempstring)))
    (checkit)
    (reverse! ret)))



#!
(eval-c-get-structlist 'gakk1.gakk2->gakk3.gakk5)
!#

(define (eval-c-getfulltype name)
  (let ((varnamelist (eval-c-get-structlist name)))

    (define (rec struct varnamelist)
      (let ((firstelem (eval-c-get-structlisttype struct (car varnamelist))))
	(if (= (length varnamelist) 1)
	    firstelem
	    (rec (eval-c-get-struct firstelem) (cdr varnamelist)))))
    
    (let ((start (assq (c-tosymbol (car varnamelist)) eval-c-typelist)))
      (if start
	  (if (> (length varnamelist) 1)
	      (rec (eval-c-get-struct (eval-c-getfulltype (car varnamelist))) (cdr varnamelist))
	      (cdr start))
	  '<undefined_t>))))

(define (eval-c-gettype name)
  (eval-c-strip-qualifiers (eval-c-getfulltype name)))


#!
(eval-c-puttype 'a '<const-static-int>)
(eval-c-gettype 'a)
!#



#!
(eval-c-add-struct '<struct-struct1> '((a <int>)
				      (b <int>)))
(eval-c-add-struct '<struct-struct2> '((c <struct-struct1> *)
				      (d <int>)))
(define a (eval-c-get-struct '<struct-struct2>))
(eval-c-get-structlistelem a 'c)
(eval-c-get-structlisttype a 'c)
(eval-c-puttype 'a '<struct-struct2>)
(eval-c-gettype 'a)
(eval-c-gettype 'a.c)
(eval-c-gettype 'a->c->a)
;; (eval-c-gettype could/should be more intelligent regarding pointers and the use of "." or "->")
!#


(define eval-c-level 0)

(define (eval-c-uplevel)
  (set! eval-c-level (1+ eval-c-level))
  (set! eval-c-typelistlist (cons eval-c-typelist eval-c-typelistlist)))

(define (eval-c-downlevel)
  (set! eval-c-level (1- eval-c-level))
  (set! eval-c-typelist (car eval-c-typelistlist))
  (set! eval-c-typelistlist (cdr eval-c-typelistlist)))


#!
(begin eval-c-typelist)
(eval-c-puttype 'name3 'atype3)
(eval-c-gettype 'name2)
(eval-c-uplevel)
(eval-c-downlevel)
(assq 'name eval-c-typelist)
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (eval-c-eval-funccall term)
  (define parser eval-parse)
  (if (null? (cdr term))
      (<-> (eval-c-cify-var (if (symbol? (car term))
				(symbol->string (car term))
				(car term)))
	   " ()")
      (apply <-> (append (list (<-> (eval-c-cify-var (if (symbol? (car term))
							 (symbol->string (car term))
							 (car term)))
				    " ("))
			 (map (lambda (x) (<-> (parser x) ", ")) (c-butlast (cdr term)))
			 (list (parser (last term)))
			 (list ")")))))


(define (eval-c-macro-result term)
  (let* ((a (cons (symbol-append *eval-macro-prefix* (car term)) (cdr term)))
	 (b (macroexpand-1 a )))
    (if (not (equal? a b))
	(eval-parse b)
	(eval-c-eval-funccall term))))


(define* (eval-parse term)
  (define parser eval-parse)
  (define macroexpander eval-c-macro-result)

  (if eval-c-very-verbose
      (c-display "top: " term))
  (cond 
   ((string? term) (<-> term " "))
   ((number? term) (<-> (number->string term) " "))
   ((symbol? term) (<-> (symbol->string term) " "))
   ((char? term) (<-> (string term) " "))
   
   ((list? term)
    (if (string? (car term))
	(eval-c-eval-funccall term)
	(let ((type (car term)))
	  
	  (if (list? type)

	      ;; ((<int> (<int> <int>)) funcname [funcpointer/lambda/NULL])
	      (let ((typename (eval-c-get-unique-name)))
		(<-> "typedef " (eval-c-etype->ctype (car type)) "(*" typename ")("
		     (if (not (null? (cadr type)))
			 (<-> (eval-c-etype->ctype (car (cadr type)))
			      (apply <-> (map (lambda (t)
						(<-> "," (eval-c-etype->ctype t)))
					      (cdr (cadr type)))))
			 "")
		     ");"
		     (parser `( ,(eval-c-ctype->etype typename) ,(cadr term) ,@(cddr term)))))
	      
	      ;; (<type> ...) / (func ...)
	      (if (not (eval-c-symbol-is-type type))
		  
		  (macroexpander term)
		  
		  ;; (<type> ....)
		  (let* ((is-toplevel (= 0 eval-c-level))
			 (ctype (and is-toplevel (eval-c-etype->ctype type)))
			 (is-static (and is-toplevel
					 (let* ((minlength (min (string-length "nonstatic ") (string-length ctype))))
					   (not (string= "nonstatic " ctype 0 minlength 0 minlength)))))
			 (type (if is-toplevel
				   (eval-c-ctype->etype (if is-static
							    (<-> "static " ctype)
							    (string-drop ctype (string-length "nonstatic "))))
				   type))
			 (varname (cadr term))
			 (isvarname? (or (string? varname) (symbol? varname))))
		    
		    ;;(c-display varname ":" term (eval-c-etype->ctype (car term)) type)

		    (if (not isvarname?)
			
			;; (<type> () .... )
			(parser `(,type ,@(map parser (cdr term))))
			
			;; (<type> varname ...)
			(begin
			  (eval-c-puttype (if (string? varname)
					      (car (string-split varname #\ ))
					      varname)
					  type)
			  
			  (if (= (length term) 2)
			      ;; (<int> a)
			      (<-> (eval-c-get-propertype type)
				   (parser varname))
			      
			      (if (and (= (length term) 3)
				       (or (not (list? (caddr term)))
					   (not (or (eq? 'lambda (caaddr term))
						    (eq? 'lambda-decl (caaddr term))))))
				  
				  ;; (<int> a 5)
				  (<-> (eval-c-get-propertype type)
				       (parser varname)
				       " = "
				       (parser (caddr term)))
				  
				  ;; (<int> a (lambda ...))
				  (let* ((funcdecl (nth 2 term))
					 (lambdaname (car funcdecl))
					 (typedefs "")
					 (funcvars (map (lambda (v)
                                                          (let ((type (car v))
								(name (cadr v)))
                                                            (if (list? type)
                                                                (let ((typename (eval-c-get-unique-name)))
								  (set! typedefs (<-> typedefs
										      "typedef " (eval-c-etype->ctype (car type)) " (*" typename ")("
										      (if (not (null? (cadr type)))
											  (<-> (eval-c-etype->ctype (car (cadr type)))
											       (apply <-> (map (lambda (t)
														 (<-> "," (eval-c-etype->ctype t)))
													       (cdr (cadr type)))))
											  "")
										      ");\n"))
								  (list (eval-c-ctype->etype typename) name))
                                                                v)))
                                                        (if (null? (cadr funcdecl))
                                                            '((<void> NOTHING))
                                                            ;;(cadr funcdecl)
                                                            (cadr funcdecl))))
					 (funcbody (cddr funcdecl)))

				    (if (not is-static)
					(eval-c-put-public-funcdecl varname `(,(car term) ,varname (lambda-decl ,(cadr funcdecl)))))

				    (<-> typedefs
					 (if (> eval-c-level 0)
					     "static "
					     "")
					 (apply <-> (map parser (cons (eval-c-get-propertype type)
								      (cons (eval-c-cify-var varname)
									    `((,lambdaname ,funcvars
											   ,@funcbody))))))))))))))))))))

(define* (eval-c-parse term)
  (set! *eval-macro-prefix* eval-c-macro-prefix)
  (eval-parse term))


#!
(eval-c-parse '(<int> ai (lambda () (return 2))))
(eval-c-parse '(<void> gakk (lambda (((<void> (<int>)) func)) (return 2))))
(eval-c-parse '(<void> gakk (lambda ((<int> func)) (return 2))))
!#

(define (eval-c-parse-line term)
  (if (and (string? term)
	   (not (string=? "" term))
	   (eq? #\# (string-ref term 0)))
      (<-> term (string #\newline))
      (<-> (eval-c-parse term) ";" (string #\newline))))

(define (eval-c-parse-lines terms)
  (apply <-> (map (lambda (x) (<-> (eval-c-parse-line x) " ")) terms)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; C-macros;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (define-c-macro def . body)
  `(define-macro ,(cons (symbol-append eval-c-macro-prefix (car def)) (cdr def)) ,@body))
(define-macro (define-faust-macro def . body)
  `(define-macro ,(cons (symbol-append eval-faust-macro-prefix (car def)) (cdr def)) ,@body))
(define-macro (define-c/faust-macro def . body)
  `(begin
     (define-macro ,(cons (symbol-append eval-faust-macro-prefix (car def)) (cdr def)) ,@body)
     (define-macro ,(cons (symbol-append eval-c-macro-prefix (car def)) (cdr def)) ,@body)))



(define-c-macro (set!-string-is-pointer-#t)
  (set! eval-c-string-is-pointer #t)
  "")

(define-c-macro (set!-string-is-pointer-#f)
  (set! eval-c-string-is-pointer #f)
  "")

(define-c-macro (if a b . c)
  (if (null? c)
      (<-> "if(" (eval-c-parse a) ")\n"
	   (eval-c-parse `(begin ,b)))
      (<-> "if(" (eval-c-parse a) ")\n"
	   (eval-c-parse `(begin ,b)) "\n"
	   "else\n"
	   (eval-c-parse (car c)))))

(define-c-macro (when a . rest)
  `(if ,a
       (begin
         ,@rest)))

(define-c-macro (?kolon a b . c)
  (<-> "("
       (eval-c-parse a)
       "\n?"
       (eval-c-parse b)
       "\n:"
       (if (not (null? c))
	   (eval-c-parse (car c))
	   "0")
       ")"))

(define-c-macro (struct-set . rest)
  (<-> "{"
       (eval-c-parse (car rest))
       (apply <-> (map (lambda (t)
			 (<-> "," (eval-c-parse t)))
		       (cdr rest)))
       "}"))
  
(define-c-macro (begin_p . rest)
  (<-> "("
       (eval-c-parse (car rest))
       (apply <-> (map (lambda (t)
			 (<-> "," (eval-c-parse t) "\n" ))
		       (cdr rest)))
       ")"))
			 

(define-c-macro (unquote something)
  (primitive-eval something))

(define-c-macro (unquote-splicing something)
  (apply <-> (map eval-parse (primitive-eval something))))

(define-c/faust-macro (include filename)
  (define ret "")
  (for-each-line-in-file (string-trim-right (eval-parse filename))
			 (lambda (line)
			   (set! ret (<-> ret line "\n"))))
  ret)

(define-c/faust-macro (x11-selection)
  (define filename (tmpnam))
  (if (not (= 0 (system (<-> "xsel >" filename))))
      (c-display "Error. x11-selection requires the \"xsel\" program: http://www.vergenet.net/~conrad/software/xsel/")
      `(include ,filename)))

(define-c/faust-macro (url das-url)
  (define filename (tmpnam))
  ;;(c-display "command:" (<-> "wget " (string-trim-right (eval-parse das-url)) " -O " filename))
  (if (not (= 0 (system (<-> "wget " (string-trim-right (eval-parse das-url)) " -O " filename))))
      (c-display "Error. url requires wget.")
      `(include ,filename)))

(define-c-macro (arraydef name length)
  (<-> (eval-c-parse name) "[" (eval-c-parse length) "]"))
		 
(define-c-macro (MIN a b)
  `(EC_MIN ,a ,b))

(define-c-macro (MAX a b)
  `(EC_MAX ,a ,b))


;; eval-c infix fixer
(for-each (lambda (op)
	    (let ((opstring (symbol->string op)))
	      (primitive-eval 
	       `(define-c-macro (,op . rest)
		  (if (= 1 (length rest))
		      (<-> "( " ,opstring (eval-c-parse (car rest)) ")")
		      (apply <-> (append (list "(")
					 (list (eval-c-parse (car rest)))
					 (map (lambda (x) (<-> ,opstring " " (eval-c-parse x) " ")) (cdr rest))
					 (list ") "))))))))
	  '(+ - * / | % & ~ == != < > <= >= || && += -= /= *= |= &= >> << ^))

; faust infix fixer
(for-each (lambda (op)
	    (define opstring #f)
	    (cond ((pair? op)
		   (set! opstring (cadr op))
		   (set! op (car op)))
		  (else
		   (set! opstring (symbol->string op))))
	    (primitive-eval 
	     `(define-faust-macro (,op . rest)
		(if (= 1 (length rest))
		    (<-> ,opstring (eval-parse (car rest)))
		    (apply <-> (append (list (eval-parse (car rest)))
				       (map (lambda (x) (<-> ,opstring " " (eval-parse x) " ")) (cdr rest))
				       ))))))
	  '(+ - * / | % & ~ == != < > <= >= || && += -= /= *= |= &= >> << ^
	      (|:>| ":>") (|<:| "<:") (|:| ":") (@ "alpha")
	      (colon ":") (comma ","))) ;; Looks better using seq and par.



(define-c-macro (= . rest)
  (c-display "\n\nError. \"=\" is not a valid eval-c function or operator. (Perhaps you ment \"==\" or \"set!\"?):\n")
  (display (cons '= rest))
  (newline)(newline))


(define-c-macro (begin . body)
  (eval-c-uplevel)
  (let ((ret (<-> "{" (string #\newline)
		  (eval-c-parse-lines body)
		  "}")))
    (eval-c-downlevel)
    ret))


(define (eval-c-cond->if terms)
  (let ((term (car terms)))
    (if (and (symbol? (car term))
	     (eq? 'else (car term)))
	(cons 'begin (cdr term))
	(if (not (null? (cdr terms)))
	    (list 'if (car term)
		  (append (list 'begin) (cdr term))
		  (eval-c-cond->if (cdr terms)))
	    (list 'if (car term)
		  (append (list 'begin) (cdr term)))))))
	      
	    

(define-c-macro (cond . terms)
  (eval-c-cond->if terms))


(define (eval-c-listify-struct rest)
  (let ((das-map '())
	(even #f))
    (define (nest rest)
      (if (not (null? rest))
	  (let ((a (car rest)))
	    (if even
		(begin
		  (if (eq? '* a)
		      (begin
			(set! das-map (cons (list even a (cadr rest)) das-map))
			(set! rest (cdr rest)))
		      (set! das-map (cons (list even a) das-map)))
		  (set! even #f))
		(set! even a))
	    (nest (cdr rest)))))
    (nest rest)
    (reverse! das-map)))


(define* (eval-c-structure structname cname rest #:optional (do-add-struct #t))
  (let ((das-map (eval-c-listify-struct rest)))
    (if do-add-struct
	(eval-c-add-struct structname
			   (map (lambda (a)
				  (if (= 3 (length a))
				      (list (caddr a) (car a) (cadr a))
				      (list (cadr a) (car a))))
				das-map)))
    (<-> "struct " cname
	 " {\n"
	 (apply <->
		(map (lambda (a)
		       (<-> "  "
			    (eval-c-get-propertype (car a))
			    (eval-c-parse (eval-c-cify-var (cadr a)))
			    (if (= 3 (length a))
				(eval-c-parse (caddr a))
				"")
			    ";\n"))
		     das-map))
	 "}")))


(define* (eval-c-define-struct name rest #:optional (do-add-struct #t))
  (eval-c-structure (<-> "<struct-" (string-trim-right (eval-c-get-propertype name)) ">")
		    (eval-c-parse (eval-c-get-propertype name))
		    rest
		    do-add-struct))

(define-c-macro (define-struct name . rest)
  (eval-c-define-struct name rest))

(define-c-macro (shared-struct structname)
  (eval-c-define-struct structname
			(apply append (map (lambda (def)
					     (if (= 3 (length def))
						 (list (cadr def)
						       (caddr def)
						       (car def))
						 (list (cadr def)
						       (car def))))
					   (let ((das-struct (eval-c-get-struct (symbol-append '<struct- (string->symbol
													  (list->string
													   (cdr
													    (string->list
													     (symbol->string
													      structname)))))))))
					     (if (not das-struct)
						 (c-display "\n\nError. struct \"" structname "\" not found.\n\n"))
					     das-struct)))
			#f))


(define (eval-c-shared-struct-but-only-known-types structname)
  (eval-c-define-struct structname
			(apply append (map (lambda (def)
					       (if (= 3 (length def))
						   (list '<void-*>
							 (car def))
						   (list (eval-c-get-known-type (cadr def))
							 (car def))))
					   (eval-c-get-struct (symbol-append '<struct- (string->symbol
											(list->string
											 (cdr
											  (string->list
											   (symbol->string
											    structname)))))))))
			#f))

#!

(eval-c-to-scm (eval-c-etype->ctype '<int>))
(eval-c-macro-result
 '(define-struct <gakk>
    <int> a
    <jmp_buf> ai
    <int> b
    <char-*> c
    <struct-gakk> * d))
(eval-c-macro-result '(shared-struct <gakk>))
(eval-c-macro-result '(shared-struct-but-only-known-types <gakk>))
(set! eval-c-typelist '())
(begin eval-c-typelist)
(eval-c-get-struct '<struct-gakk>)
(set! eval-c-structlist '())
(begin eval-c-structlist)
(eval-c-puttype 'a '<struct-gakk>)
(eval-c-gettype 'a.d.d.d.a)
!#


(define-c-macro (define-class name . rest)
  (let* ((privatevars '())
	 (publics '())
	 (publicvars '())
	 (privatefuncs '())
	 (publicfuncs '())
	 (destructors '())
	 (constructors '())
	 (cname (eval-c-etype->ctype name))
	 (structname (<-> cname "_class"))
	 (cclassname (<-> "class_" cname))
	 (classname (c-tosymbol (<-> "<" cclassname ">"))))
    
    (for-each (lambda (term)
		(cond ((eq? 'public (car term)) (set! publics (append (cdr term) publics)))
		      ((eq? 'define (car term)) (if (eq? 'destructor (caadr term))
						    (set! destructors (cons (cons (cdr (cadr term)) (cddr term)) destructors))
						    (set! constructors (cons (cons (cdr (cadr term)) (cddr term)) constructors))))
		      ((list? (cadr term)) (set! privatefuncs (cons term privatefuncs)))
		      (else (set! privatevars (cons term privatevars)))))
	      rest)
    (for-each (lambda (term)
		(if (list? (cadr term))
		    (set! publicfuncs (cons term publicfuncs))
		    (set! publicvars (cons term publicvars))))
	      publics)

    (set! privatevars (reverse! privatevars))
    (set! publicvars (reverse! publicvars))
    (set! privatefuncs (reverse! privatefuncs))
    (set! publicfuncs (reverse! publicfuncs))
    (set! destructors (reverse! destructors))
    (set! constructors(reverse! constructors))
    
    (let ((func-org-func (lambda (funcs publicorprivate)
			   (map (lambda (func)
				  (let* ((rettype (car func))
					 (rest (cadr func))
					 (def (cadr rest))
					 (name (car def))
					 (args (cdr def))
					 (body (cddr rest)))
				    (list rettype
					  (eval-c-get-cmethodname name cclassname publicorprivate)
					  (cons (list classname 'this) args)
					  body)))
				funcs))))

      (set! privatefuncs (func-org-func privatefuncs "private"))
      (set! publicfuncs (func-org-func publicfuncs "public"))

      (set! publicfuncs (cons `(,classname ,(eval-c-get-cmethodname 'constructor-internal cclassname "public") ()
					   ((let* ((this ,classname
							 (calloc 1 (sizeof ,classname))))
					      ,@(map (lambda (var)
						       `(set! ,(symbol-append 'this-> (cadr var)) ,(caddr var)))
						     (remove (lambda (var) (= (length var) 2))
							     (append privatevars publicvars)))
					      (return this))))
			      publicfuncs))

      (set! publicfuncs (cons `(<void> ,(eval-c-get-cmethodname 'destructor-internal cclassname "public") ((,classname this))
				       ((free this)))
			      publicfuncs))

      (if (null? destructors)
	  (set! destructors '((()))))
      (if (null? constructors)
	  (set! constructors '((() (return this)))))

      ;;(c-display "privatevars" privatevars "\n"
	;	 "publicvars" publicvars "\n"
	;	 "privatefuncs" privatefuncs "\n"
	;	 "publicfuncs" publicfuncs "\n"
	;	 "destructors" destructors "\n"
	;	 "constructors" constructors "\n")

    
      (<-> (eval-c-structure classname
			     structname
			     (apply append (map (lambda (var) (list (car var) (cadr var))) (append privatevars publicvars))))
	   
	   ";\n"
	   "typedef struct " structname "* " cclassname ";\n"

	   (let ((methods (append
			   (map (lambda (func)
				  (eval-c-parse `(,(car func) ,(cadr func) (lambda ,(caddr func) ,@(cadddr func)))))
				(append privatefuncs publicfuncs))
			   (map (lambda (constructor)
				  (eval-c-parse `(,classname ,(eval-c-get-cmethodname 'constructor cclassname "public")
							     (lambda ,(car constructor)
							       (let* ((this ,classname
									    (,(eval-c-get-cmethodname 'constructor-internal cclassname "public"))))
								 ,@(cdr constructor))))))
				constructors)
			   (map (lambda (destructor)
				  (eval-c-parse `(<void> ,(eval-c-get-cmethodname 'destructor cclassname "public")
							 (lambda ,(cons  (list classname 'this) (car destructor))
							   ,@(cdr destructor)
							   (,(eval-c-get-cmethodname 'destructor-internal cclassname "public") this)))))
				destructors))))
	     (apply <-> (append (map (lambda (method)
				       (<-> (car (string-split method #\{)) ";\n"))
				     methods)
				methods))
	     )
	   )
      )
  
    )
  )


(define-c-macro (new classname . args)
  `(,(eval-c-get-cmethodname 'constructor (eval-c-etype->ctype classname) "public") ,@args))

(define-c-macro (-> object methodname . args)
  `(,(eval-c-get-cmethodname methodname (eval-c-etype->ctype (eval-c-gettype object)) "public") ,object ,@args))

#!
(eval-c ""
	(<int> ais 78)
	(define-class <Bank>
	  (<int> sum (+ 2 3))
	  (<int> sum2)
	  (<int> sum3 (+ this->sum 1500))
	  (public
	   (<int> (define (deposit (<int> das_sum))
		    (<int> ai2 5)
		    (set! this->sum (+ this->sum3 das_sum 13451345))
		    (return (+ 2 this->sum))))))
	(run-now
	 (let* ((bank <class_Bank> (new <class_Bank>)))
	   (printf (string "aisann: %d %d\\n") ais (-> bank deposit 17)))))
	   
    
(c-dynamic-call "das_init" "/tmp/file6JoOzy.c.so")

(eval-c-macro-result
 '(define-class <Gakk>
    (<int> ai 5)
    (<int> (define (amethod (<int> a))
	     (return (-> this ai))))
    
    (public
     (<int> ai2)
     (<int> (define (c-scale (<float> x)
			     (<float> x1)
			     (<float> x2)
			     (<float> y1)
			     (<float> y2))
	      (return (+ (-> this gakk)
			 ;;(this->gakk2)
			 y1
			 (/ (* (- x x1) (- y2 y1))
			    (- x2 x1)))))))
    
    (define (destructor)
      (c-display "killed me"))
    
    (define (constructor (<int> a) (<int> b))
      (set! this->ai (+ (-> this ai) a b))
      (return this))))

---

(let* (gakk <class_Gakk> 
	    (new 5 6))
  (-> gakk c-scale 0 1 2 3))

->
(let* (gakk <class_Gakk>
	    (new 5 6))
  (gakk->c-scale gakk 0 1 2 3))	    

!#


#!

This definition of lambda doesn't work because I want the following to work:

  ("func" a b c)  -> func(a,b,c)

But, perhaps that last one shouldn't be allowed to work.

(define-c-macro (lambda def . body)
  `( ,(if (null? def) 
	  "()"
	  (apply <-> (append (list "(")
			     (map (lambda (x) (<-> (eval-c-parse x) ", ")) (c-butlast def))
			     (list (<-> (eval-c-parse (last def))))
			     (list ")"))))
     (begin ,@body)))
!#
      


(define eval-c-decl-symbol (gensym))

;; Workaround:
(define-c-macro (lambda def . body)
  (eval-c-uplevel)
  (let ((ret (<-> (if (null? def)
		      "()"
		      (apply <-> (append (list "(")
					 (map (lambda (x) (<-> (eval-c-parse x) ", ")) (c-butlast def))
					 (list (<-> (eval-c-parse (last def))))
					 (list ")"))))
		  (if (eq? eval-c-decl-symbol (car body))
		      ""
		      (<->  "{" (string #\newline)
			    (eval-c-parse-lines body)
			    "}" (string #\newline))))))
    (eval-c-downlevel)
    ret))

(define-c-macro (lambda-decl rest)
  `(lambda ,rest ,eval-c-decl-symbol))

(define-c-macro (get-proto name)
  (eval-c-parse (eval-c-get-public-funcdecl name)))

#!
(eval-c ""
	(<nonstatic-void> printsomething (lambda ()
					   (printf (string "something\\n")))))
(eval-c ""
	(get-proto printsomething)
	(run-now
	 (printsomething)))
!#

(define-c-macro (define def . body)
  (if (list? def)
      (<-> (eval-c-parse (eval-c-cify-var (car def)))
	   (eval-c-parse (append (list 'lambda (cdr def))
				 body)))
      `(,(car body) ,def ,(cadr body))))


(define eval-c-get-run-now-procname
  (let ((num 0))
    (lambda ()
      (set! num (1+ num))
      (<-> "run_now_" (number->string num)))))

(define-c-macro (run-now . body)
  (let ((procname (eval-c-get-run-now-procname)))
    (set! eval-c-run-nows (cons procname eval-c-run-nows))
    `(<void> ,procname (lambda ()
				,@body))))


(define* (eval-c-gen-public-func term #:optional publicname)
  (let* ((funcname_org (cadr term))
	 (funcname (eval-c-cify-var funcname_org))
	 (helperfuncname (<-> (symbol->string funcname) "_eval_c_helper"))
	 (parameters (cadr (caddr term)))
	 (rettype (car term))
	 (i 0)
	 (types (map (lambda (var)
		       (list (eval-c-to-scm (string-trim-right
					     (eval-c-get-propertype (car var))))
			     (cadr var)))
		     parameters)))
    `((<SCM> ,helperfuncname (lambda ,(map (lambda (var) (list '<SCM> (cadr var)))
					   parameters)
			       ,@(map-in-order (lambda (das-type)
						 (let* ((type (car das-type))
							(name (cadr das-type)))
						   (cond ((string=? "UNSPECIFIED" type)
							  (c-display "\n\nError! eval-c.scm/eval-c-gen-public-func: Strange type for " das-type "\n\n"))
							 ((string=? "SCM" type) "/* */")
							 ((string=? "JMP_BUF" type) "/* *")
							 (else
							  `(SCM_ASSERT ,(cond ((string=? "STRING" type) `(|| (scm_is_false ,name) (IS_STRING_P ,name)))
									      ((string=? "POINTER" type) `(POINTER_P ,name))
									      ((string=? "INTEGER" type) `(== SCM_BOOL_T (scm_number_p ,name)))
									      ((string=? "INT64" type) `(== SCM_BOOL_T (scm_number_p ,name)))
									      ((string=? "FLOAT" type) `(== SCM_BOOL_T (scm_number_p ,name)))
									      ((string=? "DOUBLE" type) `(== SCM_BOOL_T (scm_number_p ,name)))
									      (else (c-display "\n\nError! eval.cscm/eval-c-gen-public-func: What?\n\n")))
								       ,name
								       ,(let ((ret i)) (set! i (1+ i)) ret)
								       (string ,type))))))
					       types)
			       (,(let ((ret-scm (eval-c-to-scm (string-trim-right (eval-c-get-propertype rettype)))))
 				   (cond ((string=? "UNSPECIFIED" ret-scm) 'RETURN_UNSPECIFIED)
					 ((string=? "POINTER" ret-scm) 'RETURN_POINTER)
					 ((string=? "STRING" ret-scm) 'RETURN_STRING)
					 (else
					  (<-> "return MAKE_" ret-scm))))
				(,funcname ,@(map (lambda (type)
						    (list (string->symbol (<-> "GET_" (car type)))
							  (cadr type)))
						  types)))))
      (run-now
       (scm_c_define_gsubr (string ,(if publicname publicname funcname_org)) ,(length parameters) 0 0 ,helperfuncname)))))

(define-c-macro (public . body)
  (let* ((newbody '()))
    (for-each (lambda (term)
		(if (and (eval-c-symbol-is-type (car term))
			 (= 3 (length term))
			 (list? (caddr term))
			 (eq? 'lambda (caaddr term)))
		    (set! newbody (append newbody (list term) (eval-c-gen-public-func term)))
		    (set! newbody (append newbody (list term)))))
	      body)
    (eval-c-parse-lines newbody)))

#!
(eval-c-parse '(public (<int> a (lambda ((<int> b) (<char-*> c) (<void-*> d) (<float> e))
				  (return 5)))))
(eval-c ""
	(public
	 (<void> as (lambda ((<void-*> a))
		      (printf (string ("still here\\n")))))))
(as (list "A_POINTER" 1112607200))
!#

(define eval-c-get-unique-name
  (let ((num 0))
    (lambda ()
      (set! num (1+ num))
      (<-> "unique_name_" (number->string num)))))

(define (eval-c-proto->public funcdef)
  (let* ((temp (map string-trim-both (string-split funcdef #\()))
	 (retname (string-split (car temp) #\space))
	 (rettype (string-trim-both (apply <-> (map (lambda (x) (<-> x " ")) (c-butlast retname)))))
	 (name (last retname))
	 (args (let* ((temp1 (string-trim-right funcdef))
		      (temp2 (string-trim-both (substring temp1
							  (1+ (string-index temp1 #\())
							  (1- (string-length temp1))))))
		 (if (or (= (string-length temp2) 0)
			 (string=? temp2 "void"))
		     '()
		     (map (lambda (x)
			    (if (string-index x #\()
				(list "void*" (eval-c-get-unique-name))
				(let ((dassplit (map string-trim-both (string-split (string-trim-both x) #\space))))
				  (if (= 1 (length dassplit))
				      (list (car dassplit) (eval-c-get-unique-name))
				      (list (string-trim-both (apply <-> (map (lambda (x)
										(<-> x " "))
									      (c-butlast dassplit))))
					    (string-trim-both (last dassplit)))))))
			  (string-split temp2 #\,)))))
	 )

    (while (char=? #\* (car (string->list name)))
	   (begin
	     (set! name (list->string (cdr (string->list name))))
	     (set! rettype (<-> rettype "*"))))

    (eval-c-gen-public-func
     `( ,(string->symbol rettype) ,(string->symbol name) (lambda ,(map (lambda (x)
									 (let* ((type (car x))
										(name (cadr x)))
									   (if (char=? #\* (car (string->list name)))
									       (begin
										 (set! name (list->string (cdr (string->list name))))
										 (if (string=? "" name)
										     (set! name (eval-c-get-unique-name)))
										 (set! type (<-> type "*"))))
									   (list (string->symbol type)
										 (string->symbol name))))
								       args))))))
#!
(eval-c-proto->public "const char *jack_port_name (const jack_port_t *port)")
(eval-c-proto->public "const char **jack_port_get_connections (const jack_port_t *port)")

(eval-c-proto->public "void *jack_port_get_buffer (jack_port_t *, jack_nframes_t)")
(eval-c-proto->public "void jack_on_shutdown (jack_client_t *client, void (*function)(void *arg), void *arg)")
(eval-c-proto->public "void jack_on_shutdown (jack_client_t *client, void* arg)")
!#

(define-c-macro (proto->public . body)
  (eval-c-parse-lines (apply append! (map eval-c-proto->public
					  (apply append! (map (lambda (lines) (remove! (lambda (line) (string=? "" (string-trim-both line)))
										       lines))
							      (map (lambda (bod) (string-split bod #\;))
								   body)))))))


#!
(eval-c ""
	"#include <jack/jack.h>"
	(proto->public
	 "int getchar(void);
int fgetc (FILE
 *stream)"
	 "int getc(FILE *stream)"
	 "void jack_on_shutdown (jack_client_t *client, void (*function)(void *arg), void *arg)"
	 "void *jack_port_get_buffer (jack_port_t *, jack_nframes_t)"
	 "const char *jack_port_type (const jack_port_t *port)"
	 ))

(string-split "int ai();" #\;)
!#


(define-c-macro (variables->public . terms)
  (apply <-> (map (lambda (term)
		    (let ((type (car term))
			  (vars (cdr term)))
		      (apply <-> (map (lambda (var)
					(let* ((funcname (string->symbol (eval-c-get-unique-name)))
					       (funcdef `(,type ,funcname (lambda () (return ,var)))))
					  (<-> (eval-c-parse funcdef)
					       (eval-c-parse-lines (eval-c-gen-public-func funcdef var)))))
				      vars))))
		  terms)))

(define-c-macro (functions->public . terms)
  (apply <-> (map (lambda (term)
		    (<-> (eval-c-parse term)
			 (eval-c-parse `(variables->public (<void*> ,(cadr term))))))
		  terms)))

#!  

(eval-c-parse
 '(variables->public
   (<int> JackPortIsInput
	  JackPortIsOutput 
	  JackPortIsPhysical
	  JackPortCanMonitor
	  JackPortIsTerminal)
   (<char*> JACK_DEFAULT_TYPE)))
!#


(define-c-macro (set! name val)
  (<-> (eval-c-parse name) " = " (eval-c-parse val)))

(define-c-macro (or . tests)
  `(|| ,@tests))

(define-c-macro (and . tests)
  `(&& ,@tests))

(define-c-macro (quote val)
  (<-> (string #\') (string-trim-right (eval-c-parse val)) (string #\')))

(define-c-macro (let* defs . body)
  `(begin
     ,@(map (lambda (def)
	      (if (= 3 (length def))
		  (list (cadr def) (car def) (caddr def))
		  (if (and (not (symbol? (cadr def)))
			   (not (list? (cadr def))))
		      (c-display "Error. \"" (cadr def) "\" is (probably) not a type in expression" def)
		      (list (cadr def) (car def)))))
	    defs)
     ,@body))

(define-c-macro (let . something)
  (c-display "\n\nError. No such eval-c function: let. (perhaps you ment let*?)\n\n"))

(define-c-macro (cast etype var)
  (<-> "((" (eval-c-etype->ctype etype) ")" (eval-c-parse var) ")"))

(define-c-macro (not atest)
  `("!" ,atest))

(define-c-macro (1+ something)
  `(+ 1 ,something))

(define-c-macro (1- something)
  `(- ,something 1))

(define-c-macro (sizeof type)
  `("sizeof" ,(eval-c-get-propertype type)))

(define-c-macro (while test . body)
  (<-> "while (" (eval-c-parse test) ")"
       (eval-c-parse `(begin ,@body))))

(define-c/faust-macro (string astring)
  (<-> "\"" (if (string? astring)
		astring
		(string-trim-right (eval-c-parse astring)))
       "\""))

(define-c-macro (for init testing lastaction .  body)
  `(begin ,init
	  (while 1 (begin
		     (if (not ,testing) break)
		     ,@body
		     ,lastaction))))


(define-c-macro (for-each startval . rest)
  (let* ((restlen (length rest))
	 (itername (caadr (last rest)))
	 (body (cddr (last rest)))
	 (endval #f)
	 (testfunc #f)
	 (addval #f))
    (cond ((= 2 restlen)
	   ;;(for-each 0 n
	   (set! endval (car rest))
	   (set! testfunc (if (and (number? endval)
				   (number? startval))
			      (if (> startval endval)
				  '>=
				  '<)
			      '<))
	   (set! addval (if (eq? testfunc '<)
			    1
			    -1)))

	  ((= 4 restlen)
	   ;;(for-each 0 < n 1
	   (set! testfunc (car rest))
	   (set! endval (cadr rest))
	   (set! addval (caddr rest)))
	  
	  (else
	   (if (member (car rest) '(< > <= >= == != <> & | ~))
	       (begin
		 ;;(for-each 0 < n
		 (set! testfunc (car rest))
		 (set! endval (cadr rest))
		 (set! addval (if (member testfunc '(> =>))
				  -1
				  1)))
	       (begin
		 ;;(for-each 0 n 1
		 (set! endval (car rest))
		 (set! testfunc (if (and (number? endval)
					 (number? startval))
				    (if (> startval endval)
					'>=
					'<)
				    '<))
		 (set! addval (cadr rest))))))
    `(for (<int> ,itername ,startval)
	  (,testfunc ,itername ,endval)
	  (set! ,itername (+ ,itername ,addval))
	  ,@body)))
    
	      

  

#!

;; neverending loop!
(eval-c ""
	(run-now
	 (for-each 10 != 5 1 
		   (lambda (i)
		     (printf (string "%d\\n") i)))))


(eval-c-parse '(if (not (== a 2)) b))

(eval-c-parse '(for-each 0 < 5 1
			 (lambda (i)
			   (set! out[i] 0.0f))))

(eval-c-parse '(for (set! i 0) (< i 5) i++
		    (printf i)))

(eval-c-parse '(if (not (a)) b))

(eval-c-parse '(while (a) (begin (ai))))
(eval-c-parse '(<static-int> a 5))
(eval-c-parse '(define a <static-int> 5))
(eval-c-parse '(<int> (define (gakk (<int> a)) (ai))))
(eval-c-parse '(define (gakk (<int> a))))

(eval-c-parse (list 'lambda (list (list '<int> 'a))))

(define-c-macro (printf . rt)
  `("printf" 4 5 ,@rt))
(macroexpand (cons (symbol-append eval-c-macro-prefix 'printf) (list 2)))

(eval-c-parse '(<int> gakk (lambda ()
			     au
			     (au2))))

(eval-c-parse '(<static-int> gakk 452 3 "314" (printf "asdfasdf")))

(eval-c-parse '(if (not (== a b))
		   (begin
		     (printf (+ 2 5 2))
		     (ai 5))
		   (printf 3)))

(eval-c-parse '(cond (test1 todo1 todo1.2 (todo1.3))
		     (test2 todo2)
		     (else todo3)))

(eval-c-parse '(if (== 2 test1)
		   (begin
		     (todo1)
		     (todo1.2)
		     (todo1.3))
		   (if (== 2 test2)
		       (begin
			 (todo2))
		       (begin
			 (todo3)
			 (todo3.2)))))

(eval-c-parse '(<void> parse_arguments 
		       (lambda ((<int> argc) (<char**> argv))
			 (if (< argc 2)
			     (begin
			       (fprintf stderr (string "usage: %s y|n\n") package)
			       (exit 9))))))

(eval-c-parse '(<void> (define (parse_arguments (<int> argc)
						(<char**> argv))
			 (if (< argc 2)
			     (begin
			       (fprintf stderr (string "usage: %s y|n\n") package)
			       (exit 9)))
			 (if (or (== argv[1][0] 'y)
				 (== argv[1][0] 'Y)
				 (== argv[1][0] '1))
			     (set! onoff 1)
			     (set! onoff 0)))))

(eval-c-parse '(<int> process (lambda ((<jack_nframes_t> nframes)
				       (<void*> arg))
				(let* ((out <jack_default_audio_sample_t*>
					    (jack_port_get_buffer output_port nframes))
				       (in <jack_default_audio_sample_t*>
					   (jack_port_get_buffer input_port nframes)))
				  (memcpy out in
					  (* (sizeof <jack_default_audio_sample_t>) nframes)))
				
				(return 0))))


!#



;;;;;;;;;;;;;;;;;; Cache

(define eval-c-generation 9) ;;Cached code with different generation can not be used, and file is automatically deleted.

(define eval-c-cache-dir (<-> (getenv "HOME") "/snd-eval-c-cache"))

(system (<-> "mkdir " eval-c-cache-dir " >/dev/null 2>/dev/null"))
(define eval-c-cache '())

(define eval-c-cached-code #f)

;; Load cached code.
(let* ((dir (opendir eval-c-cache-dir))
       (entry (readdir dir)))
  (while (not (eof-object? entry))
	 (if (and (not (string=? "." entry))
		  (not (string=? ".." entry))
		  (>= (string-length entry) 6)
		  (string=? (string-take entry 6) "cache.")
		  (not (string=? (string-take-right entry 3) ".so")))
	     (let ((filename (<-> eval-c-cache-dir "/" entry))
		   (itsokey #t))
	       ;;(c-display "cache-eval" filename)
	       (catch #t
		      (lambda ()
			(load filename)
			(list (car eval-c-cached-code)
			      (cadr eval-c-cached-code)
			      (caddr eval-c-cached-code)
			      (cadddr eval-c-cached-code)
			      (cadr (cdddr eval-c-cached-code))
			      (caddr (cdddr eval-c-cached-code))
			      ))
		      (lambda x
			(set! itsokey #f)
			(c-display x)
			(c-display "Suspicious cached file detected:" filename "(this file should be deleted)")))
	       (if itsokey
		   (if (or (not (string=? (cadr eval-c-cached-code) (snd-version)))
			   (not (string=? (caddr eval-c-cached-code) (version)))
			   (not (= (car eval-c-cached-code) eval-c-generation)))
		       (begin
			 (c-display "Deleting obsolete cached code " (cadddr eval-c-cached-code) "and" filename ".")
			 (system (<-> "rm -f " (cadddr eval-c-cached-code) " " filename)))
		       (set! eval-c-cache (cons (cdddr eval-c-cached-code)
						eval-c-cache))))))
	 (set! entry (readdir dir)))
  (closedir dir))
;(load (<-> eval-c-cache-dir "/cache.qtSKPT"))
;(begin eval-c-cached-code)

	   
(define (eval-c-check-cached compile-options terms)
  ;;(pretty-print terms)
  (let ((ret (call-with-current-continuation
	      (lambda (return)
		(for-each (lambda (cache)
			    (if (and (string=? compile-options (cadr cache))
				     (equal? terms (caddr cache)))
				(return cache)))
			  eval-c-cache)
		(return #f)))))
    (if ret
	(catch #t
	       (lambda ()
		 (cond ((defined? 'c-dynamic-call)
			(c-dynamic-call (car ret) "das_init"))
		       (else
			(c-display "Linking" (car ret))
			(dynamic-call "das_init" (dynamic-link (car ret))))))
	       (lambda x
		 (c-display x)
		 (set! ret #f))))
    ret))

(define (eval-c-cache-it compile-options terms object-file)
  (if object-file
      (let* ((mainfile (mkstemp! (<-> eval-c-cache-dir "/cache.XXXXXX")))
	     (newobjectfile (<-> (port-filename mainfile) ".so")))
	(set! eval-c-cache (cons `(,newobjectfile ,compile-options ,terms)
				 eval-c-cache))
	(system (<-> "mv " object-file " " newobjectfile))
	(pretty-print `(set! eval-c-cached-code '(,eval-c-generation ,(snd-version) ,(version) ,newobjectfile ,compile-options ,terms)) mainfile)    
	(close mainfile))))

#!
(eval-c-cache-it "aiai" '(gakk gakk))
(pretty-print '(define (foo) (lambda (x))))
(define t (mkstemp! "/tmp/aiXXXXXX"))
(cadr (begin t))
(port-filename t)

!#


;;;;;;;;;;;;;;;;;; Compiling and stuff

;;(define (ec-waitforfile filename)
;;  (while (not (access? filename X_OK))
;;	 (usleep 100)))

(define eval-c-filestobedeleted '())
(define (delete-at-exit filename)
  (push! filename eval-c-filestobedeleted))

(add-hook! exit-hook (lambda args
		       (for-each (lambda (filename)
				   (system (<-> "rm -f " filename)))
				 eval-c-filestobedeleted)
		       #f))

(define old-tmpnam tmpnam)
(define* (tmpnam :key (autodelete #t))
  (define ret (old-tmpnam))
  (if autodelete
      (delete-at-exit ret))
  ret)

      
(define* (eval-c-eval #:key (compile-options "") . codestrings)
  (let* ((evalstring "")
	(sourcefile (<-> (tmpnam) ".c"))
	(libfile (<-> sourcefile ".so"))
	(fd (open-file sourcefile "w"))
	(guile-config (<-> (cdr (assoc 'bindir %guile-build-info)) "/guile-config")))
    (if (not (access? guile-config X_OK))
	(begin
	  (c-display "Error. " guile-config " not found, or is not an executable.")
	  (c-display "        Perhaps you need the guile-devel pacage?")
	  (newline)
	  (exit)))
    (for-each (lambda (s)
		(write-line s fd))
	      (if (eq? (car codestrings) '#:compile-options)
		  (cddr codestrings)
		  codestrings))
    (close fd)
    (if eval-c-verbose
	(c-display "Compiling" sourcefile))
    (if #f
	(c-display (<-> *eval-c-compiler* " -O3 -fPIC -shared -o " libfile " " sourcefile " "
			(if (string=? *eval-c-compiler* "icc")
			    "-L/opt/intel_cc_80/lib /opt/intel_cc_80/lib/libimf.a"
			    (<-> "-Wall " (or (getenv "CFLAGS") "") " " (if (getenv "LDFLAGS") (getenv "LDFLAGS") "") " "))
			(string #\`) guile-config " compile" (string #\`) " "
			compile-options)))
    (if (not (= 0 (system (<-> *eval-c-compiler* " -O3 -fPIC -shared -o " libfile " " sourcefile " -g " ;" -fomit-frame-pointer "
			       (if (string=? *eval-c-compiler* "icc")
				   "-L/opt/intel_cc_80/lib /opt/intel_cc_80/lib/libimf.a"
				   (<-> "-Wall " (or (getenv "CFLAGS") "") " " (if (getenv "LDFLAGS") (getenv "LDFLAGS") "") " "))
			       " " *eval-c-CFLAGS* " "
			       (string #\`) guile-config " compile" (string #\`) " "
			       compile-options))))
	(begin
	  (if eval-c-lazy-cleanup
	      (delete-at-exit sourcefile))
	  (throw 'compilation-failed)))

    ;;(ec-waitforfile libfile)
    (if (defined? 'c-dynamic-call)
	(c-dynamic-call libfile "das_init")
	(dynamic-call "das_init" (dynamic-link libfile)))
    (if (not *eval-c-do-cache*)
	(system (<-> "rm " libfile)))

    (if eval-c-cleanup
	(system (<-> "rm " sourcefile))
	(if eval-c-lazy-cleanup
	    (delete-at-exit sourcefile)))
    libfile))




(define (eval-c-parse-file terms)
  (define temp #f)
  
  (set! eval-c-run-nows '())
  (set! eval-c-typelistlist '())
  (set! eval-c-typelist '())
  ;;;(set! eval-c-structlist '())
  (set! eval-c-classlist '())
  (set! eval-c-level 0)
  (set! eval-c-string-is-pointer #f)

  (set! temp (map eval-c-parse-line terms))
  
  `(,(<-> "#include \"" snd-header-files-path "/mus-config.h\"")
    "#include <stdio.h>"
    "#include <libguile.h>"
    "#include <string.h>"
    "#include <stdlib.h>"
    "#define NOTHING"
    "#ifndef SCM_VECTOR_REF"	
    "  #define SCM_VECTOR_REF(a,b) SCM_VELTS (a)[(long)(b)]"
    "#endif"	
    "#ifndef scm_is_false"
    "#  define scm_is_false(a) ((a) == (SCM_BOOL_F))"
    "#endif"
    "#define EC_MAX(a,b) (((a)>(b))?(a):(b))"
    "#define EC_MIN(a,b) (((a)<(b))?(a):(b))"
    "#define MAKE_STRING(a) scm_protect_object(scm_mem2string(a,strlen(a)))"
    "#define RETURN_STRING(a) {char *ret=(a); return ret?MAKE_STRING(ret):SCM_BOOL_F;}"
    "#ifdef SCM_STRING_CHARS"
    "#  define GET_STRING(a) (scm_is_false(a)?NULL:(char*)SCM_STRING_CHARS(a))"
    "#else"
    "#  define GET_STRING(a) (scm_is_false(a)?NULL:(char*)scm_i_string_chars(a))"
    "#endif"
    "#define GET_POINTER3(a) (scm_is_false(a)?NULL:(void *)scm_num2ulong(a,0,\"GET_POINTER3()\"))"
    "#define GET_POINTER(a) (scm_is_false(a)?NULL:GET_POINTER3(SCM_CAR(SCM_CDR(a))))"
    "#define GET_POINTER2(a) GET_POINTER(a)"
    "#define MAKE_POINTER(a) scm_cons(MAKE_STRING(\"A_POINTER\"),scm_cons(scm_ulong2num((unsigned long)a),SCM_EOL))"
    "#define RETURN_POINTER(a) {unsigned long ret=(unsigned long)(a); return ret?MAKE_POINTER(ret):SCM_BOOL_F;}"
    "#define RETURN_UNSPECIFIED(a) {(a);return SCM_UNSPECIFIED;}"
    "#define GET_INTEGER(a) scm_to_int(scm_inexact_to_exact(scm_floor(a)))"
    "#define MAKE_INTEGER(a) scm_from_int(a)"
    "#define GET_INT64(a) scm_to_int64(scm_inexact_to_exact(scm_floor(a)))"
    "#define MAKE_INT64(a) scm_from_int64(a)"
    "#define GET_DOUBLE(a) scm_num2dbl(a,\"GET_DOUBLE\")"
    "#define MAKE_DOUBLE(a) scm_make_real((double)a)"
    "#define GET_FLOAT(a) ((float)scm_num2dbl(a,\"GET_DOUBLE\"))"
    "#define MAKE_FLOAT(a) scm_make_real((double)a)"
    "#define GET_SCM(a) (a)"
    "#define MAKE_SCM(a) (a)"
    "#define POINTER_P(a) (scm_is_false(a) || ((SCM_BOOL_T == scm_list_p(a)) && (IS_STRING_P(SCM_CAR(a)) || SCM_SYMBOLP(SCM_CAR(a))) && SCM_NULLP(SCM_CDR(SCM_CDR(a))) && (SCM_BOOL_T ==scm_number_p(SCM_CAR(SCM_CDR(a))))))"
    "#ifdef SCM_STRINGP"
    "#  define IS_STRING_P(Arg)           (SCM_STRINGP(Arg))"
    "#else"
    "#  define IS_STRING_P(Arg)           scm_is_string(Arg)"
    "#endif"
    "typedef scm_t_int64 int64;"
    ,@temp
    
    "void das_init(){"
    ,@(map (lambda (x)
	     (<-> x "();"))
	   (reverse eval-c-run-nows))
    "}"))



#!
(define (eval-c-non-macro compile-options . terms)
  (apply eval-c-eval
	 (append (list #:compile-options compile-options)
		 (eval-c-parse-file terms))))
!#

(define (eval-c-non-macro compile-options cache-key . terms)
  (if cache-key
      (if (not (eval-c-check-cached compile-options cache-key))
	  (eval-c-cache-it compile-options cache-key
			   (apply eval-c-eval
				  (append (list #:compile-options compile-options)
					  (eval-c-parse-file terms)))))
      (apply eval-c-eval
	     (append (list #:compile-options compile-options)
		     (eval-c-parse-file terms)))))
	       

(define-macro (eval-c compile-options . terms)
  `(if (not (eval-c-check-cached ,compile-options ',terms))
       (eval-c-cache-it ,compile-options ',terms
			(eval-c-eval #:compile-options ,compile-options
				     ,@(eval-c-parse-file terms)))))



(eval-c ""
	"#include <dlfcn.h>"
	"#include <unistd.h>"

	"typedef void (*functype)()"

	(variables->public
	 (<int> RTLD_LAZY RTLD_NOW RTLD_GLOBAL RTLD_LOCAL))

	(public
	 (<void-*> c_dlsym_internal (lambda ((<void-*> handle)
					     (<char-*> funcname))
				      (if (== handle NULL)
					  (begin
					    (printf (string "Handle null (%s)\\n") (dlerror))
					    (return NULL))
					  (return (dlsym handle funcname)))))
 
	 (<void> c_dynamic_call_internal (lambda ((<void-*> handle)
						  (<char-*> funcname))
					   (let* ((func <functype> (c_dlsym_internal handle funcname)))
					     (if (== func NULL)
						 (printf (string "func null (%s)\\n") (dlerror))
						 (func)))))
	 (<void-*> c_dynamic_handle_internal (lambda ((<char-*> filename)
						      (<int> flag))
					       (let* ((handle <void-*> (dlopen filename flag)))
						 (if (== handle NULL)
						     (begin
						       (printf (string "Handle null (%s)\\n") (dlerror))
						       (return NULL))
						     (return handle)))))))
(define* (c-dynamic-handle filename :key
			   (flags (logior (RTLD_GLOBAL) (RTLD_NOW))))
  (c_dynamic_handle_internal filename flags))
			   
(define (c-dlsym filename_or_handle funcname)
  (c_dlsym_internal (if (string? filename_or_handle)
			(c-dynamic-handle filename_or_handle)
			filename_or_handle)
		    funcname))
  
(define (c-dynamic-call filename_or_handle funcname)
  (if (string? filename_or_handle)
      (c-display "linking" filename_or_handle funcname))
  (c_dynamic_call_internal (if (string? filename_or_handle)
			       (c-dynamic-handle filename_or_handle)
			       filename_or_handle)
			   funcname))

  
  
;(define-macro (define-c ret-type def . body)
;  `(eval-c ""
;	   (public (,(car def) ,(cadr def) (lambda ,(cdr def)
;					   ,@body)))))

(define-macro (define-c ret-type def)
  `(eval-c ""
	   (public (,ret-type ,(car def) (lambda ,(cadr def)
					   ,@(cddr def))))))
#!
(load-from-path "eval-c.scm")

(eval-c ""
	(run-now
	 (printf (string "Hello world!\\n"))))
!#

(eval-c ""
	(<void-*> a_pointer)
	(public
	 (<int> ec-get-ints-element (lambda ((<int*> array) (<int> n))
				      (return array[n])))
	 (<double> ec-get-doubles-element (lambda ((<double*> array) (<int> n))
					(return array[n])))
	 (<float> ec-get-floats-element (lambda ((<float*> array) (<int> n))
					  (return array[n])))
	 (<char*> ec-get-strings-element (lambda ((<char**> array) (<int> n))
					   (return array[n])))
	 (<void*> ec-get-pointers-element (lambda ((<void**> array) (<int> n))
					    (return array[n])))
	 
	 (<void> ec-put-ints-element (lambda ((<int*> array) (<int> val) (<int> n))
				       (set! array[n] val)))
	 (<void> ec-put-doubles-element (lambda ((<double*> array) (<double> val) (<int> n))
					  (set! array[n] val)))
	 (<void> ec-put-floats-element (lambda ((<float*> array) (<float> val) (<int> n))
					 (set! array[n] val)))
	 (<void> ec-put-strings-element (lambda ((<char**> array) (<char*> val) (<int> n))
					  (set! array[n] val)))
	 (<void> ec-put-pointers-element (lambda ((<void**> array) (<void*> val) (<int> n))
					   (set! array[n] val)))
	 
	 (<int*> ec-make-ints (lambda ((<int> n))
				(return (calloc n (sizeof <int>)))))
	 (<int64*> ec-make-int64s (lambda ((<int> n))
				    (return (calloc n (sizeof <int64>)))))
	 (<double*> ec-make-doubles (lambda ((<int> n))
				      (return (calloc n (sizeof <double>)))))
	 (<float*> ec-make-floats (lambda ((<int> n))
				    (return (calloc n (sizeof <float>)))))
	 (<char**> ec-make-strings (lambda ((<int> n))
				     (return (calloc n (sizeof <char*>)))))
	 (<void**> ec-make-pointers (lambda ((<int> n))
				      (return (calloc n (sizeof <void*>)))))
	 (<size_t> ec-sizeof-pointer (lambda ()
				       (return (sizeof <void-*>))))
	 (<void-*> ec-pointer-to-pointer (lambda ((<void-*> pointer))
					   (set! a_pointer pointer)
					   (return &a_pointer)))
	 ;; careful
	 (<void> ec-free (lambda ((<void-*> pointer))
			   (free pointer)))

	;; No. This is dangerous, ugly and unnecesarry. #f is the same as NULL from the guile-side.
	;;(<SCM> c_NULL (lambda ()
	;;		(return (MAKE_POINTER NULL))))
	;;(run-now
	;; (scm_c_define_gsubr (string "NULL") 0 0 0 c_NULL)
	))



(define ec-pointer->integer cadr)

(define (ec-pointer? something)
  (and (pair? something)
       (string? (car something))
       (string=? "A_POINTER" (car something))))

;; This one will never return an integer array, unless das-make-func is set to ec-make-ints. Same
;; goes for creating a double-array; das-make-func must be set to ec-make-doubles.
(define* (ec-make-array array #:key das-make-func das-ret)
  (if (null? array)
      #f
      (let* ((make-func (if das-make-func
			    das-make-func
			    (cond ((string? (car array)) ec-make-strings)
				  ((ec-pointer? (car array)) ec-make-pointers)
				  (else ec-make-floats))))
	     (ret (if das-ret
		      das-ret
		      (make-func (length array))))
	     (put-func (cond ((eq? ec-make-strings make-func) ec-put-strings-element)
			     ((eq? ec-make-pointers make-func) ec-put-pointers-element)
			     ((eq? ec-make-floats make-func) ec-put-floats-element)
			     ((eq? ec-make-doubles make-func) ec-put-doubles-element)
			     ((eq? ec-make-ints make-func) ec-put-ints-element)
			     (else
			      (c-display "Illegal das-make-func argument for ec-make-array: " das-make-func)))))
	
	(c-for-each (lambda (n val)
		      (put-func ret val n))
		    array)
	ret)))

(define (ec-get-nullterminated-somethings array get-func)
  (let ((ret '()))
    (let loop ((n 0))
      (let ((res (get-func array n)))
	(if res
	    (begin
	      (set! ret (cons res ret))
	      (loop (1+ n))))))
    (reverse! ret)))

(define (ec-get-somethings array num-elements get-func)
  (if num-elements
      (let ((ret '()))
	(c-for 0 < num-elements 1
	       (lambda (n)
		 (set! ret (cons (get-func array n)
				 ret))))
	(reverse! ret))
      (ec-get-nullterminated-somethings array get-func)))
  
(define* (ec-get-ints array #:optional num)
  (ec-get-somethings array num ec-get-ints-element))
(define* (ec-get-doubles array #:optional  num)
  (ec-get-somethings array num ec-get-double-element))
(define* (ec-get-floats array  #:optional num)
  (ec-get-somethings array num ec-get-floats-element))
(define* (ec-get-strings array  #:optional num)
  (ec-get-somethings array num ec-get-strings-element))
(define* (ec-get-pointers array  #:optional num)
  (ec-get-somethings array num ec-get-pointers-element))


#!
(define a (ec-make-array '(0 1 2 3 4 5) #:das-make-func ec-make-ints))
(ec-get-ints a 10)

(define b (ec-make-array (list a a a a a a) #:das-make-func ec-make-pointers))
(begin b)
(ec-get-pointers b 6)
!#


;(define (ec-make-struct something)
;  ())

(define (free-ec-struct-from-guardian object)
  (c-display "Automatically freeing a c-object of type:" ". This should not lead to trouble, but I'm not sure.")
  (if #t
      (begin
	(ec-free object)
	(c-display "that went well...?"))
      (c-display "(not really freeing)" object))
  ;;(,(symbol-append cleanname '_delete) das-c-object)
  ;;(-> das-this destructor)
  )


(define-macro (define-ec-struct name . rest)
  (define autofree #f)
  (when (eqv? (car rest) :autofree)
    (set! autofree (cadr rest))
    (set! rest (cddr rest)))

  (let ((structname (symbol-append '<struct- (string->symbol
					      (list->string
					       (cdr
						(string->list
						 (symbol->string
						  name)))))))
	(structname-pointer (symbol-append '<struct- (string->symbol
						      (<->
						       (list->string
							(cdr
							 (c-butlast
							  (string->list
							   (symbol->string
							    name)))))
						       "-*>"))))
	(cleanname (string->symbol (eval-c-etype->ctype name)))
	(das-map (eval-c-listify-struct rest)))
    (eval-c-add-struct structname
		       (map (lambda (a)
			      (if (= 3 (length a))
				  (list (caddr a) (car a) (cadr a))
				  (list (cadr a) (car a))))
			    das-map))
    (primitive-eval
     `(eval-c ""
	      ,(eval-c-shared-struct-but-only-known-types name)
	      (public 
	       (,structname-pointer ,(symbol-append cleanname '_new) (lambda ()
								       (return (calloc 1 (sizeof ,structname)))))
	       (<void> ,(symbol-append cleanname '_delete) (lambda ((,structname-pointer arg))
							     (free arg)))
	       (<int> ,(symbol-append cleanname '_get_das_size) (lambda ()
								  (return (sizeof ,structname))))
	       ,@(apply append (map (lambda (def)
				      (let ((name (cadr def))
					    (type (if (or (pair? (car def))
							  (= 3 (length def)))
						      '<void-*>
						      (eval-c-get-known-type (car def)))))
					`((<void> ,(symbol-append cleanname '_set_ name) ,(if (eq? type '<void-*>)
											      `(lambda ((,structname-pointer arg)
													(,type newval)
													(<int> autofree))
												 (if autofree (free ,(symbol-append 'arg-> name)))
												 (set! ,(symbol-append 'arg-> name) newval))
											      `(lambda ((,structname-pointer arg)
													(,type newval))
												 (set! ,(symbol-append 'arg-> name) newval))))
					  (,type ,(symbol-append cleanname '_get_ name) (lambda ((,structname-pointer arg))
											  (return ,(symbol-append 'arg-> name)))))))
				    (remove (lambda (t)
					      (eq? '<jmp_buf> (car t)))
					    das-map))))))
    `(def-class  (,name :key
			,@(map cadr das-map))

       (define c-object
	 (let ((ret (,(symbol-append cleanname '_new))))
	   (if ,autofree
	       (add-finalizer ret free-ec-struct-from-guardian))
	   ret))


       (def-method (get-c-object)
	 c-object)

;       (define-method (get-pointer-to-c-object)
;	 (cons "A_POINTER" (cons (scm_ulong2num  ((unsigned long)a),SCM_EOL))

       (def-method (destructor)
	 (,(symbol-append cleanname '_delete) c-object))


       (def-method (get-size)
	 (,(symbol-append cleanname '_get_das_size)))

       ,@(map (lambda (def)
		`(define ,(symbol-append 'num-(cadr def)) #f))
	      das-map)

       ,@(map (lambda (def)
		(let* ((name (cadr def))
		       (type (car def))
		       (typetype (eval-c-to-scm (eval-c-etype->ctype type)))
		       (type*type (eval-c-to-scm (eval-c-etype->ctype (eval-c-get-*type type)))))
		  (if (string=? typetype "POINTER")
		      `(def-method (,name #:key das-make-func (autofree 'auto) #:rest rest)
			 (if (not (null? rest))
			     (let ((newval (car rest))
				   (org-num ,(symbol-append 'num- name)))
			       (,(symbol-append cleanname '_set_ name)
				c-object
				(if (and (not (ec-pointer? newval))
					 (list? newval))
				    (begin
				      (set! ,(symbol-append 'num- name) (length newval))
				      (ec-make-array (if (and (not (null? newval))
							      (procedure? (car newval)))
							 (map (lambda (a) (->2 a get-c-object))
							      newval)
							 newval)
						     #:das-make-func ,(cadr (member type*type
										    `("UNSPECIFIED" ec-make-pointers
										      "STRING" ec-make-pointers
										      "INTEGER" ec-make-ints
										      "INT64" ec-make-int64s
										      "DOUBLE" ec-make-doubles
										      "FLOAT" ec-make-floats
										      "POINTER" ec-make-pointers)))))
				    (begin
				      (set! ,(symbol-append 'num- name) #f)
				      newval))
				(if (eq? autofree 'auto)
				    (if org-num 1 0)
				    (if autofree 1 0))))
			     (if ,(symbol-append 'num- name)
				 (,(cadr (member type*type
						 `("UNSPECIFIED" ec-get-pointers
						   "STRING" ec-get-pointers
						   "INTEGER" ec-get-ints
						   "INT64" ec-get-int64s
						   "DOUBLE" ec-get-doubles
						   "FLOAT" ec-get-floats
						   "POINTER" ec-get-pointers)))
				  (,(symbol-append cleanname '_get_ name) c-object)
				  ,(symbol-append 'num- name))
				 (,(symbol-append cleanname '_get_ name) c-object))))
		      `(def-method (,name . rest)
			 (if (not (null? rest))
			     (,(symbol-append cleanname '_set_ name) c-object (car rest))
			     (,(symbol-append cleanname '_get_ name) c-object))))))
	      das-map)
       ,@(map (lambda (def)
		(let ((name (cadr def)))
		  (if (eq? '<SCM> (car def))
		      `(->2 this ,name ,name)
		      `(if ,name
			   (->2 this ,name ,name)))))
	      das-map)
       )
    ))

#!
(define-ec-struct <struct_name>
  <int> one
  <float-*> twos
  <char-*> three)
(define test (<struct_name> :one 1
			    :twos '(2)
			    :three "three"))


(gc)
(-> test one)
=> 1

(-> test one 4)
(-> test one)
=> 4

(-> test twos)
=> (2.0)

(-> test twos '(2 3 4))
(-> test twos)
=> (2.0 3.0 4.0)

(-> test three "four")
(-> test three)
-> "four"


(define-ec-struct <test>
  <SCM> val)

(define test (<test> :val (list 2 3 4)))
(gc)
(-> test val)


(define-ec-struct <test_struct>
  <int> one
  <float-*> floats)


(define test (<test_struct> :one 1
			    :floats '(2 3 4)))
=> #<unspecified>

(-> test scm test)
(-> test scm)

(-> test one)
=> 1

(-> test one 10)
=> #<unspecified>

(-> test one)
=> 10

(-> test get-c-object)

(define-ec-struct test


(define-ec-struct <Jack_Arg>
  <int> num_inports
  <int-*> ai
  <jack_port_t**> input_ports)
(define jack (<Jack_Arg> #:num_inports 9))
(begin jack)
(-> jack num_inports)
(-> jack num_inports 4)
(<- jack num_inports)

(-> jack get-size)
(-> jack get-c-object)

(-> jack input_ports)
(define a (ec-make-array '(0 1 2 3 4 5) #:das-make-func ec-make-ints))
(-> jack input_ports (list a a a a a))

(-> jack ai)
(-> jack ai '(0 1 2 3 4 5))

->
(eval-c-get-struct '<struct-Jack_Arg>)
(eval-c-macro-result '(shared-struct-but-only-known-types <Jack_Arg>))
(eval-c-add-struct '<struct-Jack_Arg>
		   '((<int> num_inports)
		     (<jack_port_t**> input_ports)))

(primitive-eval
 '(eval-c ""
	  (shared-struct-but-only-known-types <Jack_Arg>)
	  (public
	   (<struct-Jack_Arg-*> Jack_Arg_new (lambda (<void>)
					     (calloc 1 (sizeof <struct-Jack_Arg>))))
	   (<void> Jack_Arg_set_num_inports (lambda ((<struct-Jack_Arg> *arg)
						     (<int> newval))
					      (set! arg->num_inports newval)))
	   (<int> Jack_Arg_get_num_inports (lambda ((<struct-Jack_Arg> *arg))
					     (return arg->num_inports)))
	   (<void> Jack_Arg_set_input_ports (lambda ((<struct-Jack_Arg> *arg)
						     (<void-*> newval)
						     (<int> autofree))
					      (if autofree (free arg->input_ports newval))
					      (set! arg->input_ports newval)))
	   ...))


(def-class (<Jack_Arg> #:key num_inports input_ports)

  (define c-object (Jack_Arg_new))
  
  (def-method (num_inports . rest)
    (if rest
	(Jack_Arg_set_num_inports c-object (car rest))
	(Jack_Arg_get_num_inports c-object)))
	
  (def-method (input_ports #:key das-make-func #:key (autofree #t) #:rest rest)
    (if (and rest
	     (not (or (eq? #:das-make-func (car rest))
		      (eq? #:autofree (car rest)))))
	(let ((newval (car rest)))
	  (Jack_Arg_set_input_ports c-object (if (list? newval)
						 (ec-make-array newval das-make-func (if autofree 1 0))
						 newval)))
	(Jack_Arg_get_input_ports c-object)))

  (if num_inports
      (-> this num_inports num_inports))
  (if input_ports
      (-> this input_ports input_ports))

  )

!#

#!
(eval-c-add-struct 'testing
		   (map (lambda (a)
			  (if (= 3 (length a))
			      (list (caddr a) (car a) (cadr a))
			      (list (cadr a) (car a))))
			(eval-c-listify-struct '(<jmp_buf> ai))))
(shared-struct-but-only-known-types 'testing)
(map (lambda (def)
       (if (= 3 (length def))
	   (list '<void-*>
		 (car def))
	   (list (eval-c-get-known-type (cadr def))
		 (car def))))
     (eval-c-get-struct 'testing))
(eval-c-get-known-type '<jmp_buf>)

;  int a(void){
;    return 5;
;  }
;  int (*b)(int)=a;
;
;  printf("b(): %d\n",b(3));

(eval-c ""
	
	(run-now
	 (<int> a (lambda ()
		    (return 5)))
	 ((<int> ()) b a)
	 
	 (printf (string "okey %d\\n") (b))))
	
(eval-c ""
	(run-now
	 (for-each 0 5
		   (lambda (i)
		     (printf (string "%d\\n") i)))))

(eval-c ""
	"#include <lrdf.h>"
	(proto->public
	 "int lrdf_export_by_source(const char *src, const char *file);"
	 "lrdf_uris *lrdf_match_multi(lrdf_statement *patterns);"
	 "lrdf_statement *lrdf_matches(lrdf_statement *pattern);"))

 
(define-c <float> (c-scale ((<float> x)
			    (<float> x1)
			    (<float> x2)
			    (<float> y1)
			    (<float> y2))
			   (return (+ y1
				      (/ (* (- x x1)
					    (- y2 y1))
					 (- x2 x1))))))

(define-c <int> (fib ((<int> n))
		     (if (< n 2)
			 (return n)
			 (return (+ (fib (- n 1))
				    (fib (- n 2)))))))


(eval-c ""
	(public (<int> f-ib (lambda ((<int> n))
			       (if (< n 2)
				   (return n)
				   (return (+ (f-ib (- n 1))
					      (f-ib (- n 2)))))))))

	 

(eval-c-to-scm "  static   volatile    int  ")
(eval-c ""
	(public
	 (<int> jepp (lambda ((<int> a)(<int> b))
			      (printf (string "jepp\\n"))
			      (return (+ a b))))))

(eval-c ""
	(public
	 (<int> ratt 81)
	 (<int> jepp5 (lambda ()
		       (return ratt)))))

(eval-c ""
	(run-now (printf (string "jepp1\\n"))
		 (printf (string "jepp2\\n")))

	(run-now (printf (string "jepp3\\n"))
		 (printf (string "jepp4\\n"))))

	 
(eval-c ""
	(run-now
	 (printf (string "hello world!\\n"))))

(eval-c ""
	(<int> jepp (lambda ()
		      (printf (string "jepp\\n"))
		      (return 0))))

(begin "jepp\\n")

(eval-c "-ljack"
	
	"#include <jack/jack.h>"

	(public

	 (<int> a 5)
	 (<int> b 6)

	 (<void> parse_arguments 
		 (lambda ((<int> argc) (<char**> argv))
		   (if (< argc 2)
		       (begin
			 (exit 9)))))
	 
	 (<jack_port_t*> input_port)
	 (<jack_port_t*> output_port)
	 
	 
	 (<int> process (lambda ((<jack_nframes_t> nframes)
				 (<void*> arg))
			  (let* ((out <jack_default_audio_sample_t*> (jack_port_get_buffer output_port nframes))
				 (in <jack_default_audio_sample_t*> (jack_port_get_buffer input_port nframes)))
			    (memcpy out in
				    (* (sizeof <jack_default_audio_sample_t>) nframes)))
			  (return 0)))
	 )
	)


!#


