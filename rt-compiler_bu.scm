#!

rt-compiler.scm
-Kjetil S. Matheussen/Notam, 2005

rt-compiler.scm is developed with support from Notam/Oslo [1]
and the Arts Council Norway [2].

Oops! API might still change. This file is currently under
heavy development.

Jack must be running before loading this file!


For documentation, check out
http://www.notam02.no/arkiv/doc/snd-rt/


*****
Short
*****
rt-compiler provides various functions and macros to compile[3]
and run simple lisp[4] functions.



[1] http://www.notam02.no
[2] http://www.kulturradet.no
[3] I guess "translator" would be a more accurate word to use for 
    what does the thing, rather than "compiler".
[4] Since creating lists are not possible, calling the functions for "lisp functions"
    probably isn't correct. (?)



!#

(provide 'snd-rt-compiler.scm)



(define *tar-atomic-heap-size* (* 4 1024 1024))
(define *tar-nonatomic-heap-size* (* 1 256 1024))
(define *tar-max-mem-size* (* 512 1024))
;;(define *tar-roots-size* (* 1024 1024))



;;(use-modules (srfi srfi-1))

(if (or (not (defined? 'srfi-loaded))
	(not srfi-loaded))
    (use-modules (srfi srfi-1)))
(define srfi-loaded #t)


;;; Implementation detail: map must run its arguments in order. There must be no
;;; difference between map-in-order and map. (stupid map, luckily guile maps in proper order)
;;; In case guile change this nice behaviour of map, do: (define map map-in-order)


(if (not (provided? 'snd-oo.scm)) (load-from-path "oo.scm"))

(c-load-from-path eval-c)

;; Check if jack is loaded.
(if (not (defined? 'MUS_JACK_API))
    (eval-c (string-append "-I" snd-header-files-path)
	    "#include <_sndlib.h>"
	    (proto->public "int mus_audio_api(void)")
	    (variables->public
	     (<int> MUS_JACK_API))))

(if (not (= (mus_audio_api) (MUS_JACK_API)))
    (throw 'jack-not-running))



;; general functions and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-c  (string-append "-I" snd-header-files-path)
	 "#include <clm.h>"
	 "#include <xen.h>"
	 "#include <clm2xen.h>"
	 "#include <vct.h>"

	 "extern mus_any *g_get_mus_any(XEN os);"
	 (<SCM> rt_set_float (lambda ((<SCM> das_float) (<SCM> newval))
			       (set! (SCM_REAL_VALUE das_float) (GET_FLOAT newval))
			       (return SCM_UNDEFINED)))
	 (<SCM> gakk (lambda ((<SCM> scm))
		       (return (MAKE_POINTER (XEN_TO_MUS_ANY scm)))))
		       ;;(return (MAKE_POINTER (g_get_mus_any scm)))))
		       ;;(printf (string "ai: %d %d %d %d\\n") (cast <int> scm) (XEN_TO_MUS_XEN scm) (XEN_OBJECT_REF scm) (g_get_mus_any scm))
		       ;;(return SCM_UNDEFINED)))
	 ;;		       (return (MAKE_POINTER (XEN_TO_MUS_ANY scm)))))
	 (<SCM> gakk15 (lambda ((<SCM> scm))
			 (return (MAKE_POINTER (SCM_SMOB_DATA scm)))))
	 (<SCM> gakk2 (lambda ((<SCM> sym) (<SCM> toplevel))
			(return (scm_sym2var sym toplevel SCM_BOOL_F))))
	 (<SCM> gakk3 (lambda ((<SCM> scm))
			(return (MAKE_POINTER (XEN_TO_VCT scm)))))
	 (run-now
	  (scm_c_define_gsubr (string "rt-set-float!") 2 0 0 rt_set_float)
	  (scm_c_define_gsubr (string "XEN_TO_MUS_ANY") 1 0 0 gakk)
	  (scm_c_define_gsubr (string "SCM_SMOB_DATA") 1 0 0 gakk15)
	  (scm_c_define_gsubr (string "XEN_TO_VCT") 1 0 0 gakk3)
	  (scm_c_define_gsubr (string "c-global-symbol") 2 0 0 gakk2)))



;; Get the address of a guile SCM variable.
(define-macro (c-get-cell-address varname)
  (let ((findfunc (string->symbol (eval-c-get-unique-name))))
    `(let ((,findfunc (procedure->macro (lambda (x env)
					  (define (findit env)
					    (call-with-current-continuation
					     (lambda (return)
					       ;;(c-display "env" env)
					       (cond ((not (list? env)) (return #f))
						     ((= 1 (length env)) (return (let ((varname2 ,varname))
										   `(c-global-symbol ,varname2 ,(car env)))))
						     ;;((= 1 (length env)) (return `(c-global-symbol 'ai2 (standard-interface-eval-closure (current-module)))))
						     ((= 1 (length env)) (return (car env)))
						     (else
						      (let ((names (car (car env)))
							    (vals (cdr (car env))))
							(if (not (list? names))
							    (if (eq? ,varname names)
								(return vals))
							    (for-each (lambda (name val)
									(if (eq? ,varname name)
									    (return val)))
								      names
								      vals)))
						      (findit (cdr env)))))))
					  (findit env)))))
       (,findfunc))))
;;(c-display (,findfunc))




					     
#!
(define ai2 90.0)
(let ((ai9 50))
  (c-get-cell-address ai9))
(c-global-symbol 'ai2 (interaction-environment))
(c-global-symbol 'ai2 (standard-interface-eval-closure (current-module)))
(c-global-symbol 'ai2 (standard-interface-eval-closure (current-module)))
!#








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Start the engine ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-load-from-path rt-engine)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (defined? '*use-alsa-midi*))
    (define-toplevel '*use-alsa-midi* #t))

(if (not (defined? '*rt-midi-alsaname*))
    (define-toplevel '*rt-midi-alsaname* "rt-midi"))

(define rt-defining-macros-clears-cache #t)
(define rt-verbose #f)
(define rt-very-verbose #f)

(define rt-operators '(+ - * / = < > <= >=))

(define rt-unknown-ret-type-ops '(+ rt--/- rt--/minusoneargument * rt-min/MIN rt-max/MAX 1+ 1-))

(define rt-very-special-forms '(if begin let*))
;;(define rt-very-special-forms '(if begin let* rt-while/while))
(define rt-very-special-forms2 (cons 'lambda rt-very-special-forms))

(define rt-macro-prefix 'rt-macro-)

(define rt-globalstructname '<struct-RT_Globals>)
(define rt-globalvarname 'rt_globals)
(define rt-globalvardecl (list rt-globalstructname (symbol-append '* rt-globalvarname)))


;;; Various functions

(define (rt-symbol-starts-with? sym s)
  (let* ((das-string1 (symbol->string sym))
	 (das-string2 (symbol->string s))
	 (len (min (string-length das-string1) (string-length das-string2))))
    (string= das-string1 das-string2 0 len 0 len)))

#!
(symbol-starts-with? 'abwe4wei 'aiai)
!#

(define rt-safety
  (let ((safety 1))
    (make-procedure-with-setter
     (lambda ()
       safety)
     (lambda (n)
       (set! safety n)))))

(define (rt-is-safety?)
  (not (= 0 (rt-safety))))
  
(define (rt-print . rest)
  (if rt-verbose
      (apply c-display rest)))

(define (rt-print2 . rest)
  (if rt-very-verbose
      (apply c-display rest)))

(define rt-gensym-num 0)
(define (rt-gensym-reset)
  (set! rt-gensym-num 0))
(define* (rt-gensym :optional (name ""))
  (set! rt-gensym-num (1+ rt-gensym-num))
  (string->symbol (<-> "rt_gen_" name (number->string rt-gensym-num))))
(define rt-gensym-num2 0)
(define (rt-gensym2)
  (set! rt-gensym-num2 (1+ rt-gensym-num2))
  (string->symbol (<-> "rt_genb" (number->string rt-gensym-num2))))

;; An immediate is something that is not a function-call.

(define rt-immediates (make-hash-table 219))
(define (rt-add-immediate funcname)
  (hashq-set! rt-immediates funcname #t))

(define (rt-immediate? . rest)
  (if (null? rest)
      #t
      (and (or (not (list? (car rest)))
	       (and (not (null? (car rest))) ;; In case, there is most probably an error, but I'm not sure.
		    (hashq-ref rt-immediates (car (car rest)))))
	   (apply rt-immediate? (cdr rest)))))

(rt-add-immediate 'is-type?)
		  

;; Dummy, redefined later.
(define (rt-clear-cache!)
  #t)


;; Need pi.	
(if (not (defined? 'pi))	
    (define-toplevel 'pi 3.14159265358979))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Conversion "scheme"->eval-c ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (rt-is-number? type)
  (or (eq? type '<float>)
      (eq? type '<int>)
      (eq? type '<double>)
      (and (symbol? type)
	   (let ((type (eval-c-get-known-type type)))
	     (or (eq? type '<float>)
		 (eq? type '<int>)
		 (eq? type '<double>))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Hacks, last step before code is completely evalycified.
;; 
;; -Change the order of type and variable name for lambda-args. eval-c requires the opposite order of rt:
;;  (lambda ((a <int>)(b <float>))...) -> (lambda ((<int> a)(<float> b))...)
;; -Remove variable names for function types:
;;  (let* ((b (<int> ((c <float>))) 0))...) -> (let* ((b (<int> (<float>))))...)
;; -Surround strings with the eval-c macro "string".
;;  "gakk" -> (string "gakk")
;; -Remove all rt-dummy/dummy calls completely.
;; -Replace all '<undefined> types with <SCM>s, but give warnings.
;; -Replace all rt type-names with their C-type names.
;; -Insert <struct-RT_Globals> *rt_globals as the first argument for all functions.
;; -Insert rt_globals as the first argument in function-calls where required.
;; -Transform global variable names: gakk -> rt_globals->gakk
;;
(define (rt-last-hacks term)

  (define (make-proper-type t name) ;; Name is only used for warning-message.
    (cond ((eq? '<undefined> t)
	   (begin
	     (c-display "\n\n\nWarning, had to set undefined variable name \"" name "\" to SCM:" term ".\n\n\n")
	     '<SCM>))
	  ((list? t)
	   (list (make-proper-type (car t) name)
		 (cons '<struct-RT_Globals-*> (map (lambda (t2)
						     (make-proper-type (cadr t2) name))
						   (cadr t)))))
	  (else
           (let ()
             (define type (hashq-ref rt-types t))
             (if (not type)
                 (error "Unknown type" t))
             (-> type c-type)))))
    
  (define globals (cadr term))
			  
  ;;(define localfuncs '())
  
  (define (last-hacks term)
    (define (map2 term)
      (delete '(rt-dummy/dummy) (map last-hacks term)))

    (cond ((string? term) `(string ,term))
	  ((symbol? term)
	   (if (assq term globals)
	       (symbol-append 'rt_globals-> term)
	       term))
	  ((not (list? term)) term)
	  ((null? term) term)
	  ((eq? 'let* (car term))
	   (let ((vardecls (map (lambda (vardecl)
				  (if (and (= 3 (length vardecl))
					   (list? (caddr vardecl)))    ;; Ie. a lambda-funcion.
				      (begin
					;;(set! localfuncs (cons (car vardecl) localfuncs))
					(list (car vardecl)
					      (make-proper-type (cadr vardecl) (car vardecl))
					      (last-hacks (caddr vardecl))))
				      (if (and (= 3 (length vardecl))
					       (not (number? (caddr vardecl))))
					  (list (car vardecl)
						(make-proper-type (cadr vardecl) (car vardecl))
						(last-hacks (caddr vardecl)))
					  (list (car vardecl)
						(make-proper-type (cadr vardecl) (car vardecl))))))
				(cadr term))))
	     `(let* ,vardecls
		,@(map last-hacks (cddr term)))))
	  
	  ((or (eq? 'lambda (car term))
	       (eq? 'lambda-decl (car term)))
	   (let ((vardecl (map (lambda (t)
				 (list (make-proper-type (cadr t) (car t))
				       (car t)))
			       (cadr term))))
	     (push! rt-globalvardecl vardecl)
	     (if (eq? 'lambda (car term))
		  `(lambda ,vardecl
		     ,@(map2 (cddr term)))
		  `(lambda-decl ,vardecl))))
	  
	  ((or (eq? 'rt-if (car term))
	       (eq? 'return (car term))
	       (eq? 'while (car term))
	       (eq? 'begin (car term))
	       (eq? 'the (car term))
	       (eq? 'rt-begin (car term))
	       (eq? 'if (car term)))
	   (map2 term))
	       
	  ;((member (car term) localfuncs)
;	   (map last-hacks
;		(append (list (car term) 'rt_globals) (cdr term))))

	  (else
	   (let ((func (hashq-ref rt-funcs (car term))))
	     (if (or (not func)
		     (-> func needs-rt-globals))
		 (map last-hacks
		      (append (list (car term) 'rt_globals) (cdr term)))
		 (map2 term))))))
    
  ;;(c-display "term" term)
  ;;(c-display "caddr term" (cadr (caddr term)))
  (for-each (lambda (vardecl)
	      (if (or (= 2 (length vardecl))
		      (not (list? (caddr vardecl))))
		  (push! vardecl globals)))
	    (cadr (caddr term)))

   (last-hacks term))

#!
(rt-last-hacks '(lambda ((freq__1 <float>))
		  (let* ()
		    (return (* freq__1 (rt-/// 6.28318530717959 (mus-srate)))))))
	       
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert return calls and fix begin and if blocks
;;
;;

(define (rt-insert-returns term returntype)

  (call-with-current-continuation
   (lambda (return)
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-insert-returns:" args))
       (return #f))
     

     (define (insert term returntype)
       (define (default-behaviour)
	 (if (eq? '<void> returntype)
	     term
	     `(return ,term)))

       (cond ((not (list? term)) (default-behaviour))
	     ((null? term) (default-behaviour))
	     
	     ;; Can not happen
	     ((eq? 'lambda-decl (car term))
	      (check-failed "Somethings wrong"))
	     ;;term)

	     ((eq? 'lambda (car term))
	      (let ((body (cddr term)))
		`(lambda ,(cadr term)
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (c-butlast body))
		   ,(insert (last body) returntype))))

	     ((or (eq? 'begin (car term))
		  (eq? 'rt-begin (car term)))
	      (let ((body (cdr term)))
		`(begin
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (c-butlast body))
		   ,(insert (last body) returntype))))

	     ((eq? 'let* (car term))
	      (let* ((body (cddr term))
		     (vardecls (map (lambda (vardecl)
				      (if (and (= 3 (length vardecl))
					       (list? (caddr vardecl))
					       (eq? 'lambda (car (caddr vardecl))))
					  `(,(car vardecl) ,(cadr vardecl) ,(insert (caddr vardecl) (cadr vardecl)))
					  (list-copy vardecl)))
				    (cadr term))))
		`(let* ,vardecls
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (c-butlast body))
		   ,(insert (last body) returntype))))

	     ;; ifs are translated to rt-if (a?b:c) by the if-macro, but normal ifs can still be generated in the compiling-processs.
	     ((eq? 'if (car term))
	      (if (= 4 (length term))
		  `(if ,(cadr term)
		       ,(insert (caddr term) returntype)
		       ,(insert (cadddr term) returntype))
		  (if (eq? '<void> returntype)
		      `(if ,(cadr term)
			   ,(insert (caddr term) returntype))
		      (begin
			(c-display "Undefined return value in some function because of missing else-block in non-void-returning if sentence.")
			(check-failed "The error is not supposed to show up here. Please send your code to k.s.matheussen@notam02.no ")))))
	     
	     ((and (eq? 'rt-if (car term))
		   (eq? '<void> returntype))
	      (insert `(if ,@(cdr term)) returntype))


	     ((eq? 'begin (car term))
	      (let ((body (cdr term)))
		`(begin
		   ,@(map (lambda (t)
			    (insert t '<void>))
			  (body)))))
	     
	     ;;((eq? 'rt-begin (car term))
	     ;;((and (eq? 'rt-begin (car term))
	;;	   (eq? '<void> returntype))
	 ;;     (c-display "jepp")
	  ;;    (insert `(begin ,@(cdr term)) returntype))
	     
	     (else
	      (default-behaviour))))
     
     (insert term returntype))))

#!

(rt-insert-returns '(lambda ((v_u2 <vct-*>))
		      (let* ()
			(let* ((rt_gen674_u1 <int> 0)
			       (rt_gen675_u3 <int> 0)
			       (i_u4 <int> 0)
			       (_rt_breakcontsig_u5 <jmp_buf> 0))
			  (rt-begin
			   (set! rt_gen674_u1 (rt-vct-length/vct-length v_u2))
			   (set! rt_gen675_u3 (rt-if (rt-> rt_gen674_u1 0)
						     1
						     -1))
			   (set! i_u4 0)
			   (rt-dummy/dummy)
			   (rt-begin
			    (rt-while (not (rt-= i_u4 rt_gen674_u1))
				      (rt-vct-set!/vct-set! v_u2 i_u4 (* (rt-vct-ref/vct-ref v_u2 i_u4) 2))
				      (set! i_u4 (+ i_u4 rt_gen675_u3))))))))
		   '<void>)

(rt-insert-returns '(lambda ()
		      (rt-if (rt-= 0 b)
			     3
			     4))
		   '<int>)
(rt-insert-returns '(lambda ()
		      (+ 2 35))
		   '<int>)

(rt-insert-returns '(lambda ()
		      (let* ((a <void> (lambda ((<int> b))
					2
					(set! a 9))))
			(set! a 9)))
		   '<void>)


(rt-insert-returns '(lambda ((_rt_local_f <int>))
		      (let* ((rt_gen300 <int> 0)
			     (c <int>)
			     (a <int> 0)
			     (f <int>)
			     (d <int> (lambda ()
					(+ c f 2)))
			     (b <int> (lambda-decl ((c <int>))))
			     (rt_gen298 <int> (lambda ((_rt_local_c <int>))
						(set! c _rt_local_c)
						(rt-begin
						 (set! a (d)))))
			     (b <int> (lambda ((_rt_local_c <int>))
					(if rt_gen300
					    (let* ((rt_gen299 <int> c)
						   (_rt_ret <int> (rt_gen298 _rt_local_c)))
					      (set! <int> rt_gen299)
					      _rt_ret)
					    (let* ((_rt_ret <int>))
					      (set! rt_gen300 1)
					      (set! _rt_ret (rt_gen298 _rt_local_c))
					      (set! rt_gen300 0)
					      _rt_ret)))))
			(set! f _rt_local_f)
			(rt-begin
			 (set! a 3)
			 (b e))))
		   '<int>)

!#
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-lambda-lifter lifts up all lambda-functions so they
;; are not inside another lambda function anymore.
;;
;; This is strictly
;; not necesarry because gcc support local (nested) functions, but it
;; speeds up the access to local functions a lot. For example,
;; I think mus_src can run 2-3 times as fast...
;;
;; The function puts all local functions and variables that need to be global into the top-level.
;;
;; (lambda ()
;;  (let* ((a <int> 0)
;;	   (b <int> (lambda ((c <int>))
;;	              (let* ((d <int> (lambda ()
;;		                       (+ c 2)))))
;;		         (set! a (d))))
;;         (e <int> 0))
;;    (set! a 3)
;;    (set! e 9)
;;    (b e)))
;;
;; ->
;;
;;(lambda ()
;;    (let* ((a <int> 0)
;;           (c <int> 0)
;;           (d <int> (lambda ()
;;		        (+ c 2)))
;;	     (b <int> (lambda ((_rt_local_c <int>))
;;		        (set! c _rt_local_c)
;;		        (set! a (d)))))
;;      (let* ((e <int> 0))
;;	  (set! a 3)
;;	  (set! e 9)
;;	  (b e))))
;;
;; Explanation for the above transformation:
;;;; The first let*-block contains global variables and functions. The rest is the main-function.
;;;; (note that all global variables are put into its own struct later, so they are not really global variables though.)
;;;; (note (again) that because the rt-language needs to support callbacks from c-code, it can't do the traditional lambda-lifting, (note to myself, insert references ([])),
;;;;  (by adding extra arguments to the inner function when necessary), but the speed-penalty is probably very low because of this.
;;;;  Supporting both traditional lambda-lifting and the variant above is possible though (by letting the variant below call the traditional version),
;;;;  but that has not been implemented yet. I also
;;;;  need to do some benchmarks to see if traditional lambda-lifting really is faster. Perhaps it isn't...)
;;
;;
;; rt-remove-unused++ must have been called on the term before calling. (So that the unused variables can be catched more easely, I think.)
;;

(define (rt-lambda-lifter term)

  (define globals '())   ;; Just the names: (varname1 varname2)
  (define globals2 '())  ;; With type: ((varname1 <int>)(varname2 <float))
  (define globalfuncs '())

  (define (add-global var)
    (if (not (memq var globals))
	(push! var globals)))
  
  (define (find-globals varlist term)
    (cond ((symbol? term)
	   (if (not (memq term varlist))
	       (add-global term)))
	  
	  ((not (list? term)) #t)
	  ((null? term) #t)

	  ((eq? 'lambda (car term))
	   (let ((newvarlist (map car (cadr term))))
	     (for-each (lambda (t)
			 (find-globals newvarlist t))
		       (cddr term))))
	   
	  ((eq? 'let* (car term))
	   (let* ((newvarlist varlist))
	     (for-each (lambda (vardecl)
			 (if (and (= 3 (length vardecl))
				  (list? (caddr vardecl)))
			     (find-globals '() (caddr vardecl))
			     (push! (car vardecl) newvarlist)))
		       (cadr term))
	     (for-each (lambda (t)
			 (find-globals newvarlist t))
		       (cddr term))))

	  ((eq? 'is-type? (car term))
	   #t)
	  
	  (else
	   (for-each (lambda (t)
		       (find-globals varlist t))
		     (cdr term)))))

  (define* (lambdalifter name type term #:optional dontcapsulate)
    (let* ((globsets '())      ;; Bunch of set!-commands.
	   (globvars-here '()) ;; Global variables handled in this spesific lambda function.
	   (vars (map (lambda (var)
			(if (and (not dontcapsulate)
				 (memq (car var) globals))
			    (let ((tempname (symbol-append '_rt_local_ (car var))))
			      (push! `(set! ,(car var) ,tempname) globsets)
			      (push! var globals2)
			      (push! var globvars-here)
			      (list tempname (cadr var)))
			    var))
		      (cadr term)))

	   (das-func #f)
	   
	   (body (map lifter (cddr term)))

	   (is-first-let*? (and (list? (car body))
				(eq? 'let* (car (car body))))))
      
      
      ;;(if (not (list? (car body)))
      ;;	  (set! body `(rt-begin
      ;;		       ,@body)))
      
      ;; Find any global variables defined in the let*-block in the function.
      (if is-first-let*?
	  (for-each (lambda (var)
		      (if (memq (car var) globals)
			  (push! var globvars-here)))
		    (cadr (car body))))

      (set! das-func (if is-first-let*?
			 `(lambda ,vars
			    (let* ,(cadr (car body))
			      ,@(reverse! globsets)
			      ,@(cddr (car body))))
			 `(lambda ,vars
			    ,@(reverse! globsets)
			    ,@body)))

      ;;(c-display "lambdalifter globs/term" globvars-here term)
      ;;(c-display "das-func" das-func)
      ;;(c-display dontcapsulate)
      ;;(newline)

      (if (or (null? globvars-here)
	      dontcapsulate) ;; For some reason, we know that its safe not to put global accessible local variables on the stack. (Ie. its the main function. :-) )
	  (begin
	    das-func)
	  (let* ((realfuncname (rt-gensym))
		 (letbody (map (lambda (name)
				 (cons (rt-gensym) (reverse name)))
			       globvars-here))
		 (funccall `(,realfuncname ,@(map car vars)))
		 (capspart #f))
			     
	    (push! `(,name ,type (lambda-decl ,(cadr term)))
		   globalfuncs)
	    (push! `(,realfuncname ,type ,das-func)
		   globalfuncs)

	    
	    (set! capspart `(let* ,(if (eq? '<void> type)
				       letbody
				       (append letbody
					       (list (list '_rt_ret type funccall))))
			      ,@(if (eq? '<void> type)
				    (cons funccall
					  (map (lambda (letb)
						 `(set! ,(caddr letb) ,(car letb)))
					       letbody))
				    (append (map (lambda (letb)
						   `(set! ,(caddr letb) ,(car letb)))
						 letbody)
					    (list '_rt_ret)))))

	    
	    (if (< (length globvars-here) 5)  ;; Performance is dependent on this number. Perhaps 5 is a good value. Haven't done any benchmark.
	 	`(lambda ,vars                ;; It should be set quite high, because of larger amount of code, and branching, in the second version.
		   ,capspart)                 ;; On the other hand, stacking up these variables are probably usually unnecesarry (but has to be done),
		(let ((recnum (rt-gensym)))   ;; so the program counter usually jumps into the second version. Hard to say whats best...
		  (push! (list recnum '<int> 0) globals2)
		  `(lambda ,vars
		     (if ,recnum
			 ,capspart
			 ,(if (eq? '<void> type)
			      `(begin
				 (set! ,recnum 1)
				 ,funccall
				 (set! ,recnum 0))
			      `(let* ((_rt_ret ,type))
				 (set! ,recnum 1)
				 (set! _rt_ret ,funccall)
				 (set! ,recnum 0)
				 _rt_ret))))))))))
			    
			       
		 
  
  (define (lifter term)
    (cond ((not (list? term)) term)
	  ((null? term) term)

	  ((eq? 'let* (car term))
	   (let* ((vardecls '()))
	     (for-each (lambda (var)
			 (if (and (= 3 (length var))
				  (list? (caddr var)))
			     (if (eq? 'lambda-decl (car (caddr var)))
				 (push! var globalfuncs)
				 (push! (list (car var) (cadr var) (apply lambdalifter var))
					globalfuncs))
			     (if (memq (car var) globals)
				 (push! var globals2)
				 (push! var vardecls))))
		       (cadr term))
	     (set! vardecls (reverse! vardecls))
	     (if (null? vardecls)
		 `(rt-begin
		    ,@(cddr term))
		 `(let* ,vardecls
		    ,@(cddr term)))))

	  (else
	   (map lifter term))))


  (find-globals '() term)

  (let ((res (lambdalifter 'nameignored '<doesntmatter> term #t)))
    `(lambda ,(cadr res)
       (let* ,(append globals2 (reverse! globalfuncs))
	 ,@(cddr res)))))


#!

(rt-lambda-lifter '(lambda ((src5 <float>))
		     (let* ((func <float> (lambda ()
					    (+ 2 src5))))
		       func)))


(rt-lambda-lifter '(lambda ((f <int>))
		     (let* ((a <int> 0)
			    (b <int> 0)
			    (c <int> (lambda ()
				       (+ f b))))
		       (set! b 9)
		       (c))))

(rt-lambda-lifter '(lambda ((f <int>))
		     (let* ((a <int> 0)
			    (b <int> (lambda ((c <int>))
				       (let* ((d <int> (lambda ()
							 (+ c f 2))))
					 (set! a (d)))))
			    ;;(e <int> 0)
			    )
		       (set! a 3)
		       ;;(set! e 9)
		       (b e))))
->
(lambda ((_rt_local_f <int>))
  (let* ((rt_gen300 <int> 0)
	 (c <int>)
	 (a <int> 0)
	 (f <int>)
	 (d <int> (lambda ()
		    (+ c f 2)))
	 (b <int> (lambda-decl ((c <int>))))
	 (rt_gen298 <int> (lambda ((_rt_local_c <int>))
			    (set! c _rt_local_c)
			    (rt-begin
			     (set! a (d)))))
	 (b <int> (lambda ((_rt_local_c <int>))
		    (if rt_gen300
			(let* ((rt_gen299 <int> c)
			       (_rt_ret <int> (rt_gen298 _rt_local_c)))
			  (set! <int> rt_gen299)
			  _rt_ret)
			(let* ((_rt_ret <int>))
			  (set! rt_gen300 1)
			  (set! _rt_ret (rt_gen298 _rt_local_c))
			  (set! rt_gen300 0)
			  _rt_ret)))))
    (set! f _rt_local_f)
    (rt-begin
     (set! a 3)
     (b e))))


(lambda ((_rt_local_f <int>))
  (let* ((c <int>)
	 (a <int> 0)
	 (f <int>)
	 (d <int> (lambda ()
		    (+ c f 2)))
	 (b <int> (lambda ((_rt_local_c <int>))
		    (set! c _rt_local_c)
		    (rt-begin
		     (set! a (d))))))
    (set! f _rt_local_f)
    (let* ((e <int> 0))
      (set! a 3)
      (set! e 9)
      (b e))))

(lambda () (let* ((b_u3 <float>)
		  (a_u1 <float> (lambda-decl ()))
		  (c_u2 <float> (lambda-decl ((b_u3 <float>))))
		  (inner_u4 <void> (lambda ((n_u5 <int>))
				     (set! b_u3 n_u5)))
		  (c_u2 <float> (lambda-decl ((b_u3 <float>))))
		  (rt_gen350 <float> (lambda ((_rt_local_b_u3 <float>))
				       (set! b_u3 _rt_local_b_u3)
				       (rt-begin
					(rt-begin
					 (rt-if (rt-=/== 0 b_u3)
						(c_u2 1)
						(inner_u4 2))
					 b_u3))))
		  (c_u2 <float> (lambda ((_rt_local_b_u3 <float>))
				  (let* ((rt_gen351 <float> b_u3)
					 (_rt_ret <float> (rt_gen350 _rt_local_b_u3)))
				    (set! <float> rt_gen351)
				    _rt_ret)))
		  (a_u1 <float> (lambda ()
				  (rt-begin
				   (rt-begin
				    (c_u2 0))))))
	     (rt-begin
	      (rt-begin
	       (a_u1)))))


!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-remove-unused++ removes unused variables and numbers.
;;
;; + evaluate is-type?, returning either 0 or 1.
;; 
;; rt-check-calls must have been called on the term before calling
;;
;; (begin 2 4) -> (begin 4)
;; (if (not 0) some thing) -> some
;; (if 0 some thing) -> thing
;; 
(define (rt-remove-unused++ term)

  (call-with-current-continuation
   (lambda (return)
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-remove-unused++:" args))
       (return #f))
     
     (define vars (make-hash-table 219))
     
     (define (das-remove t)
       (cond ((null? t) t)
	     ((= (length t) 1) (list (remove++ (car t))))
	     ((not (list? (car t))) ;; Removed here.
	      (das-remove (cdr t)))
	     (else
	      (cons (remove++ (car t))
		    (das-remove (cdr t))))))
     
     (define (remove++ term)
       (rt-print2 "remove++" term)
       (cond ((not (list? term)) term)
	     ((null? term) term)

	     ;; begin
	     ((eq? 'rt-begin (car term))
	      `(rt-begin ,@(das-remove (cdr term))))

	     ;; is-type?
	     ((eq? 'is-type? (car term))
	      (if (list? (caddr term))
		  (let ((func (hashq-ref rt-funcs (car (caddr term)))))
		    (if (not func)
			(check-failed (caddr term) "not found in term (1)" term)
			(if (eq? (cadr term) (-> func return-type))
			    1
			    0)))
		  (let ((type (hashq-ref vars (caddr term))))
		    (if (not type)
			(check-failed (caddr term) "not found in term (2)" term)
			(if (eq? type (cadr term))
			    1
			    0)))))

	     ;; let*
	     ((eq? 'let* (car term))
	      (for-each (lambda (var)
			  (if (and (= 3 (length var))
				   (list? (caddr var))
				   (or (eq? 'lambda (car (caddr var)))
				       (eq? 'lambda-decl (car (caddr var)))))
			      (hashq-set! vars (car var) (list (cadr var)
							       (map car (cadr (caddr var)))))
			      (hashq-set! vars (car var) (cadr var))))
			(cadr term))
	      (let ((vardecl (map (lambda (var)
				    (list (car var) (cadr var) (remove++ (caddr var))))
				  (cadr term))))
		`(let* ,vardecl
		   ,@(das-remove (cddr term)))))

	     ;; not
	     ((eq? 'not (car term))
	      (let ((a (remove++ (cadr term))))  ;; This can be a is-type? test.
		(if (number? a)
		    (if (= 0 a)
			1
			0)
		    `(not ,a))))

	     ;; if
	     ((eq? 'rt-if (car term))
	      (let ((a (remove++ (cadr term)))) ;; This can be a is-type? test.
		(if (number? a)
		    (if (= 0 a)
			(remove++ (cadddr term))
			(remove++ (caddr term)))
		    `(rt-if ,a
			    ,(remove++ (caddr term))
			    ,(remove++ (cadddr term))))))
	     
	     ;; lambda-decl
	     ((eq? 'lambda-decl (car term))
	      (for-each (lambda (var)
			  (hashq-set! vars (cadr var) (car var)))
			(cadr term))
	      term)

	     ;; lambda
	     ((eq? 'lambda (car term))
	      (for-each (lambda (var)
			  (hashq-set! vars (car var) (cadr var)))
			(cadr term))
	      `(lambda ,(cadr term)
		 ,@(das-remove (cddr term))))

	     (else
	      (map remove++ term))))

     (remove++ term))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-replace-define-with-letrecs replaces all defines with
;; letrecs. (function copied from snd-hobbit.scm (and sligthly modified))
;;
(define (rt-replace-define-with-letrecs term)

  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (apply c-display (cons "rt-compiler.scm/rt-replace-define-with-letrecs:" args))
       (return #f))
     
     (define (replace term)
       (cond 
	((not (list? term)) term)
	((null? term) term)
	
	(else
	 (map replace
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
						      (map (lambda (t)
								      (if (list? (cadr t))
									  (list (car (cadr t)) `(lambda ,(cdadr t)
													   ,@(cddr t)))
									  (list (cadr t) (caddr t))))
								    defines))
						afterdefines))))))))))))

     (replace term))))


#!
(rt-replace-define-with-letrecs '(lambda ()
				   (define (a b)
				     c)
				   (a)))

(rt-replace-define-with-letrecs '(begin
				 (+ 1 2)
				 (define a 3)
				 (- 4 5)
				 (define b (lambda ()
					     (define d 8)
					     d))
				 (define c 7)
				 (* a (b) c)
				 ))
(begin
  (+ 1 2)
  (letrec ((a <float> 3))
    (- 4 5)
    (letrec ((b <float> (lambda ()
			  (letrec ((d <float> 8))
			    d)))
	     (c <float> 7))
      (* a (b) c))))

!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-let*-lifter does:
;; -Lift all let* variable declarations (and non-named lambdas)
;;  up to the closest lambda().
;;
;; Example:
;;
;; (lambda ()
;;   (+ 2 (let* ((a 9))
;;           a)
;;      5)))
;; -> (lambda ()
;;      (let* ((a 0))
;;         (+ 2 (begin
;;                 (set! a 9)
;;                 a)
;;            5)))
;;
;; This is much better, because begin can be used inside function-calls, let* can't:
;;   scheme: (begin (set! a 9) a)
;;        c: (a=9,a).
;;
;; (However, in many situations, where the let*-lifting
;;  hadn't been necesarry, I guess there can be a minor speed-penalty by
;;  clustering all variable-declarations at the top of each lambda-block.
;;  But I'm not so sure its a very big point to identify those situations...)
;;
;; -rt-fix-various must have been called on the term before calling.
;;  (The variable-names need to be unique to avoid name-clash)


(define (rt-let*-lifter term)
  (define (lifter term)
    (cond ((null? term) (values '() term))
	  ((not (list? term)) (values '() term))
	  
	  ((eq? 'lambda (car term))
	   (call-with-values (lambda ()
			       (lifter (cddr term)))
	     (lambda (letlist new-term)
	       (let ((form (if (not (null? letlist))
			       `(lambda ,(cadr term)
				  (let* ,letlist
				    ,@new-term))
			       `(lambda ,(cadr term)
				  ,@new-term))))
		 (values '()
			 form)))))
	  
	  ((eq? 'let* (car term))
	   (let ((lets (cadr term))
		 (values-letlist '()))
	     ;; Run lifter for the values (b's in ((a <int> b)))
	     (for-each (lambda (l)
			 (call-with-values (lambda ()
					     (lifter (cadr l)))
			   (lambda (letlist term)
			     (set! values-letlist (append! values-letlist letlist))
			     (set-car! (cdr l) term))))
		       lets)
	     ;; Run lifter for let-*body
	     (call-with-values (lambda ()
				 (lifter (cddr term)))
	       (lambda (letlist term)
		 (values (append (map (lambda (l)
					(if (and (list? (cadr l))
						 (or (eq? 'lambda (car (cadr l)))
						     (eq? 'lambda-decl (car (cadr l)))))
					    l
					    (list (car l) (cond ((rt-symbol-starts-with? '_rt_breakcontsig (car l))
								 '<jmp_buf>)
								
								((and (number? (cadr l))    ;; Quick way to determine type. Type doesn't need to be determined here, but it
								      (exact? (cadr l)))    ;; can speed up things if its possible to know type already at this stage.
								 '<int>)                    ;; The type does not need to be correct either, but a number must be
								                            ;; a number, etc.
								((number? (cadr l))
								 '<float>)
								((string? (cadr l))
								 '<char-*>)
								((not (list? (cadr l)))
								 '<undefined>)
								((memq (car (cadr l)) rt-unknown-ret-type-ops)
								 '<undefined>)
								(else
								 (let ((func (hashq-ref rt-funcs (car (cadr l)))))
								   (if func
								       (-> func return-type)
								       '<undefined>)))))))
				      
				      lets)
				 values-letlist
				 letlist)
			 `(rt-begin ,@(map (lambda (l)
					     `(rt-set*! ,(car l) ,(cadr l)))
					   (remove (lambda (l)
						     (or (and (list? (cadr l))
							      (or (eq? 'lambda (car (cadr l)))
								  (eq? 'lambda-decl (car (cadr l)))))
							 (rt-symbol-starts-with? '_rt_breakcontsig (car l))))
						   lets))
				    ,@term))))))
	  (else
	   (let ((new-letlist '())
		 (new-term '()))
	     (for-each (lambda (t)
			 (call-with-values (lambda ()
					     (lifter t))
			   (lambda (letlist term)
			     (set! new-letlist (append! new-letlist letlist))
			     (set! new-term (cons term new-term)))))
		       term)
	     (values new-letlist
		     (reverse! new-term))))))

  (call-with-values (lambda ()
		      (lifter term))
    (lambda (letlist term)
      term)))

#!
(rt-let*-lifter '(lambda ()
		   (gakk (+ 2 3) (lambda (a)
				   (+ a 9)))))
(rt-let*-lifter '(lambda ()
		   (+ 2 (let* ((a (let* ((b 10))
				    b)))
			  a))
		   5))
!#


					    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S-expression transformer (for rt-macros)

(define rt-transformers '())
(define (add-rt-transformer-do ev func)
  (push! (cons func ev) rt-transformers))
(define (remove-rt-transformer func)
  (set! rt-transformers (remove (lambda (transformer)
				  (equal? func ((cdr transformer))))
				rt-transformers)))

(define-macro (add-rt-transformer func)
  (define expr (gensym))
  (define ev (gensym))
  (if (pair? func)
      `(let ((,ev ,func))
	 (add-rt-transformer-do (lambda ()
				  ,ev)
				(lambda (,expr)
				  (,ev ,expr))))
      `(add-rt-transformer-do (lambda ()
				,func)
			      (lambda (,expr)
				(,func ,expr)))))
  

(define (rt-transform expr)
  (call-with-current-continuation
   (lambda (return)
     (for-each (lambda (transformer)
		 (let ((res (transformer expr)))
		   (if (not (equal? expr res))
		       (return (rt-transform res))
		       (set! expr res))))
	       (map car rt-transformers))
     expr)))



#|		       
(define (reverse-term term)
  (if (and (pair? term)
	   (number? (car term))
	   (not (= 5 (car term)))
	   (not (number? (last term))))
      (reverse term)
      term))

(add-rt-transformer reverse-term)
(rt-macroexpand '(6 23 aiai a))
(rt-macroexpand '(5 23 aiai a))
(remove-rt-transformer reverse-term)
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-macroexpand . First thing to do.
;;
;; + setter handling
;; (set! (asetter 5) 2)     -> (setter!-asetter 5 2)
;;
;; + infix handler:
;; (+ 2:-ms a:-s b :-s (c):-ms) -> (+ (infix-ms 2) (infix-s a) (infix-s b) (infix-ms (c)))
;;
(define (rt-macroexpand-1 term)
  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-macroexpand-1:" args))
       (return #f))
     
     (define (expand term)
       (cond ((null? term) term)
	     ((not (list? term)) term)
	     ((and (eq? 'set! (car term))
		   (list? (cadr term)))
	      (let ((newcadr (cadr term)));;(expand (cadr term))))
		(expand `(,(symbol-append 'setter!- (car newcadr)) ,@(cdr newcadr) ,@(cddr term)))))
	     ((list? (car term))
	      term)
	     (else
	      (if (not (symbol? (car term)))
		  (check-failed "Illegal function call:" term ".")
		  (begin
		    (let* ((args (cdr term))
			   (orgterm (cons (symbol-append rt-macro-prefix (car term)) args))
			   (newterm (catch #t
					   (lambda ()
					     (macroexpand-1 orgterm))
					   (lambda (key . args)
					     (c-display "Error while macroexpanding" term key args)
					     (throw 'macroexpanderror)))))
		      (if (not newterm)
			  (return #f)
			  (if (not (equal? orgterm newterm))
			      newterm
			      term))))))))
     (expand term))))
  

(define (rt-macroexpand-expand term)
  (let ((res (call-with-current-continuation
	      (lambda (return)
		
		(define (check-failed . args)
		  (newline)
		  (apply c-display (cons "rt-compiler.scm/rt-macroexpand:" args))
		  (return #f))
		
		(define (expand term)
		  ;;(c-display "term" term)
		  (cond ((null? term) term)
			((not (list? term)) term)
			((and (eq? 'set! (car term))
			      (list? (cadr term)))
			 (let ((newcadr (cadr term)));;(expand (cadr term))))
			   (expand `(,(symbol-append 'setter!- (car newcadr)) ,@(cdr newcadr) ,@(cddr term)))))
			((list? (car term))
			 (map expand term))
			((eq? 'lambda (car term))
			 `(lambda ,(cadr term)
			    ,@(map expand (cddr term))))
			((or (eq? 'rt-let/let* (car term))
			     (eq? 'let* (car term))
			     (eq? 'letrec (car term))
			     (eq? 'letrec* (car term)))
			 ;; @#$T#$T#$%!!!
			 (let ((daslets (map (lambda (letvar)
					       `(,(car letvar) ,@(map expand (cdr letvar))))
					     (cadr term))))
			   `(,(car term) ,daslets
			     ,@(map expand (cddr term)))))
			((not (symbol? (car term)))
			 term)
			(else
			 (if (not (symbol? (car term)))
			     (check-failed "Illegal function call:" term ".")
			     (begin
			       (let* ((funcname (car term))
				      (args (cdr term))
				      (orgterm (cons (symbol-append rt-macro-prefix funcname) args))
				      (newterm (catch #t
						      (lambda ()
							(macroexpand-1 orgterm))
						      (lambda (key . args)
							(c-display "\n\nError while macroexpanding" term "\n\n" key "\n\n" args "\n\n")
							(throw 'macroexpanderror)))))
				 (if (not newterm)
				     (return #f)
				     (if (not (equal? orgterm newterm))
					 (expand newterm)
					 (cons funcname
					       (map expand args))))))))))
		
		(expand term)))))
    (if (not res)
	(throw 'compilation-error))
    res))


;; (+ 2:-ms a:-s) -> (+ (infix-ms 2) (infix-s a))
(define* (rt-fix-infix2 code :key (macroexpander rt-macroexpand))
  (let loop ((code code))
    (cond ((null? code) '())
          ((symbol? code)
           (let ((sym code))
             (define first '())
             (let loop ((string (string->list (symbol->string sym))))
               (cond ((null? string)
                      sym)
                     ((and (char=? #\: (car string))
                           (char=? #\- (cadr string)))
                      (macroexpander
                       `(,(string->symbol (<-> "infix-" (list->string (cddr string))))
                         ,(let ((string (list->string (reverse! first))))
                            (or (string->number string)
                                (string->symbol string))))))
                     (else
                      (push! (car string) first)
                      (loop (cdr string)))))))
          ((not (pair? code))
           code)
          (else
           (map loop code)))))

;; (+ b :-s (c):-ms) -> (+ (infix-s b) (infix-ms (c)))
(define (rt-fix-infix code)
  (cond ((null? code) '())
        ((not (pair? code)) code)
        (else
         (let ()
           (define first '())
           (let loop ((term code))
             (cond ((null? term)
                    (map rt-fix-infix code))
                   ((and (not (null? first))
                         (not (null? (cdr first)))
                         (keyword? (car term))
                         (char=? #\- (car (string->list (symbol->string (keyword->symbol (car term)))))))
                    (rt-fix-infix
                     `(,@(c-butlast first)
                       (,(<_> 'infix (keyword->symbol (car term)))
                        ,(last first))
                       ,@(cdr term))))
                   (else
                    (set! first (append first (list (car term))))
                    (loop (cdr term)))))))))
#!
(rt-fix-infix '(block :duration (between 200 1000):-:ms aiai more and more))
(rt-fix-infix '(block :duration (between 200 1000):-m))
!#

(define (rt-fix-infix-nonrt code)
  (rt-fix-infix2
   (macroexpand
    (rt-fix-infix code))
   :macroexpander rt-fix-infix-nonrt))
  
#!
(rt-fix-infix-nonrt '(<rt-out> :dur 0 3:-s (oscil)))
!#

;; add-rt-transformer stuff is not used.
(add-rt-transformer rt-macroexpand-expand)
(add-rt-transformer rt-fix-infix)



;; Not quite working yet. Full rewrite of the macrosystem is probably needed.
;;(define rt-macroexpand (lambda (expr)
;;			 (rt-transform expr)))

(define (rt-macroexpand term)
  (rt-fix-infix2
   (rt-macroexpand-expand
    (rt-fix-infix term))))


  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-fix-various does various things.
;;
;; -makes all internal variable names unique,
;; -makes all variable-names legal c-names
;; -transforms all lisp let/let*/letrec/letrec*s into the eval-c version of let*.
;; -returns an assoc-list of the renamed global variable names. All variables,
;;  both local and global needs to be uniqified to avoid name-clash with c-functions,
;;  types, variables, etc.
;; -names starting with _rt_ is not uniqified. (unless it must be legalized...)
;; 
;; (let* ((a 5)) ...)       -> (let* ((a <float> 5)) ...)
;; (let* ((<int> a 5)) ...) -> (let* ((a <int> 5)) ...)
;; (define (a b c) d e)     -> (define a (lambda (b c) d e))
;; (lambda (a b)...)        -> (lambda ((<float> a)(<float> b))...)
;;
;; -locate functions that returns SCM's
;;  (vector-ref, car, cdr, etc.), and insert code to convert
;;  the SCM's to whats expected.
;;  (+ 2 (vector-ref vec 3)) -> (+ 2 (rt-scm-to-float (vector-ref vec 3)))
;;
;; + more, check code.
;;
;; rt-replace-define-with-letrecs must have been called on the term before calling.
;; 
(define (rt-fix-various term)
  (call-with-current-continuation
   (lambda (return)

     (define all-renamed-variables (make-hash-table 997))
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-fix-various:" args))
       (return #f))
     
     (define renamed-guile-vars '())

     (define get-unique-name
       (let ((n 0))
	   (lambda (orgname)
	     (set! n (1+ n))
	     (let ((das-string (symbol->string orgname)))
	       (if (and (> (string-length das-string) 7) ;; Very special situation. Don't rename internal rt-variables (starting with "_rt_dr_")
			(string= "_rt_rn_" das-string 0 7 0 7))
		   orgname
		   (string->symbol (string-append das-string "__" (number->string n))))))))
       
       
     (define* (get-new-name name #:optional is-guile-var)
       
       (define* (legalize-c-name scheme-name)
	 (let ((charlist (string->list (symbol->string scheme-name))))
	   (string->symbol
	    (list->string (map (lambda (char)
				 (if (and (not (char-alphabetic? char))
					  (not (char=? char #\_))
					  (not (char-numeric? char)))
				     #\_
				     char))
			       (if (char-numeric? (car charlist))
				   (cons #\_ charlist)
				   charlist))))))
						 
       (let ((newname (get-unique-name (legalize-c-name name))))
	 (if is-guile-var
	     (push! (list name newname) renamed-guile-vars))
	 (if (and rt-verbose is-guile-var)
	     (c-display "Variable" name "renamed as" newname))
	 (hashq-set! all-renamed-variables newname #t)
	 newname))
     
     ;; letrec has this stupid(?) rule... (Check out 4.2.2 in R5RS) (The handling here is overstrict though.)
     (define illegal-vars (make-hash-table 151))
     (define (add-illegal-vars . vars)
       (for-each (lambda (var) (hashq-set! illegal-vars var #t)) vars))
     (define (remove-illegal-vars . vars)
       (for-each (lambda (var) (hashq-remove! illegal-vars var)) vars))
     
     
     (define* (fix varlist term #:optional isnamed)  ;; If isnamed is #t, don't letify lambdas.
       ;;(c-display "fixing" term)
       (cond ((null? term) term)
	     ((string? term) term)
	     ((number? term) term)

	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ;; A variable
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ((symbol? term)

	      (let ((var (let ((var (hashq-ref all-renamed-variables term))) ;; In case the variable is already a renamed version. Can happen with macros.
			   (if var
			       term
			       (let ((var (assq term varlist)))
				 (if var
				     (cadr var)
				     (let ((var (assq term renamed-guile-vars)))
				       (if var
					   (cadr var)
					   #f))))))))
		(if var
		    (let ((illegal-var (hashq-ref illegal-vars var)))
		      (if illegal-var
			  (check-failed "The variable name \"" term "\" can not be accessed here, because it is"
					"a reference to another variable defined in the same letrec-block.")
			  var))
		    (begin
		      (get-new-name term #t)))))
  


	     ((not term)     ;; term=#f
	      0)
	     ((eq? #t term)  ;; term=#t
	      1)
	     
	     ((not (list? term))
	      (check-failed "Don't know how to handle" term))

	     ((eq? 'lambda (car term))
	      (if isnamed
		  (let* ((args (cadr term))
			 (varnames (map (lambda (t)
					  (list t
						(get-new-name t)))
					args))
			 (lambdaargs (map cadr varnames)))
		    `(lambda ,lambdaargs
		       ,@(map (lambda (t)
				(fix (append varnames varlist) t))
			      (cddr term))))
		  (let ((funcname (rt-gensym)))
		    (fix varlist
			 `(rt-let/let* ((,funcname ,term))
				       ,funcname)))))
	     
	     ;; ((lambda () 5)) -> (let ((u23 (lambda () 5))) (u23))
	     ((list? (car term))
	      (let ((funcname (rt-gensym)))
		(fix varlist
		     `(rt-let/let* ((,funcname ,(car term)))
				   (,funcname ,@(cdr term))))))
	      

	     ((eq? 'rt-begin (car term))
	      `(rt-begin ,@(map (lambda (t)
					    (fix varlist t))
					  (cdr term))))
	     
	     ((eq? 'rt-if (car term))
	      `(rt-if ,@(map (lambda (t)
			       (fix varlist t))
			     (cdr term))))
	     
	     ((eq? 'is-type? (car term))
	      `(is-type? ,(cadr term) ,(fix varlist (caddr term))))
	     
	     ((eq? 'the (car term))
	      `(the ,(cadr term) ,(fix varlist (caddr term))))
	     
	     ;; Convert let/let*/letrec/letrec* to the eval-c version of let*
	     ((or (eq? 'rt-let/let* (car term))
		  (eq? 'let* (car term))
		  (eq? 'letrec (car term))
		  (eq? 'letrec* (car term)))
	      (if (< (length term) 3)
		  (check-failed "Bad" (car term) "-form: " term ".")
		  (if (not (list? (cadr term)))
		      (check-failed "First argument to" (car term) "must be a list of variables:" term ".")
		      (begin
			(let ((das-vardecls (map (lambda (var)
						   (cond ((not (list? var))
							  (check-failed "\"" var "\" is not a list in expression " term "."))
							 ((not (symbol? (car var)))
							  (check-failed "Illegal variable name: " (car var) " in expression " term "."))
							 ((= 1 (length var))
							  (check-failed "Variable \""
									(car var) "\" in expression " term " does not have a value assigned."))
							 (else
							  var)))
						 (cadr term))))
			  
			  (cond ((eq? 'rt-let/let* (car term))
				 (let* ((newvarlist varlist)
					(vardecls (map (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl)))
									 (ret `(,uname ,(fix varlist (cadr vardecl) #t))))
								    (push! (list (car vardecl) uname) newvarlist)
								    ret))
								das-vardecls)))
				   `(let* ,vardecls
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))
				
				((eq? 'let* (car term))
				 (let* ((body (cddr term))
					(vardecls (map (lambda (vardecl)
								  (let* ((uname (get-new-name (car vardecl)))
									 (ret `(,uname ,(fix varlist (cadr vardecl) #t))))
								    (push! (list (car vardecl) uname) varlist)
								    ret))
								das-vardecls)))
				   `(let* ,vardecls
				      ,@(map (lambda (t)
					       (fix varlist t))
					     body))))
				
				((eq? 'letrec (car term))
				 (let* ((newvarlist varlist)
					(funclist '())
					(das-das-vardecls (map (lambda (vardecl)
								 (let ((uname (get-new-name (car vardecl))))
								   (add-illegal-vars uname)
								   (push! (list (car vardecl) uname) newvarlist)
								   (cons uname (cdr vardecl))))
							       das-vardecls))
					(vardecls (map (lambda (vardecl)
								  (let ((uname (car vardecl)))
								    (if (and (list? (cadr vardecl))
									     (eq? 'lambda (car (cadr vardecl))))
									(begin
									  (push! (list uname (cadr vardecl))
										 funclist)
									  `(,uname (lambda-decl ,(cadr (cadr vardecl)))))
									`(,uname ,(fix newvarlist (cadr vardecl))))))
								
								das-das-vardecls)))
				   (apply remove-illegal-vars (map car das-das-vardecls))
				   `(let* ,(append vardecls (map (lambda (funcdecl)
								   `(,(car funcdecl) ,(fix newvarlist (cadr funcdecl) #t)))
								 (reverse! funclist)))				      
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))
				
				((eq? 'letrec* (car term))
				 (let* ((newvarlist varlist)
					(funclist '())
					(vardecls (map (lambda (vardecl)
							 ;;(c-display "vardecl" vardecl)
							 (let* ((uname (get-new-name (car vardecl))))
							   (push! (list (car vardecl) uname) newvarlist)
							   (if (and (list? (cadr vardecl))
								    (eq? 'lambda (car (cadr vardecl))))
							       (begin
								 (push! (list uname (cadr vardecl))
									funclist)
								 `(,uname (lambda-decl ,(cadr (cadr vardecl)))))
							       `(,uname ,(fix newvarlist (cadr vardecl))))))
						       das-vardecls)))
				   `(let* ,(append vardecls (map (lambda (funcdecl)
								   `(,(car funcdecl) ,(fix newvarlist (cadr funcdecl) #t)))
								 (reverse! funclist)))
				      ,@(map (lambda (t)
					       (fix newvarlist t))
					     (cddr term)))))))))))
	      

	     (else
	      (if (not (symbol? (car term)))
		  (check-failed "Illegal function call:" term ".")
		  (let* ((funcname (let ((var (assq (car term) varlist)))
				     (if var
					 (cadr var)
					 (car term))))
			 (args (map (lambda (t)
				      (fix varlist t))
				    (cdr term))))
			 
		    (cons funcname args))))))
     
     ;;(c-display "fix-term:" term)
     (let ((ret (fix '() term #t)))
       ;;(c-display "fixed term" ret)
       ;;(rt-print2 "renamed:" renamed-guile-vars)
       (list (map (lambda (var)
		    (list (cadr var) (car var)))
		  renamed-guile-vars)
	     ret)))))
     

#!
(rt-fix-various '(lambda ()
		   (letrec ((a (lambda (b)
				 b)))
		     (a 2))))

(rt-fix-various '(lambda ()
		   (gakk (+ 2 3) (lambda ((<int> a))
				   (+ a 9)))))

(rt-let*-lifter (rt-fix-various '(lambda ()
				   (gakk (+ 2 3) (lambda ((<int> a))
						   (+ a 9))))))

(car (rt-fix-various '(lambda ()
			(locsig loc 0.2))))

(rt-fix-various '(lambda (x)
		   (if 1
		       (let* ((y (* x x)))
			 y)
		       0)))

(rt-fix-various '(- 2))
(rt-fix-various '(min 4 (max 5)))
(rt-fix-various '(and (+ 5 a)))
(rt-fix-various '(let* ((a 5)) (and (+ 5 a) (- 2 3) 4 5)))
(rt-fix-various '(begin (define (a b c) d e)))
(rt-fix-various '(begin (define (a (<int> b) c) d e)))
(rt-check-calls '(lambda ()
		   (let* ((a <int> 5))
		     (+ 2 3))))


(rt-3.5 '(lambda (a b c)
	   (or a (and a))))

(rt-3 '(lambda (a b c)
	 (and a b c
	     (or a b c)
	     (and a b c c c c b)
	     (or a (and b (or c a))
		 (and c)))))

(rt-3 '(lambda ()
	 (let* ((u5 (if a a 0)))
	   (if u5
	       u5
	       0))))

(rt-3 '(lambda (a b c)
	 (if a
	     (let* ((u254 2))
	       (if u254
		   u254
		   0))
	     0)))
(rt-3 '(lambda (a)
	 (begin
	   (+ a 2)
	   (- a 5))))

(macroexpand-1 '(rt-macro-and a (or a)))
(macroexpand-1 '(rt-macro-or a))
(macroexpand-1 '(rt-macro-oscil 2 3))


(rt-3 '(lambda ()
	 (let* ((unique_name_72 (+ 5 a)))
	   (if unique_name_72
	       5))))


!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-insert-types insert types for let* and lambda
;;
;; Note that at this stage of compilation, all let/let*/letrec/letrec*s have been converted to let*s,
;; and all variable-names are unique.
;;
;; (let* ((a 0)) (set! a 5)) -> (let* ((a <int> 0)) (set! a 5))
;; (let* ((a 0)) (set! a 5.0)) -> (let* ((a <float> 0)) (set! a 5.0))
;; (let* ((a 0)) (set! a (inexact->exact 5.0))) -> (let* ((a <int> 0)) (set! a (inexact->exact 5.0)))
;;
;; rt-let-lifter* must have been called on the term before calling.
;; 
;; (Warning, ugly ugly ugly code)
;; 
;; Note, during the insert-type-process, lambdas look like this: (lambda ((a <int>) (b <float>)...)...),
;; not like this: (lambda ((<int> a)(<float> b)...)...), which is the case everywhere else.
;;
(define (rt-insert-types term renamed-vars)
  (call-with-current-continuation
   (lambda (return)

     (define (debug . rest)
       (if #f
	   (apply c-display rest)))
     
     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-insert-types:" args))
       (return #f))

     
     ;; The type for let*-variables are just determinded once by checking the value of its type. However, some special care
     ;; has been taken for values where the type can not be determined right away. (I have a feeling the scheme is a bit too intelligent though...)
     ;; This hash-table contains all let*-variables as keys and theire type as value.
     (define let*-variables (make-hash-table 251))

     ;; This hash-table contains declared variables as keys, and theire type as value.
     (define declared-variables (make-hash-table 251))
     
     (define external-vars '())

     
     (define (merge-types type1 type2 term) ;; Term is only used to display understandable error-message.
       (debug "merge-types" type1 type2 term)
       (cond ((eq? type1 type2)
	      type1)
	     ((eq? type1 '<undefined>)
	      type2)
	     ((eq? type2 '<undefined>)
	      type1)

	     ((or (eq? type1 '<void>)
		  (eq? type2 '<void>))
	      '<void>)
	     
	     ((or (list? type1)
		  (list? type2))
	      (if (or (not (list? type1))
		      (not (list? type2))
		      (not (equal? (list (car type1)
					 (map (lambda (t)
						(if (list? t)
						    (cadr t)
						    t))
					      (cadr type1)))
				   (list (car type2)
					 (map (lambda (t)
						(if (list? t)
						    (cadr t)
						    t))
					      (cadr type2))))))
		  (check-failed type1 "and" type2 "are not compatible types. in term" term)
		  type1))
	     
	     ((and (rt-is-number? type1)
		   (rt-is-number? type2))
	      (if (or (eq? type1 '<double>)
		      (eq? type2 '<double>))
		  '<double>
		  (if (or (eq? type1 '<float>)
			  (eq? type2 '<float>))
		      '<float>
		      '<int>)))
	     (else
	      (if (or (not (symbol? type1))
		      (not (symbol? type2)))
		  (check-failed "Very error in rt-insert-types/merge-types" type1 type2 "term: " term))
	      (let ((rt-type1 (hashq-ref rt-types type1))
		    (rt-type2 (hashq-ref rt-types type2)))
		(if (not rt-type1)
		    (check-failed type1 "is not a legal type(1):" term "(" rt-types ")"))
		(if (not rt-type2)
		    (check-failed type2 "is not a legal type(2):" term))
		(if (-> rt-type1 type-ok? type2)
		    (-> (-> rt-type1 get-most-spesific-type type2) rt-type)
		    (if (-> rt-type2 type-ok? type1)
			(-> (-> rt-type2 get-most-spesific-type type1) rt-type)
			(check-failed "Incompatitble types for \"" type1 "\" and \"" type2 "\" in term \"" term "\".")))))))

     (define (check-compatible-types type1 type2 term)
       (merge-types type1 type2 term))



     ;; (set! a b) -> a and b have equal types, and are store in the hash-table below.
     ;; (set! a b) will make an empty hash-table that looks like this if a and b are undefined:
     ;; ((#:key a #:val (list (list 'b) '<undefined>))
     ;;  (#:key b #:val (list (list 'a) '<undefined>)))
     (define equal-typed-variables (make-hash-table 251))

     (define (add-equal-variables! name1 name2 type1 type2 term) ;; Term is only used for error-output
       (let* ((equal1 (hashq-ref equal-typed-variables name1))
	      (equal2 (hashq-ref equal-typed-variables name2))
	      (merged-type (merge-types type1 type2 term)))

	 (if (not equal1)
	     (begin
	       (set! equal1 (list (list name2) merged-type))
	       (hashq-set! equal-typed-variables name1 equal1)))
	 
	 (if (not equal2)
	     (begin
	       (set! equal2 (list (list name1) merged-type))
	       (hashq-set! equal-typed-variables name2 equal2)))
	       
	 (for-each (lambda (e)
		     (let ((equal (hashq-ref equal-typed-variables e)))
		       (set-car! equal (cons name2 (cons name1 (car equal))))
		       (set-car! (cdr equal) merged-type)))
		   (append (car equal1) (car equal2)))))
     

     (define (set-equal-type! name newtype term)
       (let* ((equal (hashq-ref equal-typed-variables name)))
	 (if equal
	     (let ((merged-type (merge-types newtype (cadr equal) term)))
	       (for-each (lambda (e)
			   (let ((equal (hashq-ref equal-typed-variables e)))
			     (set-car! (cdr equal) merged-type)))
			 (car equal))
	       merged-type)
	     newtype)))
	      


     (define* (add-external-var varname type #:optional iswriting)
       (let ((old (assq varname external-vars)))
	 (if old
	     (if (eq? type '<undefined>)
		 (-> (cadr old) rt-type)
		 (let ((rt-type (cadr old)))
		   (if (not (-> rt-type type-ok? type))
		       (set-car! (cdr old)
				 (hashq-ref rt-types (merge-types type (-> rt-type rt-type)
								  (format #f " Different types for guile variable \"~A\": \"~A\"/\"~A\"."
									  varname type (-> rt-type rt-type) ))))
		       (set-car! (cdr old) (-> rt-type get-most-spesific-type type)))
		   (set-car! (cddr old) (or iswriting (caddr old)))
		   (if (not (cadr old))
		       (c-display "Error. Something is wrong with the variable" old "in function rt-insert-types."))
		   (-> (cadr old) rt-type)))
	     (let ((rt-type (hashq-ref rt-types type)))
	       (if (not rt-type)
		   (check-failed "Unknown type " type ".")
		   (push! (list varname rt-type iswriting (let ((orgname (assq varname renamed-vars)))
							    (if orgname
								(cadr orgname)
								varname)))
			  external-vars))
	       type))))
     
     
     ;; The types for lambda-blocks and let*-blocks must be inserted before get-returntype is called.
     ;;
     ;; follow-variable is a variable that always has the same type as term.
     ;;   In "(let* ((a (lambda (b) b))))", "a" is the follow-variable for the term "(lambda (b) b)".
     ;; follow-variable is only used in case the returntype can not be determined now ('<undefined>), which in case its added to the equal-variables hash-table.
     ;;
     (define* (get-returntype varlist term #:optional follow-variable)
       (debug "get-returntype for " term "varlist:" varlist)
       (let ((ret (cond ((symbol? term)
			 (let ((avar (assq term varlist)))
			   (if avar
			       (if (and (= 3 (length avar))
					(list? (caddr avar))
					(eq? 'lambda (car (caddr avar))))
				   (list (cadr avar) (map list-copy (cadr (caddr avar)))) ;; Return-type is a lambda-function.
				   (begin
				     (if (and follow-variable
					      (eq? '<undefined> (cadr avar)))
					 (begin
					   (debug "follow-equal for" follow-variable (car avar) (cadr avar))
					   (add-equal-variables! follow-variable (car avar) '<undefined> '<undefined> term)
					   ))
				     (cadr avar)))
			       (begin (debug "gakk" term)
			       (add-external-var term '<undefined>)))))
			
			((number? term)
			 (if (exact? term)
			     '<int>
			     '<float>))
			
			((string? term)
			 '<char-*>)
			
			((or (not (list? term)) (null? term))
			 (check-failed "Unable to determine type for \"" term "\"."))
			
			((memq (car term) rt-unknown-ret-type-ops)
			 (let ((ret '<undefined>))
			   (for-each (lambda (t)
				       (set! ret (merge-types ret
							      (get-returntype varlist t follow-variable)
							      term)))
				     (cdr term))
			   ret))
			
			;; let*
			((eq? 'let* (car term))
			 (get-returntype (append (cadr term) varlist)
					 (last (cddr term))))
			
			;; lambda
			((eq? 'lambda (car term))
			 ;;(c-display "hmm" (cadr term) (last (cddr term)))
			 (get-returntype (append (cadr term) varlist)
					 (last (cddr term))
					 follow-variable))
			
			;; begin
			((eq? 'rt-begin (car term))
			 (get-returntype varlist (last term)))

			;; while
			((eq? 'rt-while (car term))
			 '<void>)
			
			;; if
			((eq? 'rt-if (car term))
			 (merge-types (get-returntype varlist (caddr term) follow-variable)
				      (get-returntype varlist (cadddr term) follow-variable)
				      term))
			
			;; is-type?
			((eq? 'is-type? (car term))
			 '<int>)
			
			;; the
			((eq? 'the (car term))
			 (check-compatible-types (cadr term)
						 (get-returntype varlist (caddr term) follow-variable)
						 term)
			 (cadr term))
			
			(else
			 (let ((func (assq (car term) varlist)))
			   (rt-print2 "got func for " (car term) ":" func)
			   (if func
			       (if (and (list? (cadr func))
					(or (= 2 (length func))
					    (not (list? (caddr func)))))
				   (car (cadr func))
				   (cadr func))
			       (let ((func (hashq-ref rt-funcs (car term))))
				 (if func
				     (-> func return-type)
				     (check-failed "Unknown function(2) \"" (car term) "\": " term)))))))))

	 (debug "Get returntype was " ret " for term" term)
	 ret))
       


     ;; This one also returns the type.
     (define (set-type-in-! var type term)
       (debug "set-type-in-!" var type term)

       (let* ((varname (car var))
	      (vartype (cadr var))
	      (let*-variable (hashq-ref let*-variables varname)))
	 
	 (if (and let*-variable
		  (not (eq? '<undefined> let*-variable)))
	     (begin
	       (set! type let*-variable)
	       (set! vartype let*-variable)))
	 
	 (cond ((hashq-ref declared-variables var)
		(hashq-ref declared-variables var)) ;; Nothing can overo the declared type of a variable. This is the type.
	       
	       (else
		(let ((vartype (if (and (= 3 (length var))
					(list? (caddr var))) ;; A lambda function.
				   (list vartype (map list-copy (cadr (caddr var))))
				   vartype)))
		  (let ((ret (merge-types vartype
					  type
					  term)))
		    (set-car! (cdr var) ret)
		    
		    (if let*-variable
			(hashq-set! let*-variables varname ret))
		    (debug "set to" ret)
		    ret))))))

     ;; Set the type for a lambda*-variable. (I'm not sure this works properly, or what is supposed to work at all.)
     (define (set-type-in-lambda! var type term)                       ;; Term is only used to make understandable error-output
       (debug "in lambda" var type term)
       (set-type-in-! var type term))

     (define* (set-type! varlist name type term #:optional iswriting)  ;; Term is only used to make understandable error-output
       ;; Set the type for a variable in a varlist
       (define (set-type-in-varlist! var type term)                       ;; Term is only used to make understandable error-output 
	 ;;(debug "set-type-in-varlist" var type term)                  ;; (Hmmm, this function is equal to set-type-in-lambda!...)
	 (set-type-in-! var type term))

       ;;(debug "set-type! name/type/term" name type term)
       (let ((merged-type (set-equal-type! name type term)))
	 (let ((var (assq name varlist)))
	   (if (not var)
	       (add-external-var name type iswriting)
	       (set-type-in-varlist! var merged-type term)))))
       
     
     
     ;; Check-call checks correct types for function-call.
     ;; This is actually the else-block for the cond-block in the insert function.
     (define (check-call varlist term)
       (debug "check-call" term)
       (let ((funcname (car term)))
	 (if (not (symbol? funcname))
	     (check-failed "Illegal term: " term)
	     
	     (let ((func (assq (car term) varlist)))
	       

	       (if (and func (= 2 (length func)))
		   (check-failed (<-> "Snd-rt does not normally support function names as arguments.\n"
				      "Either use the body of the function as argument instead, or tranform\n"
				      "the function into a macro. (please let me know if you need\n"
				      "to use function names as arguments, -Kjetil.)")
				 func))
	       
	       (if func

		   ;; Local function
		   (let ((name (car func))
			 (functype (if (list? (caddr func))
				       ;;(list (cadr func) (map list-copy (cadr (caddr func))))
				       (list (cadr func) (cadr (caddr func)))
				       (cadr func)))
			 (returntype #f)
			 (args #f))

		     ;;(debug "functype " functype)

		     (if (or (not (list? functype))
			     (= 1 (length functype)))
			 (check-failed "Local variable \"" funcname "\" is not a function:" term ". func:" func))

		     (set! returntype (car functype))
		     (set! args (cadr functype))
		     
		     (if (not (= (length args)
				 (- (length term) 1)))
			 (check-failed "Illegal number of argumentes to local function \"" funcname "\":" term))
		     
		     ;; Then check arg-types.
		     (let ((ret (map (lambda (t)
						(insert varlist t))
					      term)))

		       (cons (car ret) (map (lambda (t funcarg)
					      (let ((argtype (cadr funcarg))
						    (ret-type (get-returntype varlist t)))
						(if (and (eq? '<SCM> ret-type)
							 (not (eq? '<SCM> argtype)))
						    (begin
						      `(,(-> (hashq-ref rt-types argtype) c-transformfunc) ,t))
						    (begin
						      (set-type-in-lambda! funcarg ret-type term)
						      (debug "after lambda-set:" funcarg varlist)
						      (if (symbol? t)
							  (begin
							    (set-type! varlist t argtype term)))
						      t))))
					    (cdr term)
					    args))))
		   
		 
		   ;; Global function
		   (let ((func (hashq-ref rt-funcs funcname)))
		     (if (not func)
			 (check-failed "Unknown function \"" funcname "\": " term)
			 (if (not (-> func legal-number-of-arguments? term))
			     (return #f)
			     (let ((ret (map (lambda (t)
							(insert varlist t))
						      term)))
			       (cons (car ret) (map (lambda (t argtype)
						      (let ((ret-type (get-returntype varlist t)))
							;;(debug "ret-type" ret-type)
							(if (and (eq? '<SCM> ret-type)
								 (not (eq? '<SCM> argtype))
								 (not (eq? '<SCM> (-> (hashq-ref rt-types argtype) supertype))))
							    `(,(-> (hashq-ref rt-types argtype) c-transformfunc) ,t)
							    (begin
							      (check-compatible-types argtype ret-type term)
							      (if (symbol? t)
								  (set-type! varlist t argtype term))
							      t))))
						    (cdr ret)
						    (list-tabulate (length (cdr term))
								   (lambda (i)
								     (-> func arg-type i))))))))))))))

     (define (get-external-varlist)
       (map (lambda (v) (list (car v)
			      (-> (cadr v) rt-type)))
	    external-vars))

     (define (insert varlist term)
       (debug "insert" term " - " varlist)
       (cond ((not (list? term)) term)
	     ((null? term) term)


	     ;;; LET*
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'let* (car term))
	      (let* ((newvarlist varlist)
		     (lambda_decls '())
		     (vardecls (map (lambda (var)
					       (let ((ret (if (list? (cadr var))
							      (if (eq? 'lambda-decl (car (cadr var)))
								  ;; A lambda declaration. (as the result of a letrec function)
								  (let ((ret (list (car var) '<undefined>
										   (append (list (car (cadr var)))
											   (list (map (lambda (t)
													(list t '<undefined>))
												      (car (cdr (cadr var)))))))))
								    (push! ret lambda_decls)
								    ret)

								  ;; A lambda function
								  (let* ((name (car var))
									 (body (cadr var))
									 (type #f)
									 (decl (cadr body))
									 (ifdecl (assq name lambda_decls))) ;; Check if there is a lambda declaration
								    (if ifdecl
									(begin
									  ;; Fill in types from the declaration.
									  (set! decl (map (lambda (v1 v2)
											    (list v1 (cadr v2)))
											  decl
											  (cadr (caddr ifdecl))))))
								    (set! body (insert newvarlist `(lambda ,decl ,@(cddr body))))
								    (set! type (get-returntype newvarlist body))
								    (list name type body)))
							      (if (rt-symbol-starts-with? (car var) '_rt_breakcontsig)
								  (begin
								    (list (car var) '<jmp_buf> 0)
								    )
								  (list (car var) (cadr var) 0)))))
						 (push! ret newvarlist)
						 ret))
					     (cadr term)))
		     (body (map (lambda (t)
				  (insert newvarlist t))
				(cddr term))))

		(debug "1vardecls" vardecls)
		(debug "1newvarlist" newvarlist)

		;; Fix up equal typed and declared variables.
		(set! vardecls (map (lambda (var)
				      (let ((decl (hashq-ref declared-variables (car var))))
					(if decl
					    (list (car var) decl (if (= 3 (length var))
								     (caddr var)
								     0))
					    (let ((equal (hashq-ref equal-typed-variables (car var))))
					      (if equal
						  (let* ((name (car var))
							 (def (cadr equal)))
						    (if (and (list? (cadr equal))
							     (list? (caddr var)))
							(let* ((ret-type (car def))
							       (arg-types (map cadr (cadr def))))
							  `(,name ,ret-type (lambda ,(map (lambda (v1 v2)
											    (list (car v2) v1))
											  arg-types
											  (cadr (caddr var)))
									      ,@(cddr (caddr var)))))
							(list (car var) (cadr equal) (if (= 3 (length var))
											 (caddr var)
											 0))))
						  var)))))
				    vardecls))

		;; Make another attempt to determine return-type for lambda-functions. A function like this: "(lambda (a) a)" can sometimes
		;; not determine its return-type right away because the type for a is not always known at the evaluation time for the function.
		(set! vardecls
		      (let ((newvarlist varlist))
			(map (lambda (var)
					(let ((ret (if (and (eq? '<undefined> (cadr var))
							    (list? (caddr var))
							    (eq? 'lambda (car (caddr var))))
						       (let ((type (get-returntype newvarlist (caddr var))))
							 (if (eq? '<undefined> type)
							     (check-failed "Unable to determine return-type for function" (car var)
									   ". (Perhaps you need to use the \"the\" operator?)"))
							 (list (car var) type (caddr var)))
						       var)))
					  (push! ret newvarlist)
					  ret))
				      vardecls)))
						     
		;; Fill in lambda-decls types
		(set! vardecls (map (lambda (var)
					       (let ((decl (assq (car var) lambda_decls)))
						 (if decl
						     (let ((real (assq (car var) (reverse vardecls))))
						       (set! lambda_decls (delete decl lambda_decls))
						       ;;(debug "var/real" (car var) real (reverse vardecls))
						       `(,(car var) ,(cadr real) (lambda-decl ,(cadr (caddr real)))))
						     var)))
					     vardecls))
		
		(let ((vardecls (remove (lambda (var)
					  (eq? (cadr var) '<undefined>))
					vardecls)))
		  (rt-print2 "vardecls" vardecls)
		  (rt-print2 "newvarlist" newvarlist)
		  (if (null? vardecls)
		      `(rt-begin
			 ,@body)
		      `(let* ,vardecls
			 ,@body)))))

	     
	     ;; LAMBDA
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'lambda (car term))
	      (let* ((vardecls (map (lambda (var)
				      (if (list? var)
					  var
					  (list var '<undefined>)))
				    (cadr term)))
		     (body (map (lambda (t)
				  (insert (append vardecls varlist) t))
				(cddr term))))

		;; Set correct type for equal typed and declared variables.
		(set! vardecls (map (lambda (var)
				      (let ((decl (hashq-ref declared-variables (car var))))
					(if decl
					    (list (car var) decl)
					    (let ((equal (hashq-ref equal-typed-variables (car var))))
					      (if equal
						  (list (car var) (cadr equal))
						  var)))))
				    vardecls))
		
		`(lambda ,vardecls
		   ,@body)))

	     
	     ;; SET! and RT-SET*! (rt-set*! is the result of let*-lifting and needs special treatment, because the type can't change when rt-set*!).
	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ((or (eq? 'set! (car term))
		  (eq? 'rt-set*! (car term)))
	      (let* ((ret-term (insert varlist (caddr term)))
		     (type (get-returntype varlist ret-term))
		     (newtype  (if (eq? 'rt-set*! (car term))
				   type
				   (get-returntype varlist (cadr term)))))

		(if (eq? 'rt-set*! (car term))
		    (hashq-set! let*-variables (cadr term) type))
		
		(if (and (eq? '<SCM> type)
			 (not (or (eq? '<SCM> newtype)
				  (eq? '<undefined> newtype))))
		    (begin
		      `(set! ,(cadr term) (,(-> (hashq-ref rt-types newtype) c-transformfunc) ,ret-term)))
		    (begin
		      (set-type! varlist (cadr term) type term #t)
		      (if (symbol? (caddr term))
			  (begin
			    (add-equal-variables! (cadr term) (caddr term) newtype type term)
			    ;;(debug "setting type for" (caddr term) "to" newtype)
			    (set-type! varlist (caddr term) newtype term)))
		      `(set! ,(cadr term) ,ret-term)))))
	     

	     
	     ;; DECLARE
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'declare (car term))
	      (for-each (lambda (decl)
			  (let* ((type (if (eq? (car decl) 'type) (cadr decl) (car decl)))
				 (vars (if (eq? (car decl) 'type) (cddr decl) (cdr decl)))
				 (rt-type (hashq-ref rt-types type)))
			    (if (not rt-type)
				(check-failed type "is not a known type in expression" term))
			    (for-each (lambda (var)
					(hashq-set! declared-variables var type)
					(set-type! varlist var type term)
					)
				      vars)
			    ))
			(cdr term))
	      '(rt-dummy/dummy))
	      

	     ;; THE
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'the (car term))
	      (if (symbol? (caddr term))
		  (set-type! varlist (caddr term) (cadr term) term))
	      (let* ((c (insert varlist (caddr term)))
		     (ret-type (get-returntype varlist c)))
		;;(c-display "the" term c ret-type)
		(if (and (eq? '<SCM> ret-type)
			 (not (eq? '<SCM> (cadr term))))
		    `(the ,(cadr term) (,(-> (hashq-ref rt-types (cadr term)) c-transformfunc) ,c))
		    `(the ,(cadr term) ,c))))


	     ;; IS-TYPE?
             ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'is-type? (car term))
	      term)

	     ;; BEGIN
	     ;;;;;;;;;;;;;;;;;;;;;;
	     ((eq? 'rt-begin (car term))
	      `(rt-begin ,@(map (lambda (t)
					    (insert varlist t))
					  (cdr term))))

	     
	     ;; (EVERYTHING) ELSE
	     ;;;;;;;;;;;;;;;;;;;;;;
	     (else
	      (check-call varlist term))))

     
     (let ((ret (insert '() term)))


       (for-each (lambda (var)
		   (let ((decl (hashq-ref declared-variables (car var))))
		     (if decl
			 (set-car! (cdr var) (hashq-ref rt-types decl))
			 (let ((equal (hashq-ref equal-typed-variables (car var))))
			   (if equal
			       (set-car! (cdr var) (hashq-ref rt-types (cadr equal))))))))
		 external-vars)
       
       (let ((extnumbers '())
	     (extpointers '())
	     (extnumbers-writing '())
	     (returntype (get-returntype '() ret)))
	 (for-each (lambda (extvar)
		     (if (caddr extvar)
			 (push! extvar extnumbers-writing)
			 (if (rt-is-number? (-> (cadr extvar) rt-type))
			     (push! extvar extnumbers)
			     (push! extvar extpointers))))
		   external-vars)
	 (debug "wrting:" extnumbers-writing)
	 (debug "renamed-vars" renamed-vars)
	 (list ret
	       returntype
	       extnumbers extpointers extnumbers-writing))))))



(define (rt-insert-types2 term)
  (let ((ret (rt-insert-types term '())))
    (if ret
	(list (car ret)
	      (cadr ret)
	      (map (lambda (v) (list (car v)
				     (-> (cadr v) rt-type)))
		   (append (caddr ret) (cadddr ret) (cadr (cdddr ret))))))))


#!
(pretty-print
 (rt-insert-types2 '(lambda ()
		     (lambda ()
		       (let* ((rt_gen498__1 <float>)
			      (rt_gen500__2 <undefined>)
			      (rt_gen499__4 <void-*>)
			      (rt_gen501__6 (lambda (dir__7)
					      (let* ((rt_gen502__8 (lambda (dire__9)
								     0.2)))
						(declare (<int> dir__7))
						(the <float>
						     (rt-begin
						      (rt_gen502__8 dir__7)))))))
			 (rt-begin
			  (rt-set*! rt_gen498__1 0.0)
			  (rt-set*! rt_gen500__2 sr__3)
			  (rt-set*! rt_gen499__4 (rt-mus-environ/mus_environ rt_gen500__2))
			  (setter!-mus-environ/mus_set_environ rt_gen500__2 (rt-get-environ))
			  (set! rt_gen498__1 (rt-mus-src/mus_src rt_gen500__2 renamed_var__5 (rt-begin rt_gen501__6)))
			  (setter!-mus-environ/mus_set_environ rt_gen500__2 rt_gen499__4) rt_gen498__1)))))
 )



(-> (hashq-ref rt-funcs 'rt-mus-src/mus_src) arg-type 1)


(rt-2 '(lambda ()
	 (src sr das-src (lambda (dire)
			   0.2))))

(rt-insert-types2 '(lambda ()
		     (let* ((a (lambda (func)
				 ;;(declare ((<void> (<int>)) func))
				 (func 5))))
		       (a (lambda (b)
			    b)))))

(rt-insert-types2 '(lambda ()
		     (rt-/// (the <float> a) b)))
		     
(rt-insert-types2 '(lambda ()
		     (let* ((a (lambda ()
				 9))
			    (b <undefined>))
		       (rt-set*! b a)
		       (b))))


(rt-insert-types2 '(lambda ()
		     (let* ((ai (lambda (a)
				  5)))
		       (ai 2))))

(rt-insert-types2 '(lambda ()
		     (let* ((a <float>))
		       (rt-set*! a 5.2)
		       (declare (<int> a))
		       a)))
 
(define a (rt-2 '(lambda ()
		   (let ((a 2))
		     (let ((gakk (lambda ()
				   (if (< a 10)
				       (set! a (1+ a))))))
		       (gakk))
		     a))))


(rt-insert-types2 '(lambda ()
		     (let* ((a 0))
		       (rt-set*! a 9)
		       (< a 7))))
(rt-insert-types2 '(lambda (a b)
		     (rt-if 1
			    (set! a 9)
			    (set! a 3.9))
		     (set! b 2.3)
		     (+ a b)))
		      
(rt-insert-types2 '(lambda (n1)
	 (let* ((fib (lambda-decl (n2)))
		(fib (lambda (n2)
		       (rt-if (< n2 2)
			      n2
			      (+ (fib (rt--/- n2 1))
				 (fib (rt--/- n2 2)))))))
	   (fib n1))))

(rt-2 '(lambda (n1)
	 (letrec ((fib (lambda (n2)
			 (rt-if (< n2 2)
				(+ a n2)
				(+ (fib (rt--/- n2 1))
				   (fib (rt--/- n2 2)))))))
	   (fib n1))))

(lambda ((<float> n1_u1))
  (let* ((fib_u3 <float> (lambda ((<float> n2_u4))
			   (return
			    (rt-if (< n2_u4 2)
				   n2_u4
				   (+ (fib_u2 (rt--/- n2_u4 1))
				      (fib_u2 (rt--/- n2_u4 2))))))))
    (return
     (rt-begin
      (fib_u3 n1_u1)))))

(rt-insert-types2 '(lambda ()
		     (let* ((a (lambda ()
				 (rt-if 1
					(* 2 3)
					(+ 2 3)))))
		       (a))))

(define a (rt-2 '(lambda ()
		   (let* ((a (lambda (c)
			       ;;(declare (<int> c))
			       (the <char-*> "adsf")))
			  (d (lambda (e)
			       ;;(declare (<double> e))
			       "gakk"))
			  (b a))
		     (set! b d)
		     (b 7)
		     (+ 5 9)
		     ))))
(rt-funcall a)

(lambda (n)
  (letrec ((fib (lambda (n)
		  (rt-if (< n 2)
				n
				(+ (fib (rt--/- n 1))
				   (fib (rt--/- n 2)))))))
    (fib n)))




(rt-insert-types2 '(lambda ()
		     (let* ((a (lambda (c)
				 ;;(declare (<int> c))
				 (the <char-*> "adsf")))
			    (d (lambda (e)
				 ;;(declare (<double> e))
				 "gakk"))
			    (b 0))
		       (set! b a)
		       (set! b d)
		       (b 7)
		       (+ 5 9)
		       )))

(rt-insert-types '(lambda ()
		    (let* ((fib (lambda-decl (n)))
			   (ai (lambda (g)
				 (declare (<double> g))
				 (fib 6)
				 (set! g 9)
				 ;;(set! g (fib 3))
				 (set! a 9)))
			   (fib (lambda (n)
				  (declare (<int> n))
				  (the <double>
				       (rt-if (< n 2)
						      n
						      (+ (fib (rt--/- n 1))
							 (fib (rt--/- n 2)))))))
			   )
		      (ai 3)
		      ;;(fib 5)
		      ))
		 '())

(rt-insert-types '(lambda ()
		    (let* ((a 0))
		      (set! a (+ a 3))))
		 '())

(rt-insert-types '(lambda (a b)
		    (let* ((c 0)
			   (d 0))
		      (declare (<double> a b c f))
		      (+ a b c f)))
		 '())
		    
(rt-insert-types '(lambda ()
		    (let* ((a 0))
		      ;;(+ a (rt-vector-ref/vector-ref vec 3))
		      (set! a (rt-vector-ref/vector-ref vec 4))
		      (+ a 9)
		      (set! b a)
		      ))
		 
		 '())

(rt-insert-types '(lambda ()
		    (let* ((a (lambda (b)
				(+ 1 b))))
		      (+ 2 (a (rt-vector-ref/vector-ref vec 2)))))
		 '())

(rt-insert-types '(lambda ()
		    (let* ((d 0)
			   ;;(a_u1 (lambda-decl (b_u3)))
			   (a_u1 (lambda (b_u2)
				   c_u2)))
		      (a_u1 (+ c_u2 2.9))
		      ))
		 '())

(rt-insert-types '(lambda ()
		    (let* ((a 0)
			   ;;(f 0)
			   (b (lambda (r2 r3)
				;;(set! r2 a)
				;;(+ a 2)
				;;(+ r2 6)
				(rt-mus-channels/mus_channels r2)
				(rt-oscil/mus_oscil r2 3 4)
				)))
		      (set! f (rt-vector-ref/vector-ref vec 9))
		      (set! a (b d 9))
		      (set! a 9)))
		 '())
  
(rt-insert-types '(lambda ()
		    (let* ((a (lambda (b)
				;;8
				;;(+ b 4)
				b
				)))
		      (+ 1 (a 9))
		      ))
		 '())
!#
		   
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rt-check-syntax does the following:
;; *various checks that that the term is a legal expression.
;;
;; rt-macroexpand must have been called on the term before calling.
;; 
(define (rt-check-syntax term)

  (call-with-current-continuation
   (lambda (return)

     (define (check-failed . args)
       (newline)
       (apply c-display (cons "rt-compiler.scm/rt-check-calls:" args))
       (return #f))
     

     (define (check-calls term)
       ;;(c-display "check-calls varlist/term" varlist term)
       (cond ((not (list? term)) #t)
	     ((null? term) #t)

	     ;; RT_WHILE/WHILE
	     ;((eq? 'rt-while (car term))
	     ; (check-calls varlist (cadr term))
	     ; (check-calls varlist (caddr term)))
	      
	     ;; RT-LAMBDA_DECL/LAMBDA_DECL	
	     ((eq? 'lambda-decl (car term))
	      #t)
	     
	     ;; LAMBDA 
	     ((eq? 'lambda (car term))
	      (if (< (length term) 3)
		  (check-failed "Bad lambda-form: " term ".")
		  (if (not (list? (cadr term)))
		      (check-failed "Second argument for lambda is not a list: " term ".")	       
		      (if (not (= (length (cadr term))
				  (length (delete-duplicates (cadr term)))))
			  (check-failed "Same argument name for lambda function used more than once: " term ".")
			  (begin
			    (for-each (lambda (varname)
					(if (not (symbol? varname))
					    (check-failed "Illegal variable \"" varname "\" in lambda-form: " term ".")))
				      (cadr term))
			    (for-each check-calls (cddr term)))))))
	     
	     ;; SET!
	     ((eq? 'set! (car term))
	      (if (not (symbol? (cadr term)))
		  (check-failed "Illegal set!(2) term: " term)
		  (if (not (= 3 (length term)))
		      (check-failed "Illegal set!(3) term: " term))))

	     ;; BEGIN
	     ((eq? 'rt-begin (car term))
	      (if (null? (cdr term))
		  (check-failed "begin needs a body: " term ".")
		  (for-each check-calls (cdr term))))
	     
	     ;; LET*
	     ((eq? 'let* (car term))
	      (for-each (lambda (var)
			  (if (not (symbol? (cadr var)))
			      (check-calls (cadr var))))
			(cadr term))
	      (for-each check-calls (cddr term)))
	     
	     ;; IF
	     ((eq? 'rt-if (car term))
	      (if (< (length term) 3)
		  (check-failed "To few arguments for if:" term ".")
		  (if (> (length term) 4)
		      (check-failed "To many arguments for if:" term ".")))
	      (check-calls (cadr term))
	      (check-calls (caddr term))
	      (if (= (length term) 4)
		  (check-calls (cadddr term))))
	     
	     (else
	      (for-each check-calls term))))
     
     (if (not (eq? 'lambda (car term)))
	 (check-failed "This is not a lambda function: " term))

     (rt-print2)
     (rt-print2 "check-calls term" term)
     (rt-print2)
     
     (check-calls term))))


#!
((<float> c (lambda ((<float> d)) (+ a b)))
 (<float> b 5)
 (<float> a))

(rt-check-calls '(lambda ()
		   (rt-if a
				 a
				 a))
		'((a ai)))

(rt-check-calls '(lambda ()
		   ;;(oscil osc2)
		   (set! setfloat-2 (sin osc))
		   (sin (* 0.2 (sin osc) (sin osc2))))
		'((setfloat-2 gakkgakk)))

(+ 2 3)

(rt-check-calls '(lambda ()
		   (set! osc 9)
		   (rt-oscil/mus_oscil_0 osc)))

(map car (caddr 
	  (rt-check-calls '(lambda ((<float> a))
			     (let* ((b <float> 5)
				    (c <float> (lambda ((<float> d))
						 (+ 2 a b))))

			       (c (c d))
			       (rt-oscil/mus_oscil_0 anosc)
			       (set! anosc 5)
			       (set! extw 9)
			       (+ ext1 ext2))))
	  ))

!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rt-types (make-hash-table 251))

;; rt-type         -> Name used in rt
;; checkfunc       -> Guile-function to check if type is correct.
;; c-transformfunc -> Name of a rt-ec-function (or macro) that converts an SCM version of the type to something that can be used in rt.
;;                    (For example when using vector-ref, list-ref, car, cdr, etc.)
;;                    This function must call rt-error if the variable can not be converted to a compatible type.
;; transformfunc   -> A Guile function that is run on the variable before putting it to rt. Type is already checked with checkfunc.
;; error-message   -> ???
;; c-type          -> Name of the type on the c-side. Usually the same as rt-type.
;; suptype-of      -> This type can be used for all situation which the type for "subtype-of" is compatible with.

(def-class (<rt-type> rt-type checkfunc c-transformfunc #:key transformfunc error-message (c-type rt-type) subtype-of)
  (def-method (rt-type)
    rt-type)
  (def-method (c-type)
    c-type)
  (def-method (supertype)
    subtype-of)
  (def-method (c-transformfunc)
    c-transformfunc)
  (def-method (check type)
    (if checkfunc
	(checkfunc type)
	(eq? type rt-name)))

  (define compatible-types '())
  (def-method (add-compatible-type type)
    (push! type compatible-types))
  
  (def-method (type-ok? type)
    ;;(c-display "type/rt-typ/compatibele-types" type rt-type compatible-types)
    (or (member type (cons rt-type compatible-types))
	(and subtype-of
	     (-> (hashq-ref rt-types subtype-of) type-ok?
		 type))))

  ;; Don't use.
  (def-method (get-most-compatible-type othertype)
    (let ((other ((hashq-ref rt-types othertype))))
      (if (eq? this othertype)
	  this
	  #f)))
	       
  ;; We assume rt-type and type are compabible.
  (def-method (get-most-spesific-type type)
    (if (eq? type rt-type)
	this
	(if (eq? subtype-of type)
	    this
	    (hashq-ref rt-types type))))

  (def-method (transform var varname #:optional das-add-extra-gc-var-func)
    (let ((ret (if (not (this->check var))
		   (begin
		     (c-display "rt-compiler/<rt-type>. Wrong type. \"" varname "\" with value \"" var "\" is not a" rt-type ".")
		     (if (eq? '<mus_any-*> rt-type)
			 (c-display "(this usually means that only a clm method is used on the variable, and\n"
				    " therefore it was impossible to figure out its real type.\n"
				    " [for example only (mus-location envelope), but no (env envelope).])\n"))
		     (throw 'wrong-type))
		   (if transformfunc
		       (begin
			 (transformfunc var)
			 )
		       var))))
      (if (and (list? ret)
	       (eq? 'extra-gc-var (car ret)))
	  (begin
	    ;;(c-display "Warning, extra-gc-var is returned in the tranform method in the <rt-types> class. That is not supposed to happen....")
	    (if das-add-extra-gc-var-func
		(das-add-extra-gc-var-func (cadr ret)))
	    (caddr ret))
	  ret)))
  
  (hashq-set! rt-types rt-type this)

  (if subtype-of
      (-> (hashq-ref rt-types subtype-of) add-compatible-type
	  rt-type))
  
  )

;; Never called!
(define (c-nevercalled-true? . something)
  (c-display "Error. What the? c-nevercalled-true? is never supposed to be called. Arguments:" something)
  #f)


(define-c-macro (rt-mus-any?/mus_xen_p scm)
  (if (rt-is-safety?)
      `(?kolon (mus_xen_p ,scm)
	       (XEN_TO_MUS_ANY ,scm)
	       (begin_p
		(rt_error rt_globals (string "Variable is not a CLM generator."))
		NULL))
      `(XEN_TO_MUS_ANY ,scm)))

(define (rt-number-2-rt scm)
  (cond ((not scm) 0)
	((number? scm) scm)
	((eq? #t scm) 1)
	(else
	 (c-display "Error. Wrong type for variable with value " scm ". It is not a number")
	 #f)))
(define (rt-number-2-rt? scm)
  (or (number? scm)
      (eq? #f scm)
      (eq? #t scm)))

(begin
  (<rt-type> '<double>  rt-number-2-rt? 'rt_scm_to_double #:transformfunc rt-number-2-rt)
  (<rt-type> '<float> rt-number-2-rt? 'rt_scm_to_float #:transformfunc rt-number-2-rt) ;;  #:subtype-of <double>)
  (<rt-type> '<int> rt-number-2-rt? 'rt_scm_to_int #:transformfunc rt-number-2-rt)   ;;  #:subtype-of <float>)
  (<rt-type> '<char-*> string? 'rt_scm_to_error) ;; Function does not exist
  (<rt-type> '<vct-*> vct? 'rt_scm_to_vct #:transformfunc XEN_TO_VCT)
  ;;(<rt-type> '<vector> vector? #f #:c-type '<SCM>) 
;;  (<rt-type> '<mus_any-*> c-nevercalled-true? 'rt-mus-any?/mus_xen_p)
  (<rt-type> '<mus_any-*> c-nevercalled-true? 'rt_scm_to_mus_any)
  (<rt-type> '<void-*> c-nevercalled-true? #f)
  (<rt-type> '<undefined> (lambda x
			    (c-display "Warning, unused variable with value:" x)
			    #t)
	     #f #:c-type '<SCM>)
  (<rt-type> '<void> c-nevercalled-true? #f)
  (<rt-type> '<SCM> (lambda (t) #t) #f)
  (<rt-type> '<jmp_buf> c-nevercalled-true? #f)
  ;;(<rt-type> '<pair> pair? #f #:c-type '<SCM> #:subtype-of '<SCM>)      ;; Some checking is needed here. I don't think this is safe.
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rt-funcs (make-hash-table 251))


(def-class (<rt-func> name returntype args #:key min-arguments max-arguments is-immediate needs-rt-globals)
  (define last-type (if (null? args)
			#f
			(last args)))

  (def-method (legal-number-of-arguments? term)
    (if (and max-arguments
	     (>= (1- (length term)) max-arguments))
	(begin
	  (c-display "rt-compiler.scm/<rt-func>: Wrong number of arguments in \"" term "\". Expected maximum"
		     max-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	  #f)
	(if (and min-arguments
		 (< (1- (length term)) min-arguments))
	    (begin
	      (c-display "rt-compiler.scm/<rt-func>: Wrong number of arguments in \"" term "\". Expected minimum"
			 min-arguments "arguments for" (car term) ". Found" (- (length term) 1) ".")
	      #f)
	    (if (and (not min-arguments)
		     (not max-arguments)
		     (not (= (1- (length term)) (length args))))
		(begin
		  (c-display "rt-compiler.scm/<rt-func>: Wrong number of arguments in \"" term "\". Expected"
			     (length args) "arguments for" (car term) ". Found" (- (length term) 1) ".")
		  #f)
		#t))))
  
  (def-method (needs-rt-globals)
    needs-rt-globals)
  
  (def-method (return-type)
    returntype)
  
  (def-method (arg-type argnum)
    (if (>= argnum (length args))
	last-type
	(list-ref args argnum)))
  
  ;(if (not min-arguments)
  ;    (set! min-arguments (length args)))

  ;(if (not max-arguments)
  ;    (set! max-arguments (length args)))

  (if is-immediate
      (rt-add-immediate name))
  
  (hashq-set! rt-funcs name this)

  )

(begin

  ;; Functions defined directly. (Note that most functions are defined indirectly using the rt-renamefunc macro.)
  
  ;; Basic
  (<rt-func> '+ '<float> '(<float>) #:min-arguments 1)
  (<rt-func> 'rt--/- '<float> '(<float>) #:min-arguments 2)
  (<rt-func> 'rt--/minusoneargument '<float> '(<float>))
  (<rt-func> '* '<float> '(<float>) #:min-arguments 2)
  (<rt-func> 'rt-/// '<double> '(<float>) #:min-arguments 2)
  
  (<rt-func> '1+ '<float> '(<float>))
  (<rt-func> '1- '<float> '(<float>))

  (<rt-func> 'rt-min/MIN '<float> '(<float> <float>))
  (<rt-func> 'rt-max/MAX '<float> '(<float> <float>))

  (<rt-func> 'set! '<void> '(<float> <float>))
  
  (<rt-func> 'not '<int> '(<float>))
  
  
  ;; mus_float_t operations
  (for-each (lambda (func)
	      (primitive-eval `(<rt-func> ',func '<float> '(<float>))))
	    `(sin cos tan acos asin atan exp log log10 sqrt
		  asinh acosh atanh cosh sinh tanh))

  (<rt-func> 'atan2 '<float> '(<float> <float>))
  (<rt-func> 'hypot '<float> '(<float> <float>))

  ;; Bitwise operations
  (<rt-func> 'rt-ash/<< '<int> '(<int> <int>))
  (<rt-func> 'rt-ash/>> '<int> '(<int> <int>))

  ;; Various
  (<rt-func> 'rt-if '<float> '(<int> <float> <float>)) ;; Special form, only return-type is checked
  (<rt-func> 'rt-begin '<float> '(<float>) #:min-arguments 1) ;; Special form, only return-type is checked
  (<rt-func> 'lambda-decl '<float> '()) ;; ???
  (<rt-func> 'rt-while '<void> '(<int> <float>) #:min-arguments 1)
  (<rt-func> 'rt-break/break '<void> '(<int>))
  (<rt-func> 'rt-break/return '<void> '(<float>))
  (<rt-func> 'rt-contbreakvar/jmp_buf '<void> '())
  (<rt-func> 'rt-setjmp/setjmp '<int> '(<jmp_buf>))
  (<rt-func> 'rt-break/longjmp '<void> '(<jmp_buf>))
  (<rt-func> 'rt-continue/longjmp '<void> '(<jmp_buf>))

  (<rt-func> 'rt-mus-any?/mus_xen_p '<mus_any-*> '(<SCM>) #:needs-rt-globals #t)




  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (define-rt-macro def . body)
  (define (is-expand varname)
    (let ((s (symbol->string varname)))
      (if (and (> (string-length s) 7)
	       (string= "expand/" s 0 7 0 7))
	  (string->symbol (string-drop s 7))
	  #f)))
  	   
  (if rt-defining-macros-clears-cache
      (rt-clear-cache!))
  (cond ((not (pair? def))
	 `(define-macro ,(symbol-append rt-macro-prefix def)
	    ,@body))
	((dotted-list? def)
	 (let* ((das-last (cdr (last-pair def)))
		(exp (is-expand das-last)))
	   (if exp
	       `(define-macro ,(cons (symbol-append rt-macro-prefix (car def)) (cdr def))
		  (let ((,exp (map rt-macroexpand ,das-last)))
		    ,@body))
	       `(define-macro ,(cons (symbol-append rt-macro-prefix (car def)) (cdr def))
		  ,@body))))
        ((find keyword? (cdr def))
         `(define-macro ,(symbol-append rt-macro-prefix (car def))
            (labamba ,(cdr def)
              ,@body)))
	(else
	 (let* ((name (car def))
		(new-name (symbol-append rt-macro-prefix name))
		(expand-args '())
		(args (cdr def))
		(clean-args '())
		(optionals '())
		(rest (rt-gensym))
		(min-args (rt-gensym))
		(max-args (rt-gensym))
		(return (rt-gensym)))
	   (for-each (lambda (arg)
		       (if (list? arg)
			   (set! optionals (append! optionals (list (list (car arg) (keyword->symbol (car arg)) (cadr arg)))))
			   (let ((exp (is-expand arg)))
			     (if exp
				 (set! expand-args (append! expand-args (list exp))))))
		       (set! clean-args (append! clean-args (list (if (list? arg) (keyword->symbol (car arg)) arg)))))
		     args)
	   `(define-macro ,(cons new-name rest)
	      (call-with-current-continuation
	       (lambda (,return)
		 (let ((,min-args ,(- (length clean-args) (length optionals)))
		       (,max-args ,(length args))
		       ,@(map (lambda (varname)
				(list varname '(quote rt-undefined)))
			      clean-args)
		       ,@(map (lambda (expvarname)
				(list expvarname '(quote rt-undefined)))
			      expand-args))
		   (define (set-key-args ,rest)
		     (cond ((null? ,rest) #t)
			   ,@(map (lambda (optarg)
				    `((eq? (car ,rest) ,(car optarg))
				      (set! ,(cadr optarg) (cadr ,rest))
				      (set-key-args (cddr ,rest))))
				  optionals)
			   (else
			    (if (not (keyword? (car ,rest)))
				(c-display "Unknown argument \"" (car ,rest) "\" for rt-macro \"" ',name "\":" ,rest)
				(c-display "Unknown key-word argument \"" (car ,rest) "\" for rt-macro \"" ',name "\":" ,rest))
			    (,return #f))))
		   
		   (if (< (length ,rest) ,min-args)
		       (begin
			 (c-display "To few arguments for rt-macro \"" ',name "\". Expected at least" ,min-args ", found " (length ,rest) ":" ,rest)
			 (,return #f)))
		   
		   (if (> (length ,rest) (+ ,min-args (* ,(length optionals) 2)))
		       (begin
			 (c-display "To many arguments for rt-macro \"" ',name "\". Expected at most" ,max-args ", found:" ,rest)
			 (,return #f)))
		   
		   ;;(c-display "rest" ,rest)
		   ;;(c-display "ai" ,(list-ref clean-args 4) (list-ref ,rest 4))
		   
		   ,@(map (lambda (name n)
			    `(set! ,name (list-ref ,rest ,n)))
			  clean-args
			  (iota (- (length clean-args) (length optionals))))

		   (set-key-args (list-tail ,rest ,min-args))

		
		   ,@(map (lambda (optarg)			 
			    `(if (eq? 'rt-undefined ,(cadr optarg))
				 (set! ,(cadr optarg) ,(caddr optarg))))
			  optionals)
		   
		   (let ,(map (lambda (expvarname)
				`(,expvarname (rt-macroexpand ,(symbol-append 'expand/ expvarname))))
			      expand-args)
		     ,@body)))))))))
  
#!    
(define-rt-macro (add . args)
  `(+ ,@args))

(rt-funcall (rt-compile (lambda (a b c)
			  (add a b c)))
	    2 3 4)
=> 9

(define-rt-macro (add a1 a2 (#:a3 3) (#:a4 4) (#:a5 5))  
  `(+ ,a1 ,a2 ,a3 ,a4 ,a5))

(rt-funcall (rt-compile (lambda ()
			  (add 1 2 #:a4 9))))
=> 20
(rt-macroexpand '(add 1 2 :a4 9))
(+ 1 2 3 9 5)
!#



(define-macro (define-rt+-macro . rest)
  `(begin
     (define-rt-macro ,@rest)
     (define-macro ,@rest)))

(define-macro (define-rt/stalin-macro . rest)
  `(begin
     (define-rt-macro ,@rest)
     (define-stalin-macro ,@rest)))


(define-macro (rt-renamefunc rt-name c-name returntype . args)
  (rt-clear-cache!)
  (let ((funcname (symbol-append 'rt- rt-name '/ c-name)))
    (apply <rt-func> (append (list funcname returntype) args))
    (primitive-eval `(define-c-macro ,(cons funcname 'rest )
		       `(,',c-name ,@rest)))
    `(define-rt-macro ,(cons rt-name 'rest)
       `(,',funcname ,@rest))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; define-rt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rt-functions (make-hash-table 219))

(define-macro (define-rt def . body)
  (rt-clear-cache!)
  (hashq-set! rt-functions
	      (car def)
	      `(define ,def
		 ,@body))
  #t) ;; hashq-set! actually returns the value...



  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; rt-ec-functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;rt-ec-functions are eval-c functions which are inserted into the compiled functions
;;when needed.

;; (rt-find-all-funcs '(lambda () (+ (- (aiai 90)))) -> '(+ - aiai)
(define (rt-find-all-funcs term)
  (define ret '())
  (define (find term)
    (cond ((not (list? term)) #f)
	  ((null? term) #f)
	  ((eq? 'let* (car term))
	   (for-each (lambda (vardecl)
		       (find (cdr vardecl)))
		     (cadr term))
	   (find (cddr term)))
	  ((eq? 'lambda (car term))
	   (find (cddr term)))
	  ((symbol? (car term))
	   (push! (car term) ret)
	   (for-each find (cdr term)))
	  (else
	   (for-each find term))))
  (find term)
  ret)


(define rt-ec-functions '())

(define (rt-get-ec-function name)
  (assq name rt-ec-functions))

(define-macro (rt-ec-private-function ret-type name body)
  (define def (map (lambda (arg)
		     (fix-defines
		      (define type (car arg))
		      (define name (cadr arg))
		      (define rt-type (hashq-ref rt-types type))
		      ;;(c-display "type/rt-type: " type rt-type)
		      (if rt-type
			  `(,(-> rt-type c-type) ,name)
			  arg)))
		   (cadr body)))
  (if (hashq-ref rt-types ret-type)
      (set! ret-type (-> (hashq-ref rt-types ret-type) c-type)))

  (set! body `(,(car body) ,def ,@(cddr body)))
  (rt-clear-cache!)
  (let ((dependents (rt-find-all-funcs body))
	(old (assq name rt-ec-functions)))
    (if old
	`(set-cdr! (assq ',name rt-ec-functions) (list '(,@dependents)
						       '(,ret-type ,name ,body)))
	`(set! rt-ec-functions (append! rt-ec-functions
					(list (list ',name
						    '(,@dependents)
						    '(,ret-type ,name ,body))))))))
(define-macro (define-rt-ec ret-type name body)
  (fix-defines
   (when (not (eq? 'lambda (car body)))
     (c-display "Illegal body for define-rt-ec: " body)
     (throw 'anerror))
   (define def (cadr body))
   (cond ((null? def)
	  `(begin
	     (rt-ec-private-function ,ret-type ,name ,body)
	     (<rt-func> ',name ',ret-type '() :needs-rt-globals #f)))
	 ((or (equal? '(unquote rt-globalvardecl) (car def))
	      (equal? '(<struct-RT_Globals-*> rt_globals) (car def)))
	  `(begin
	     (rt-ec-private-function ,ret-type ,name ,body)
	     (<rt-func> ',name ',ret-type ',(map car (cdr def)) :needs-rt-globals #t)))
	 (else
	  `(begin
	     (rt-ec-private-function ,ret-type ,name ,body)
	     (<rt-func> ',name ',ret-type ',(map car def) :needs-rt-globals #f))))))


#!
(pretty-print
 (macroexpand '(rt-ec-private-function <nonstatic-void> rt_error (lambda (,rt-globalvardecl (<char-*> msg))
								   (fprintf stderr (string "RT RUNTIME ERROR: %s. (Removing instrument)\\n") msg)
								   (set! rt_globals->remove_me 1)
								   ,(if (rt-is-safety?)
									'(longjmp rt_globals->engine->error_jmp 5)
									"/* */")))))


(pretty-print
 (macroexpand '(rt-ec-private-function <void> hello1 (lambda ((<bus> ai))
					       #t))))
(pretty-print
 (macroexpand '(define-rt-ec <void> hello1 (lambda (,rt-globalvardecl)
					       #t))))
(pretty-print
 (macroexpand '(define-rt-ec <void> hello1 (lambda (,rt-globalvardecl (<void> gakk))
					       #t))))
!#




(begin
  
  (rt-ec-private-function <nonstatic-void> rt_error (lambda (,rt-globalvardecl (<char-*> msg))
						      (rt_debug (string "RT RUNTIME ERROR: %s. (Removing instrument)\\n") msg)
						      (set! rt_globals->remove_me 1)
						      ,(if (rt-is-safety?)
							   '(longjmp rt_globals->engine->error_jmp 5)
							   "/* */")))
  (<rt-func> 'rt_error '<void> '(<char-*>) #:needs-rt-globals #t)
  (define-rt-macro (rt-error . rest)
    `(rt_error ,@rest))
  
  ;; scm_to_double
  (define-rt-ec <double> rt_scm_to_double (lambda (,rt-globalvardecl (<SCM> name))
					   (if (SCM_INUMP name)
					       (return (SCM_INUM name))
					       (if (SCM_REALP name)
						   (return (SCM_REAL_VALUE name))
						   (begin
						     (rt_error rt_globals (string "Variable is not a number (to_double)"))
						     (return 0))))))
  

  ;;; scm_to_float
  (define-rt-ec <float> rt_scm_to_float (lambda (,rt-globalvardecl (<SCM> name))
					 (if (SCM_INUMP name)
					     (return (SCM_INUM name))
					     (if (SCM_REALP name)
						 (return (SCM_REAL_VALUE name))
						 (begin
						   (rt_error rt_globals (string "Variable is not a number (to_float)"))
						   (return 0))))))
  
  
  ;; scm_to_int
  (define-rt-ec <int> rt_scm_to_int (lambda (,rt-globalvardecl (<SCM> name))
				     (if (SCM_INUMP name)
					 (return (SCM_INUM name))
					 (if (SCM_REALP name)
					     (return (SCM_REAL_VALUE name))
					     (begin
					       (rt_error rt_globals (string "Variable is not a number (to_int)"))
					       (return 0))))))


  ;; scm_to_mus_any
  (define-rt-ec <mus_any-*> rt_scm_to_mus_any (lambda (,rt-globalvardecl (<SCM> name))
						  (if (mus_xen_p name)
						      (return (cast <void-*> (XEN_TO_MUS_ANY name)))
						      (if (not (SCM_SMOB_PREDICATE rt_readin_tag name))
							  (begin
							    (rt_error rt_globals (string "Variable is not a CLM generator or rt-readin generator"))
							    (return NULL))
							  (return (cast <void-*> (SCM_SMOB_DATA name)))))))
  


  ;; create-thread
  (define-rt-ec <void> rt_create_thread (lambda ((<ThreadFunc> func))
					 "pthread_t _rt_thread={0}"
					 (<int> isrunning 0)
					 (<void-*> threadfunc (lambda ((<void-*> arg))
								(<ThreadFunc> dasfunc arg)
								(set! isrunning 1)
								(dasfunc)
								(return NULL)))
					 (pthread_create &_rt_thread NULL threadfunc func)
					 (while (not isrunning) ;; I'm not quite sure why...
						(usleep 50))
					 ))
  (define-rt-macro (create-thread thunk)
    `(rt_create_thread ,thunk))
  
  
  (rt-ec-private-function <void-*> rt_alloc_old (lambda (,rt-globalvardecl (<int> size))
					      (let* ((ret <void-*> rt_globals->allocplace)
						     (alignment <int> (sizeof <long>))
						     (new <char-*> (+= rt_globals->allocplace size)))
						"new = (char *) (((unsigned long) new + alignment - 1) & - alignment)"
						(set! rt_globals->allocplace new)
						,(if (rt-is-safety?)
						     `(if (>= rt_globals->allocplace rt_globals->allocplace_end)
							  (rt_error rt_globals (string "Out of memory when calling rt_alloc.")))
						     "/* */")
						(return ret))))

  (rt-ec-private-function <void*> rt_alloc (lambda (,rt-globalvardecl
						    (<int> size))
					     (return (tar_alloc rt_globals->heap size))))

  (rt-ec-private-function <void*> rt_alloc_atomic (lambda (,rt-globalvardecl
                                                           (<int> size))
                                                    (return (tar_alloc_atomic rt_globals->heap size))))

  (rt-ec-private-function <void-*> rt_alloc_zero (lambda (,rt-globalvardecl (<int> size))
						   (let* ((ret <void-*> (rt_alloc_atomic rt_globals size)))
						     ;;(memset ret 0 size)
						     (return ret))))
  
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (=> object das-method . rest)
  (define method (keyword->symbol das-method))
  (define object-decomposed (map string->symbol (string-split (symbol->string object) #\:)))
  (let ()
    (define struct-name (car object-decomposed))
    (define object-name (if (null? (cdr object-decomposed))
			    (car object-decomposed)
			    (cadr object-decomposed)))
    `(,(append-various 'access- struct-name ":" method) ,object-name ,@rest)))


(define-rt-macro (setter!-=> object das-method . rest)
  (define method (keyword->symbol das-method))
  (define object-decomposed (map string->symbol (string-split (symbol->string object) #\:)))
  (let ()
    (define struct-name (car object-decomposed))
    (define object-name (if (null? (cdr object-decomposed))
			    (car object-decomposed)
			    (cadr object-decomposed)))
    `(,(append-various 'setter!- struct-name ":" method) ,object-name ,@rest)))

(define-rt-macro (=> object das-method)
  (if (or (not (symbol? object))
	  (not (keyword? das-method)))
      (begin
	(c-display "Syntax error" `(=> ,object ,das-method))
	(throw 'syntax-error)))
  (let ()
    (define method (keyword->symbol das-method))
    (define object-decomposed (map string->symbol (string-split (symbol->string object) #\:)))
    (let ()
      (define struct-name (car object-decomposed))
      (define object-name (if (null? (cdr object-decomposed))
			      (car object-decomposed)
			      (cadr object-decomposed)))
      `(,(append-various 'getter- struct-name ":" method) ,object-name))))


(define-macro (define-rt-something-struct something name . das-slots)
  (define name-name (rt-gensym))
  (define val-name (rt-gensym))
  (define slots '())
  
  (for-each (lambda (slot)
	      (if (keyword? slot)
		  (push-back! (list (append-various slot) 0) slots)
		  (set-cdr! (last slots) (list slot))))
	    das-slots)

  (let ((slot-names (map car slots)))
    `(begin

       ;; guile
       (define* (,(symbol-append 'make- name) :key ,@slots)
	 (,something ,@slot-names))
       ,@(let ((i -1))
	   (map-in-order (lambda (slot)
			   (set! i (1+ i))
			   `(define ,(append-various 'access- name ":" slot)
			      (make-procedure-with-setter
			       (lambda (,name-name)
				 (,(symbol-append something '-ref) ,name-name ,i))
			       (lambda (,name-name ,val-name)
				 (,(symbol-append something '-set!) ,name-name ,i ,val-name)))))
			 slot-names))

       ;; rt
       (define ,(rt-gensym)
	 (list
	  ,@(let ((i -1))
	      (map-in-order (lambda (slot)
			      (set! i (1+ i))
			      `(define-rt (,(append-various 'setter!- name ":" slot) ,name-name ,val-name)
				 (declare (<float> ,val-name))
				 (,(symbol-append something '-set!) ,name-name ,i ,val-name)))
			    slot-names))
	  ,@(let ((i -1))
	      (map-in-order (lambda (slot)
			      (set! i (1+ i))
			      `(define-rt (,(append-various 'getter- name ":" slot) ,name-name)
				 (,(symbol-append something '-ref) ,name-name ,i)))
			    slot-names)))))))

(define-macro (define-rt-vct-struct name . das-slots)
  `(define-rt-something-struct vct ,name ,@das-slots))

(define-macro (define-rt-vector-struct name . das-slots)
  `(define-rt-something-struct vector ,name ,@das-slots))


#!
(define-rt-vct-struct str
  :a 1
  :b)
(set! (=> str:str) 200)
(set! (=> str :b) 200)
(=> str:str)
(=> str :b)

(macroexpand '(=> str:str :b))
(macroexpand '(=> str :b))
(rt-macroexpand '(=> str :b))
(rt-macroexpand '(set! (=> str :b) 200))
!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Various Macros and functions ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rt-macro (inc! var how-much)
  `(begin
     (set! ,var (+ ,how-much ,var))
     ,var))

(define-c-macro (the type somethingmore)
  (let ((c-type (hashq-ref rt-types type)))
    (if c-type
	`(cast ,(-> c-type c-type) ,somethingmore)
	`(cast ,type ,somethingmore))))

(define-rt-macro (inexact->exact z)
  `(the <int> ,z))

(define-rt-macro (exact->inexact z)
  `(the <float> ,z))

(define-rt-macro (exact? expand/var)
  (cond ((number? var) (if (exact? var)
			   1
			   0))
	((rt-immediate? var)
	 `(is-type? <int> ,var))
	(else
	 (let ((s (rt-gensym)))
	   `(let ((,s ,var))
	      (is-type? <int> ,s))))))

(define-rt-macro (inexact? var)
  `(not (exact? ,var)))

(define-rt-macro (number? var)
  (cond ((number? var) 1)
	((rt-immediate? var)
	 `(or (is-type? <int> ,var)
	      (is-type? <float> ,var)
	      (is-type? <double> ,var)))
	(else
	 (let ((s (rt-gensym)))
	   `(let ((,s ,var))
	      (or (is-type? <int> ,s)
		  (is-type? <float> ,s)
		  (is-type? <double> ,s)))))))

(define-rt-macro (string? var)
  (cond ((stringr? var) 1)
	((rt-immediate? var)
	 `(is-type? <char-*> ,var))
	(else
	 (let ((s (rt-gensym)))
	   `(let ((,s ,var))
	      (is-type? <char-*> ,s))))))
  
(define-rt-macro (- firstarg . rest)
  (if (null? rest)
      `(rt--/minusoneargument ,firstarg)
      `(rt--/- ,firstarg ,@rest)))
(define-c-macro (rt--/minusoneargument a)
  `(- ,a))
(define-c-macro (rt--/- . rest)
  `(- ,@rest))


(rt-renamefunc expt pow <float> (<float> <float>))
(rt-renamefunc abs fabsf <float> (<float>))

(rt-renamefunc floor floorf <float> (<float>))
(rt-renamefunc ceiling ceilf <float> (<float>))

(rt-renamefunc logand & <int> (<int> <int>))
(rt-renamefunc logior | <int> (<int> <int>))
(rt-renamefunc lognot ~ <int> (<int>))
  
(rt-renamefunc remainder % <int> (<int> <int>))


;; < > <= >= =
(for-each (lambda (op)
	    (let ((rt-op (symbol-append 'rt- op))
		  (rt-op2 (symbol-append 'rt-2 op))
		  (rt-op3 (symbol-append 'rt-3 op)))
	      (<rt-func> rt-op '<int> '(<float> <float>))
	      (primitive-eval `(define-c-macro ,(cons rt-op 'rest)
				 `(,',op ,@rest)))
	      (primitive-eval `(define-rt-macro (,rt-op2 expand/a expand/b)
				 (if (and (number? a)
					  (number? b))
				     (if (,op a b)
					 1
					 0)
				     `(,',rt-op ,a ,b))))
	      (primitive-eval `(define-rt-macro (,rt-op3 . rest)
				 (let ((newvars '())
				       (usevars '())
				       (body #f))
				   (for-each (lambda (var)
					       (if (rt-immediate? var)
						   (push! var usevars)
						   (let ((newvarname (rt-gensym)))
						     (push! (list newvarname var) newvars)
						     (push! newvarname usevars))))
					     rest)
				   (set! body `(and ,@(map (lambda (first second)
							     `(,',rt-op2 ,first ,second))
							   (reverse (cdr usevars))
							   (cdr (reverse usevars)))))
				   (if (not (null? newvars))
				       `(let ,(reverse newvars)
					  ,body)
				       body))))
				      
	      (primitive-eval `(define-rt-macro (,op . rest)
				 (if (<= (length rest) 1)
				     1
				     (if (= (length rest) 2)
					 `(,',rt-op2 ,@rest)
					 `(,',rt-op3 ,@rest)))))))
								 
	  '(< > <= >= =))

(define-c-macro (rt-= a b)
  `(== ,a ,b))


(define-rt-macro (zero? expand/z)
  (if (number? z)
      (if (= 0 z) 1 0)
      `(= 0 ,z)))

(define-rt-macro (positive? expand/z)
  (if (number? z)
      (if (> z 0) 1 0)
      `(> ,z 0)))

(define-rt-macro (negative? expand/z)
  (if (number? z)
      (if (< z 0) 1 0)
      `(< ,z 0)))


  
;; modulo-logic picked up from snd-run.c
(define-rt-macro (modulo a b)
  (let ((x (rt-gensym))
	(y (rt-gensym))
	(z (rt-gensym)))
    `(let* ((,x (the <int> ,a))
	    (,y (the <int> ,b))
	    (,z (the <int> (remainder ,x ,y))))
       (if (or (and (negative? ,y)
		    (positive? ,z))
	       (and (positive? ,y)
		    (negative? ,z)))
	   (+ ,z ,y)
	   ,z))))
#!
(define-rt-macro (modulo a b)
  (let ((x (rt-gensym))
	(y (rt-gensym))
	(z (rt-gensym)))
    `(let* ((,x ,a)
	    (,y ,b)
	    (,z (remainder ,x ,y)))
       (if (or (and (negative? ,y)
		    (positive? ,z))
	       (and (positive? ,y)
		    (negative? ,z)))
	   (+ ,z ,y)
	   ,z))))
!#

(define-c-macro (rt-/// . rest)
  `(/ ,@rest))
(define-rt-macro (quotient a b)
  `(rt-/// (the <int> ,a) (the <int> ,b)))
(define-rt-macro (/% a b)
  `(quotient ,a ,b))
(define-rt-macro (/ a . b)
  (cond ((null? b)
	 `(rt-/// 1.0 (the <float> ,a)))
	((not (null? (cdr b)))
	 `(/ (rt-/// (the <float> ,a) (the <float> ,(car b))) ,@(cdr b)))
	(else
	 `(rt-/// (the <float> ,a) (the <float> ,(car b))))))

#!
(define-rt-macro (/ a . b)
  (cond ((null? b)
	 `(rt-/// 1.0 (the <double> ,a)))
	((not (null? (cdr b)))
	 `(/ (rt-/// (the <double> ,a) (the <double> ,(car b))) ,@(cdr b)))
	(else
	 `(rt-/// (the <double> ,a) (the <double> ,(car b))))))
!#


(define-rt-macro (odd? n)
  `(remainder ,n 2))
(define-rt-macro (even? n)
  `(not (odd? ,n)))


	 
;; truncate-logic picked up from snd-run.c
(define-rt-macro (truncate expand/a)
  (if (rt-immediate? a)
      `(if (negative? ,a)
	   (- (floor (- ,a)))
	   (floor ,a))
      (let ((x (rt-gensym))  )
	`(let ((,x ,a))
	   (if (negative? ,x)
	       (- (floor (- ,x)))
	       (floor ,x))))))

;; round-logic picked up from snd-run.c (I'm not sure this one works correctly, because of the "/"-casting)
(define-rt-macro (round a)
  (let ((plus_half (rt-gensym))
	(result (rt-gensym)))
    `(let* ((,plus_half (+ ,a 0.5))
	    (,result (floor ,plus_half)))
       (if (and (= ,plus_half ,result)
		(not (= (/ ,plus_half 2) (floor (/ ,plus_half 2)))))
	   (- ,result 1)
	   ,result))))
       

;;logxor-logic picked up from snd-run.c
(define-rt-macro (logxor expand/x expand/y)
  (if (and (number? x)
	   (number? y))
      (logxor x y)
      (if (rt-immediate? x y)
	  `(logand (lognot (logand ,x ,y))
		   (logior ,x ,y))
	  (let ((a (rt-gensym))
		(b (rt-gensym)))
	    `(let* ((,a ,x)
		    (,b ,y))
	       (logand (lognot (logand ,a ,b))
		       (logior ,a ,b)))))))


;;ash-logic picked up from snd-run.c
(define-rt-macro (ash expand/a expand/b)
  (if (and (number? a)
	   (number? b))
      (ash a b)
      (if (rt-immediate? a b)
	  `(if (>= ,b 0)
	       (rt-ash/<< ,a ,b)
	       (rt-ash/>> ,a ,(if (number? b)
				  (- b)
				  `(- ,b))))
	  (let ((arg1 (rt-gensym))
		(arg2 (rt-gensym)))
	    `(let* ((,arg1 ,a)
		    (,arg2 ,b))
	       (if (>= ,arg2 0)
		   (rt-ash/<< ,arg1 ,arg2)
		   (rt-ash/>> ,arg1 (- ,arg2))))))))
(define-c-macro (rt-ash/<< a b)
  `(<< ,a ,b))
(define-c-macro (rt-ash/>> a b)
  `(>> ,a ,b))

(rt-renamefunc mus-random mus_random <float> (<float>))
(rt-renamefunc random mus_frandom <float> (<float>))
(rt-renamefunc random mus_irandom <float> (<float>))
(define-rt-macro (random expand/val)
  `(if (exact? ,val)
       (rt-random/mus_irandom ,val)
       (rt-random/mus_frandom ,val)))


(define-rt-macro (max . expand/rest)
  (define (expand rest)
    (if (= 1 (length rest))
	(car rest)
	`(rt-max/MAX ,(expand (cdr rest)) ,(car rest))))  
  (if (null? rest)
      (begin
	(c-display "Error. \"max\" expect at least one argument: (max).")
	#f)
      (if (= 1 (length rest))
	  (car rest)
	  (let ((number-args '())
		(rest-args '())
		(new-args '())
		(varnames '()))
	    (for-each (lambda (arg)
			(if (rt-immediate? arg)
			    (begin
			      (push! arg number-args )
			      (push! arg new-args))
			    (begin
			      (push! arg rest-args)
			      (push! (rt-gensym) varnames)
			      (push! (car varnames) new-args))))
		      rest)
	    (set! number-args (reverse! number-args))
	    (set! rest-args (reverse! rest-args))
	    (set! new-args (reverse! new-args))
	    (set! varnames (reverse! varnames))
	    (if (null? rest-args)
		(expand number-args)
		`(let* ,(map list
			     varnames
			     rest-args)
		   ,(expand new-args)))))))
(define-c-macro (rt-max/MAX a b)
  `(MAX ,a ,b))

(define-rt-macro (min . expand/rest)
  (define (expand rest)
    (if (= 1 (length rest))
	(car rest)
	`(rt-min/MIN ,(expand (cdr rest)) ,(car rest))))  
  (if (null? rest)
      (begin
	(c-display "Error. \"min\" expect at least one argument: (min)")
	#f)
      (if (= 1 (length rest))
	  (car rest)
	  (let ((number-args '())
		(rest-args '())
		(new-args '())
		(varnames '()))
	    (for-each (lambda (arg)
			(if (rt-immediate? arg)
			    (begin
			      (push! arg number-args )
			      (push! arg new-args))
			    (begin
			      (push! arg rest-args)
			      (push! (rt-gensym) varnames)
			      (push! (car varnames) new-args))))
		      rest)
	    (set! number-args (reverse! number-args))
	    (set! rest-args (reverse! rest-args))
	    (set! new-args (reverse! new-args))
	    (set! varnames (reverse! varnames))	    
	    (if (null? rest-args)
		(expand number-args)
		`(let* ,(map list
			     varnames
			     rest-args)
		   ,(expand new-args)))))))

(define-c-macro (rt-min/MIN a b)
  `(MIN ,a ,b))

  
(define-rt-macro (and . expand/rest)
  (define (expand ret rest)
    (if (null? rest)
	ret
	(let ((var (car rest)))
	  (if (number? var)
	      (if (= 0 var)
		  0
		  (expand var (cdr rest)))
	      (if (rt-immediate? var)
		  `(if ,var
		       ,(expand var (cdr rest))
		       0)
		  (let ((varname (rt-gensym)))
		    `(let* ((,varname ,var))
		       (if ,varname
			   ,(expand varname (cdr rest))
			   0))))))))

  (expand 1 rest))


(define-rt-macro (or . expand/rest)
  (define (expand rest)
    (if (null? rest)
	0
	(let ((var (car rest)))
	  (if (number? var)
	      (if (= 0 var)
		  (expand (cdr rest))
		  var)
	      (if (rt-immediate? var)
		  `(if ,var
		       ,var
		       ,(expand (cdr rest)))
		  (let ((varname (rt-gensym)))
		    `(let ((,varname ,var))
		       (if ,varname
			   ,varname
			   ,(expand (cdr rest))))))))))

  (expand rest))




(<rt-func> 'rt-or/|| '<int> '(<float>) #:min-arguments 1)
(define-c-macro (rt-or/|| . rest)
  (if (null? (cdr rest))
      (car rest)
      `(|| ,@rest)))

(<rt-func> 'rt-and/&& '<int> '(<float>) #:min-arguments 1)
(define-c-macro (rt-and/&& . rest)
  (if (null? (cdr rest))
      (car rest)
      `(&& ,@rest)))

;; if is a macro, rt-if is a special form
(define-rt-macro (if a b . c)
  (if (> (length c) 1)
      (begin
	(apply c-display (append (list "Too many arguments for if:" a b) c))
	#f)
      (let ((ae (rt-macroexpand a)))
	(if (number? ae)
	    (if (= 0 ae)
		(if (null? c)
		    '(rt-dummy/dummy)
		    (car c))
		b)
	    (begin
	      (if (list? a)
		  (cond ((eq? 'or (car a))
			 (set! ae (cons 'rt-or/|| (cdr a))))
			((eq? 'and (car a))
			 (set! ae (cons 'rt-and/&& (cdr a))))))
	      (if (null? c)
		  `(rt-if ,ae ,b (rt-dummy/dummy))
		  `(rt-if ,ae ,b ,(car c))))))))

(define-c-macro (rt-if . rest)
  `(?kolon ,@rest))

(define-rt-macro (when a . b)
  `(if ,a
       (begin
	 ,@b)))

(define (rt-cond->if terms)
  (let ((term (car terms)))
    (if (and (symbol? (car term))
	     (eq? 'else (car term)))
	(cons 'begin_p (cdr term))
	(if (not (null? (cdr terms)))
	    (list 'if (car term)
		  (append (list 'begin_p) (cdr term))
		  (rt-cond->if (cdr terms)))
	    (list 'if (car term)
		  (append (list 'begin_p) (cdr term)))))))

(define-rt-macro (cond . terms)
  (rt-cond->if terms))


;; Note the 'else hack here. Unless the case block has an else, '(else 0) is added at the end. (@$#@! static typing)
(define-rt-macro (case key . terms)
  (let ((das-key (rt-gensym)))
    `(let ((,das-key ,key))
       (cond ,@(map (lambda (term)
		      (let ((datums (car term))
			    (expr (cdr term)))
			(if (eq? 'else datums)
			    term
			    `((or ,@(map (lambda (datum)
					   `(= ,das-key ,datum))
					 datums))
			      ,@expr))))
		    (if (eq? 'else (car (last terms)))
			terms
			(append terms (list '(else 0)))))))))

;; begin and begin_p are macros, while rt-begin is a special form
(define-rt-macro (begin . rest)
  `(begin_p ,@rest))

(define-rt-macro (begin_p . rest)
  `(rt-begin ,@rest))

(define-c-macro (rt-begin . rest)
  `(begin_p ,@rest))



#|
(define-rt-macro (while test . body)
  (let ((whilefunc (rt-gensym))
	(dasfunc (rt-gensym)))
    `(let* ((,whilefunc (lambda ()
			 (let* ((_rt_breakcontsig 0)
				(,dasfunc (lambda ()
					    (rt-while ,test
						      (begin
							,@body)))))
			   (if (< (rt-setjmp/setjmp _rt_breakcontsig) 2)
			       (,dasfunc))))))
       (,whilefunc))))


(define-rt-macro (while test . body)
  `(let loop ()
     (if ,test
	 (begin
	   ,@body
	   (loop)))))
;;  `(rt-while ,test ,@body))

(define-rt-macro (while test . body)
  `(let ((bodyfunc (lambda ()
		     ,@body)))
     (rt-while ,test
	       (bodyfunc))))

;	       (begin
;		 ,@body))

|#


(define-rt-macro (while test . body)
  (let ((whilefunc (rt-gensym)))
    `(let ((,whilefunc (lambda ()
			 (rt-while ,test
				   (begin
				     ,@body)))))
       (,whilefunc))))







#|
(define-rt-macro (while test . body)
  (define (rec-search term)
    (call-with-current-continuation
     (lambda (return)
       (define (search term)
	 (cond ((eq? '_rt_breakcontsig term)
		(return #t))
	       ((list? term)
		(for-each search term))))
       (search term)
       #f)))
  (let ((test (rt-macroexpand test))
	(body (map rt-macroexpand body)))
    (if (rec-search (cons test body))
	`(let* ((_rt_breakcontsig 0))
	   (if (< (rt-setjmp/setjmp _rt_breakcontsig) 2)
	       (rt-while ,test
			 ,@body)))
	`(rt-while ,test
		   ,@body))))
|#

(define-c-macro (rt-while test . body)
  `(while ,test ,@body))
(define-c-macro (rt-setjmp/setjmp das-sig)
  `(setjmp ,das-sig))

(rt-renamefunc rt-longjmp longjmp <void> (<jmp_buf> <int>))

(define-rt-macro (catch throw-name code . throw-code)
  (set! throw-name (<_> '_rt_breakcontsig throw-name))
  `(let ((,throw-name 0))
     (if (= 0 (rt-setjmp/setjmp ,throw-name))
	 (,code)
	 ,@(if (or (null? throw-code)
		   (not (car throw-code)))
	       '()
	       `((,(car throw-code)))))))

(define-rt-macro (throw throw-name)
  (set! throw-name (<_> '_rt_breakcontsig throw-name))
  `(rt-longjmp ,throw-name 1))

#!
(<rt-play> (lambda ()
	     (catch aiai
		    (lambda ()
		      (printf "gakk gakk\\n")
		      (throw aiai)
		      (printf "gakk gakk2\\n")))
	     (printf "Yes, I got here.\\n")
	     (remove-me)))
(<rt-play> (lambda ()
	     (catch aiai
		    (lambda ()
		      (printf "gakk gakk\\n")
		      (throw aiai)
		      (printf "gakk gakk2\\n"))
		    #f)
	     (printf "Yes, I got here.\\n")
	     (remove-me)))
(<rt-play> (lambda ()
	     (catch aiai
		    (lambda ()
		      (printf "gakk gakk\\n")
		      (throw aiai)
		      (printf "gakk gakk2\\n"))
		    (lambda ()
		      (printf "Yes, I got here. Certainly no gakk gakk2.\\n")))
	     (remove-me)))
!#

#|
;; Implementaions for these are just hacks.
(define-rt-macro (break)
  `(rt-break/longjmp _rt_breakcontsig))
(define-c-macro (rt-break/longjmp das-sig)
  `(longjmp ,das-sig 2))
(define-rt-macro (continue)
  `(rt-continue/longjmp _rt_breakcontsig))
(define-c-macro (rt-continue/longjmp das-sig)
  `(longjmp ,das-sig 1))
|#


(define-rt-macro (do variables test . commands)
  `(let ,(map (lambda (variable) (list (car variable) (cadr variable)))
	      variables)
     (while (not ,(car test))
	    ,@commands
	    ,@(map (lambda (variable)
		     `(set! ,(car variable) ,(caddr variable)))
		   (remove (lambda (var) (null? (caddr var)))
			   variables)))
     ,@(cdr test)))

(<rt-func> 'rt-add-int! '<int> '(<int> <int>))
(define-c-macro (rt-add-int! var inc)
  (<-> (eval-c-parse var)
       (if (number? inc)
	   (if (= 1 inc)
	       "++"
	       (if (= -1 inc)
		   "--"
		   (<-> "+="  (eval-c-parse inc))))
	   (<-> "+="  (eval-c-parse inc)))))

(define-rt-macro (range varname start end . body)
  (let ((das-end (rt-gensym))
    	(das-add (rt-gensym))
	(whilefunc (rt-gensym)))
    (if (list? varname)
	(begin
	  (set! das-add (cadr varname))
	  (set! varname (car varname))))
    (if (or (number? das-add)
	    (and (number? start)
		 (number? end)))
	(let ((das-add (if (number? das-add)
			   das-add
			   (if (> end start) 1 -1))))
	  `(let* ((,whilefunc (lambda (,varname ,das-end)
				(declare (<int> ,varname ,das-end))
				(rt-while (not (= ,varname ,das-end))
					  ,@body
					  (rt-add-int! ,varname ,das-add)))))
	     (,whilefunc ,start ,end)))
	`(let* ((,das-end ,end)
		(,varname ,start))
	   (declare (<int> ,das-end ,varname))
	   (if (> ,das-end ,start)
	       (while (not (= ,varname ,das-end))
		      ,@body
		      (rt-add-int! ,varname 1))
	       (while (not (= ,varname ,das-end))
		      ,@body
		      (rt-add-int! ,varname -1)))))))

(define-rt-macro (rt-range2 varname start end inc . body)
  `(range ,(list varname inc) ,start ,end ,@body))

  
#!
(rt-funcall (rt-2 '(lambda (s)
		     (range i -2 0
			    (printf "%f\\n" (+ s i)))
		     (range i 5 10
			    (printf "%f\\n" (+ s i)))))
	    5)

!#

#|
;; This is bad. Return-values from continuations shouldn't be limited to floats only. (Type is set immediately for let-variables)
;; void-returning functions aren't supported.
(define-rt-macro (call-with-current-continuation func)
  (let ((res (rt-gensym))
	(thunk (rt-gensym)))
    `(let* ((_rt_breakcontsig 0)
	    (,res 0.0)            
	    (,(caadr func) (lambda (retval)
			     (set! ,res retval)
			     (break)))
	    (,thunk (lambda ()
		      ,@(cddr func))))
       (if (= (rt-setjmp/setjmp _rt_breakcontsig) 0)
	   (set! ,res (,thunk)))
       ,res)))
|#


	    
(define-rt-macro (printf string . rest)
  `(begin
     (debug "Warning: printf might cause snd-rt to crash. Use \"debug\" instead.")
     (rt-printf/fprintf ,string ,@rest)))
(define-c-macro (rt-printf/fprintf string . rest)
;;  `(listener_append ,string))
  `(fprintf stderr ,string ,@rest))
(<rt-func> 'rt-printf/fprintf '<int> '(<char-*> <float>) #:min-arguments 1)


(define-rt-macro (infix-f t)
  t)
(define-rt-macro (infix-b t)
  `(* ,*rt-block-size* ,t))
(define-rt-macro (infix-ms t)
  `(inexact->exact (floor (/ (* ,t ,(rte-samplerate))
                             1000))))
(define-rt-macro (infix-s t)
  `(* ,(inexact->exact (rte-samplerate)) ,t))
(define-rt-macro (infix-m t)
  `(* ,(* 60 (inexact->exact (rte-samplerate))) ,t))
(define-rt-macro (infix-h t)
  `(* ,(* 60 60 (inexact->exact (rte-samplerate))) ,t))


(define-macro (infix-f t)
  `(/ t (rte-samplerate)))
(define-macro (infix-b t)
  `(/ (* ,*rt-block-size* ,t) (rte-samplerate)))
(define-macro (infix-ms t)
  `(/ ,t 1000))
(define-macro (infix-s t)
  t)
(define-macro (infix-m t)
  `(* 60 ,t))
(define-macro (infix-h t)
  `(* 60 60 ,t))



;; let is implemented as a macro to easier be able to support named let. The real name for non-named let is rt-let/let*
(define-rt-macro (let a b . c)
  (if (not (symbol? a))
      `(rt-let/let* ,a ,b ,@c)
      `(letrec ((,a (lambda ,(map car b)   ;; Named let
		      ,@c)))
	 (,a ,@(map cadr b)))))


(<rt-func> 'rt-dummy/dummy '<void> '())
(define-c-macro (rt-dummy/dummy)
  "/* */")

(define-c-macro (rt-remove-me)
  "rt_globals->remove_me=1")
(rt-renamefunc remove-me rt-remove-me <void> ())

(define-rt (scale x x1 x2 y1 y2)
  (declare (<float> x x1 x2 y1 y2))
  (+ y1 (/ (* (- x x1)
	      (- y2 y1))
	   (- x2 x1))))


;; Oh, horror. This implemenation is ugly...
;; '(a b c) -> '( (a b c) (a c) (a b) (a) (b c) (b) (c) ())
(define (permutate alist)
  (cond ((null? alist)
	 (list '()))
	((<= (length alist) 1)
	 (list alist '()))
	(else
	 (let ((ret (list (list (car alist))))
	       (alist2 (list (list (last alist)))))
	   (for-each (lambda (s)
		       (push! (cons s (car alist2))
			      alist2))
		     (cdr (reverse (cdr alist))))
	   ;;(set! alist2 (append! alist2 (list '())))
	   (for-each (lambda (s rest)
		       (set! ret (append ret (permutate rest)))
		       (for-each (lambda (r)				
				   (push! (cons s r) ret))
				 (permutate rest)))
		     alist
		     alist2)
	   (delete-duplicates! ret)))))


#!
(begin
  (define a-val a)
  (define b-val b)
  (cond ((and (rt-immidiate? a)
	      (rt-immidiate? b))
	 (let ((a (gensym))
	       (b (gensym)))
	   ...))
	((and (rt-immidiate? a)
	      (not (rt-immidiate? b)))
	 (let ((a (gensym)))
	   ...))
	((and (not (rt-immidiate? a))
	      (rt-immidiate? b))
	 (let ((b (gensym)))
	   ...))
	(else
	 (begin
	   ...))))
!#

(define-macro (rt-automate-immediate . rest)
  (let* ((vars (c-butlast rest))
	 (tempvars (map (lambda (var)
			  (cons var (rt-gensym)))
			vars))
	 (body (last rest))
	 (perm (permutate vars)))
    (c-display "vars/body" vars body perm)
    `(cond ,@(map (lambda (immediates)
		    (if (null? immediates)
			`(else ,body)
			`( (and ,@(map (lambda (var)
					 (if (member var immediates)
					     `(rt-immediate? ,var)
					     `(not (rt-immediate? ,var))))
				       vars))
			   (let ,(append (map (lambda (im)
						`(list ,(cdr (assq im tempvars)) ,im))
					      immediates)
					 (map (lambda (im)
						`(list ,im `(rt-gensym)))
					      immediates))
			     `(let ,(map (lambda (im)
					   (c-display "tempvars:" ,tempvars)
					   (let ((n (cdr (assq im ,tempvars))))
					     (list im n)))
					 ',immediates)
				,,body)))))
		  perm))))

#!
(define-macro (ai a b)
  (rt-automate-immediate a
			 `(+ ,a ,b)))

(macroexpand '(ai 9 3))

(macroexpand '(rt-automate-immediate a
				     `(+ 2 5)))
(begin `,`(+ 2 3))

(cond ((and (rt-immediate? a))
       (let ((rt_gen45 a)
	     (a (rt-gensym)))
	 (quasiquote (let (unquote (map (lambda (im)
					  (let ((n (cdr (assq im ((a . rt_gen45))))))
					    (quasiquote (list (unquote im) (unquote n)))))
					(quote (a))))
		       (unquote (quasiquote (+ 2 5)))))))
      (else
       (quasiquote (+ 2 5))))


(cond ((and (rt-immediate? a))
       (let ((rt_gen24 a)
	     (a (rt-gensym))
	     (varnames (map (lambda (im)
			      (let ((n (cdr (assq im tempvars))))
				(list im n)))
			    (a))))
	 (quasiquote (let (unquote (map (lambda (s) (list (car s) (cadr s))) varnames)) (unquote (quasiquote (+ (unquote a) 5))))))) (else (quasiquote (+ (unquote a) 5))))

(let ((newbody `(+ ,a 5)))
  (cond ((and (rt-immediate? a))
	 (let ((rt_gen152 a)
	       (a (rt-gensym))
	       (varnames ((a rt_gen152))))
	   `(let ,(map (lambda (s)
			 (list (car s) (cadr s)))
		       varnames)
	      ,newbody)))
	(else
	 `(+ ,a 5))))
!#


#!

(define-rt-macro (vector-ref expand/vec expand/pos)
  (if (rt-is-safety?)
      (rt-automate-immediate vec pos
			     `(begin
				(if (not (vector? ,vec))
				    (rt-error "ai" "Operation vector-ref failed because first argument is not a vector."))
				(if (>= ,pos (vector-length ,vec))
				    (rt-error "Operation vector-ref failed because the length of the vector is to small"))
				(rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))
      `(rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))
!#


;; VECTORS

(rt-renamefunc vector? SCM_VECTORP <int> (<SCM>))
(rt-renamefunc vector-length SCM_VECTOR_LENGTH <int> (<SCM>))

(define-rt-macro (vector-ref expand/vec expand/pos)
  (if (rt-is-safety?)
      (if (not (rt-immediate? vec pos))
	  (let ((das-vec (rt-gensym))
		(das-pos (rt-gensym)))
	    `(let ((,das-vec ,vec)
		   (,das-pos ,pos))
	       (if (not (vector? ,das-vec))
		   (rt-error ,(<-> "Operation vector-ref failed because first argument is not a vector. " (format #f "vector name: ~A pos: ~A" vec pos))))
	       (if (>= ,das-pos (vector-length ,das-vec))
		   (rt-error "Operation vector-ref failed because the length of the vector is to small"))
	       (rt-vector-ref/vector-ref ,das-vec (rt-castint/castint ,das-pos))))
	  `(begin
	     (if (not (vector? ,vec))
		 (rt-error ,(<-> "Operation vector-ref failed because first argument is not a vector. " (format #f "vector name: ~A pos: ~A" vec pos))))
	     (if (>= ,pos (vector-length ,vec))
		 (rt-error "Operation vector-ref failed because the length of the vector is to small"))
	     (rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))
      `(rt-vector-ref/vector-ref ,vec (rt-castint/castint ,pos))))

(define-c-macro (rt-vector-ref/vector-ref vec pos)
  `(SCM_VECTOR_REF ,vec ,pos))
(<rt-func> 'rt-vector-ref/vector-ref '<SCM> '(<SCM> <int>))




;; PAIRS and LISTS

(rt-renamefunc rt-car SCM_CELL_OBJECT_0 <SCM> (<SCM>))
(rt-renamefunc rt-cdr SCM_CELL_OBJECT_1 <SCM> (<SCM>))

(rt-renamefunc pair? SCM_CONSP <int> (<SCM>))
(rt-renamefunc null? SCM_NULLP <int> (<SCM>))


(define-rt-macro (car expand/p)
  (if (not (rt-is-safety?))
      `(rt-car ,p)
      (if (rt-immediate? p)
	  `(begin
	     (if (not (pair? ,p))
		 (rt-error "Operation CAR failed because p is not a pair. (1)"))
	     (rt-car ,p))
	  (let ((s (rt-gensym)))
	    `(let ((,s ,p))
	       (if (not (pair? ,s))
		   (rt-error  "Operation CAR failed because p is not a pair. (2)"))
	       (rt-car ,s))))))
	    
(define-rt-macro (cdr expand/p)
  (if (not (rt-is-safety?))
      `(rt-cdr ,p)
      (if (rt-immediate? p)
	  `(begin
	     (if (not (pair? ,p))
		 (rt-error  "Operation CDR failed because p is not a pair. (1)"))
	     (rt-cdr ,p))
	  (let ((s (rt-gensym)))
	    `(let ((,s ,p))
	       (if (not (pair? ,s))
		   (rt-error "Operation CDR failed because p is not a pair. (2)"))
	       (rt-cdr ,s))))))
	   
(define-rt-macro (cadr p)
  `(car (cdr ,p)))
(define-rt-macro (caddr p)
  `(car (cdr (cdr ,p))))
(define-rt-macro (cadddr p)
  `(car (cdr (cdr (cdr ,p)))))
(define-rt-macro (caddddr p)
  `(car (cdr (cdr (cdr (cdr ,p))))))
(define-rt-macro (cddr p)
  `(cdr (cdr ,p)))
(define-rt-macro (cdddr p)
  `(cdr (cdr (cdr ,p))))
(define-rt-macro (cddddr p)
  `(cdr (cdr (cdr (cdr ,p)))))
(define-rt-macro (cdddddr p)
  `(cdr (cdr (cdr (cdr (cdr ,p))))))
(define-rt-macro (cdadr p)
  `(cdr (car (cdr ,p))))
(define-rt-macro (cdaddr p)
  `(cdr (car (cdr (cdr ,p)))))
(define-rt-macro (cdadddr p)
  `(cdr (car (cdr (cdr (cdr ,p))))))
(define-rt-macro (caar p)
  `(car (car ,p)))
(define-rt-macro (caadr p)
  `(car (car (cdr ,p))))
(define-rt-macro (caaddr p)
  `(car (car (cdr (cdr ,p)))))
(define-rt-macro (caadddr p)
  `(car (car (cdr (cdr (cdr ,p))))))

#!
(define-rt (rt_length das-list n)
  (if (null? das-list)
      n
      (rt_length das-list (1+ n))))
!#
(define-rt (rt_length das-list n)
  (declare (<int> n))
  (while (not (null? das-list))
	 (set! n (1+ n))
	 (set! das-list (cdr das-list)))
  n)

(define-rt-macro (length das-list)
  `(rt_length ,das-list 0))

(define-rt (rt_list_ref das-list n)
  (while (> n 0)
	 (set! das-list (cdr das-list))
	 (set! n (1- n)))
  (car das-list))
  


(define-rt-macro (list-ref das-list n)
  (define (help n)
    (if (= 0 n)
	das-list
	`(cdr ,(help (1- n)))))
  (if (and (number? n)
	   (< number 8)
	   (>= number 0))
      `(car ,(help n))
      `(rt_list_ref ,das-list ,n)))


;; rt-insert-types is not designed correctly, therefore the declares of the lists and the ,funcname stuff. Shouldn't have been necesarry.
(define-rt-macro (for-each func . lists)
  (let ((lnames (map (lambda (n) (rt-gensym)) (iota (length lists))))
	(funcname (rt-gensym)))
    `(let ((,funcname (lambda ,(cadr func)
			,@(map (lambda (llist)
				 `(declare (<SCM> ,llist)))
			       (cadr func))
			,@(cddr func)))
	   ,@(map (lambda (lname llist)
		    (list lname llist))
		  lnames
		  lists))
       ,@(map (lambda (llist)
		`(declare (<SCM> ,llist)))
	      lists)
       (while (and ,@(map (lambda (lname)
			    `(not (null? ,lname)))
			  lnames))
	      (,funcname ,@(map (lambda (lname)
				  `(car ,lname))
				lnames))
	      ,@(map (lambda (lname)
		       `(set! ,lname (cdr ,lname)))
		     lnames)))))
    

#!
(define l '(1 2 3 4))
(define a (rt-2 '(lambda ()
		   (for-each (lambda (n)
			       (printf "%f\\n" (+ 100 n)))
			     l))))
(rt-funcall a)
!#
		  

;; Beware that caching happens before macroexpanding!
(define-rt-macro (unquote something)
  (local-eval something *rt-local-code-environment*))

(define-rt-macro (include-guile-func name)
  (procedure-source (local-eval name *rt-local-code-environment*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; debug ;''''';;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-rt-macro (debug string . rest)
  `(rt-debug ,string ,@rest))
(define-c-macro (rt-debug string . rest)
  ;;  `(listener_append ,string))
  `(rt_debug ,string ,@rest))
(<rt-func> 'rt-debug '<void> '(<char-*> <float>) #:min-arguments 1)

#!
(<rt-play> (lambda ()
             (debug "hello2 %d %feee!" 50 60.0)))

(eval-c ""
        (run-now
         (printf (string "len: %d\\n") (strlen (string "1234")))))
!#

;;define-rt-ec <void> debug
;;(lambda (,rt-globalvardecl
;;         (<char-*> string))

(eval-c ""
        "#include <stdarg.h>"
        "#include <jack/ringbuffer.h>"
        "#define MAXSTRING 4096"

        (<jack_ringbuffer_t*> rb)
        (run-now
         (set! rb (jack_ringbuffer_create 1024*64)))

        (<char> schemestring[MAXSTRING] "{0}")

        (public 
         (<void> debug_scheme_thread (lambda ()                                       
                                       (if (not (== 0 schemestring[0]))
                                           (scm_eval_string (MAKE_STRING schemestring)))))
         (<void> debug_scheme_reset_string (lambda ()
                                             (set! schemestring[0] 0))))

        (<void*> debug_thread (lambda ((<void*> arg))
                                (<int> size 0)
                                (<char> string[MAXSTRING])
                                (while 1
                                  (while (>= (jack_ringbuffer_read_space rb) (sizeof <int>))
                                    (jack_ringbuffer_read rb (cast <void*> &size) (sizeof <int>))
                                    (while (< (jack_ringbuffer_read_space rb) size)
                                      (usleep 200))
                                    (jack_ringbuffer_read rb (cast <void*> string) size)
                                    (set! string[size] 0)        
                                    (fprintf stderr (string "%s\\n") string))
                                  (usleep 1000000/4))
                                (return NULL)))
        
        (<pthread_t> thread "{0}")
        (run-now
         (pthread_create &thread NULL debug_thread NULL))

        (<char> string[MAXSTRING]) ;; Can not put this one on a coroutine stack.
        (<nonstatic-void> rt_debug (lambda ((<const-char*> fmt)
                                            (<NOTHING> ...))
                                     (<int> size)
                                     (<va_list> argp)
                                     ;;(fprintf stderr fmt)
                                     (if (> (strlen fmt) (- MAXSTRING 256))
                                         (begin
                                           (fprintf stderr (string
                                              "Error. Debug string too large. Can not show string. First three letters: %c%c%c\\n")
                                              fmt[0] fmt[1] fmt[2])
                                           return))
                                     
                                     (va_start argp fmt)
                                     (vsprintf string fmt argp)
                                     (va_end argp)
                                     (set! size (strlen string))
                                    
                                     (when (not (strncmp (string "\\n/tmp/file") string (strlen (string "\\n/tmp/file"))))
                                       (sprintf schemestring (string "(display-stalin-error \\\"%s\\\")") string+1))
                                     (when (not (strncmp (string "/tmp/file") string (strlen (string "/tmp/file"))))
                                       ;;(fprintf stderr (string "(display-stalin-error \\\"%s\\\")") string)
                                       (sprintf schemestring (string "(display-stalin-error \\\"%s\\\")") string))
                                     
                                     (if (< (jack_ringbuffer_write_space rb) (+ (sizeof <int>) size))
                                         (begin
                                           (fprintf stderr (string
                                            "Error. Debug buffer is full. snd-rt might crash because of this (%s)\\n")
                                            string)
                                           return))

                                     (jack_ringbuffer_write rb (cast <void*> &size) (sizeof <int>))
                                     (jack_ringbuffer_write rb string size)))
                                          
        (public
         (<void> test-rt-debugger (lambda ()
                                    (rt_debug (string "/tmp/fileTYkBns.scm:18:462:"))
                                    (rt_debug (string "hello %d %f\\n") 50 2.3f)))))


(let debug_scheme_loop ()
  (in 1000
      (lambda ()
        (catch #t
               (lambda ()
                 (debug_scheme_thread))
               (lambda x
                 (c-display "Error: " x)))
        (debug_scheme_reset_string)
        (debug_scheme_loop))))

#!
(test-rt-debugger)

!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Output and Input ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-c-macro (rt-cast-float val)
  `(cast <float> ,val))
(<rt-func> 'rt-cast-float '<float> '(<void>) #:min-arguments 1 #:is-immediate #t)
(define-c-macro (rt-cast-vct val)
  `(cast <vct-*> ,val))
(<rt-func> 'rt-cast-vct '<vct-*> '(<void>) #:min-arguments 1 #:is-immediate #t)



(define-rt-macro (out . rest)
  (let* ((org-channels (c-butlast rest))
	 (channels org-channels)
	 (val (last rest)))

    (if (null? channels)
	(set! channels '(0 1)))

    (cond ((and (list? val)
		(or (eq? 'vct-scale! (car val))
		    (eq? 'vct-offset! (car val))
		    (eq? 'vct-fill! (car val))))
	   `(out ,@org-channels ,(rt-macroexpand-1 val)))

	  ((and (list? val)
		(eq? 'vct (car val)))
	   (let ((ch (1- (car channels))))
	     `(begin
		,@(map (lambda (something)
			 (set! ch (1+ ch))
			 `(rt_write_bus out-bus ,ch ,something))
		       (cdr val)))))
	  ((not (symbol? val))
	   (let ((new-val (rt-gensym)))
	     `(let ((,new-val ,val))
		(out ,@org-channels ,new-val))))
	  ((= 1 (length channels))
	   `(if (is-type? <vct-*> ,val)
		(rt_write_bus_vct out-bus (rt-cast-vct ,val))
		(rt_write_bus out-bus ,(car channels) (rt-cast-float ,val))))
	  ((= 0 (length org-channels))
	   `(if (is-type? <vct-*> ,val)
		(rt_write_bus_vct out-bus (rt-cast-vct ,val))
		(begin
		  (rt_write_bus out-bus 0 (rt-cast-float ,val))
		  (rt_write_bus out-bus 1 (rt-cast-float ,val)))))
	  (else
	   `(begin
	      ,@(map (lambda (ch)
		       `(rt_write_bus out-bus ,ch ,val))
		     channels))))))

(define-rt-macro (in . channels)
  (if (null? channels)
      (set! channels '(0 1)))
  (if (= 1 (length channels))
      `(rt_read_bus in-bus ,(car channels))
      `(+ ,@(map (lambda (ch)
		   `(rt_read_bus in-bus ,(car channels)))
		 channels))))



  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; CLM/etc. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;; CLM Generators ;;;;;;;;;;;;;;;

;; This expressions is generated from code commented out in rt-stalin.scm
;; This struct is hopefully just going to be a temporary solution.
(define clm-constructor-protos
'((make-all-pass
   #:feedback
   #:feedforward
   #:size
   #:initial-contents
   ;;(#:initial-element 0.0)
   #:max-size
   (#:type mus-interp-linear))
 (make-asymmetric-fm
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0)
   (#:r 1.0)
   (#:ratio 1.0))
 (make-comb
   #:scaler
   #:size
   #:initial-contents
   ;;(#:initial-element 0.0)
   #:max-size
   (#:type mus-interp-linear))
 (make-convolve #:input #:filter #:fft-size)
 (make-delay
   #:size
   #:initial-contents
   (#:initial-element 0.0)
   (#:max-size)
   (#:type mus-interp-linear))
 (make-env
   #:envelope
   (#:scaler 1.0)
   #:duration
   (#:offset 0.0)
   (#:base 1.0)
   #:end
   #:length)
 (make-fft-window
   type
   size
   #:optional
   (beta 0.0)
   (alpha 0.0))
 (make-file->frame
   filename
   #:optional
   buffer-size)
 (make-file->sample
   filename
   #:optional
   buffer-size)
 (make-filter #:order #:xcoeffs #:ycoeffs)
 (make-filtered-comb
   #:scaler
   #:size
   #:initial-contents
   (#:initial-element 0.0)
   #:max-size
   (#:type mus-interp-linear)
   #:filter)
 (make-fir-coeffs order v)
 (make-fir-filter #:order #:xcoeffs)
 (make-firmant #:frequency #:radius)
 (make-formant #:frequency #:radius)
 (make-frame chans val0 val1 ...)
 (make-frame->file
   filename
   #:optional
   chans
   data-format
   header-type
   comment)
 (make-granulate
   #:input
   (#:expansion 1.0)
   (#:length 0.15)
   (#:scaler 0.6)
   (#:hop 0.05)
   (#:ramp 0.4)
   (#:jitter 1.0)
   #:max-size
   #:edit)
 (make-iir-filter #:order #:ycoeffs)
 (make-locsig
   (#:degree 0.0)
   (#:distance 1.0)
   (#:reverb 0.0)
   (#:output *output*)
   (#:revout *reverb*)
   (#:channels 2) ;(mus-channels *output*))
   (#:type mus-interp-linear))
 (make-mixer chans val0 val1 ...)
 (make-move-sound
   dloc-list
   #:optional
   (out *output*)
   (rev *reverb*))
 (make-moving-average
   #:size
   #:initial-contents
   (#:initial-element 0.0))
 (make-ncos
   (#:frequency *clm-default-frequency*)
   (#:n 1))
 (make-notch
   #:scaler
   #:size
   #:initial-contents
   (#:initial-element 0.0)
   #:max-size
   (#:type mus-interp-linear))
 (make-nrxycos
   (#:frequency *clm-default-frequency*)
   (#:ratio 1.0)
   (#:n 1)
   (#:r 0.5))
 (make-nrxysin
   (#:frequency *clm-default-frequency*)
   (#:ratio 1.0)
   (#:n 1)
   (#:r 0.5))
 (make-nsin
   (#:frequency *clm-default-frequency*)
   (#:n 1))
 (make-one-pole #:a0 #:b1)
 (make-one-zero #:a0 #:a1)
 (make-oscil
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0))
 (make-phase-vocoder
   #:input
   #:fft-size
   #:overlap
   #:interp
   #:pitch
   #:analyze
   #:edit
   #:synthesize)
 (make-polyshape
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0)
   #:coeffs
   (#:partials (quote (1 1)))
   (#:kind mus-chebyshev-first-kind))
 (make-polywave
   (#:frequency *clm-default-frequency*)
   (#:partials (quote (1 1)))
   (#:type mus-chebyshev-first-kind))
 (make-pulse-train
   (#:frequency *clm-default-frequency*)
   (#:amplitude 1.0)
   (#:initial-phase 0.0))
 (make-rand
   (#:frequency *clm-default-frequency*)
   (#:amplitude 1.0)
   #:envelope
   #:distribution
   #:size)
 (make-rand-interp
   (#:frequency *clm-default-frequency*)
   (#:amplitude 1.0)
   #:envelope
   #:distribution
   #:size)
 (make-readin
   #:file
   (#:channel 0)
   (#:start 0)
   (#:direction 1)
   #:size)
 (make-sample->file
   filename
   #:optional
   chans
   data-format
   header-type
   comment)
 (make-sawtooth-wave
   (#:frequency *clm-default-frequency*)
   (#:amplitude 1.0)
   (#:initial-phase 0.0))
 (make-scalar-mixer chans value)
 (make-sine-summation
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0)
   (#:n 1)
   (#:a 0.5)
   (#:ratio 1.0))
 (make-square-wave
   (#:frequency *clm-default-frequency*)
   (#:amplitude 1.0)
   (#:initial-phase 0.0))
 (make-src #:input (#:srate 1.0) (#:width 10))
 (make-ssb-am
   (#:frequency *clm-default-frequency*)
   (#:order 40))
 (make-sum-of-cosines
   (#:cosines 1)
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0))
 (make-sum-of-sines
   (#:sines 1)
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0))
 (make-table-lookup
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0)
   #:wave
   (#:size (clm-table-size))
   #:type)
 (make-triangle-wave
   (#:frequency *clm-default-frequency*)
   (#:amplitude 1.0)
   (#:initial-phase 0.0))
 (make-two-pole
   #:a0
   #:b1
   #:b2
   or
   #:frequency
   #:radius)
 (make-two-zero
   #:a0
   #:a1
   #:a2
   or
   #:frequency
   #:radius)
 (make-wave-train
   (#:frequency *clm-default-frequency*)
   (#:initial-phase 0.0)
   #:wave
   (#:size (clm-table-size))
   #:type)
 (make-waveshape
   (#:frequency *clm-default-frequency*)
   (#:partials (quote (1 1)))
   (#:size (clm-table-size))
   #:wave)))


;; clm constructors
;;
;; This is just a _very_ quick get-up-and-running implementation. More work is needed.
(for-each (lambda (clm-def)
            ;;(c-display "clm-def" clm-def)
            (let* ((name (car clm-def)) ;; make-oscil
                   (gen-name (string->symbol (substring (symbol->string name) 5 (string-length (symbol->string name))))) ;; oscil
                   (args (cdr clm-def))
                   (argnames (map (lambda x (rt-gensym)) (iota (length args))))
                   (fixed-args-list (map (lambda (arg)
                                           (let ((def 0)
                                                 (n #f))
                                             (if (pair? arg)
                                                 (begin
                                                   (set! n (car arg))
                                                   (if (not (null? (cdr arg)))
                                                       (set! def (primitive-eval (cadr arg)))))
                                                 (set! n arg))
                                             (if (keyword? n)
                                                 (set! n (keyword->symbol n)))
                                             (list n def)))
                                         args))
                   )

              (supereval
               (lambda (out)

                 (out "(define-rt-ec <mus_any-*> make_" gen-name "_ (lambda (")
                 (for-each (lambda (arg)
                             (out `(<float> ,arg)))
                           argnames)
                 (out ")(return (mus_make_" gen-name " ")
                 (for-each (lambda (arg)
                             (out " " arg))
                           argnames)
                 (out "))))\n")
                 
                 (out "(define-rt-macro " name)
                 (out "  (labamba (:optkey")
                 (for-each (lambda (arg)
                             (out arg " "))
                           fixed-args-list)
                 (out ")\n")
                 (out "  `(" 'make_ gen-name "_ ")
                 (for-each (lambda (arg)
                             (out "," (car arg) " "))
                           fixed-args-list)
                 (out ")))\n")))))
          clm-constructor-protos)

#!
(define-rt-ec <mus_any-*> make_oscil_
  (lambda ((<float> a)
           (<float> b))
    (return (mus_make_oscil a b))))

(define-rt-macro (make-oscil :optkey
                             (frequency *clm-default-frequency*)
                             (initial-phase 0.0)
                             freq)
  `(make_oscil_ ,(or freq frequency) ,initial-phase))
!#

(define rt-clm-generators `((all-pass     (input (pm 0)))
			    (asymmetric-fm (index (fm 0)))
			    (comb         (input (pm 0)))
			    (convolve     (input-function)) ;; redefined later
			    (delay        (input (pm 0)))
			    (env          ())
			    (filter       (input))
			    (filtered-comb (input (pm 0)))
			    (fir-filter   (input))
			    (formant      (input))
			    (granulate    (input-function (edit-function 0))) ;; redefined later
			    (iir-filter   (input))
			    ;;(in-any       (
			    (locsig       (input)) ;; redefined later to use out instead of out-any
			    ;;(move-sound   (input)) ;; Lots of work to support.
			    (moving-average      (input))
			    ,@(if (defined? 'ncos?) ;; Snd-ls uses an older version of Snd.
				  '((ncos         ((fm 0)))
				    (nsin         ((nsin 0)))
				    (nrxysin      ((fm 0)))
				    (nrxycos      ((fm 0)))
				    ;(sine-summation ((fm 0)))
				    (polywave     ((index 1) (fm 0)))
				    )
				  '())
			    (notch        (input (pm 0)))
			    (one-pole     (input))
			    (one-zero     (input))
			    (oscil        ((fm 0) (pm 0)))
			    ;;(out-any      (
			    (polyshape    ((index 1) (fm 0)))
			    (phase-vocoder (input-function analyze-function edit-function synthesize-function)) ;; redefined later
			    (pulse-train   ((fm 0)))
			    (rand          ((sweep 0)))
			    (rand-interp   ((sweep 0)))
			    (readin        ())       ;; Lots of things redefined later
			    (sawtooth-wave ((fm 0)))
			    (sine-summation ((fm 0)))
			    (square-wave    ((fm 0)))
			    (src            (sr-change input-function)) ;; redefined later
			    (ssb-am         ((insig 0) (fm 0)))
			    (sum-of-cosines ((fm 0)))
			    (sum-of-sines   ((fm 0)))
			    (table-lookup   ((fm 0)))
			    (tap            ((offset 0)) delay)
			    (triangle-wave  ((fm 0)))
			    (two-pole       (input))
			    (two-zero       (input))
			    (wave-train     ((fm 0)))
			    (waveshape      ((index 1) (fm 0)))))

(for-each (lambda (clm-generator)
	    (let* ((name (car clm-generator))              ;; -> all-pass
		   (name2 (symbol-append name '-internal)) ;; -> all-pass-internal
		   (args (cadr clm-generator))             ;; (input (fm 0))
		   (args1 (remove list? args))             ;; (input)
		   (args2 (filter-org list? args))         ;; ((fm 0))
		   (belongsto-name (if (= 3 (length clm-generator)) ;; all-pass (Third optional argument, Example: 'delay, because tap belongs to 'delay and not to 'tap.)
				       (caddr clm-generator)
				       name))
		   (c-name (string->symbol                 ;; all_pass
			    (list->string (map (lambda (c)
						 (if (equal? #\- c) #\_ c))
					       (string->list (symbol->string name))))))
		   (c-belongsto-name (string->symbol                 ;; all_pass
				      (list->string (map (lambda (c)
							   (if (equal? #\- c) #\_ c))
							 (string->list (symbol->string belongsto-name))))))
		   (etype (symbol-append                   ;; -> <mus_all-pass-*>
			   '<mus_ belongsto-name '-*>))
		   (testfunc (primitive-eval               ;; all-pass?
			      (symbol-append belongsto-name '?)))
		   (c-func (symbol-append 'mus_ c-name))   ;; mus_all_pass
		   (macroname (symbol-append               ;; -> rt-all-pass/mus_all_pass
			       'rt- name '/ c-func))
		   
		   (macro-belongsto-name (symbol-append               ;; -> rt-all-pass/mus_all_pass
					  'rt- belongsto-name '/ c-func))
		   
		   (c-transformfuncname (symbol-append macro-belongsto-name '?)) ;; -> rt-all-pass/mus_all_pass?
		   (c-transformfuncname2 (symbol-append 'mus_ c-belongsto-name '_p)) ;; -> all_pass_p

		   )
	      
	      (if (eq? belongsto-name name)
		  (<rt-type> etype testfunc c-transformfuncname #:c-type '<mus_any-*> #:transformfunc XEN_TO_MUS_ANY #:subtype-of '<mus_any-*>))
	      (<rt-func> macroname '<float> (cons etype (map (lambda (a) '<float>) args)))
	      (primitive-eval `(define-rt-macro (,name2 osc ,@args1 . rest)
				 (if (> (length rest) ,(length args2))
				     (begin
				       (c-display "rt-macro, too many arguments for " ',name ":" rest)
				       #f)
				     (let ((n -1))
				       (append (list ',macroname osc)
					       (list ,@args1)
					       (map (lambda (arg)
							       (set! n (1+ n))
							       (if (> (length rest) n)
								   (list-ref rest n)
								   arg))
							     (list ,@(map cadr args2))))))))
	      (primitive-eval `(define-rt-macro (,name . rest)
				 (let ((args '())
				       (make-extern-anyway #f))
				   (define constructor-args
				     (let loop ((rest rest))
				       (cond ((null? rest) '())
					     ((keyword? (car rest))
					      (if (eqv? :extern (car rest))
						  (begin
						    (set! make-extern-anyway #t)
						    (loop (cdr rest)))
						  `(,(car rest) ,(cadr rest)
						    ,@(loop (cddr rest)))))
					     (else
					      (set! args rest)
					      '()))))
				   ;;(c-display "constr/args" constructor-args args)
				   (if (or (null? rest)
					   make-extern-anyway
					   (not (null? constructor-args)))
				       `(,',name2 (extern (,(symbol-append 'make- ',name) ,@constructor-args))
						  ,@args)
				       `(,',name2 ,@rest)))))

	      (primitive-eval `(define-c-macro (,macroname osc . rest)
				 `(,',c-func ,osc ,@rest)))
	      (if (eq? belongsto-name name)
		  (begin
		    (primitive-eval `(define-c-macro (,c-transformfuncname scm)
				       (if (rt-is-safety?)
					   `(?kolon (&& (mus_xen_p ,scm)
							(,',c-transformfuncname2 (XEN_TO_MUS_ANY ,scm)))
						    (XEN_TO_MUS_ANY ,scm)
						    (begin_p
						     (rt_error rt_globals (string "Variable is not a CLM generator (2)"))
						     NULL))
					   `(XEN_TO_MUS_ANY ,scm))))
		    (<rt-func> c-transformfuncname etype '(<SCM>))
		    
		    ))))

	  rt-clm-generators)


(for-each (lambda (generator)
	    (fix-defines
	     (define basename (car generator))
	     (define new-name (<_> basename '*))
	     (define args (cadr generator))
	     (define first-arg (and (not (null? args))
				    (nth 0 args)))
	     (when (equal? first-arg '(fm 0))
               (let ((makename (<_> 'make- basename)))
                 (primitive-eval `(define ,makename ;; Set default frequency back to 440.
                                    (let ((old ,makename))
                                      (lambda rest
                                        (cond ((null? rest)
                                               (old :frequency 440))
                                              ((eqv? :freq (car rest))
                                               (apply old (cons :frequency (cdr rest))))
                                              (else
                                               (apply old rest))))))))
               (supereval (lambda (out)
                            (out "(define-rt-macro " new-name)
                            (out "  (labamba (:optkey (frequency 440))")
                            (out "   (if (number? frequency)")
                            (out "       `(" basename " :frequency ,frequency)")
                            (out "       `(" basename " :frequency 0")
                            (out "                      (hz->radians ,frequency)))))"))))))
	  rt-clm-generators)


#!
(<rt-out> (oscil* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (pulse-train* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (sawtooth-wave* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (sine-summation* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (square-wave* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (sum-of-cosines* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (sum-of-sines* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (triangle-wave* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (table-lookup* (<slider> "freq" 0 200 5000 :log #t)))
(<rt-out> (wave-train* (<slider> "freq" 0 200 5000 :log #t)))

(<rt-out> (let* ((sos (extern (make-sum-of-sines :sines n)))
		 (new-n (<slider> "num_cosines" 1 10 100 :scale 1)))
	    (when (not (= (extern n 10) new-n))
	      (set! n new-n)
	      (set! (mus-length sos) n)
	      (mus-reset sos))
	    (sum-of-sines sos (hz->radians (<slider> "freq" 0 200 5000 :log #t)))))

(define sos (make-sum-of-sines :sines 10))
(mus-length sos)
(set! (mus-length sos) 11)
!#


;;A more efficient implementation of oscil*:
(define-rt-macro osci*
  (lambda* (:optional (frequency 440))
    (define phase (rt-gensym))
    (define ret (rt-gensym))
    (cond ((number? frequency)
	   (let ((phaseinc (hz->radians frequency)))
	     `(begin
		(declare (<double> ,phase))
		(set! ,phase (+ (extern ,phase (- ,phaseinc))
				,phaseinc))
		(sin ,phase))))
	  (else
	   `(let ((,ret (sin (extern ,phase 0))))
	      (declare (<double> ,phase))
	      (set! ,phase (+ ,phase (hz->radians ,frequency)))
	      ,ret)))))

#!
(hz->radians 20000)
(rt-macroexpand '(osci))
(<rt-out> (oscil* 900))
(<rt-out> (oscil* (<slider> "freq" 0 400 1500 :log #t)))
!#


;;;;;; CLM Methods ;;;;;;;;;;;;;;;

(for-each (lambda (descr)
	    (let* ((returntype (car descr))
		   (name (cadr descr))
		   (args (cons '<mus_any-*> (caddr descr)))
		   (n -1)
		   (argnames (cons 'gen (map (lambda (arg)
					       (set! n (1+ n))
					       (symbol-append 'arg_ (string->symbol (number->string n))))
					     (caddr descr))))
		   (is-setter (and (> (length descr) 3) (cadddr descr)))
		   (rt-name (if is-setter
				(symbol-append 'setter!-mus- (string->symbol (substring (symbol->string name) 4)))
				(symbol-append 'mus- name)))
		   (c-name (symbol-append 'mus_ name))
		   (funcname (if is-setter
				 (symbol-append rt-name '/ c-name)
				 (symbol-append 'rt- rt-name '/ c-name))))
	      (<rt-func> funcname returntype args)
	      (if (or (eq? name 'set_xcoeff)
		      (eq? name 'xcoeff))
		  (begin
		    (rt-print "c-name" c-name)
		    (rt-print "funcname" funcname)
		    (rt-print "rt-name" rt-name)))
	      (primitive-eval `(define-c-macro ,(cons funcname 'rest )
				 `(,',c-name ,@rest)))
	      (primitive-eval `(define-rt-macro ,(cons rt-name 'rest)
				 `(,',funcname ,@rest)))))
	  '((<int> release ())
	    (<char-*> describe ())
	    (<int> equalp (<mus_any-*>))
	    (<float-*> data ())
	    (<float-*> set_data (<float-*>) #t)
	    (<int> length ())
	    (<int> set_length (<int>) #t)
	    (<float> frequency ())
	    (<float> set_frequency (<float>) #t)
	    (<float> phase ())
	    (<float> set_phase (<float>) #t)
	    (<float> scaler ())
	    (<float> set_scaler (<float>) #t)
	    (<float> increment ())
	    (<float> set_increment (<float>) #t)
	    (<float> run (<float> <float>))
	    (<void-*> environ ())
	    (<void-*> set_environ (<void-*>) #t)
	    (<int> channels ())
	    (<float> offset ())
	    (<float> set_offset (<float>) #t)
	    (<float> width ())
	    (<float> set_width (<float>) #t)
	    (<float> xcoeff (<int>))
	    (<float> set_xcoeff (<int> <float>) #t)
	    (<int> hop ())
	    (<int> set_hop (<int>) #t)
	    (<int> ramp ())
	    (<int> set_ramp (<int>) #t)
	    ;;(<int> read_sample (<int> <int>))
	    ;;(<float> write_sample (<int> <int> <float>))
	    (<char-*> file_name ())
	    (<int> end ())
	    (<int> location ())
	    (<int> set_location (<int>) #t)
	    (<int> channel ())
	    (<float> ycoeff (<int>))
	    (<float> set_ycoeff (<int> <float>) #t)
	    (<float-*> xcoeffs ())
	    (<float-*> ycoeffs ())
	    ;;(<void-*> wrapper ())
	    (<void> reset ())
	    ))


;; Remember to remove this when snd-ls is up to date with snd.
;;(define old-make-env make-env)
;;(define (make-env . rest)
;;  (if (pair? (car rest))
;;      (apply old-make-env (cons (flatten (car rest))
;;				(cdr rest)))
;;      (apply old-make-env rest)))

;; mus-feedback	
(define-rt-macro (mus-feedback ins)	
  `(mus-increment ,ins))	
(define-rt-macro (setter!-mus-feedback ins val)	
  `(setter!-mus-increment ,ins ,val))	

;; mus-feedforward	
(define-rt-macro (mus-feedforward ins)	
  `(mus-scaler ,ins))	
(define-rt-macro (setter!-mus-feedforward ins val)	
  `(setter!-mus-scaler ,ins ,val))



;; restart-env
(rt-renamefunc restart-env mus_restart_env <void> (<mus_env-*>))
;; env-interp
(rt-renamefunc env-interp mus_env_interp <float> (<float> <mus_env-*>))

	       
;; polynomial
(<rt-func> 'rt-polynomial/mus_polynomial '<float> '(<vct-*> <float>))
(define-rt-macro (polynomial coeffs x)
  `(rt-polynomial/mus_polynomial ,coeffs ,x))
(define-c-macro (rt-polynomial/mus_polynomial v x)
  (<-> "mus_polynomial(" (eval-c-parse v) "->data," (eval-c-parse x) "," (eval-c-parse v) "->length)"))

;; fft
(<rt-func> 'rt-mus-fft/mus_fft '<void> '(<vct-*> <vct-*> <int> <int>))
(define-rt-macro (fft v1 v2 i1 i2)
  `(rt-mus-fft/mus_fft ,v1 ,v2 ,i1 ,i2))
(define-c-macro (rt-mus-fft/mus_fft v1 v2 i1 i2)
  (<-> "mus_fft(" (eval-c-parse v1) "->data," (eval-c-parse v2) "->data," (eval-c-parse i1) "," (eval-c-parse i2) ")"))

;; spectrum
(<rt-func> 'rt-spectrum/mus_spectrum '<void> '(<vct-*> <vct-*> <vct-*> <int> <int>))
(define-rt (spectrum v1 v2 v3 i1)
  (let ((n (max (vct-length v1)
		(vct-length v2)
		(vct-length v3))))
    (if (not (= 0 (logand (1- n) n)))
	(let* ((nf (/ (log n) 2.0))
	       (np (the <int> nf)))
	  (set! n (the <int> (expt 2.0 np)))))
    (rt-spectrum/mus_spectrum v1 v2 v3 i1 n)))

(define-c-macro (rt-spectrum/mus_spectrum v1 v2 v3 i1 i2)
  (<-> "mus_spectrum(" (eval-c-parse v1) "->data," (eval-c-parse v2) "->data," (eval-c-parse v3) "->data," (eval-c-parse i1) "," (eval-c-parse i2) ")"))


;; hz->radians
;;;;;;;;;;;;;;
;; Can't use this one, because mus-srate might differ:
;;  (rt-renamefunc hz->radians mus_hz_to_radians <float> (<float>))
;; But this one should be fine: (Should probably compute w_rate and put it somewhere though.)
(define-rt-macro (hz->radians hz)
  (if (number? hz)
      (* hz (/ (* pi 2) (-> *rt-engine* samplerate)))
      `(* ,hz ,(/ (* pi 2) (-> *rt-engine* samplerate)))))

;; mus-srate
;(<rt-func> 'mus-srate '<float> '() #:is-immediate #t)
;(define-c-macro (mus-srate)
;  "rt_globals->samplerate")

(define-rt-macro (mus-srate)
  (-> *rt-engine* samplerate))


;; move-locsig
(rt-renamefunc move-locsig mus_move_locsig <void> (<mus_locsig-*> <float> <float>))

;; locsig-set!
(rt-renamefunc locsig-set! mus_locsig_set <float> (<mus_locsig-*> <int> <float>))

;; locsig-reverb-set!
(rt-renamefunc locsig-reverb-set! mus_locsig_reverb_set <float> (<mus_locsig-*> <int> <float>))



;, Locsig, or at least an attempt. I think its okey, but theres no reverb.
(define-rt-macro (locsig loc val)
  (let ((dasval (rt-gensym))
	(i (rt-gensym)))
    `(let ((,dasval ,val))
       (range ,i 0 (mus-channels ,loc)
	      (rt-set-locvals ,loc ,i ,dasval))
       (range ,i 0 (mus-channels (rt-get-loc-outf ,loc))
	      (out ,i (rt-get-float-val (mus-data (rt-get-loc-outf ,loc)) ,i))))))

(<rt-func> 'rt-set-locvals '<void> '(<mus_locsig-*> <int> <float>))
(<rt-func> 'rt-set-loc-rev-vals '<void> '(<mus_locsig-*> <int> <float>))
(define-c-macro (rt-set-locvals loc i val)
  (<-> "((locs*)" (eval-c-parse loc) ")->outf->vals[" (eval-c-parse i) "]=" (eval-c-parse val) "* ((locs*)" (eval-c-parse loc) ")->outn[" (eval-c-parse i) "]"))
(define-c-macro (rt-set-loc-rev-vals loc i val)
  (<-> "((locs*)" (eval-c-parse loc) ")->revf->vals[" (eval-c-parse i) "]=" (eval-c-parse val) "* ((locs*)" (eval-c-parse loc) ")->revn[" (eval-c-parse i) "]"))

(<rt-func> 'rt-get-float-val '<float> '(<float-*> <int>))
(define-c-macro (rt-get-float-val float* place)
  (<-> (eval-c-parse float*) "[" (eval-c-parse place) "]"))

(<rt-func> 'rt-get-loc-revf '<mus_any-*> '(<mus_locsig-*>))
(define-c-macro (rt-get-loc-revf loc)
  (<-> "((locs*)" (eval-c-parse loc) ")->revf"))

(<rt-func> 'rt-get-loc-outf '<mus_any-*> '(<mus_locsig-*>))
(define-c-macro (rt-get-loc-outf loc)
  (<-> "(mus_any*)((locs*)" (eval-c-parse loc) ")->outf"))

(<rt-func> 'rt-get-loc-rev-channels '<int> '(<mus_locsig-*>))
(define-c-macro (rt-get-loc-rev-channels loc)
  (<-> (eval-c-parse loc) "->rev_chans"))

(<rt-func> 'rt-get-environ '<void-*> '() #:is-immediate #t)
(define-c-macro (rt-get-environ)
  "(void*)rt_globals")

;; The marcros below aren't quite hygienic. Must fix.
(begin
  ;; Src needs special treatment as well.
  (define-rt-macro (src gen sr-change input-function)
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 ;;(declare (<mus_any-*> ,das-gen))
	 (set! (mus-environ ,das-gen) (rt-get-environ))
	 (set! ,ret (rt-mus-src/mus_src ,das-gen ,sr-change (lambda (dir)
							      (declare (<int> dir))
							      (the <float> (,input-function dir)))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))
  (<rt-func> 'rt-mus-src/mus_src '<float> '(<mus_src-*> <float> (<float> (<int>))))
  (define-c-macro (rt-mus-src/mus_src gen sr-change input-function)
    (<-> "mus_src(" (eval-c-parse gen) "," (eval-c-parse sr-change) ", (void*)" (eval-c-parse input-function) ")"))

  ;; Same for convolve
  (define-rt-macro (convolve gen input-function)
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 (set! (mus-environ ,das-gen) (rt-get-environ))
	 (set! ,ret `(rt-mus-convolve/mus_convolve ,das-gen (lambda (direction2)
							      (declare (<int> direction2))
							      (the <float> (,input-function direction2)))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))

  (<rt-func> 'rt-mus-convolve/mus_convolve '<float> '(<mus_convolve-*> (<float> (<int>))))
  (define-c-macro (rt-mus-convolve/mus_convolve gen input-function)
    (<-> "mus_convolve(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function) ")"))

  
  ;; And granulate
  (define-rt-macro (granulate gen input-function . rest)
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 (set! (mus-environ ,das-gen) (rt-get-environ))

	 ,(if (null? rest)
	      `(set! ,ret (rt-mus-granulate/mus_granulate ,das-gen
							  (lambda (arg2 direction2)
							    (declare (<int> direction2)
								     (<void-*> arg2))
							    (the <float> (,input-function direction2)))))
	      `(set! ,ret (rt-mus-granulate/mus_granulate_with_editor ,das-gen
								      (lambda (direction2)
									(declare (<int> direction2))
									(the <float> (,input-function direction2)))
								      (lambda ()
									(the <int> (,(car rest)))))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))
	 
  (<rt-func> 'rt-mus-granulate/mus_granulate '<float> '(<mus_granulate-*> (<float> (<void-*> <int>))))
  (define-c-macro (rt-mus-granulate/mus_granulate gen input-function)
    (<-> "mus_granulate(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function) ")"))
  (<rt-func> 'rt-mus-granulate/mus_granulate_with_editor '<float> '(<mus_granulate-*> (<float> (<int>)) (<int> (<mus_any-*>))))
  (define-c-macro (rt-mus-granulate/mus_granulate_with_editor gen input-function edit-function)
    (<-> "mus_granulate_with_editor(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function) ",(void*)" (eval-c-parse edit-function) ")"))

  
  ;; And even phase-vocoder 
  (define-rt-macro (phase-vocoder gen input-function (#:edit-function #f) (#:synthesize-function #f))
    (let ((ret (rt-gensym))
	  (oldenv (rt-gensym))
	  (das-gen (rt-gensym)))
      `(let* ((,ret 0.0)
	      (,das-gen ,gen)
	      (,oldenv (mus-environ ,das-gen)))
	 (set! (mus-environ ,das-gen) (rt-get-environ))
	 (set! ,ret (rt-mus-phase-vocoder/mus_phase_vocoder ,das-gen
							    (lambda (dir2)
							      (declare (<int> dir2))
							      (the <float> (,input-function dir2)))
							    ,(if edit-function
								 `(lambda ()
								    (the <int> (,edit-function)))
								 `(rt-mus-pv/NULL1))
							    ,(if synthesize-function
								 `(lambda ()
								    (the <float> (,synthesize-function)))
								 `(rt-mus-pv/NULL2))))
	 (set! (mus-environ ,das-gen) ,oldenv)
	 ,ret)))
	 
  (<rt-func> 'rt-mus-phase-vocoder/mus_phase_vocoder '<float> '(<mus_phase-vocoder-*> (<float> (<int>)) (<int> ()) (<float> ())))
  (define-c-macro (rt-mus-phase-vocoder/mus_phase_vocoder gen input-function edit-function synthesize-function)
    (<-> "mus_phase_vocoder_with_editors(" (eval-c-parse gen) ",(void*)" (eval-c-parse input-function)
	 ",NULL,(void*)" (eval-c-parse edit-function) ",(void*)" (eval-c-parse synthesize-function) ")"))
  (<rt-func> 'rt-mus-pv/NULL1 '(<int>) '() #:is-immediate #t)
  (define-c-macro (rt-mus-pv/NULL1)
    "NULL")
  (<rt-func> 'rt-mus-pv/NULL2 '(<float> ()) '() #:is-immediate #t)
  (define-c-macro (rt-mus-pv/NULL2)
    "NULL")
	     
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Readin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ec-struct <mus_rt_readin>
  <mus_any_class-*> core
  <void-*> readin_func ;; Pointer to the function rt_readin
  <void-*> buffer ;; Pointer to struct buffer.
  <int> channel
  <int> channels
  <mus_long_t> location
  <float> increment
  <mus_long_t> length
  <mus_any-*> readin
  <SCM> scm_readin)


(eval-c (<-> "-I" snd-header-files-path " " (string #\`) "pkg-config --libs sndfile" (string #\`) )
	"#include <clm.h>"
	"#include <xen.h>"
	"#include <clm2xen.h>"
	"#include <sndfile.h>"

	(shared-struct <mus_rt_readin>)

	(define-struct <buffer>
	  <void-*> buffer
	  <struct-buffer-*> next
	  <int> num_frames
	  <int> num_visitors
	  <void-*> readin_raw_func
	  <int> channel
	  <char> filename[500])

  
	;;;;;; Buffer handling.
	;;;;;; A buffer is only freed if no one is using it. Perhaps it should never be freed at all?
	(<struct-buffer-*> buffers NULL)

	(<struct-buffer-*> find_buffer (lambda ((<char-*> filename)
						(<int> channel))
					 (let* ((buffer <struct-buffer-*> buffers))
					   (while (not (== NULL buffer))
						  (if (and (== buffer->channel channel)
							   (not (strncmp buffer->filename filename 499)))
						      (begin
							buffer->num_visitors++
							(return buffer)))
						  (set! buffer buffer->next))
					   (set! buffer (calloc 1 (sizeof <struct-buffer>)))
					   (strncpy buffer->filename filename 499)
					   (set! buffer->channel channel)
					   (set! buffer->num_visitors 1)
					   (set! buffer->next buffers)
					   (set! buffers buffer)
					   (return buffer))))

	(<void> free_buffer (lambda ((<struct-buffer-*> buffer))
			      buffer->num_visitors--
			      (if (== 0 buffer->num_visitors)
				  (begin
				    (free buffer->buffer)
				    (set! buffer->buffer NULL)))))
	

	;;;;;;; rt-readin
	"typedef float (*Callback)(void *,int pos)"
	
	(<float> get_byte (lambda ((<char-*> data)(<int> pos))
			    (return (/ data[pos] 128.0f))))
	(<float> get_short (lambda ((<short-*> data)(<int> pos))
			    (return (/ data[pos] 32768.0f))))
	(<float> get_float (lambda ((<float-*> data)(<int> pos))
			     (return data[pos])))
	
	(<nonstatic-float> rt_readin (lambda ((<struct-mus_rt_readin-*> readin))
				       (let* ((buffer <struct-buffer-*> readin->buffer)
					      (callback <Callback> buffer->readin_raw_func)
					      (ret <float> (?kolon (or (< readin->location 0)
								       (>= readin->location readin->length))
								   0.0f
								   (callback buffer->buffer readin->location))))
					 ;;(fprintf stderr (string "dir: %d, pos: %d ret: %f\\n") dir pos ret)
					 ;;(fprintf stderr (string "gakk2\\n"))
					 (+= readin->location readin->increment)
					 (return ret))))
	
	(public
	 (<float> rt-readin (lambda ((<SCM> rt_readin_smob))
			      (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
				;;(fprintf stderr (string "gakk\\n"))
				(return (rt_readin readin))))))
	
	
	;;;;;;; Das SMOB
	
	(<scm_t_bits> rt_readin_tag)
	(<nonstatic-scm_t_bits> get_rt_readin_tag (lambda ()
						    (return rt_readin_tag)))
	 
	(public
	  (<SCM> rt-readin-p (lambda ((<SCM> rt_readin_smob))
			       (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
				   (return SCM_BOOL_T)
				   (return SCM_BOOL_F)))))
	 
	 (<SCM> mark_rt_readin (lambda ((<SCM> rt_readin_smob))
				 (let* ((rt_readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
				   (return rt_readin->scm_readin))))
	 (<size_t> free_rt_readin (lambda ((<SCM> rt_readin_smob))
				    (let* ((rt_readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
				      (free_buffer rt_readin->buffer)
				      (free rt_readin->core)
				      (free rt_readin)
				      (return 0))))
	 (<int> print_rt_readin (lambda ((<SCM> rt_readin_smob) (<SCM> port) (<scm_print_state-*> pstate))
				  (scm_puts (string "#<rt_readin ... > ") port)
				  (return 1)))
	 
	 (public

	   ;;;;;;; CLM methods for rt-readin
	  (<int> rt-readin-channels (lambda ((<SCM> rt_readin_smob))
				      (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					  (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					    (return readin->channels))
					  (return 0))))
	  (<int> rt-readin-channel (lambda ((<SCM> rt_readin_smob))
				     (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					 (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					   (return readin->channel))
					 (return 0))))
	  (<float> rt-readin-increment (lambda ((<SCM> rt_readin_smob))
					 (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					     (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					       (return readin->increment)))
					 (return 0)))
	  (<float> rt-readin-set_increment (lambda ((<SCM> rt_readin_smob)
						    (<float> val))
					     (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
						 (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
						   (set! readin->increment val)
						   (return val)))
					     (return 0)))
	  (<int> rt-readin-length (lambda ((<SCM> rt_readin_smob))
				    (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					(let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					  (return readin->length)))
				    (return 0)))
	  (<char-*> rt-readin-file-name (lambda ((<SCM> rt_readin_smob))
					  (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					      (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
						(return (mus_file_name "((struct mus_rt_readin *) readin)->readin"))))
					  (return NULL)))
	  (<int> rt-readin-location (lambda ((<SCM> rt_readin_smob))
				      (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					  (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
					    (return readin->location)))
				      (return 0)))
	  (<int> rt-readin-set_location (lambda ((<SCM> rt_readin_smob)						 
						 (<int> loc))
					  (if (SCM_SMOB_PREDICATE rt_readin_tag rt_readin_smob)
					      (let* ((readin <struct-mus_rt_readin-*> (cast <void-*> (SCM_SMOB_DATA rt_readin_smob))))
						(set! readin->location loc)
						(return loc)))
					  (return 0)))
	
	  (<SCM> make-rt-readin2 (lambda ((<SCM> scm_readin))
				  (let* ((readin <mus_any-*> (XEN_TO_MUS_ANY scm_readin))
					 (ret <struct-mus_rt_readin-*> (calloc 1 (sizeof <struct-mus_rt_readin>)))
					 (scmret <SCM>)
					 (filename <char-*> (mus_file_name readin))
					 (channel <int> (mus_channel readin))
					 (buffer <struct-buffer-*> (find_buffer filename channel)))

				    ;;(fprintf stderr (string "readin (make): %x\\n") ret)
				    
				   (set! ret->readin readin)
				   (set! ret->scm_readin scm_readin)
				   (set! ret->readin_func rt_readin)

				   (set! ret->buffer buffer)
				   ;;(fprintf stderr (string "readin (make), buffer: %x\\n") buffer)
				   
				   (if (== NULL buffer->buffer)
				       (let* ((sfinfo <SF_INFO> {0})
					      (sndfile <SNDFILE-*> (sf_open filename SFM_READ &sfinfo))
					      (framesize <int> 4)
					      (format <int> (& SF_FORMAT_SUBMASK sfinfo.format)))
					 
					 ;;(SCM_ASSERT (!= NULL sndfile) scm_readin 0 (string "make-rt-readin: Could not open file."))
					 (if (== NULL sndfile)
					     (begin
					       (printf (string "libsndfile could not open file: "))
					       (printf filename)
					       (printf (string "\\n"))
					       (return SCM_BOOL_F)))
					 
					 (cond ((== format SF_FORMAT_PCM_S8)
						(set! framesize 1)
						(set! buffer->readin_raw_func get_byte))
					       ((== format SF_FORMAT_PCM_U8)
						(set! framesize 1)
						(set! buffer->readin_raw_func get_byte))
					       ((== format SF_FORMAT_PCM_16)
						(set! framesize 2)
						(set! buffer->readin_raw_func get_short))
					       (else
						(set! buffer->readin_raw_func get_float)))
					 
					 (set! buffer->buffer (malloc (* framesize sfinfo.frames)))
					 (set! buffer->num_frames sfinfo.frames)
					 ;;(fprintf stderr (string "framesize: %d format: %d frames: %d\\n") framesize format buffer->num_frames)
					 (for-each 0 sfinfo.frames
						   (lambda (i)
						     (cond ((== framesize 1)
							    (if (== sfinfo.format SF_FORMAT_PCM_S8)
								(let* ((new[sfinfo.channels] <char>))
								  (sf_read_raw sndfile new 1)
								  (set! "((char *)buffer->buffer)[i]" new[channel]))
								(let* ((new[sfinfo.channels] <short>))
								  (sf_readf_short sndfile new 1)
								  (set! "((char *)buffer->buffer)[i]" (/ new[channel] 128)))))
							   ((== framesize 2)
							    (let* ((new[sfinfo.channels] <short>))
							      (sf_readf_short sndfile new 1)
							      (set! "((short *)buffer->buffer)[i]" new[channel])))
							   (else
							    (let* ((new[sfinfo.channels] <float>))
							      (sf_readf_float sndfile new 1)
							      (set! "((float *) buffer->buffer)[i]" new[channel]))))))
					 (sf_close sndfile)))

				   
				   (set! ret->channels  (mus_channels readin))
				   (set! ret->channel channel)
				   (set! ret->location  0)
				   (set! ret->increment (mus_increment readin))
				   (set! ret->length    (mus_length readin))

				   (SCM_NEWSMOB scmret rt_readin_tag ret)
				   (return scmret)))))

	(run-now
	 (set! rt_readin_tag (scm_make_smob_type (string "rt_readin") (sizeof <struct-mus_rt_readin>)))
	 (scm_set_smob_mark rt_readin_tag mark_rt_readin)
	 (scm_set_smob_free rt_readin_tag free_rt_readin)
	 (scm_set_smob_print rt_readin_tag print_rt_readin)))


(define (make-rt-readin areadin)
  (let ((ret (if (string? areadin)
		 (make-rt-readin2 (make-readin areadin))
		 (make-rt-readin2 areadin))))
    (if (not ret)
	(begin
	  (c-display "Could not make rt-readin for" areadin)
	  (throw 'could-not-make-rt-readin))
	ret)))

#!
(make-rt-readin "/home/kjetil/flute2.wav")
!#

;;(<rt-type> '<rt-readin> rt-readin-p 'rt_scm_to_rt_readin #:transformfunc SCM_SMOB_DATA #:c-type '<struct-mus_rt_readin-*> #:subtype-of '<mus_any-*>)
(<rt-type> '<rt-readin>
	   (lambda (readin)
	     (or (readin? readin)
		 (rt-readin-p readin)))
	   'rt_scm_to_rt_readin
	   #:transformfunc (lambda (readin)
			     (if (rt-readin-p readin)
				 (SCM_SMOB_DATA readin)
				 (let ((rt-readin (make-rt-readin readin)))
				   (list 'extra-gc-var
					 rt-readin
					 (SCM_SMOB_DATA rt-readin)))))
	   #:c-type '<struct-mus_rt_readin-*>
	   #:subtype-of '<mus_any-*>
	   )


(define-rt-ec <struct-mus_rt_readin-*> rt_scm_to_rt_readin
	     (lambda (,rt-globalvardecl (<SCM> name))
	       ,(if (rt-is-safety?)
		    `(if (not (SCM_SMOB_PREDICATE rt_readin_tag name))
			 (begin
			   (rt_error rt_globals (string "Variable is not an rt-readin generator"))
			   (return NULL))
			 (return (cast <void-*> (SCM_SMOB_DATA name))))
		    `(return (cast <void-*> (SCM_SMOB_DATA name))))))


;;(<rt-func> 'rt_readin '<float> '(<rt-readin>))
(rt-renamefunc readin rt_readin <float> (<rt-readin>))

;(rt-renamefunc readin rt-c-dasreadin <float> (<rt-readin>))
;(define-c-macro (rt-c-dasreadin readin)
;  (<-> "((ReadinFunc) ( ((struct mus_rt_readin *)" (eval-c-parse readin) ")->readin_func)) ((struct mus_rt_readin *)" (eval-c-parse readin) ")"))
  

(for-each (lambda (gen)
	    (let* ((funcname (car gen))
		   (rettype (cadr gen))
		   (elname (if (= 3 (length gen))
			       (caddr gen)
			       funcname)))
	      (primitive-eval `(define-rt-macro (,(symbol-append 'mus- funcname) expand/gen)
				 (if (rt-immediate? gen)
				     `(if (is-type? <rt-readin> ,gen)
					  (,',(symbol-append 'rt-readin- funcname) ,gen)
					  (,',(symbol-append 'rt-mus- funcname '/mus_ funcname) ,gen))
				     (let ((g (rt-gensym)))
				       `(let ((,g ,gen))
					  (if (is-type? <rt-readin> ,g)
					      (,',(symbol-append 'rt-readin- funcname) ,g)
					      (,',(symbol-append 'rt-mus- funcname '/mus_ funcname) ,g)))))))
	      
	      ;;(<rt-func> (symbol-append 'rt-readin- funcname) rettype '(<rt-readin>) #:is-immediate #t)
	      (<rt-func> (symbol-append 'rt-readin- funcname) rettype '(<mus_any-*>) #:is-immediate #t)
	      (primitive-eval `(define-c-macro (,(symbol-append 'rt-readin- funcname) agen2)
				 (<-> (symbol->string ,'agen2) "->" ,(symbol->string elname))))))
	  '((channels <int>)
	    (channel <int>)
	    (increment <float>)
	    (length <int>)
	    (file-name <char-*> file_name)
	    (location <int>)))


(define-rt-macro (setter!-mus-location expand/gen expand/val)
  (if (rt-immediate? gen val)
      `(if (is-type? <rt-readin> ,gen)
	   (rt-readin-set_location ,gen ,val)
	   (setter!-mus-location/mus_set_location ,gen ,val))
      (let ((g (rt-gensym))
	    (g2 (rt-gensym)))
	`(let ((,g ,gen)
	       (,g2 ,val))
	   (if (is-type? <rt-readin> ,g)
	       (rt-readin-set_location ,g ,g2)
	       (setter!-mus-location/mus_set_location ,g ,g2))))))
(<rt-func> 'rt-readin-set_location '<void> '(<rt-readin> <int>) #:is-immediate #t)
(define-c-macro (rt-readin-set_location gen val)
  (<-> (symbol->string gen) "->location=" (eval-c-parse val)))


(define-rt-macro (setter!-mus-increment expand/gen expand/val)
  (if (rt-immediate? gen val)
      `(if (is-type? <rt-readin> ,gen)
	   (rt-readin-set_increment ,gen ,val)
	   (setter!-mus-increment/mus_set_increment ,gen ,val))
      (let ((g (rt-gensym))
	    (g2 (rt-gensym)))
	`(let ((,g ,gen)
	       (,g2 ,val))
	   (if (is-type? <rt-readin> ,g)
	       (rt-readin-set_increment ,g ,g2)
	       (setter!-mus-increment/mus_set_increment ,g ,g2))))))
(<rt-func> 'rt-readin-set_increment '<void> '(<rt-readin> <int>) #:is-immediate #t)
(define-c-macro (rt-readin-set_increment gen val)
  (<-> (symbol->string gen) "->increment=" (eval-c-parse val)))








#!

(define file (make-readin "/home/kjetil/t1.wav"))
(mus-file-name file)
(set! (mus-file-name file) "gakkgakk")
(define rt-file (make-rt-readin file))
(rt-readin rt-file)
(set! (mus-increment file) 1)
(set! (mus-location file) 5470)
(mus-channel file)

(define a (rt-2 '(lambda ()
		   (out (readin rt-file)))))
setter!-rt-mus-set_location/mus_set_location
setter!-rt-mus-location/mus_location

(rt-2 '(lambda ()
	 (set! (mus-location rt_file) 0)))
(macroexpand-1 '(rt-macro-setter!-mus-location ai))


(define file (make-readin "/home/kjetil/t1.wav"))

(define rt-file (make-rt-readin file))

(rt-run 0 100
	(lambda ()
	  (if (>= (mus-location file) (mus-length file))
	      (set! (mus-location file) 200))
	  (out (readin file))))
(mus-length file)
(mus-location file)	  
(set! (mus-location file) 2000)
(-> *rt-engine* start)
(-> *rt-engine* stop)
(rt-funcall a)


(let ((rs (make-readin "/home/kjetil/t1.wav")))
  (-> (rt (lambda ()
	       (if (>= (mus-location rs) (mus-length rs))
		   (set! (mus-location rs) 0))
	       (out (readin rs))))
      play))


(define osc (make-oscil #:frequency 440))
(define i (rt (lambda ()
		   (oscil osc))))
(-> i play)
(-> i stop)
(rt-clear-cache!)
(-> *rt-engine* start)

(define osc (make-oscil #:frequency 440))
(define vol 0.4)
(define instrument (rt (lambda ()
			    (out (* vol (oscil osc))))))
(-> instrument play)
(-> instrument stop)

!#



#!

(define osc (make-oscil #:frequency 440))
(define loc (make-locsig :degree 80 :distance 1 :channels 2))
(rt-2 '(lambda ()
	 (mus-frequency (vector-ref a b))))

	 (set! (mus-frequency (vector-ref a b)) 200)
	 ;;(* transposition (vct-ref peak-freqs k)))))
	  (locsig loc (* (oscil osc) 0.5))
!#






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; ALSA-MIDI. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *rt-midi* #f)


(define rt-alsa-midi-parser-func
  '(<void> receive_midi (lambda ((<struct-RT_Globals-*> rt_globals)
				 (<snd_seq_t-*> seq)
				 ((<void> (<struct-RT_Globals-*> <int> <int> <int>)) func))
			  (<snd_seq_event_t-*> event)
			  (if (snd_seq_event_input_pending seq 1)
	 "ai:
                      snd_seq_event_input(seq,&event);
      switch(event->type){
    case SND_SEQ_EVENT_NOTEON:
      //printf(\"Noteon, channel: %d note: %d vol: %d\\n\",event->data.note.channel,event->data.note.note,event->data.note.velocity);
      func(rt_globals,0x90+event->data.note.channel,event->data.note.note,event->data.note.velocity);
      break;
    case SND_SEQ_EVENT_NOTEOFF:
      //printf(\"Noteoff, channel: %d note: %d vol: %d\\n\",event->data.note.channel,event->data.note.note,event->data.note.velocity);
      func(rt_globals,0x90+event->data.note.channel,event->data.note.note,0);
      break;
    case SND_SEQ_EVENT_KEYPRESS:
      //printf(\"Keypress, channel: %d note: %d vol: %d\\n\",event->data.note.channel,event->data.note.note,event->data.note.velocity);
      func(rt_globals,0xa0+event->data.note.channel,event->data.note.note,event->data.note.velocity);
      break;
    case SND_SEQ_EVENT_CONTROLLER:
      //printf(\"Control: %d %d %d\\n\",event->data.control.channel,event->data.control.param,event->data.control.value);
      func(rt_globals,0xb0+event->data.control.channel,event->data.control.param,event->data.control.value);
      break;
    case SND_SEQ_EVENT_PITCHBEND:
      //printf(\"Pitch: %d %d %d\\n\",event->data.control.channel,event->data.control.param,event->data.control.value);
      {
int val=event->data.control.value + 0x2000;
func(rt_globals,0xe0+event->data.control.channel,val&127,val>>7);
      }
      break;
    case SND_SEQ_EVENT_CHANPRESS:
      //printf(\"chanpress: %d %d %d\\n\",event->data.control.channel,event->data.control.param,event->data.control.value);
      func(rt_globals,0xc0+event->data.control.channel,event->data.control.value,0);
      break;
    case SND_SEQ_EVENT_PGMCHANGE:
      // printf(\"pgmchange: %d %d %d\\n\",event->data.control.channel,event->data.control.param,event->data.control.value);
      func(rt_globals,0xc0+event->data.control.channel,event->data.control.value,0);
      break;
    case SND_SEQ_EVENT_START:
      func(rt_globals,0xfa,0,0);
      break;
    case SND_SEQ_EVENT_CONTINUE:
      func(rt_globals,0xfb,0,0);
      break;
    case SND_SEQ_EVENT_STOP:
      func(rt_globals,0xfc,0,0);
      break;
      //case SND_SEQ_EVENT_SETPOS_TICK:
      //func(rt_globals,0xf,0,0);
      //break;
      //case SND_SEQ_EVENT_SETPOS_TIME:
      //func(rt_globals,0xf,0,0);
      //break;
      //case SND_SEQ_EVENT_TEMPO:
      //func(rt_globals,0xf,0,0);
      //break;
      //case SND_SEQ_EVENT_CLOCK:
      //func(rt_globals,0xf,0,0);
      //break;
      //case SND_SEQ_EVENT_TICK: 
      //func(rt_globals,0xf,0,0);
      //break;
    case SND_SEQ_EVENT_RESET:
      func(rt_globals,0xff,0,0);
      break;
    case SND_SEQ_EVENT_SENSING:
      func(rt_globals,0xfe,0,0);
      break;
    default:
      break;
    }
    if (snd_seq_event_input_pending (seq, 0))
       goto ai;
"))))



(if *use-alsa-midi*
    (eval-c "-lasound"
	    "#include <stdio.h>"
	    "#include  <alsa/asoundlib.h>"
	    (public
	     (<snd_seq_t-*> create_alsa_seq (lambda ((<char-*> client_name)
						     (<int> isinput))
					      (<snd_seq_t-*> seq)
					      (<int> err)
					      (set! err (snd_seq_open &seq (string "default") SND_SEQ_OPEN_DUPLEX 0))
					      (if err
						  (begin
						    (fprintf stderr (string ,(<-> "Could not open ALSA sequencer, aborting\\n\\n%s\\n\\n"
										  "Make sure you have configure ALSA properly and that\\n"
										  "/proc/asound/seq/clients exists and contains relevant\\n"
										  "devices.\\n"))
							     (snd_strerror err))
						    (return NULL)))
					      (snd_seq_set_client_name seq client_name)
					      (set! err (snd_seq_create_simple_port seq
										    "isinput?\"Input\":\"Output\""
										    "(isinput?SND_SEQ_PORT_CAP_WRITE:SND_SEQ_PORT_CAP_READ)|SND_SEQ_PORT_CAP_SUBS_READ|SND_SEQ_PORT_CAP_SUBS_WRITE"
										    SND_SEQ_PORT_TYPE_APPLICATION|SND_SEQ_PORT_TYPE_SPECIFIC))
					      (if err
						  (begin
						    (fprintf stderr (string "Could not create ALSA port (%s)\\n") (snd_strerror err))
						    (snd_seq_close seq)
						    (return NULL)))
					      (return seq))))

	    "struct RT_Globals{int *controls;int *data1s;int *data2s;int num_events;SCM ret;};"
	    
	    ,rt-alsa-midi-parser-func

	    
	    (<void> receiver (lambda ((<struct-RT_Globals-*> globals)
				      (<int> control)
				      (<int> data1)
				      (<int> data2))
			       (if (== globals->num_events 500)
				   return)
			       (set! globals->controls[globals->num_events] control)
			       (set! globals->data1s[globals->num_events] data1)
			       (set! globals->data2s[globals->num_events] data2)
			       globals->num_events++))
	    (<nonstatic-void> rt_receive_midi (lambda ((<void-*> data)
						       (<int> block_time)
						       (<snd_seq_t-*> seq)
						       ((<void> (<void-*> <int> <int> <int>)) func))
						(<static-int> controls[500])
						(<static-int> data1s[500])
						(<static-int> data2s[500])
						(<static-int> num_events 0)
						(<static-int> last_read_time 0)
						(if (!= block_time last_read_time)
						    (begin
						      (<struct-RT_Globals> globals)
						      (set! globals.controls controls)
						      (set! globals.data1s data1s)
						      (set! globals.data2s data2s)
						      (set! globals.num_events 0)
						      (receive_midi &globals seq receiver)
						      (set! num_events globals.num_events)
						      (set! last_read_time block_time)))
						(for-each 0 num_events
							  (lambda (n)
							    (func data controls[n] data1s[n] data2s[n])))))
	    
	    (<void> receiver2 (lambda ((<struct-RT_Globals-*> globals)
				       (<int> data1)
				       (<int> data2)
				       (<int> data3))
				(set! globals->ret (scm_cons (scm_cons (MAKE_INTEGER data1)
								       (scm_cons (MAKE_INTEGER data2)
										 (scm_cons (MAKE_INTEGER data3)
											   SCM_EOL)))
							     globals->ret))))
	    (public
	     (<SCM> rt-receive-midi (lambda ((<snd_seq_t-*> seq))
				      (<struct-RT_Globals> globals)
				      (set! globals.ret SCM_EOL)
				      (receive_midi &globals seq receiver2)
				      (return globals.ret))))
	    (run-now
	     (printf (string "Alsa library loaded.\\n")))))
	    





(if *use-alsa-midi*
    (set! *rt-midi* (create_alsa_seq *rt-midi-alsaname* 1)))


(<rt-func> 'rt_receive_midi '<void> '(<int>
				      <snd_seq_t-*>
				      (<void> (<int> <int> <int>)))
	   #:needs-rt-globals #t)

(<rt-type> '<snd_seq_t-*>
	   (lambda (seq)
	     (and (list? seq)
		  (= 2 (length seq))
		  (string? (car seq))
		  (string=? (car seq) "A_POINTER")
		  (number? (cadr seq))))
	   #f)


;;(define-c-macro (rt-get-framenum)
;;  "rt_globals->framenum")
;;(<rt-func> 'rt-get-framenum '<int> '() #:is-immediate #t)
;;(define-rt-macro (first-frame?)
;;  `(= 0 (rt-get-framenum)))

(define-c-macro (get-time)
  "rt_globals->time")
;;  "(rt_globals->time+rt_globals->framenum)")
(<rt-func> 'get-time '<int> '() #:is-immediate #t)

;; No, this is not good. Should return frames not seconds. Must fix.
;;(define-rt-macro (get-time)
;;  `(/ (rt-get-exact-time) (mus-srate)))
	 

(define-c-macro (rt-get-block-time)
  "rt_globals->block_time")
(<rt-func> 'rt-get-block-time '<int> '() #:is-immediate #t)
(define-c-macro (rt-midi-read)
  "rt_globals->is_midi_read")
(<rt-func> 'rt-midi-read '<int> '() #:is-immediate #t)
(define-c-macro (rt-midi-is-read!)
  "rt_globals->is_midi_read=1")
(<rt-func> 'rt-midi-is-read! '<void> '() #:is-immediate #t)

(define-rt-macro (receive-midi func)
  `(if (not (rt-midi-read))
       (begin
	 (rt_receive_midi (rt-get-block-time) *rt-midi* (lambda ,(cadr func)
							  (declare (<int> ,@(cadr func)))
							  ,@(c-butlast (cddr func))
							  (the <void> ,(last (cddr func)))))
	 (rt-midi-is-read!))))


;; Logic partly taken from pd by Miller Puckette.
(define-rt-macro (midi-to-freq f)
  (define freq (rt-gensym))
  `(let ((,freq ,f))
     (cond ((<= ,freq 0) 0)
           ((> ,freq 135) 20000)
           (else
            (* 8.17579891564 (exp (* .0577622650 ,freq)))))))

(define (midi-to-freq f)
  (* 8.17579891564 (exp (* .0577622650 f))))  

#!
(caddr (assq 'receive_midi rt-ec-functions))
!#




(define all-midi-receivers '())
(define midi-receivers-running #f)
(define *midi-poll-freq* 500)

(define (receive-midi func)
  (letrec ((poll-freq (/ 1000 *midi-poll-freq* ))
	   (das-loop (lambda ()
		       (if (not midi-receivers-running)
			   (set! all-midi-receivers '())
			   (begin
			     (for-each (lambda (ai)
					 (for-each (lambda (func)
						     (catch #t
							    (lambda ()
							      (apply func ai))
							    (lambda (key . args)
							      (c-display "Error: key/args" key args))))
						   all-midi-receivers))
				       (reverse! (rt-receive-midi *rt-midi*)))
			     (in poll-freq das-loop)))))
	   (add-it (lambda ()
		     (if (and (not midi-receivers-running)
			      (not (null? all-midi-receivers)))
			 (in 50 add-it)
			 (begin
			   (set! midi-receivers-running #t)
			   (push! func all-midi-receivers)
			   (if (= 1 (length all-midi-receivers))
			       (das-loop)))))))
    (add-it)))


(define (stop-receiving-midi!)
  (set! midi-receivers-running #f))

#!
(receive-midi (lambda (data1 data2 data3)
		(if (not (= data1 254))
		    (c-display data1 data2 data3))))
(stop-receiving-midi!)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Time macros. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rt-macro (^f t)
  t)
(define-rt-macro (^b t)
  (* 1024 ,t)) ;;FIX
(define-rt-macro (^ms t)
  `(* ,(/ (rte-samplerate) 1000) ,t))
(define-rt-macro (^s t)
  `(* ,(rte-samplerate) ,t))
(define-rt-macro (^m t)
  `(* ,(* 60 (rte-samplerate)) ,t))
(define-rt-macro (^h t)
  `(* ,(* 60 60 (rte-samplerate)) ,t))


#!
(^s 1) = 1 second
(^ms 1) = 1 millisecond
(^f 1) = 1 frame
(^b 1) = 1 block (not sure how this one to work exactly)
(^m 1) = 1 minute
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Wait. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (wait w 50 thunk)  waits 50 samples



#!
old syntax: (not very nice)
(define (make-wait)
  (vct 0 0 0))

(define-rt-macro (wait_old w len thunk)
  `(begin
     (if (not (vct-ref ,w 2))
	 (begin
	   (vct-set! ,w 0 ,len)
	   (vct-set! ,w 1 ,len)
	   (vct-set! ,w 2 1))
	 (if (> (vct-ref ,w 0) 1)
	     (vct-set! ,w 0 (1- (vct-ref ,w 0)))
	     (if (< (vct-ref ,w 2) 2)
		 (begin
		   (vct-set! ,w 2 2)
		   (,thunk)))))))
!#


(define-rt-macro (wait_old len thunk)
  (define counter (rt-gensym))
  `(when (< (extern ,counter 0) ,len)
     (declare (<int> ,counter))
     (set! ,counter (1+ ,counter))
     (when (= ,len ,counter)
       (set! ,counter 0)
       (,thunk))))

#!
(<rt-play> (lambda ()
	     (wait (* 1 (mus-srate))
		   (lambda ()
		     (printf "1\\n")
		     (remove-me)))))
!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; LADSPA. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ec-struct <mus_rt_ladspa>
  <LADSPA_Descriptor-*> descriptor
  <LADSPA_Handle> handle
  <float-*> controls
  <float-**> ins
  <float-**> outs

  <float-*> controls_mins
  <float-*> controls_defaults
  <float-*> controls_maxs
  
  <int> num_controls_in
  <int> num_controls_out
  <int> num_audio_outs
  <int> num_audio_ins

  <int-*> audioin_port_nums
  <int-*> audioout_port_nums
  <int-*> controlin_port_nums
  <int-*> controlout_port_nums

  <float-*> controls_out
  
  <vct-*> output
  
  <SCM> scm_output
  <SCM> scm_descriptor
  <SCM> scm_handle
  <SCM> scm_ladspa
  )


(eval-c (<-> "-I" snd-header-files-path " ")
	"#include <ladspa.h>"
	"#include <clm.h>"
	"#include <xen.h>"
	"#include <vct.h>"
	;;;;;;; Das SMOB

	(shared-struct <mus_rt_ladspa>)
	
	(<scm_t_bits> rt_ladspa_tag)
	(<nonstatic-scm_t_bits> get_rt_ladspa_tag (lambda ()
						    (return rt_ladspa_tag)))

	(public
	  (<SCM> rt-ladspa-p (lambda ((<SCM> rt_ladspa_smob))
			       (if (SCM_SMOB_PREDICATE rt_ladspa_tag rt_ladspa_smob)
				   (return SCM_BOOL_T)
				   (return SCM_BOOL_F))))
	  (<void> ladspa-set2 (lambda ((<SCM> rt_ladspa_smob)				       
				       (<int> controlnum)
				       (<float> val))
				(if (not (SCM_SMOB_PREDICATE rt_ladspa_tag rt_ladspa_smob))
				    (fprintf stderr (string "Not a ladspa object\\n"))
				    (let* ((ladspa <struct-mus_rt_ladspa-*> (cast <void-*> (SCM_SMOB_DATA rt_ladspa_smob))))
				      (if (< controlnum ladspa->num_controls_in)
					  (set! ladspa->controls[controlnum] val))))))

	  (<SCM> make-ladspa2 (lambda ((<struct-mus_rt_ladspa-*> ladspa))

				(<SCM> scmret)
				
				(set! ladspa->ins (calloc (sizeof <float-*>) ladspa->num_audio_ins))
				(for-each 0 ladspa->num_audio_ins
					  (lambda (n)
					    (set! ladspa->ins[n] (calloc (sizeof <float>) ,rt-max-frame-size))
					    (ladspa->descriptor->connect_port ladspa->handle 
									      ladspa->audioin_port_nums[n]
									      ladspa->ins[n])))
				
				(set! ladspa->outs (calloc (sizeof <float-*>) ladspa->num_audio_outs))
				(for-each 0 ladspa->num_audio_outs
					  (lambda (n)
					    (set! ladspa->outs[n] (calloc (sizeof <float>) ,rt-max-frame-size))
					    (ladspa->descriptor->connect_port ladspa->handle 
									      ladspa->audioout_port_nums[n]
									      ladspa->outs[n])))

				;;(set! ladspa->controls (calloc (sizeof <float>) ladspa->num_controls_in))
				(for-each 0 ladspa->num_controls_in
					  (lambda (n)
					    (ladspa->descriptor->connect_port ladspa->handle 
									      ladspa->controlin_port_nums[n]
									      &ladspa->controls[n])))
					    
				(set! ladspa->controls_out (calloc (sizeof <float>) ladspa->num_controls_out))
				(for-each 0 ladspa->num_controls_out
					  (lambda (n)
					    (ladspa->descriptor->connect_port ladspa->handle 
									      ladspa->controlout_port_nums[n]
									      &ladspa->controls_out[n])))
					    
				(if ladspa->descriptor->activate
				    (ladspa->descriptor->activate ladspa->handle))
				
				(SCM_NEWSMOB scmret rt_ladspa_tag ladspa)
				(set! ladspa->scm_ladspa scmret)

				(return scmret))))
	 
	(<SCM> mark_rt_ladspa (lambda ((<SCM> rt_ladspa_smob))
				(let* ((rt_ladspa <struct-mus_rt_ladspa-*> (cast <void-*> (SCM_SMOB_DATA rt_ladspa_smob))))
				  ;;(fprintf stderr (string "scm_output: %p, scm_descriptor: %p\\n") rt_ladspa->scm_output rt_ladspa->scm_descriptor)
				  (scm_gc_mark rt_ladspa->scm_output)
				  (scm_gc_mark rt_ladspa->scm_descriptor)
				  (scm_gc_mark rt_ladspa->scm_handle)
				  (return rt_ladspa->scm_ladspa))))
	
	(<size_t> free_rt_ladspa (lambda ((<SCM> rt_ladspa_smob))
				   (let* ((ladspa <struct-mus_rt_ladspa-*> (cast <void-*> (SCM_SMOB_DATA rt_ladspa_smob))))

				     ;;(fprintf stderr (string "starint to free\\n"))
				     (if ladspa->descriptor->deactivate
					 (ladspa->descriptor->deactivate ladspa->handle))
				     (if ladspa->descriptor->cleanup
					 (ladspa->descriptor->cleanup ladspa->handle))
				     
				     (free ladspa->controls)
				     (free ladspa->controls_out)
				     
				     (for-each 0 ladspa->num_audio_ins
					       (lambda (n)
						 (free ladspa->ins[n])))
				     (free ladspa->ins)
				     (for-each 0 ladspa->num_audio_outs
					       (lambda (n)
						 (free ladspa->outs[n])))
				     (free ladspa->outs)
				     (free ladspa->audioin_port_nums)
				     (free ladspa->audioout_port_nums)
				     (free ladspa->controlin_port_nums)
				     (free ladspa->controlout_port_nums)
				     
				     (free ladspa)
				     ;;(fprintf stderr (string "freed\\n"))
				     (return 0))))
	
	(<int> print_rt_ladspa (lambda ((<SCM> rt_ladspa_smob) (<SCM> port) (<scm_print_state-*> pstate))
				 (scm_puts (string "#<rt_ladspa ... > ") port)
				 (return 1)))
	
	(run-now
	 (set! rt_ladspa_tag (scm_make_smob_type (string "rt_ladspa") (sizeof <struct-mus_rt_ladspa>)))
	 (scm_set_smob_mark rt_ladspa_tag mark_rt_ladspa)
	 (scm_set_smob_free rt_ladspa_tag free_rt_ladspa)
	 (scm_set_smob_print rt_ladspa_tag print_rt_ladspa)))


(define-macro (ladspa-set! l c v)
  `(ladspa-set2 (cadr ,l) ,c ,v))

(define (ladspa-get-min ladspa control-num)
  (list-ref (-> (car ladspa) controls_mins) control-num))
(define (ladspa-get-default ladspa control-num)
  (list-ref (-> (car ladspa) controls_defaults) control-num))
(define (ladspa-get ladspa control-num)
  (list-ref (-> (car ladspa) controls) control-num))
(define (ladspa-get-max ladspa control-num)
  (list-ref (-> (car ladspa) controls_maxs) control-num))

(define (make-ladspa-gui das-ladspa)
  (letrec* ((ladspa (car das-ladspa))
	    (descriptor (caddr das-ladspa))
	    (name (.Name descriptor))
	    (libraryname (cadddr das-ladspa))
	    (effectname (cadr (cdddr das-ladspa)))
	    (author (.Maker descriptor))
	    (lisense (.Copyright descriptor))
	    (exit (lambda () (-> dialog hide)))
	    (Help (lambda ()
		    (let ((dashelp (assoc (string-append libraryname effectname) ladspa-help-assoclist)))
		      (help-dialog author
				   (string-append (if dashelp
						      (caddr dashelp)
						      lisense)
						  (string #\newline #\newline)
						  "Processing can be stopped by pressing C-g")))))
	    (Reset (lambda ()
		     (for-each (lambda (c v)
				 (ladspa-set! das-ladspa c v))
			       (iota (-> ladspa num_controls_in))
			       (-> ladspa controls_defaults))
		     (-> dialog hide)
		     (make-ladspa-gui das-ladspa)))
	    (toggles #f)
	    (dialog (<dialog> name exit
			      "Close" exit
			      "Reset" Reset
			      "Print" (lambda () (print-ladspa das-ladspa))
			      (if (assoc (string-append libraryname effectname) ladspa-help-assoclist)
				  "Help"
				  "Not much help")
			      Help)))

    (define (get-hint portnum)
      (car (list-ref (.PortRangeHints descriptor) portnum)))

    (define (ishint dashint dashint2)
      (not (= (logand dashint dashint2 ) 0)))

    (define (stupid->nonstupid stupid)
      (let ((gakk (-> ladspa controlin_port_nums)))
	(- (length gakk) (length (member stupid gakk)))))
    (define (s->n stupid)
      (stupid->nonstupid stupid))
    
    (if (> (-> ladspa num_controls_in) 0)
	(dialog 'add-sliders
		(map (lambda (portnum)
		       (let* ((lo (ladspa-get-min das-ladspa (s->n portnum)))
			      (init (ladspa-get das-ladspa (s->n portnum)))
			      (hi (ladspa-get-max das-ladspa (s->n portnum)))
			      (hint (get-hint portnum))
			      (scale (if (ishint hint LADSPA_HINT_INTEGER)
					 1
					 (if use-gtk 1000.0 100.0)))
			      (name (list-ref (.PortNames descriptor) portnum)))
			 (list name
			       lo
			       init
			       hi
			       (lambda (val)
				 (ladspa-set! das-ladspa (s->n portnum) val))
			       scale)))
		     (remove (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
			     (-> ladspa controlin_port_nums)))))
    
    
    ;; Add toggle buttons.
    (set! toggles (map (lambda (portnum)
			 (let* ((hint (get-hint portnum))
				(hi (ladspa-get-max das-ladspa (s->n portnum)))
				(lo (ladspa-get-min das-ladspa (s->n portnum)))
				(portname (list-ref (.PortNames descriptor) portnum))
				(ison (> (ladspa-get das-ladspa (s->n portnum)) 0)))
			   (<checkbutton> dialog
					  portname
					  (lambda (on)
					    (ladspa-set! das-ladspa (s->n portnum) (if on hi lo)))
					  ison)))
		       (filter-org (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
				   (-> ladspa controlin_port_nums))))
    
    (-> dialog show)
    dialog))
    

(define (print-ladspa das-ladspa)
  (let ((ladspa (car das-ladspa)))
    (define (stupid->nonstupid stupid)
      (let ((gakk (-> ladspa controlin_port_nums)))
	(- (length gakk) (length (member stupid gakk)))))
    (define (s->n stupid)
      (stupid->nonstupid stupid))
    (if (> (-> ladspa num_controls_in) 0)
	(begin
	  (c-display "\n(let ((plugin l))")
	  (for-each (lambda (portnum)
		      (c-display "  (ladspa-set! plugin" (s->n portnum) (ladspa-get das-ladspa (s->n portnum)) ")"))
		    (-> ladspa controlin_port_nums))
	  (c-display ")")))))


(define (make-ladspa libname pluginname)
  (let* ((descriptor #f)
	 (ladspa #f)
	 (handle #f)
	 (input-controls '())
	 (output-controls '())
	 (input-audios '())
	 (output-audios '())
	 (output-vct #f)
	 (smob #f))
    
    (define (get-hint portnum)
      (car (list-ref (.PortRangeHints descriptor) portnum)))
    
    (define (ishint portnum dashint)
      (not (= (logand (get-hint portnum) dashint) 0)))
    
    (define (get-lo portnum)
      (* (if (ishint portnum LADSPA_HINT_SAMPLE_RATE)
	     (srate)
	     1)
	 (if (not (ishint portnum LADSPA_HINT_BOUNDED_BELOW))
	     0                                   ;The value Ardour use.
	     (cadr (list-ref (.PortRangeHints descriptor) portnum)))))
    
    (define (get-hi portnum)
      (* (if (ishint portnum LADSPA_HINT_SAMPLE_RATE)
	     (srate)
	     1)
	 (if (not (ishint portnum LADSPA_HINT_BOUNDED_ABOVE))
	     (if (not (ishint portnum  LADSPA_HINT_TOGGLED))
		 4                                   ;The value Ardour use.
		 1)
	     (caddr (list-ref (.PortRangeHints descriptor) portnum)))))
    
    
    (set! descriptor (ladspa-descriptor libname pluginname))
    (if (not descriptor)
	(throw (symbol-append 'ladspa-plugin-not-found- (string->symbol libname) '- (string->symbol pluginname))))
    
    (set! ladspa (<mus_rt_ladspa> #:descriptor (list "A_POINTER" (cadr descriptor))))
    
    (set! handle (ladspa-instantiate descriptor (c-integer (mus-srate))))
    (-> ladspa handle (list "A_POINTER" (cadr handle)))
    (-> ladspa scm_handle handle)
    
    (c-for-each (lambda (n x)
		      (if (> (logand x LADSPA_PORT_CONTROL) 0)
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! input-controls (append input-controls (list n)))
			      (set! output-controls (append output-controls (list n))))
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! input-audios (append input-audios (list n)))
			      (set! output-audios (append output-audios (list n))))))
		(.PortDescriptors descriptor))
    
    (-> ladspa controlin_port_nums input-controls)
    (-> ladspa controlout_port_nums output-controls)
    (-> ladspa audioin_port_nums input-audios)
    (-> ladspa audioout_port_nums output-audios)

    (-> ladspa num_controls_in (length input-controls))
    (-> ladspa num_controls_out (length output-controls))
    (-> ladspa num_audio_outs (length output-audios))
    (-> ladspa num_audio_ins (length input-audios))
    
    (set! output-vct (make-vct (-> ladspa num_audio_outs)))
    (-> ladspa scm_output output-vct)
    (-> ladspa output (XEN_TO_VCT output-vct))

    (-> ladspa scm_descriptor descriptor)

    (-> ladspa controls_maxs (map get-hi input-controls))
    (-> ladspa controls_mins (map get-lo input-controls))
    
    (-> ladspa controls_defaults (map (lambda (x)
					(let ((hint (car (x 1)))
					      (lo (cadr (x 1)))
					      (hi (caddr (x 1))))
					  (define (ishint dashint)
					    (= (logand hint LADSPA_HINT_DEFAULT_MASK) dashint))
					  (define (ishint_notdefault dashint)
					    (not (= (logand hint dashint ) 0)))
					  
					  (cond ;;((and def-vals (assoc (x 0) def-vals )) (cdr (assoc (x 0) def-vals)))
					   ((ishint LADSPA_HINT_DEFAULT_0) 0)
					   ((ishint LADSPA_HINT_DEFAULT_MINIMUM) lo)
					   ((ishint LADSPA_HINT_DEFAULT_LOW) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
										 (exp (+ (* 0.75 (log lo)) (* 0.25 (log hi))))
										 (+ (* 0.75 lo) (* 0.25 hi))))
					   ((ishint LADSPA_HINT_DEFAULT_1) 1)
					   ((ishint LADSPA_HINT_DEFAULT_MAXIMUM) hi)
					   ((ishint LADSPA_HINT_DEFAULT_HIGH) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
										  (exp (+ (* 0.75 (log hi)) (* 0.25 (log lo))))
										  (+ (* 0.75 hi) (* 0.25 lo))))
					   ((ishint LADSPA_HINT_DEFAULT_MIDDLE) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
										    (exp (+ (* 0.5 (log hi)) (* 0.5 (log lo))))
										    (+ (* 0.5 hi) (* 0.5 lo))))
					   ((ishint LADSPA_HINT_DEFAULT_100) 100)
					   ((ishint LADSPA_HINT_DEFAULT_440) 440)
					   ((ishint LADSPA_HINT_SAMPLE_RATE) (srate))
					   (else
					    (/ (+ lo hi) 2)))))
				      
				      (map (lambda (x) (<array> x
								(list-ref (.PortRangeHints descriptor) x)))
					   input-controls)))

    (-> ladspa controls (-> ladspa controls_defaults))
					
    (set! smob (make-ladspa2 (-> ladspa get-c-object)))
    
    (list ladspa
	  smob
	  descriptor
	  libname
	  pluginname
	  )))


;; SCM->ladspa converter
(define-rt-ec <struct-mus_rt_readin-*> rt_scm_to_rt_ladspa
		(lambda (,rt-globalvardecl (<SCM> name))
		  (<SCM> ladspa (SCM_CAR (SCM_CDR name)))
		  ,(if (rt-is-safety?)
		       `(if (not (SCM_SMOB_PREDICATE rt_ladspa_tag ladspa))
			    (begin
			      (rt_error rt_globals (string "Variable is not an rt-ladspa object"))
			      (return NULL))
			    (return (cast <void-*> (SCM_SMOB_DATA ladspa))))
		       `(return (cast <void-*> (SCM_SMOB_DATA ladspa))))))


;; The ladspa type
(<rt-type> '<ladspa>
	   (lambda (l)
	     (and (list? l)
		  (rt-ladspa-p (cadr l))))
	   'rt_scm_to_rt_ladspa
	   #:transformfunc (lambda (l) (SCM_SMOB_DATA (cadr l)))
	   #:c-type '<struct-mus_rt_ladspa-*>)


;; The ladspa-run function, ladspa-run calls rt_ladspa_run which replace data in the vct.
;; (Yes, some kind of general buffer mechanism should be implemented. This is inefficient.)
(define-rt-ec <vct-*> rt_ladspa_run
		(lambda ((<ladspa> ladspa)
			 (<vct-*> input))
		  (let* ((minin <int> (MIN ladspa->num_audio_ins input->length)))
		    (for-each 0 minin
			      (lambda (n)
				(set! ladspa->ins[n][0] input->data[n])))
		    (ladspa->descriptor->run ladspa->handle 1)
		    (for-each 0 ladspa->num_audio_outs
			      (lambda (n)
				(set! ladspa->output->data[n] ladspa->outs[n][0])))
		    
		    (return ladspa->output))))

(define-rt-macro (ladspa-run ladspa input)
  `(rt_ladspa_run ,ladspa ,input))


;; The ladspa-set function
(define-rt-ec <void> rt_ladspa_set
	     (lambda ((<ladspa> ladspa)
		      (<int> controlnum)
		      (<float> val))
	       (if (< controlnum ladspa->num_controls_in)
		   (set! ladspa->controls[controlnum] val))))

(define-rt-macro (ladspa-set! ladspa controlnum val)
  `(rt_ladspa_set ,ladspa ,controlnum ,val))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Stalin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-load-from-path rt-stalin)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Faust ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-load-from-path rt-faust)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Ringbuffer. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!
(define-ec-struct <rt_ringbuffer_b>
  <SCM> scm_b
  <vct-*> b
  <int> s
  <int> e)

(define-ec-struct <rt_ringbuffer>
  <struct-rt_ringbuffer_b-*> b0
  <struct-rt_ringbuffer_b-*> b1
  <struct-rt_ringbuffer_b-*> b2
  <int> position
  <SCM> callback
  <int> size)

  
(define *rt-request-ringbuffer* (jack_ringbuffer_create 8192))
(define *rt-ringbuffer-poll-freq* (/ 1000 500))
(define *rt-rb-overlap* 10) ;; 10% overlap

(eval-c (<-> "-I" snd-header-files-path " -ffast-math ")
	"#include <clm.h>"
	"#include <xen.h>"
	"#include <vct.h>"
	"#include <jack/ringbuffer.h>"
	(shared-struct <rt_ringbuffer_b>)
	(shared-struct <rt_ringbuffer>)
	(public
	 (<void> rt_rb_set_start_endp (lambda ((<struct-rt_ringbuffer-*> rb)
					       (<int> w1)
					       (<int> startpos)
					       (<int> endpos))
					(let* ((temp <struct-rt_ringbuffer_b-*> NULL))
					  (cond ((== w1 0)
						 (set! temp rb->b0))
						((== w1 1)
						 (set! temp rb->b1))
						((== w1 2)
						 (set! temp rb->b2)))
					  (set! temp->s startpos)
					  (set! temp->e endpos))))
					    
						 
	 (<void> rt_rb_swap (lambda ((<struct-rt_ringbuffer-*> rb)
				     (<int> w1)
				     (<int> w2))
			      (let* ((temp1 <struct-rt_ringbuffer_b-*> NULL)
				     (temp2 <struct-rt_ringbuffer_b-*> NULL))
				
				(cond ((== w1 0)
				       (set! temp1 rb->b0))
				      ((== w1 1)
				       (set! temp1 rb->b1))
				      ((== w1 2)
				       (set! temp1 rb->b2)))
				(cond ((== w2 0)
				       (set! temp2 rb->b0)
				       (set! rb->b0 temp1))
				      ((== w2 1)
				       (set! temp2 rb->b1)
				       (set! rb->b1 temp1))
				      ((== w2 2)
				       (set! temp2 rb->b2)
				       (set! rb->b2 temp1)))
				(cond ((== w1 0)
				       (set! rb->b0 temp2))
				      ((== w1 1)
				       (set! rb->b1 temp2))
				      ((== w1 2)
				       (set! rb->b2 temp2))))))
	 
	 
	 (<void> rt_check_rb_request (lambda ((<jack_ringbuffer_t-*> req_ringbuffer)
					      (<SCM> func))
				       (while (>= (jack_ringbuffer_read_space req_ringbuffer)
						  (sizeof <struct-rt_ringbuffer-*>))
					      (let* ((rb <struct-rt_ringbuffer-*> NULL))
						(jack_ringbuffer_read req_ringbuffer (cast <char-*> rb) (sizeof <struct-rt_ringbuffer-*>))
						(scm_apply func
							   (MAKE_POINTER rb)
							   (scm_list_4 (MAKE_INTEGER rb->position)
								       (MAKE_INTEGER rb->size)
								       rb->callback
								       (scm_list_3 (scm_list_3 rb->b0->scm_b
											       (MAKE_INTEGER rb->b0->s)
											       (MAKE_INTEGER rb->b0->e))
										   (scm_list_3 rb->b1->scm_b
											       (MAKE_INTEGER rb->b1->s)
											       (MAKE_INTEGER rb->b1->e))
										   (scm_list_3 rb->b2->scm_b
											       (MAKE_INTEGER rb->b2->s)
											       (MAKE_INTEGER rb->b2->e)))))))))))
	

		 

(define (rt-ringbuffer-callback rb position size callback poss)
  (define (start-pos which)
    (cadr (list-ref poss which)))
  (define (end-pos which)
    (caddr (list-ref poss which)))
  (define (inside? pos which)
    (let ((whichwhich (list-ref poss which)))
      (and (>= pos (cadr whichwhich))
	   (<= pos (caddr whichwhich)))))
  (define (read which startpos endpos)
    (call-with-values (lambda ()
			(callback (car (list-ref poss which)) startpos endpos))
      (lambda (startpos endpos)
	(rt_rb_set_start_endp rb which startpos endpos))))

  (if (not (inside? position 1))
      (cond ((inside? position 0)
	     (rt_rb_swap rb 0 1)
	     (rt_rb_swap rb 2 0)
	     (read 0 (- (start-pos 0)) (size)) (start-pos 0))
	    ((inside? position 2)
	     (rt_rb_swap rb 1 2)
	     (rt_rb swap rb 0 2)
	     (read 2 (1+ (end-pos 2) (+ 1 (size) (end-pos 2))))))))
	  

    
(define (rt_request_check)
  (rt_check_rb_request)
  (in *rt-ringbuffer-poll-freq* rt_request_check))
(rt_request_check)

(rt_check_rb_request *rt-request-ringbuffer*)      
  
(define (make-ringbuffer-location bytes callback)
  (<rt_ringbuffer> #:b0 (-> (<rt_ringbuffer_b> #:scm_b (make-vct bytes)) get-c-object)
		   #:b1 (-> (<rt_ringbuffer_b> #:scm_b (make-vct bytes)) get-c-object)
		   #:b2 (-> (<rt_ringbuffer_b> #:scm_b (make-vct bytes)) get-c-object)
		   #:callback callback
		   #:size bytes))

(define r (make-ringbuffer-location 4096 #f))


!#
  
#!
(define file (file->sample "/home/kjetil/t1.wav"))
(define rb (make-ringbuffer-location (* 8192 256)
				     (lambda (location)
				       (file->sample file location))))
(define position 0)
(<rt-play> 0 100
	   (lambda ()
	     (out (* 0.8 (ringbuffer-location rb position))) ;; If data is not available, a value from the buffer is returned instead. Might produce less clicks than zero.
	     (set! position (1+ position))))


;;To delay playing until data is available:
(<rt-play> 0 100
	   (lambda ()
	     (if (ringbuffer-location? rb position)         ;; ringbuffer-location? whether data at the position is available. If \#f, a request is sent.
		 (begin
		   (out (* 0.8 (ringbuffer-location rb position)))
		   (set! position (1+ position))))))
!#






;; rt -> snd

(define-rt-vct-struct rt-rb
  :read
  :write
  :unread
  :isrunning)

(define rt-rb-header-size (vct-length (make-rt-rb)))

(define rt-running-ringbuffers '())

(define (make-ringbuffer size)
  (let ((ret (make-vct (+ rt-rb-header-size size))))
    ret))

(define* (ringbuffer-get rt-rb func #:optional (interval 2))
  (letrec ((ai (lambda ()
		 (if (= 0 (=> rt-rb :isrunning))
		     (begin
		       (set! rt-running-ringbuffers (remove (lambda (x) (eq? x rt-rb)) rt-running-ringbuffers)))
		     (begin
		       (while (> (c-integer (=> rt-rb :unread)) 0)
			      (let ((read-pos (c-integer (=> rt-rb :read))))
				(func (vct-ref rt-rb (+ rt-rb-header-size read-pos)))
				(set! (=> rt-rb :read) (if (>= read-pos (- (vct-length rt-rb) rt-rb-header-size 1))
							   0
							   (1+ read-pos))))
			      (set! (=> rt-rb :unread) (1- (=> rt-rb :unread))))
		       (in interval ai))))))
    (set! (=> rt-rb :isrunning) 1)
    (push! rt-rb rt-running-ringbuffers)
    (ai)))

(define-rt (get-ringbuffer rt-rb bufferempty)
  ;;(vct-set! rb ,rt-rb-isrunning 1)
  (let ((unread (the <int> (=> rt-rb :unread))))
    (if (> unread 0)
	(let* ((read-pos (the <int> (=> rt-rb :read)))
	       (ret (vct-ref rt-rb (+ ,rt-rb-header-size read-pos))))
	  (set! (=> rt-rb :read) (if (>= read-pos (- (vct-length rt-rb) ,rt-rb-header-size 1))
				       0
				       (1+ read-pos)))
	  (set! (=> rt-rb :unread) (1- unread))
	  ret)
	bufferempty)))

(define (ringbuffer-stop rt-rb)
  (set! (=> rt-rb :isrunning) 0))
(define-rt (ringbuffer-stop rt-rb)
  (set! (=> rt-rb :isrunning) 0))
(define (ringbuffer-stop-all)
  (for-each ringbuffer-stop rt-running-ringbuffers))

(define-rt (put-ringbuffer rt-rb val)
  ;;  (if (= 0 (vct-ref rb ,rt-rb-isrunning))
  ;;    #t
  (let ((write-pos (=> rt-rb :write))
	(unread (=> rt-rb :unread)))
    (declare (<int> write-pos unread))
    (if (>= unread (- (vct-length rt-rb) ,rt-rb-header-size))
	(begin
	  (debug "Ringbuffer full\\n")
	  #f)
	(begin
	  (vct-set! rt-rb (+ write-pos ,rt-rb-header-size) val)
	  (set! (=> rt-rb :write) (if (>= write-pos (- (vct-length rt-rb) ,rt-rb-header-size 1))
				      0
				      (1+ write-pos)))
	  (set! (=> rt-rb :unread) (1+ unread))
	  #t))))
  
  
(define-rt (clear-ringbuffer rt-rb)
  (set! (=> rt-rb :unread) 0)
  (set! (=> rt-rb :read) 0)
  (set! (=> rt-rb :write) 0))


#!
(define rb (make-ringbuffer 2000))
(<rt-play> 0 1
	   (lambda ()
	     (put-ringbuffer rb (in 0))))
(ringbuffer-get rb
		(lambda (val)
		  (c-display "got:" val)))
	      
(ringbuffer-stop rb)
(rte-silence!)

(length rt-running-ringbuffers)
(set! rt-running-ringbuffers '())
(ringbuffer-stop-all)

!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; BUS. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!
(define-rt-ec <struct-mus_rt_readin-*> rt_scm_to_rt_bus
	     (lambda (,rt-globalvardecl (<SCM> name))
	       ,(if (rt-is-safety?)
		    `(if (not (SCM_SMOB_PREDICATE rt_bus_tag name))
			 (begin
			   (rt_error rt_globals (string "Variable is not an rt-bus object"))
			   (return NULL))
			 (return (cast <void-*> (SCM_SMOB_DATA name))))
		    `(return (cast <void-*> (SCM_SMOB_DATA name))))))
!#


(<rt-type> '<bus>
	   rt-bus-p
	   #f ;;'rt_scm_to_rt_bus (The engine needs to know which buses are used by the instances. It can't know that if its hidden in an <SCM>.
	   #:transformfunc (lambda (scmbus)
			     (SCM_SMOB_DATA scmbus))
	   #:c-type '<struct-rt_bus-*>)


(define-c-macro (bus-length bus)
  (<-> (eval-c-parse bus) "->num_channels"))
(<rt-func> 'bus-length '<int> '(<bus>) #:is-immediate #t)
       


(define (rt-clean-write-bus val)
  `(begin
     (if (< data->last_written_to block_time)
	 (set! data->val ,val)
	 (+= data->val ,val))
     (set! data->last_written_to block_time)))

(define-rt-ec <void> rt_write_bus (lambda (,rt-globalvardecl (<bus> bus) (<int> ch) (<float> val))
				   ,(if (rt-is-safety?)
					'(if (< ch 0)
					     (rt_error rt_globals (string "Channel number for write-bus less than zero")))
					"/* */")
				   
				   (if (>= ch bus->num_channels)
				       return)
				   (let* ((block_time <int> rt_globals->block_time)
					  (framenum <int> (- rt_globals->time block_time))
					  (data <struct-rt_bus_data-*> "&bus->data[(bus->num_channels*framenum)+ch]"))
				     ,(rt-clean-write-bus 'val)
				     )))
#!
(-> (hashq-ref rt-types '<bus>) c-type)
!#

(define-rt-ec <void> rt_write_bus_vct (lambda (,rt-globalvardecl (<bus> bus) (<vct-*> vct))
				       (<float-*> vctdata vct->data)
				       (<int> num_channels (EC_MIN vct->length bus->num_channels))
				       (<int> block_time rt_globals->block_time)
				       (<int> framenum (- rt_globals->time block_time))
				       (<int> base (* bus->num_channels framenum))
				       (for-each 0 num_channels
						 (lambda (ch)
						   (let* ((data <struct-rt_bus_data-*> "&bus->data[base++]"))
						     ,(rt-clean-write-bus 'vctdata[ch]))))))


(define-rt-ec <float> rt_read_bus (lambda (,rt-globalvardecl (<bus> bus) (<int> ch))
				   ,(if (rt-is-safety?)
					'(if (< ch 0)
					     (rt_error rt_globals (string "Channel number for read-bus less than zero")))
					"/* */")
				   (if (>= ch bus->num_channels)
				       (return 0))

				   (let* ((time <int> rt_globals->prev_block_time)
					  (framenum <int> (- rt_globals->time rt_globals->block_time))
					  (data <struct-rt_bus_data-*> "&bus->data[(bus->num_channels*framenum)+ch]"))
				     (return (?kolon (< data->last_written_to time)
						     0
						     data->val)))))

(define-rt-ec <vct-*> rt_read_bus_vct (lambda (,rt-globalvardecl (<bus> bus))
					  (let* ((vct <vct-*> (rt_alloc_vct rt_globals bus->num_channels))
						 (vctdata <float-*> vct->data)
						 (time <int> rt_globals->prev_block_time)
						 (framenum <int> (- rt_globals->time rt_globals->block_time))
						 (base <int> (* bus->num_channels framenum)))
					    (for-each 0 bus->num_channels
						      (lambda (ch)
							(let* ((data <struct-rt_bus_data-*> "&bus->data[base++]"))
							  (set! vctdata[ch]
								(?kolon (< data->last_written_to time)
									0
									data->val)))))
					    (return vct))))





;;(write-bus bus 0.2)               -> (rt_write_bus bus 0 0.2)
;;(write-bus bus 0 0.2)               -> (rt_write_bus bus 0 0.2)
;;(write-bus bus 1 0.2)               -> (rt_write_bus bus 1 0.2)
;;(write-bus bus vct)               -> (rt_write_bus_vct bus vct)
;;(write-bus bus (vct 0.2 0.5 0.6)) -> (let ((,das-vct (vct 0.2 0.5 0.6)))
;;				         (rt_write_bus_vct bus ,das-vct))
(define-rt-macro (write-bus bus . rest)
  (cond ((and (= 1 (length rest))
	      (number? (car rest)))
	 `(rt_write_bus ,bus 0 ,(car rest)))
	((> (length rest) 2)
	 (c-display "Error. write-bus. Too many arguments.")
	 #f)
	((= (length rest) 2)
	 `(rt_write_bus ,bus ,@rest))
	(else
	 `(if (is-type? <vct-*> ,(car rest))
	      (rt_write_bus_vct ,bus (rt-cast-vct ,(car rest)))
	      (rt_write_bus ,bus 0 (rt-cast-float ,(car rest)))))))




;;(read-bus bus 0)   -> (rt_read_bus bus 0)      ;; returns float
;;(read-bus bus 1)   -> (rt_read_bus bus 1)      ;; returns float
;;(read-bus bus)     -> (rt_read_bus_vct bus)    ;; returns vct
;;(read-bus bus 0 1) -> (vct (rt_read_bus bus 0) ;; returns vct
;;			     (rt_read_bus bus 1))
;;
(define-rt-macro (read-bus bus . rest)
  (cond ((null? rest)
	 `(rt_read_bus_vct ,bus))
	((= 1 (length rest))
	 `(rt_read_bus ,bus ,(car rest)))
	((not (rt-immediate? bus))
	 (let ((das-bus (rt-gensym)))
	   `(let ((,das-bus ,bus))
	      (read-bus ,das-bus ,@rest))))
	 (else
	  `(vct ,@(map (lambda (ch)
			 `(rt_read_bus ,bus ,ch))
		       rest)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; VAR. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (make-var #:optional (val 0))
  (make-vct 1 val))
(define (read-var var)
  (vct-ref var 0))
(define (write-var var val)
  (vct-set! var 0 val))
(define-rt-macro (read-var var)
  `(rt-vct-ref/vct-ref ,var 0))
(define-rt-macro (write-var var val)
  `(rt-vct-set!/vct-set! ,var 0 ,val))


(define (make-glide-var default-val maximum-change)
  (vct default-val default-val maximum-change maximum-change))

(define (write-glide-var var val)
  (vct-set! var 2
	    (if (> val (vct-ref var 0))
		(vct-ref var 3)
		(- (vct-ref var 3))))
  (vct-set! var 1 val))

(define (read-glide-var var)
  (let ((a (vct-ref var 0))
	(b (vct-ref var 1)))
    (if (= a b)
	a
	(let ((ret (if (> (abs (- a b))
			  (vct-ref var 3))
		       (+ a (vct-ref var 2))
		       b)))
	  (vct-set! var 0 ret)
	  ret))))

(define-rt (write-glide-var var val)
  ,(cons 'begin (cddr (procedure-source write-glide-var))))

(define-rt (read-glide-var var)
  ,(cons 'begin (cddr (procedure-source read-glide-var))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; VCT. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scm_to_vct
(define-rt-ec <vct-*> rt_scm_to_vct (lambda (,rt-globalvardecl (<SCM> name))
				     ,(if (rt-is-safety?)
					  `(if (mus_vct_p name)
					       (return (XEN_TO_VCT name))
					       (begin
						 (rt_error rt_globals (string "Variable is not a VCT."))
						 (return NULL)))
					  `(return (XEN_TO_VCT name)))))

#!
(define-rt-ec <vct-*> rt_alloc_vct (lambda (,rt-globalvardecl (<int> length))
                                       (let* ((ret <vct-*> (rt_alloc rt_globals (sizeof <vct>)))
                                              (floats <float-*> (rt_alloc_atomic rt_globals (* (sizeof <float>) length))))
                                         (set! ret->length length)
                                         (set! ret->data floats)
                                         (return ret))))
!#

(define-rt-ec <vct-*> rt_alloc_vct (lambda (,rt-globalvardecl (<int> length))
                                       (let* ((ret <vct-*> (rt_alloc_atomic rt_globals
                                                                            (+ (sizeof <vct>)
                                                                               (* (sizeof <float>) length))))
                                              (floats <float-*> (cast <float-*> (+ ret 1))))
                                         (set! ret->length length)
                                         (set! ret->data floats)
                                         (return ret))))


(define-rt-ec <vct-*> rt_vct_scale (lambda ((<vct-*> vct) (<float> scl))
				       (<int> lokke)
				       "for(lokke=0;lokke<vct->length;lokke++){"
				       (*= vct->data[lokke] scl)
				       "}"
				       (return vct)))


(define-rt-ec <vct-*> rt_vct_offset (lambda ((<vct-*> vct) (<float> scl))
					(<int> lokke)
					"for(lokke=0;lokke<vct->length;lokke++){"
					(+= vct->data[lokke] scl)
					"}"
					(return vct)))


(define-rt-ec <vct-*> rt_vct_fill (lambda ((<vct-*> vct) (<float> scl))
				      (<int> lokke)
				      "for(lokke=0;lokke<vct->length;lokke++){"
				      (set! vct->data[lokke] scl)
				      "}"
				      (return vct)))



(define-rt-macro (vct-length vct)
  `(rt-vct-length/vct-length ,vct))

(define-rt-macro (rt-vct-legal-pos das-vct pos2 funcname body)
  (if (rt-is-safety?)
      `(begin (if (< ,pos2 0)
		  (rt-error ,(format #f "Illegal second argument for ~A pos: pos<0. (Body: ~A)" funcname body)))
	      (if (>= ,pos2 (vct-length ,das-vct))
		  (rt-error ,(format #f "Illegal second argument for ~A vct-legal-pos: pos>=length. (Body: ~A)" funcname body)))
	      ,body)
      body))


(define-rt-macro (vct-set! das-vct expand/pos val)
  (define das-das-vct (rt-gensym))
  (if (and (rt-immediate? pos)
	   (rt-immediate? das-vct))
      `(rt-vct-legal-pos ,das-vct ,pos "vct-set!"
			 (rt-vct-set!/vct-set! ,das-vct (rt-castint/castint ,pos) ,val))
      (let ((x (rt-gensym)))
	`(let ((,das-das-vct ,das-vct)
	       (,x ,pos))
	   (rt-vct-legal-pos ,das-das-vct ,x "vct-set!"
			     (rt-vct-set!/vct-set! ,das-das-vct (rt-castint/castint ,x) ,val))))))

(define-rt-macro (vct-ref das-vct pos)
  (define das-das-vct (rt-gensym))
  (define x (rt-gensym))
  (if (and (rt-immediate? pos)
	   (rt-immediate? das-vct))
      `(rt-vct-legal-pos ,das-vct ,pos "vct-ref"
			 (rt-vct-ref/vct-ref ,das-vct (rt-castint/castint ,pos)))
      `(let ((,das-das-vct ,das-vct)
	     (,x ,pos))
	 (rt-vct-legal-pos ,das-das-vct ,x "vct-ref"
			   (rt-vct-ref/vct-ref ,das-das-vct (rt-castint/castint ,x))))))

#!
(rt-macroexpand '(vct-ref positions1 i))
!#

(define-rt-macro (vct . rest)
  (let ((ret (rt-gensym))
	(n -1))
    `(let ((,ret (rt_alloc_vct ,(length rest))))
       ,@(map (lambda (something)
		(set! n (1+ n))
		`(rt-vct-set!/vct-set! ,ret ,n ,something))
	      rest)
       ,ret)))

(define-rt-macro (make-vct len . initial-element)
  (if (null? initial-element)
      `(rt_vct_fill (rt_alloc_vct ,len) 0.0)
      `(rt_vct_fill (rt_alloc_vct ,len) ,(car initial-element))))
      


(define-rt-macro (vct-scale! vct scl)
  (if (and (list? vct)
	   (rt-immediate? scl)
	   (eq? 'vct (car vct)))
      `(vct ,@(map (lambda (val)
		     `(* ,scl ,val))
		   (cdr vct)))
      `(rt_vct_scale ,vct ,scl)))
(define-rt-macro (vct-offset! vct scl)
  (if (and (list? vct)
	   (rt-immediate? scl)
	   (eq? 'vct (car vct)))
      `(vct ,@(map (lambda (val)
		    `(+ ,scl ,val))
		  (cdr vct)))
      `(rt_vct_offset ,vct ,scl)))
(define-rt-macro (vct-fill! vct scl)
  (if (and (list? vct)
	   (rt-immediate? scl)
	   (eq? 'vct (car vct)))
      `(vct ,@(map (lambda (val)
		     ,scl)
		   (cdr vct)))
      `(rt_vct_fill ,vct ,scl)))

(define-rt-macro (vct-map! vct thunk)
  (let ((das-vct (rt-gensym))
	(i (rt-gensym)))
    `(let ((,das-vct (the <vct-*> ,vct)))
       (rt-range2 ,i 0 (vct-length ,das-vct) 1
		  (rt-vct-set!/vct-set! ,das-vct ,i (begin
						      ,@(cddr thunk)))))))
		  
(define-c-macro (rt-castint/castint val)
  `(cast <int> ,val))
(<rt-func> 'rt-castint/castint '<int> '(<int>))

(define-c-macro (rt-vct-data/vct-data vct)
  (<-> (eval-c-parse vct) "->data"))
(define-c-macro (rt-vct-ref/vct-ref vct pos)
  (<-> (eval-c-parse vct) "->data[" (eval-c-parse pos) "]"))
(<rt-func> 'rt-vct-ref/vct-ref '<float> '(<vct-*> <int>))

(define-c-macro (rt-vct-set!/vct-set! vct pos val)
  (<-> (eval-c-parse vct) "->data[" (eval-c-parse pos) "]=" (eval-c-parse val)))
(<rt-func> 'rt-vct-set!/vct-set! '<void> '(<vct-*> <int> <float>))

(define-c-macro (rt-vct-length/vct-length vct)
  (<-> (eval-c-parse vct) "->length"))
(<rt-func> 'rt-vct-length/vct-length '<int> '(<vct-*>))


#!
(define v (vct 2 3 4))
(begin v)
(define a (rt-2 '(lambda ()
		   (vct-scale! v 2)
		   5)))
(rt-funcall a)
(define a (rt-2 '(lambda ()
		   (vct-set! v 0 5))))
		   
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Realtime pool handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *max-num-pools* 100)
(define *rt-pools* '())

(define-ec-struct <rt_pool>
  <struct-rt_pool*> next
  <void*> reset_func
  <void*> element)


(eval-c ""
	"#include <pthread.h>"
	(shared-struct <rt_pool>)

	(<struct-rt_pool**> pools NULL)
	(<struct-rt_pool*> empty_pools NULL)
	;;(<nonstatic-void*> rt_get_pool (lambda-decl ((<int> poolnum))))
	(run-now
	 (set! pools (calloc (sizeof <struct-rt_pool*>) ,*max-num-pools*)))

	;; push and get must/should be made threadsafe.
	(<pthread_mutex_t> mutex PTHREAD_MUTEX_INITIALIZER)

	(<nonstatic-void> rt_push_to_pool (lambda ((<void*> element)
						   (<void*> reset_func)
						   (<int> poolnum))
					    ;;(pthread_mutex_lock &mutex)
					    (<struct-rt_pool*> pool empty_pools)
					    (set! empty_pools pool->next)
					    (set! pool->element element)
					    (set! pool->reset_func reset_func)
					    (set! pool->next pools[poolnum])
					    (set! pools[poolnum] pool)
					    ;;(pthread_mutex_unlock &mutex)
					    ))

	(<nonstatic-void*> rt_get_pool_element (lambda ((<int> poolnum))
						 ;;(pthread_mutex_lock &mutex)
						 (<struct-rt_pool*> pool pools[poolnum])
						 (if (== NULL pool)
						     (begin
						       ;;(pthread_mutex_unlock &mutex)
						       (return NULL)))
						 (set! pools[poolnum] pool->next)
						 (set! pool->next empty_pools)
						 (set! empty_pools pool)
						 (let* ((reset_func (<void> (<void*>)) pool->reset_func)
							(ret <void*> pool->element))
						   (if reset_func
						       (reset_func ret))
						   ;;(pthread_mutex_unlock &mutex)
						   (return ret))))
	(public
	 ;; Note, not safe to call when any rt instances are running.
	 (<void> rt-init-pool (lambda ((<SCM> elements) ;; a vector of pointers
				       (<int> poolnum)
				       ((<void> (<void*>)) reset_func))
				(<int> num_elements (SCM_VECTOR_LENGTH elements))
				(for-each 0 num_elements
					  (lambda (i)
					    (<struct-rt_pool*> pool (malloc (sizeof <struct-rt_pool>)))
					    (set! pool->next empty_pools)
					    (set! empty_pools pool)))
				;;(fprintf stderr (string "numel num_el: %d poolnum: %d %p\\n") num_elements poolnum empty_pools)
				(for-each 0 num_elements
					  (lambda (i)
					    (<void*> element (GET_POINTER (SCM_VECTOR_REF elements i)))
					    (rt_push_to_pool element reset_func poolnum))))))
	)


(define-rt-vector-struct rt-pool
  :name
  :num
  :reset_func)

(define next-pool-num 0)

(define* (get-rt-pool name :optional (throw-error #t))
  (call/cc (lambda (return)
	     (for-each (lambda (rt-pool)
			 (if (eq? name (=> rt-pool :name))
			     (return rt-pool)))
		       *rt-pools*)
	     (if throw-error
		 (throw (symbol-append 'unknown-pool- name))
		 #f))))
(define (get-rt-pool-num name)
  (let ((rt-pool (get-rt-pool name #f)))
    (and rt-pool
	 (=> rt-pool :num))))

(define* (new-rt-pool name :key reset_func)
  (define pool-num (or (get-rt-pool-num name) ;; Will only happen during development. (memory leak)
		       next-pool-num))
  (inc! next-pool-num 1)
  (push! (make-rt-pool :name name
		       :num pool-num
		       :reset_func reset_func)
	 *rt-pools*))

(define* (init-rt-pool name create_func :key (num-elements 100))
  (define pool-num (get-rt-pool-num name))
  (let ((rt-pool (get-rt-pool name)))
    (define reset_func (=> rt-pool :reset_func))
    (rt-init-pool (list->vector (map (lambda (n) (create_func)) (iota num-elements))) pool-num reset_func)))





#!

(add-rt-pool 'testpool (lambda ()
			 (XEN_TO_MUS_ANY (make-oscil))))

(eval-c ""
	(get-proto rt_get_pool_element)	
	(public
	 (<void> test2 (lambda ((<int> poolnum))
			 (printf (string "aisann: %p\\n") (rt_get_pool_element poolnum))))
	 (<void*> testing (lambda ()
			    (return (rt_get_pool_element 0))))))
	 

(<rt> (lambda ()
	(define coroutine (rt_get_pool 1))
	(spawn coroutine
	       (lambda ()
		 ...))
	...))
->
(begin
  (add-pool 'coroutine *rt-coroutine-pool-size*)
  (<rt> (lambda ()
	  (define coroutine (rt_make_coroutine))
	  ...)))
   

(eval-c (<-> "-I" snd-header-files-path " -ffast-math ") ;; "-ffast-math") ;; " -Werror "
	"#include <clm.h>"
	(public
	 (<int> get-mus-object-size (lambda ()
				      (return (sizeof mus_any_class))))))
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coroutine handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-rt-ec <void> rt_return_void
		(lambda ()
		  (rt-dummy/dummy)
		  ))


(c-load-from-path rt-coroutines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extern variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rt-extern-variables '())
(define (add-extern-rt-variable name var)
  (push-back! (list name var) rt-extern-variables))
(define (get-extern-rt-variable name)
  (cadr (assq name rt-extern-variables)))
(define (rt-extern-defined? name)
  (assq name rt-extern-variables))

#!
(add-extern-rt-variable 'a (make-oscil))
(rt-extern-reset)
!#
(define-rt/stalin-macro (extern . rest)
  (fix-defines
   (cond ((null? (cdr rest))
	  (define varname (rt-gensym))
	  (add-extern-rt-variable varname (car rest))
	  varname)
	 ((null? (cddr rest))
	  (add-extern-rt-variable (car rest) (cadr rest))
	  (car rest))
	 (else
	  (add-extern-rt-variable (car rest) (cadr rest))
	  `(extern ,@(cddr rest))))))
(define-rt-macro (external . rest)
  `(extern ,@rest))

#!
(rte-silence!)
(rt-gensym)
(rt-gensym-reset)
(rt-macroexpand '(extern ai (make-oscil) b 2 c 3))
(begin rt-extern-rt-variables)
(rt-extern-reset)
(<rt-out> :len 2 (oscil))
(<rt-out> :len 2 (oscil))
!#
(define (rt-extern-reset)
  (set! rt-extern-variables '()))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (default-rt-dialog get-rt :key (show #t) (close-button #t) (stop-button #t) (start-button #t))
  (define rt-dialog #f)
  (let ((dialog (cond ((and get-rt
			    (defined? 'rt-dialog-unique-name *rt-local-code-environment*))
		       (let ((dialog (local-eval 'rt-dialog-unique-name *rt-local-code-environment*)))
			 (set-car! dialog (cons get-rt (car dialog)))
			 (cadr dialog)))
		      ((and get-rt
			    (rt-extern-defined? 'rt-standard-dialog))
		       (get-extern-rt-variable 'rt-standard-dialog))
		      (else		    
		       (letrec* ((for-each-instrument (lambda (func)
							(for-each (lambda (instrument)
								    (func (force instrument)))
								  (if get-rt
								      (list get-rt)
								      (delete-duplicates (car rt-dialog))))))
				 (dialog (<dialog> "dialog" (lambda ()
							      (for-each-instrument (lambda (instrument)
										     (-> instrument stop)))
							      (-> dialog hide)))))
			 (for-each (lambda (args)
				     (if (car args)
					 (<button> dialog (cadr args) (caddr args))))
				   `((,close-button "Close" ,(lambda ()
							       (for-each-instrument (lambda (instrument)
										      (-> instrument stop)))
							       (-> dialog hide)))
				     (,stop-button "Stop" ,(lambda ()
							     (for-each-instrument (lambda (instrument)
										    (-> instrument stop)))))
				     (,start-button "Start" ,(lambda ()
							       (for-each-instrument (lambda (instrument)
										      (-> instrument play)))))))
			 (add-extern-rt-variable 'rt-standard-dialog dialog)
			 (set! rt-dialog (list '() dialog (make-hash-table 19)))
			 dialog)))))
    (if show
	(-> dialog show))
    (if get-rt
	dialog
	rt-dialog)))

(define-macro define-multi-instrument-rt-dialog
  (lambda x
    `(define rt-dialog-unique-name (default-rt-dialog #f ,@x))))

(define-macro (get-multi-instrument-rt-dialog)
  '(cadr rt-dialog-unique-name))


#!
(let ()
  (define-multi-instrument-rt-dialog)
  (<rt-out> (* (<slider> "oscil" 0 0.5 1)
	       (<slider> "vol2" 0 1 2)
	       (oscil)))
  (<rt-out> (* (<slider> "square" 0 0.5 2)
	       (<slider> "vol2" 0 1 2)
	       (square-wave))))

(macroexpand '(define-multi-instrument-rt-dialog :show #t))

(let ()
  (define ai 0.2)
  (<rt-out> (* (extern ai) (oscil))))

!#

(define-rt-macro (<slider> name min curr max (:glide #t) (:log #f) (:scale #f))
  (fix-defines 
   (define v (gensym))  
   (define v2 (rt-gensym))  
   (define min2 (gensym))
   (define max2 (gensym))
   (define glide2 (gensym))
   
   (define dialog (gensym))
   (define slider (gensym))
   (define ret (gensym))
   (define topmost (gensym))
   
   (define extern-name (gensym))

   (if (eq? glide #t)
       (set! glide -1))
   
   (if glide
       `(read-glide-var
	 (extern    (let ((,extern-name (string->symbol (<-> "rt-internal-<slider>-" ,name)))
			  (,dialog (and (defined? 'rt-dialog-unique-name *rt-local-code-environment*)
					(local-eval 'rt-dialog-unique-name *rt-local-code-environment*))))
		      (define ,topmost #f)
		      ;;(c-display "extern-name" ,extern-name *rt-local-code-environment*)
		      (let* ((,min2 ,min)
			     (,max2 ,max)
			     (,glide2 ,glide)
			     (,v (make-glide-var ,curr (if (= -1 ,glide2)
							   (/ (- ,max2 ,min2) (/ (mus-srate) 3))
							   ,glide2)))
			     (,slider (if (and ,dialog
					       (hashq-ref (nth 2 ,dialog) ,extern-name))
					  (hashq-ref (nth 2 ,dialog) ,extern-name)
					  (let* ((,slider (<slider> (default-rt-dialog (%delay rt-current-rt))
								    ,name ,min2 (read-glide-var ,v)
								    ,max2
								    (lambda (val)
								      (if ,topmost
									  (for-each (lambda (v)
										      (write-glide-var v val))
										    (car ,topmost))
									  (write-glide-var ,v val)))
								    (or ,scale
									(max 100 (/ 1000 (- ,max2 ,min2))))
								    #f
								    ,log)))
					    (define ,ret (list '() ,slider))
					    (when ,dialog
					      (hashq-set! (nth 2 ,dialog) ,extern-name ,ret)
					      (set! ,topmost ,ret))
					    ,ret))))
			(set-car! ,slider (cons ,v (car ,slider)))
			,v))))
       `(extern ,v2 (let ((,extern-name (string->symbol (<-> "rt-internal-<slider>-" ,name)))
			  (dialog (and (defined? 'rt-dialog-unique-name *rt-local-code-environment*)
				       (local-eval 'rt-dialog-unique-name *rt-local-code-environment*))))
		      (if (and dialog
			       (hashq-ref (nth 2 dialog) ,extern-name))
			  (hashq-ref (nth 2 dialog) ,extern-name)
			  (let* ((,min2 ,min)
				 (,max2 ,max)
				 (,v ,curr)
				 (slider (<slider> (default-rt-dialog (%delay rt-current-rt))
						   ,name ,min2 ,v
						   ,max2
						   (lambda (val)
						     (set! (-> rt-current-rt ,v2) val))
						   (or ,scale
						       (max 100 (/ 1000 (- ,max2 ,min2))))
						   #f
						   ,log)))
			    (if dialog
				(hashq-set! (nth 2 dialog) ,extern-name ,v))
			    ,v)))))))

#!

(rt-macroexpand '(<slider> "545test" 0 0.5 1 :glide #f))
(<rt-out> (* (<slider> "vol" 0 0.2 1 :glide #t) 
	     (<slider> "vol2" 0 0.2 1 :glide #t) 
	     (oscil :extern (hz->radians 40))))

!#

(define-rt-macro (<checkbutton> name on-or-off (:glide #t) (:togglebutton #f))
  (define v (gensym))
  (define v2 (rt-gensym))
  (define val (gensym))
  (define glide2 (gensym))

  (define extern-name (gensym))

  (c-display "checkbutton")
  
  (if (eq? glide #t)
      (set! glide -1))

  `(begin
     (extern ,v2 (let ((,extern-name (string->symbol (<-> "rt-internal-" (if ,togglebutton "<togglebutton>-" "<checkbutton>-") ,name)))
		       (dialog (and (defined? 'rt-dialog-unique-name *rt-local-code-environment*)
				    (local-eval 'rt-dialog-unique-name *rt-local-code-environment*))))
		   (if (and dialog
			    (hashq-ref (nth 2 dialog) ,extern-name))
			 (hashq-ref (nth 2 dialog) ,extern-name)
			 (let* ((,val (cond ((eq? #t ,on-or-off) 1)
					    ((eq? #f ,on-or-off) 0)
					    ((number? ,on-or-off)
					     (if (= 0 ,on-or-off)
						 0
						 1))
					    (else 1)))
				(,v ,(if glide
					 `(make-glide-var ,val (let ((,glide2 ,glide))
								 (if (= ,glide2 -1)
								     (/ 50 (mus-srate))
								     ,glide2)))
					 val)))
			   ((if ,togglebutton
				<togglebutton>
				<checkbutton>)
			    (default-rt-dialog (%delay rt-current-rt)) 
			    ,name (lambda (val)
				    ,(if glide
					 `(write-glide-var ,v (if val 1 0))
					 `(set! (-> rt-current-rt ,v2) (if val 1 0))))
			    (if (= 0 ,val)
				#f
				#t))
			   (if dialog
			       (hashq-set! (nth 2 dialog) ,extern-name ,v))
			   ,v))))
     ,(if glide
	  `(read-glide-var ,v2)
	  `(begin 
	     (declare (<float> ,v2)) ;; This is horrible. The type inference routine is really
	                             ;; bad when its not even able to figure out a float, the
	                             ;; default type.
	     ,v2))))

(define-rt-macro (<togglebutton> name on-or-off (:glide #t))
  `(<checkbutton> ,name ,on-or-off :glide ,glide :togglebutton #t))



#!
(rt-macroexpand '(<checkbutton> "on/off" #f :glide #t))
(rt-macroexpand '(<togglebutton> "on/off" #f :glide #f))
(<rt-out> (* (<slider> "vol" 0 0.2 1) 
	     (<slider> "vol2" 0 0.2 1 :glide #f)
	     (<checkbutton> "on/off" #t :glide #t)
	     (<togglebutton> "on/off" #t :glide #f)
	     (oscil :frequency 0
		    (hz->radians (<slider> "freq" 50 200 1000 :log #t)))))
(oscil :frequency 0
       (hz->radians (<slider> "freq"  50 200 1000 :log #t)))

(rt-macroexpand '(oscil :frequency 0 2 3))

(<rt-out> (let ((outval (* (<checkbutton> "on/off" #t)
			   (oscil (extern (make-oscil 0))
				  (hz->radians (<slider> "freq" 50 200 8000 :log #t))))))
	    (vct (* (<slider> "left" 0 0.2 1) outval)
		 (* (<slider> "right" 0 0.2 1) outval))))
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; <realtime>/rt-compile/rt/rt-run/rt-funcall/etc.;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; There are certain limitations in the optional/keyword handling in guile.

(def-class (<realtime> func
		       arg
		       toprotect
		       #:key (engine *rt-engine*))

  ;;(define procfunc (<RT_Procfunc> #:func func #:arg arg))
  (define procfunc (rt_make_procfunc func arg toprotect))
  (define procfunc-data (rt_get_procfunc_data procfunc))

  (def-method (stop-abs #:optional (end (-> engine get-time)))
    (-> engine add-event
	(-> engine get-frame-time end)
	(rt_remove_procfunc)
	procfunc-data))
  
  (define (play-abs-do start duration position)
    (cond ((and duration (<= duration 0))
	   (c-display "Error. <realtime> -> play, duration<=0 (play-now " start duration ")"))
	  ((and (not (eq? position 'first)) (not (eq? position 'last)))
	   (c-display "Error. <realtime> -> play, position must be first or last, not" position))
	  (else
	   ;;(c-display "play, now/start/end" (-> engine get-time) start end)
	   (rt_protect_var procfunc) ;; Unprotection happens in the rt_non_check_non_rt function.
	   (-> engine add-event
	       (-> engine get-frame-time start)
	       (if (eq? position 'first)
		   (rt_insert_procfunc)
		   (rt_append_procfunc))
	       procfunc-data)
	   (if duration
	       (this->stop-abs (+ start duration))))))

  (def-method (play-abs . rest) ;;#:optional (start (-> engine get-time)) duration key-ignore (position 'first))
    (let* ((args (take-while (lambda (x) (not (keyword? x)))
			     rest))
	   (keyargs (find-tail keyword? rest))
	   (arglength (length args)))
      (play-abs-do (if (>= arglength 1)
		       (car args)
		       (-> engine get-time))
		   (if (>= arglength 2)
		       (cadr args)
		       #f)
		   (if (not keyargs)
		       'first
		       (cadr keyargs)))))

  (def-method (stop #:optional (end 0))
    (this->stop-abs (+ (-> engine get-time) end)))
		 
  (define (play-do start duration keyargs)
    (if (and duration (<= duration 0))
	(c-display "Error. <realtime> -> play, duration<=0 (play-now " start duration ")")
	(let ((start-time (-> engine get-time)))
	  (apply this->play-abs (append (list (+ start-time start)
					      (if duration
						  duration
						  #f))
					 keyargs)))))
  (def-method (play . rest)
    (let* ((args (take-while (lambda (x) (not (keyword? x)))
			    rest))
	   (keyargs (let ((ret (find-tail keyword? rest))) (if (not ret) '() ret)))
	   (arglength (length args)))
      (play-do (if (>= arglength 1)
		   (car args)
		   0)
	       (if (>= arglength 2)
		   (cadr args)
		   #f)
	       keyargs))))




















(define (rt-4 term)

  (call-with-current-continuation
   (lambda (return)

     (let ((uniqify-res #f)
	   (renamed-vars #f)
	   (external-vars #f)
	   (insert-types-res #f)
	   (orgargs #f)
	   (returntype #f)
	   (extnumbers #f)
	   (extpointers #f)
	   (extnumbers-writing #f))

       (rt-print "*RT: Expanding macros")
       (set! term (rt-macroexpand term))

       (if (not term)
	   (return #f))

       ;; Inserted used functions
       (let ((function-names '())
	     (functions '()))
	 (define (add-func funcname)
	   (if (not (member funcname function-names))
	       (let ((func (hashq-ref rt-functions funcname)))
		 (if func
		     (let ((expanded-func (rt-macroexpand func)))
		       (push! funcname function-names)    ;; Add function-name to the list of included function
		       (for-each add-func (rt-find-all-funcs expanded-func))
		       (push! expanded-func functions))))))     ;; Add function-body to be included.
	 (for-each add-func (rt-find-all-funcs term))
	 (set! term `(,(car term) ,(cadr term)
		      ,@(reverse! functions)
		      ,@(cddr term))))

	 
       (rt-print "*RT: Checking syntax")
	      
       (if (not (rt-check-syntax term))
	   (return #f))

       (rt-print "*RT: Replacing defines with letrecs")
	      
       (set! term (rt-replace-define-with-letrecs term))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Fixing various")
	      
       (set! uniqify-res (rt-fix-various term))
       (if (not uniqify-res)
	   (return #f))

	      
       (set! renamed-vars (car uniqify-res))
       (set! term (cadr uniqify-res))

       (rt-print "*RT: Let*-lifter" term)
	      
       (set! term (rt-let*-lifter term))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Inserting types" term)
       
       (set! insert-types-res (rt-insert-types term renamed-vars))
       (if (not insert-types-res)
	   (return #f))

	      
       (set! orgargs (map (lambda (org new)
			    (rt-print2 "org/new" org new)
			    (list org (-> (hashq-ref rt-types (cadr new)) c-type)))
			  (cadr term)
			  (cadr (car insert-types-res))))

       (set! term (car insert-types-res))
       (set! returntype (cadr insert-types-res))
       (set! extnumbers (caddr insert-types-res))
       (set! extpointers (cadddr insert-types-res))
       (set! extnumbers-writing (car (cdr (cdddr insert-types-res))))
       
       (rt-print2 "before" (cadr term))
       (set-car! (cdr term)
		 (append (map (lambda (var)
				(list (car var)
				      (-> (cadr var) rt-type)))
			      (append extnumbers-writing extpointers extnumbers))
			 orgargs))
       (rt-print2 "after" (cadr term))
       
       (rt-print "*RT: Removing unused stuff" term orgargs (map (lambda (a) (-> (cadr a) rt-type)) extpointers))
       
       (set! term (rt-remove-unused++ term))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Lifting lambdas.")
       
       (set! term (rt-lambda-lifter term))
       (if (not term)
	   (return #f))
       
       (rt-print "*RT: Inserting returns" term)
       (set! term (rt-insert-returns term returntype))
       (if (not term)
	   (return #f))

       (rt-print "*RT: Performing last hacks" term)
       (set! term (rt-last-hacks term))
       (if (not term)
	   (return #f))

       (rt-print "RT: Final term:" term)
       (list extnumbers
	     extpointers
	     extnumbers-writing
	     orgargs
	     returntype
	     term)))))



(define (rt-3.5 term)
  (let ((t (rt-4 term)))
    (if t
	(caddr (cdddr t)))))

		   

;; NOTE! "setternames" is an extremely confusing name. Its used for all variables which must be
;; transformed in Guile before the c function is called, and not at all for all variables which
;; may be setted. (must fix this)

(define (rt-3 term)
  (let ((rt-4-result (rt-4 term)))
    (if (not rt-4-result)
	#f
	(let* ((extnumbers (append (caddr rt-4-result) (car rt-4-result)))
	       (extpointers (cadr rt-4-result))
	       ;;(extnumbers-writing (caddr rt-4-result))
	       (extnumbers-writing '())
	       (orgargs (cadddr rt-4-result))
	       (returntype (cadr (cdddr rt-4-result)))
	       (term (caddr (cdddr rt-4-result)))
	       (mainfuncargs (cadr term))
	       
	       (funcname (rt-gensym2))
	       (das-funcname (rt-gensym2))
	       (rt-innerfuncname (rt-gensym2))
	       (rt-faustsetvarfuncname (rt-gensym2))
	       (rt-funcname (rt-gensym2))

	       (make-globals-func (rt-gensym2))
	       (free-globals-func (rt-gensym2))

	       (funcarg (rt-gensym2))

	       (globalvars (map reverse (remove (lambda (vardecl)
						  (= 3 (length vardecl)))
						(cadr (caddr term)))))
	       
	       (publicargs (append (map (lambda (extvar)
					  (rt-print2 "extvar1" extvar)
					  `(<SCM> ,(symbol-append '_rt_scm_ (car extvar))))
					extnumbers-writing)
				   (map (lambda (extvar)
					  (rt-print2 "extvar2" extvar)
					  (list (-> (cadr extvar) c-type) (car extvar)))
					(append extpointers extnumbers))
				   (map (lambda (a)
					  (list (cadr a) (car a)))
					orgargs)))

	       ;; The names of the first getternames must be the same as the first setternames
	       (getternames (map (lambda (extvar)
				   (list (rt-gensym2) extvar))
				 (append extnumbers-writing extnumbers))) ;;extpointers
	       (setternames (map (lambda (extvar)
				   (list (rt-gensym2) extvar))
				 (remove (lambda (vardecl)
					   (eq? '<struct-rt_bus-*> (-> (cadr vardecl) c-type)))
					 (append extnumbers-writing extnumbers extpointers))))
	       
	       ;; Not a good name for this variable
	       (busnames (map (lambda (extvar)
				(list (-> (cadr extvar) c-type)
				      (car extvar)
				      (symbol-append (car extvar) '_mirror)
				      (cadddr extvar)
				      (rt-gensym2)))
			      (remove (lambda (vardecl)
					(not (eq? '<struct-rt_bus-*> (-> (cadr vardecl) c-type))))
				      extpointers)))

	       (inbusdef (member 'in-bus busnames (lambda (a b)
						    (eq? a (nth 3 b)))))
	       (outbusdef (member 'out-bus busnames (lambda (a b)
						      (eq? a (nth 3 b)))))
					    

	       (i 0)
	       
	       (types (map (lambda (var)
			     (rt-print2 "var" var)
			     (list (eval-c-to-scm
				    (string-trim-right
				     (eval-c-get-propertype
				      (car var))))
				   (cadr var)))
			   publicargs)))

	  ;;(c-display "busnames" busnames)
	  ;;(rt-print "publicargs" publicargs)
	  ;;(rt-print "orgargs" orgargs)
	  ;;(rt-print "mainfuncargs" mainfuncargs)
	  ;;(rt-print "types" types)

	  ;;(c-display "extnumbers-writing" extnumbers-writing)
	  ;;(c-display "extpointers" extpointers)
	  ;;(c-display "extnumbers" extnumbers)
	  ;;(c-display "orgargs" orgargs)
	  ;;(c-display "publicargs" publicargs)
	  ;;(c-display "globalvars" globalvars)

	  ;;(rt-print2 "term" term)
	  (newline)
	  
	  (list funcname             ; 0
		rt-faustsetvarfuncname     ; 1
		rt-funcname          ; 2
		extnumbers-writing   ; 3
		extpointers          ; 4
		extnumbers           ; 5

		make-globals-func    ; 6
		
		getternames          ; 7
		setternames          ; 8
		busnames             ; 9
		
		`( ;;(define-struct <func_args>
		   ;;  <int> num_outs
		   ;;  <float-*> outs
		   ;;  <int> num_ins
		   ;;  <float-*> ins
		   ;;  <float> time
		   ;;  <float> samplerate
		   ;;  <float> res
		   ;;  <char-*> error
		   ;;  <SCM> errorvariable
		   ;;  <int> errorvarnum)

                  "#include <rollendurchmesserzeitsammler.h>"
                  "#include <ucontext.h>"

                  "#include <rt-various.h>"

		  "#include <jack/ringbuffer.h>"
		  ,(if *use-alsa-midi*
		       "#include  <alsa/asoundlib.h>"
		       "/* */")
		  
		  (shared-struct <RT_Event>)
		  (shared-struct <RT_Procfunc>)
		  (shared-struct <RT_Engine>)
		  
		  (shared-struct <mus_rt_readin>)
		  (shared-struct <mus_rt_ladspa>)
		  (shared-struct <mus_rt_faust>)
		  (shared-struct <rt_coroutine>)

		  ,bus-struct

		  "#ifdef __GNUC__"
		  "#define RT_NORETURN __attribute__ ((noreturn))"
		  "#else"
		  "#define RT_NORETURN"
		  "#endif"

		  (define-struct <RT_Globals>
		    <void*> freefunc
		    ,@rt_coroutine_elements_in_rt_globals
		    
		    ;; Global/Guile variables
		    ,@(apply append publicargs)

		    ;; Arguments for the main-function and variables made global because of nested functions.
		    ,@(apply append globalvars)
		    
		    <int> is_midi_read
		    ;;<int> num_outs
		    ;;<float-**> outs
		    ;;<int> num_ins
		    ;;<float-**> ins

		    <char-*> allocplace
		    <char-*> allocplace_end

		    ;; Adding bus-mirrors.
		    ,@(apply append (map (lambda (vardecl)
					   (list (car vardecl) (caddr vardecl)))
					 busnames))

		    <struct-RT_Engine*> engine

		    ;;<struct-rt_coroutine*> coroutines
		    <struct-mus_rt_faust*> faust

                    ;; for the GC
                    <tar_heap_t*> heap
		    )

		  
		  ,(if (rt-is-safety?)
		       "void rt_error (struct RT_Globals *rt_globals, char *msg) RT_NORETURN"
		       "/* */")

		  
		  ;; Setters and getters for the RT_Globals struct
           	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		  (public

		   ,@(map (lambda (getter)
			    (let ((funcname (car getter))
				  (varname (car (cadr getter)))
				  (type (-> (cadr (cadr getter)) c-type)))
			      `(,type ,funcname (lambda ((<struct-RT_Globals-*> rt_globals))
						  (return ,(symbol-append 'rt_globals-> varname))))))
			  getternames)
		   ,@(map (lambda (setter)
			    (let ((funcname (car setter))
				  (varname (car (cadr setter)))
				  (type (-> (cadr (cadr setter)) c-type)))
			      `(<void> ,funcname (lambda ((<struct-RT_Globals-*> rt_globals) (,type das_var))
						   (set! ,(symbol-append 'rt_globals-> varname) das_var)))))
			  setternames)

		   ;; Setter for the buses
		   ,@(map (lambda (setter)
			    (let ((type (car setter))
				  (funcname (cadr (cdddr setter)))
				  (varname (caddr setter)))
			      `(<void> ,funcname (lambda ((<struct-RT_Globals-*> rt_globals) (,type das_var))
						   (set! ,(symbol-append 'rt_globals-> varname) das_var)))))
			  busnames)
		   )
		  

		  ;;(get-proto copyheap)

		  (get-proto get_rt_readin_tag)
		  (get-proto get_rt_bus_tag)
		  (get-proto get_rt_ladspa_tag)

		  (<scm_t_bits> rt_readin_tag)
		  (<scm_t_bits> rt_bus_tag)
		  (<scm_t_bits> rt_ladspa_tag)

		  (run-now
		   (set! rt_readin_tag (get_rt_readin_tag))
		   (set! rt_bus_tag (get_rt_bus_tag))
		   (set! rt_ladspa_tag (get_rt_ladspa_tag)))
		  
                  (get-proto rt_debug)
		  (get-proto rt_readin)
		  (get-proto rt_receive_midi)		   
		  (get-proto rt_get_pool_element)

		  ;;(get-proto coroutine_entry)
		  (get-proto rt_set_stack_low_value)


                  "typedef float (*ThreadFunc)(void)"
                  "typedef float (*ReadinFunc)(struct mus_rt_readin*)"
                  "typedef void (*FaustComputeFunc)(void* self,int len,float** inputs,float** outputs)"

                  (<struct-rt_coroutine> dummy_coroutine)
                   

                  (<char*> start_dyn)
                  (<char*> end_dyn)

                  (run-now
                   (if (== 0 (tar_get_dynamic_roots_for (cast <char*> &dummy_coroutine)
                                                        &start_dyn
                                                        &end_dyn))
                       (printf (string "Error. Could not find dynamic start and end. Not good.\\n"))))
                  
                   ;; Preserve the values of local variables made global because of nested functions.
                   (<void> rt_switch_to_coroutine (lambda (,rt-globalvardecl
                                                           (<struct-rt_coroutine*> coroutine))
                                                    ,@(map (lambda (globalvar)
                                                             `(,(car globalvar) ,(cadr globalvar) ,(<_> 'rt_globals-> (cadr globalvar))))
                                                           globalvars)
                                                    (rt_set_stack_low_value rt_globals->current_coroutine)
                                                    (set! rt_globals->current_coroutine coroutine)
                                                    (set! rt_globals->time coroutine->time)
                                                    (co_call coroutine->co)
                                                    ,@(map (lambda (globalvar)
                                                             `(set! ,(<_> 'rt_globals-> (cadr globalvar)) ,(cadr globalvar)))
                                                           globalvars)))

		   ;; Finding needed define-rt-ecs to include in this file.
		   ,@(let ((function-names '())
			   (functions '()))

		       (define (add-func funcname)
			 (if (not (member funcname function-names))
			     (let ((func (rt-get-ec-function funcname)))
			       (if func
				   (begin
				     (push! funcname function-names)    ;; Add function-name to the list of included function
				     (for-each add-func (nth 1 func))                         ;; Add functions used by the function
				     (push! (nth 2 func) functions))))))     ;; Add function-body to be included.
		       
		       ;;(c-display "all-funcs:" (rt-find-all-funcs (cdr term)))
		       (for-each add-func (append '(rt_error rt_insert_coroutine_in_queue
							     rt_spawn rt_run_scheduler)
						  (rt-find-all-funcs (cdr term))))
		       (reverse! functions))

		   
		   ;; Inserting all inner functions. All global variables have been put into the rt_global struct, and is therefore not used here.
		   ,@(map (lambda (vardecl)
			    (cond ((and (= 3 (length vardecl))
					(eq? 'lambda (car (caddr vardecl)))
					(let ((das-string (symbol->string (car vardecl))))
					  (and (> (string-length das-string) 7)
					       (string= "_rt_rn_" das-string 0 7 0 7)))
					`(functions->public
					  (,(cadr vardecl) ,(car vardecl) ,(caddr vardecl)))))    ;; Public available function (ie. a spawn)
				  ((= 3 (length vardecl))
				   (list (cadr vardecl) (car vardecl) (caddr vardecl)))    ;; Function or function declaration
				  (else
				   "/* */")))                                              ;; Variable
			  ;;(list (cadr vardecl) (car vardecl))))
			  (cadr (caddr term)))
     
		   
		   ;;(,returntype ,rt-innerfuncname (lambda ,(cadr term)
		   (,returntype ,rt-innerfuncname (lambda ,(cons rt-globalvardecl '()) ;; (map (lambda (t) (list (cadr t) (car t)))
										      ;; orgargs)) ;;,mainfuncargs ;;,publicargs
						    ,@(cddr (caddr term))))

		   (<void> realtime_innercall (lambda (,rt-globalvardecl)
						(,rt-innerfuncname rt_globals)))
		   
		   ;; (Not used in realtime.)
		   (<void> ,das-funcname (lambda ,(cons rt-globalvardecl '())
					   (if (> (setjmp rt_globals->engine->error_jmp) 0)
					       return)
					   
					   ,(if (rt-is-number? returntype)
						`(set! rt_globals->engine->res (,rt-innerfuncname rt_globals ));;,@(map car orgargs)))
						`(,rt-innerfuncname rt_globals )) ;;,@(map car orgargs)))
					   
					   ))
		   

		   (<int> rt_faustprocess (lambda (,rt-globalvardecl
						   (<int> startframe)
						   (<int> endframe))

					    (<int> num_frames (- endframe startframe))
					    
					    (<struct-mus_rt_faust*> faust rt_globals->faust)
					    (<FaustComputeFunc> compute faust->compute_func)
					    		
					    ;; In
					    ,(if inbusdef
						 `(begin
						    ;; bus=rt_engine->in_bus_3_mirror
						    (<int> block_time rt_globals->engine->time)
						    (<struct-rt_bus-*> bus ,(symbol-append 'rt_globals->
											   (nth 2 (car inbusdef))))
						    (<int> num_channels (EC_MIN faust->num_inputs bus->num_channels))
						    (<int> channels_diff (- bus->num_channels num_channels))
						    (<int> base 0)
						    
						    (for-each 0 num_frames
							      (lambda (n)
								(for-each 0 num_channels
									  (lambda (ch)
									    (let* ((data <struct-rt_bus_data-*> "&bus->data[base++]"))
									      (set! faust->ins[ch][n]
										    (?kolon (< data->last_written_to block_time)
											    0
											    data->val)))))
								(+= base channels_diff))))
						 "/* No inbus */")

					    ;; Compute
					    (compute faust->dsp num_frames faust->ins faust->outs)
					    
					    ;; Out
					    ,(if outbusdef
						 `(begin
						    (<int> block_time rt_globals->engine->time)
						    ;; bus=rt_engine->out_bus_4_mirror
						    (<struct-rt_bus-*> bus ,(symbol-append 'rt_globals->
											   (nth 2 (car outbusdef))))
						    (<int> num_channels (EC_MIN faust->num_outputs bus->num_channels))
						    (<int> channels_diff (- bus->num_channels num_channels))
						    (<int> base 0)
						    
						    (for-each 0 num_frames
							      (lambda (n)
								;;(<int> base (* bus->num_channels n))
								(for-each 0 num_channels
									  (lambda (ch)
									    (let* ((data <struct-rt_bus_data-*> "&bus->data[base++]"))
									      ,(rt-clean-write-bus 'faust->outs[ch][n]))))
								(+= base channels_diff))))
						 "/* No outbus */")
					    
					    (return 0)))

		   (<void> free_globals_func (lambda ((<struct-RT_Globals-*> rt_globals)
                                                      (<int> do_I_free_questionmark)) ;; free_globals_func is called two times. if do_I_free_questionmark is 0, it is called from the relatime thread immediately after being removed from the list of instruments.
                                               (if do_I_free_questionmark
                                                   (begin
                                                     (fprintf stderr (string "hepp, deleting rt_globals: %p, heap: %p, coroutine: %p\\n")
							      rt_globals rt_globals->heap rt_globals->main_coroutine.co)
                                                     (free rt_globals->queue)
                                                     (free rt_globals->block_queue)
						     (scm_gc_unregister_collectable_memory rt_globals->heap ,(+ (* *tar-nonatomic-heap-size* 2) (* *tar-max-mem-size* 4)) (string "rollendurch/stalin heap"))
						     (tar_delete_heap rt_globals->heap true) ;; tar_init_block is always called!
                                                     ;(if (> rt_globals->engine->num_procfuncs 0)
                                                     ;    (tar_delete_heap rt_globals->heap true)
                                                     ;    (tar_delete_heap rt_globals->heap false))
                                                     (free rt_globals)
                                                     )
                                                   (begin ;; In case the scheduler in rt_engine has removed us, the coroutines will be freed here. (this can be quite heavy...)
                                                     ;; rescheduling everything to mininmize number of context switches.
                                                     (<struct-rt_coroutine*> coroutines NULL)
                                                     (while (or (> rt_globals->queue_size 0)
                                                                (> rt_globals->block_queue_size 0))
                                                       (begin
                                                         (<struct-rt_coroutine*> coroutine (rt_get_first_coroutine_in_queue rt_globals))
                                                         (if (!= coroutine &rt_globals->main_coroutine)
                                                             (begin
                                                               (set! coroutine->next coroutines)
                                                               (set! coroutines coroutine)
                                                               (set! coroutine->stop_me 1)))))
                                                     (while (!= NULL coroutines)
                                                       (rt_insert_coroutine_in_queue rt_globals coroutines 0 0)
                                                       (set! coroutines coroutines->next))
                                                     (rt_insert_coroutine_in_queue rt_globals &rt_globals->main_coroutine 1 0) ;; This one last.
                                                     (rt_run_scheduler rt_globals)))))

		   (public

		    (<void-*> ,make-globals-func (lambda ((<struct-RT_Engine-*> engine))
						   (let* ((rt_globals <struct-RT_Globals-*> (calloc 1 (sizeof <struct-RT_Globals>))))
						     (set! rt_globals->freefunc free_globals_func)
						     (set! rt_globals->engine engine)
						     
                                                     ;; INT_MAX __MUST__ be set to INT64_MAX when switching to 64 bit.
                                                     (set! rt_globals->next_scheduled_time INT_MAX)

						     (set! rt_globals->current_coroutine &rt_globals->main_coroutine)
						     (begin
						       (set! rt_globals->queue (calloc ,queue-max-size (sizeof <struct-rt_coroutine*>)))
						       (set! rt_globals->block_queue (calloc ,queue-max-size (sizeof <struct-rt_coroutine*>)))
						       (set! dummy_coroutine.time 0)
						       (for-each 0 ,queue-max-size
								 (lambda (i)
								   (set! rt_globals->queue[i] &dummy_coroutine)
								   (set! rt_globals->block_queue[i] &dummy_coroutine))))

                                                     ;;(fprintf stderr (string "new heap start %p\\n") &dummy_coroutine)
                                                     (set! rt_globals->heap (tar_create_heap))
						     (scm_gc_register_collectable_memory rt_globals->heap ,(+ (* *tar-nonatomic-heap-size* 2) (* *tar-max-mem-size* 4)) (string "rollendurch heap"))
                                                     ;;(fprintf stderr (string "new heap end %p %p\\n") start_dyn end_dyn)

						     (return rt_globals))))
		   

		    (<void*> ,rt-faustsetvarfuncname (lambda ((<struct-RT_Globals*> rt_globals)
							      (<struct-mus_rt_faust*> faust))
						       (if (== NULL faust)
							   (return rt_globals->faust))
						       (set! rt_globals->faust faust)
						       (return faust))))
						
		    

                   (<void> myerror (lambda ((<char*> string))
                                     (rt_debug (string "Error: %s.") string)))

		   (functions->public

		    (<int> ,rt-funcname (lambda (,rt-globalvardecl
						 (<int> startframe)
						 (<int> endframe))
					  
					  ,(if (null? orgargs)
					       `(begin
						  ;;(fprintf stderr (string "jepp %u\\n") rt_globals->engine)
						  (<struct-RT_Engine-*> engine rt_globals->engine)
                                                  (<tar_heap_t*> heap rt_globals->heap)

						  (if (not (== 0 rt_globals->remove_me))
						      (return 1))

						  (if (!= NULL rt_globals->faust)
						      (return (rt_faustprocess rt_globals startframe endframe)))

                                                  (tar_before_using_heap heap)
						  
						  (set! rt_globals->prev_block_time engine->prev_block_time)
						  (set! rt_globals->block_time engine->block_time)
 						  (set! rt_globals->time (+ engine->block_time
									    startframe))
						  
						  (set! rt_globals->allocplace_end engine->allocplace_end)

						  (set! rt_globals->is_midi_read 0)

						  ;;((<struct-rt_bus-*> renamed_var__3 renamed_var__3_mirror out-bus rt_gen529))
						  ;; Copying buses
						  ,@(map (lambda (busdecl)
							   `(set! ,(symbol-append 'rt_globals-> (nth 1 busdecl))   ;; busname
								  ,(symbol-append 'rt_globals-> (nth 2 busdecl)))) ;; busname_mirror
							 busnames)
						  
						  (if (and (== 0 rt_globals->queue_size)
                                                           (== 0 rt_globals->block_queue_size))
						      (rt_spawn rt_globals realtime_innercall 0))
						  
						  ;; Note that current_coroutine points to main_coroutine here.
						  (set! rt_globals->main_coroutine.co (co_current))
						  (rt_insert_coroutine_in_queue rt_globals
										rt_globals->current_coroutine
										(+ rt_globals->block_time
										   endframe)
										0)
                                                  (let* ((old_heap <tar_heap_t*> (clm_set_tar_heap heap))
                                                         (old_ef  <error_func_t> (clm_set_error_func myerror)))
                                                    (rt_run_scheduler rt_globals)
                                                    (clm_set_error_func old_ef)
                                                    (clm_set_tar_heap old_heap))

						  (if (and (== 0 rt_globals->queue_size)
                                                           (== 0 rt_globals->block_queue_size))
						      (set! rt_globals->remove_me 1))
						  

                                                  (if (== (tar_after_using_heap heap) true)
                                                      (when (== 0 rt_globals->remove_me)
                                                        ;;(rt_debug (string "data: %d, stack: %d %d %d, used mem: %d, used atomic mem: %d")
                                                        ;;          (abs (- end_dyn start_dyn))
                                                        ;;          -1 -1 -1
							;;		  ;;(abs (- stack_top stack_bot))
							;;                  ;;stack_bot
							;;                  ;;stack_top
							;;                  (tar_get_used_mem heap)
							;;                  (tar_get_used_atomic_mem heap)
							;;                  )
                                                
                                                        (tar_add_root_concurrently heap start_dyn end_dyn) ; static data
                                                        (for-each 0 rt_globals->queue_size
                                                                  (lambda (i)
                                                                    (<struct-rt_coroutine*> coroutine rt_globals->queue[i+1])
                                                                    (tar_add_root_concurrently heap coroutine->stack_low coroutine->stack_high) ;stacks
                                                                    (tar_add_root_concurrently heap coroutine->co (+ (cast <char*> coroutine->co)   ;registerss
														     (EC_MAX (sizeof <ucontext_t>)
															     (sizeof <jmp_buf>))))))
                                                        (for-each 0 rt_globals->block_queue_size
                                                                  (lambda (i)
                                                                    (<struct-rt_coroutine*> coroutine rt_globals->block_queue[i+1])
                                                                    (tar_add_root_concurrently heap coroutine->stack_low coroutine->stack_high) ;stacks
                                                                    (tar_add_root_concurrently heap coroutine->co (+ (cast <char*> coroutine->co)   ;registerss
														     (EC_MAX (sizeof <ucontext_t>)
															     (sizeof <jmp_buf>))))))
                                                        (tar_add_root_concurrently heap rt_globals (+ (cast <char*> rt_globals) (sizeof <struct-RT_Globals>))) ;; dynamic data
                                                        (tar_start_gc heap)

                                                        ))

                                                  (return rt_globals->remove_me))
                           
                                               `(begin
                                                  (rt_error rt_globals (string "Main function can not take any arguments."))
                                                  (return 1))))))
		   
		   (public			      
		    (<float> ,funcname (lambda ((<SCM> argvect))
					 (<struct-RT_Engine> temp2 {0})
					 (<struct-RT_Engine-*> engine &temp2)
					 
					 (,rt-globalstructname temp {0} )
					 ,(append rt-globalvardecl '(&temp))

					 (set! rt_globals->engine engine)
					 
					 ;;(<struct-func_args> ,funcarg "{0}")

					 (SCM_ASSERT (== ,(length publicargs) (SCM_VECTOR_LENGTH argvect))
						     argvect
						     0
						     (string "Wrong number of arguments."))
							 
					 ,@(map (lambda (n name)
						  `(<SCM> ,(cadr name) (SCM_VECTOR_REF argvect ,n)))
						(iota (length publicargs))
						publicargs)
							      
					 ,@(map (lambda (das-type)
							   (let* ((type (car das-type))
								  (name (cadr das-type)))
							     (cond ((string=? "UNSPECIFIED" type)
								    (c-display "\n\nError! rt-compiler.scm/eval-c.scm/eval-c-gen-public-func: Strange type for " das-type "\n\n"))
								   ((string=? "SCM" type) "/* */")
								   (else
								    `(SCM_ASSERT ,(cond ((string=? "STRING" type) `(|| (scm_is_false ,name) (XEN_STRING_P ,name)))
											((string=? "POINTER" type) `(POINTER_P ,name))
											((string=? "INTEGER" type) `(== SCM_BOOL_T (scm_number_p ,name)))
											((string=? "FLOAT" type) `(== SCM_BOOL_T (scm_number_p ,name)))
											((string=? "DOUBLE" type) `(== SCM_BOOL_T (scm_number_p ,name)))
											(else (c-display "\n\nError! eval.cscm/eval-c-gen-public-func: What?\n\n")))
										 ,name
										 ,(let ((ret i)) (set! i (1+ i)) ret)
										 (string ,type))))))
							 types)
					 
					 ,@(map (lambda (type)
						  `(set! ,(symbol-append 'rt_globals-> (cadr type)) ,(list (string->symbol (<-> "GET_" (car type)))
													   (cadr type))))
						types)
					 
					 (,das-funcname rt_globals)
					 
					 (SCM_ASSERT (== NULL rt_globals->engine->error)
						     rt_globals->engine->errorvariable
						     rt_globals->engine->errorvarnum
						     rt_globals->engine->error)
					 ;;(SCM_ASSERT (== NULL ,(symbol-append funcarg '.error))
					 ;;	     ,(symbol-append funcarg '.errorvariable)
					 ;;	     ,(symbol-append funcarg '.errorvarnum)
					 ;;	     ,(symbol-append funcarg '.error))
					 (return rt_globals->engine->res))))))))))



#!

(do ((i 0 (1+ i)))
    ((= i 10))
  (display i)
  (newline))

(macroexpand-1 '(rt-macro-do ((i 0 (1+ i)))
			     ((= i 10))
			     (printf "%f\\n" i)))
(macroexpand-1 '(rt-macro-while (not (= i 10))
				  (printf "%f\\n" i)
				  (set! i (1+ i))))

(define a (rt-2 '(lambda ()
		   (do ((i 0 (1+ i)))
		       ((= i 10) (+ 2 5))
		     (printf "%d\\n" i)))))
(define a (rt-2 '(lambda ()
		   (let* ((a 0))
		     a)
		   3)))
(rt-funcall a 8)

(define a (rt-2 '(lambda (a)
		   (begin
		     (printf "sdf")))))

(begin a)

(define c 6)
(define b 9)
(let ((d 5.2))
  (c-display "res" (rt-funcall a 2))
  (c-display "c" c))
(begin c)

(rt (lambda ()
      (sin 50)))


(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))))

(define-rt2 (tak-rt x y z)
  (declare (<int> x y z))
  (if (not (< y x))
      z
      (tak-rt (tak-rt (- x 1) y z)
	      (tak-rt (- y 1) z x)
	      (tak-rt (- z 1) x y))))

(tak-rt 30 13 6)
(tak 30 13 6)


(define (hanoi n)
  (letrec ((move-them 
	    (lambda (n from to helper)
	      (if (> n 1)
		  (begin
		    (move-them (- n 1) from helper to)
		    (move-them (- n 1) helper to from))))))
    (move-them n 0 1 2)))
(define-rt2 (hanoi-rt n)
  (letrec ((move-them (lambda (n from to helper)
			(declare (<int> n))
			(if (> n 1)
			    (begin
			      (move-them (- n 1) from helper to)
			      (move-them (- n 1) helper to from))))))
    (move-them n 0 1 2)))

(hanoi 19)
(hanoi-rt 19)

!#



(define* (rt-2 term #:key (engine *rt-engine*))
  (let ((rt-3-result (rt-3 term))
	(orgterm term)
	(orgargs (cadr term)))
    ;;(c-display (nth 3 rt-3-result) (nth 5 rt-3-result))
    (if (not rt-3-result)
	(throw 'compilation-failed)
	(let* ((funcname (nth 0 rt-3-result))
	       (rt-faustsetvarfuncname (nth 1 rt-3-result))
	       (rt-funcname (nth 2 rt-3-result))
	       ;;(extnumbers-writing (caddr rt-3-result))
	       (extnumbers-writing '()) ;; Numbers are not writable anymore. See two lines down.
	       (extpointers (nth 4 rt-3-result))
	       (extnumbers (append (nth 3 rt-3-result) (nth 5 rt-3-result))) ;; extnumbers-writing trasfered here.
	       (make-globals-func (nth 6 rt-3-result))
	       (getternames (nth 7 rt-3-result))
	       (setternames (nth 8 rt-3-result))
	       (busnames    (nth 9 rt-3-result))
	       (term        (nth 10 rt-3-result))
	       (callmacro (procedure->macro
			   (lambda (x env)
			     (if (null? extnumbers-writing)
				 `(,funcname (vector ,@(map (lambda (extvar)
							      (let ((name (cadddr extvar))
								    (type (cadr extvar)))
								`(->2 ,type transform ,name ',name)))
							    (append extnumbers-writing extpointers extnumbers))
						     ,@(cdr x)))
				 `(begin
				    ;;,@(map (lambda (extvar)
				    ;;	      `(if (number? ,(cadddr extvar))
				    ;;		   (set! ,(cadddr extvar) (exact->inexact ,(car extvar)))))
				    ;;	    extnumbers-writing)
				    (,funcname (vector ,@(map (lambda (extvar)
								(let ((name (cadddr extvar))
								      (type (cadr extvar)))
								  `(->2 ,type transform ,name ',name)))
							      (append extnumbers-writing extpointers extnumbers))
						       ,@(cdr x))))))))
	       (externs (list-copy rt-extern-variables))
	       (rt-callmacro (procedure->macro
			      (lambda (x env)
				(let ((isoutdefined? (defined? 'out-bus env))
				      (isindefined? (defined? 'in-bus env)))
				  `(begin
				     (set! *rt-local-code-environment* (the-environment))
				     (letrec* ((rt-current-rt #f)
					       ,@externs)
				       (let* ((extra-gc-vars (make-hash-table 19))
					      (buses (make-hash-table 19))
					      (rt-globals (,make-globals-func (-> *rt-engine* engine-c)))
					      ;;(isoutdefined? (defined? 'out-bus env))
					      (ret (<realtime> (,rt-funcname)
							       rt-globals
							       ;; This variable is gc-marked manually in the funcall smob.
							       (list extra-gc-vars ,@(map (lambda (p)
											    (let ((ret (cadddr p)))
											      (if (eq? 'out-bus ret)
												  (if isoutdefined?
												      'out-bus
												      '*out-bus*)
												  (if (eq? 'in-bus ret)
												      (if isindefined?
													  'in-bus
													  '*in-bus*)
												      ret))))
											  extpointers))
							       ,engine)))
					 
				       ;; Make sure these never dissapear.
				       (if ,isoutdefined?
					   (hashq-set! extra-gc-vars (gensym) out-bus))
				       (if ,isindefined?
					   (hashq-set! extra-gc-vars (gensym) in-bus))
				       
				       ;; Setting all Guile variables (tranforming vars Guile->RT)
				       ,@(map (lambda (setter)
						(let* ((funcname (car setter))
						       (extvar (cadr setter))
						       (name (cadddr extvar))
						       (type (cadr extvar)))
						  `(begin
						     (,funcname rt-globals
								(->2 ,type transform
								     ,name
								     ',name
								     (lambda (var)
								       (hashq-set! extra-gc-vars (gensym) var)))))))
					      setternames)

				       ;; Setting all Guile bus variables (tranforming vars Guile->RT)				       
				       ;;((<struct-rt_bus-*> renamed_var__3 renamed_var__3_mirror out-bus rt_gen529))
				       ,@(map (lambda (busname)
						;;(c-display "gakk?" isoutdefined?)
						(let ((type (hashq-ref rt-types '<bus>))
						      (name (cadddr busname))
						      (funcname (cadr (cdddr busname))))
						  ;;(c-display type name funcname)
						  (if (eq? 'out-bus name)
						      (if isoutdefined?
							  `(,funcname rt-globals
								      (->2 ,type transform
									   out-bus
									   'out-bus
									   (lambda (var)
									     (hashq-set! extra-gc-vars (gensym) var))))
							  `(,funcname rt-globals
								      (->2 ,type transform
									   *out-bus*
									   '*out-bus*
									   (lambda (var)
									     (hashq-set! extra-gc-vars (gensym) var)))))
						      (if (eq? 'in-bus name)
							  (if isindefined?
							      `(,funcname rt-globals
									  (->2 ,type transform
									       in-bus
									       'in-bus
									       (lambda (var)
										 (hashq-set! extra-gc-vars (gensym) var))))
							      `(,funcname rt-globals
									  (->2 ,type transform
									       *in-bus*
									       '*in-bus*
									       (lambda (var)
										 (hashq-set! extra-gc-vars (gensym) var)))))
							  `(,funcname rt-globals
								      (->2 ,type transform
									   ,name
									   ',name
									   (lambda (var)
									     (hashq-set! extra-gc-vars (gensym) var))))))))
					      busnames)
				       
				       ;; Adding setters for number-variables.
				       ,@(map (lambda (getter setter)
						(let* ((setterfuncname (car setter))
						       (extvar (cadr setter))
						       (name (cadddr extvar))
						       (type (cadr extvar)))
						  `(->2 ret add-method ',name (make-procedure-with-setter
									       (lambda ()
										 (,(car getter) rt-globals))
									       (lambda (newval)
										 (,setterfuncname rt-globals (rt-number-2-rt newval)))))))
					      getternames
					      setternames)

				       (->2 ret add-method 'faust
					    (let ((faust #f))
					      (lambda arg
						(cond ((null? arg)
						       faust)						      
						      ((not (and (object? (car arg))
								 (eq? '<mus_rt_faust> (-> (car arg) class-name))))
						       (c-display "Error." (car arg) "is not a faust object."))
						      (else
						       (set! faust (car arg))
						       (,rt-faustsetvarfuncname rt-globals (-> faust get-c-object)))))))
				     
				       ;; Adding "getter" for the pointers.
				       ,@(map (lambda (pointer)
						`(->2 ret add-method ',pointer (lambda ()
										 ,pointer)))
					      (map cadddr extpointers))
				       

				       ;; Adding setters for buses
				       ;;((<struct-rt_bus-*> renamed_var__3 renamed_var__3_mirror out-bus rt_gen529))
				       ,@(map (lambda (busname)
						(let ((type (car busname))
						      (name (cadddr busname))
						      (func (cadr (cdddr busname))))
						`(->2 ret add-method ',name (make-procedure-with-setter
									     (lambda ()
									       ,name)
									     (lambda (newval)
									       (if (not (rt-bus-p newval))
										   (c-display "Error." newval "is not a bus.")
										   (begin
										     ;; There is a memory-leak here. Should (gensym) be exchanged with ,name?
										     (hashq-set! extra-gc-vars (gensym) newval) 
										     (,func rt-globals (SCM_SMOB_DATA newval)))))))))
					      busnames)
				       
				       ;; Adding "getter" for in-bus and out-bus (TODO, setter as well, that would be very cool.)
				       ;(if ,isoutdefined?
					;   (->2 ret add-method 'out-bus (lambda ()
					;				  out-bus))
					;   (->2 ret add-method 'out-bus (lambda ()
					;				  *out-bus*)))
				       ;(if ,isindefined?
				;	   (->2 ret add-method 'in-bus (lambda ()
					;				 in-bus))
					;   (->2 ret add-method 'in-bus (lambda ()
					;				 *in-bus*)))
				       (set! rt-current-rt ret)
				       ret))))))))
	  
	  (apply eval-c-non-macro (append (list (<-> "-I" snd-header-files-path " -ffast-math ") ;; "-ffast-math") ;; " -Werror "
						#f
						"#include <math.h>"
						"#include <clm.h>"
						"#include <xen.h>"
						"#include <vct.h>"
						"#include <clm2xen.h>"
						"#include <ladspa.h>"
						"#include <pcl.h>"

						(if (provided? 'snd-pd-external)
						    "#include <m_pd.h>"
						    "")
						(if (provided? 'snd-pd-external)
						    "#include <snd_pd_external.h>"
						    "")
						"typedef struct {
                                                   mus_any_class *core;
                                                   int chans;
                                                   mus_float_t *vals;
                                                   bool data_allocated;
                                                 } mus_frame"
						
						"typedef struct {
						  mus_any_class *core;
						  mus_any *outn_writer;
						  mus_any *revn_writer;
						  mus_frame *outf, *revf;
						  mus_float_t *outn;
						  mus_float_t *revn;
						  int chans, rev_chans;
						  mus_interp_t type;
						  mus_float_t reverb;
						} locs;"

						)
					  
					  
					  term))
	  
	  (list 'rt-rt
		callmacro
		rt-callmacro
		(primitive-eval funcname)
		(primitive-eval rt-funcname))))))



;; Yepp, has to redefine set!
(define rt-old-set! set!)
(define-macro (set! var val)
  (if (and (list? var)
	   (eq? '-> (car var)))
      `((setter (<- ,@(cdr var))) ,val)
      `(,rt-old-set! ,var ,val)))

		
;; rt-1 + compiled code cache handling.
(define *rt-cached-funcs* (make-hash-table 997))
(define* (rt-1 term #:key (engine *rt-engine*))
  (rt-gensym-reset)
  (rt-extern-reset)

  ;;(c-display "term after:" term)

  (let* ((key (list term
		    (rt-safety)               ;; Everything that can change the compiled output must be in the key.
		    (-> engine samplerate))) ;; If saving to disk, the result of (version) and (snd-version) must be added as well. (and probably some more)
	 (cached (hash-ref *rt-cached-funcs* key)))
    ;;(c-display "term/cached" term cached)
    (or	cached
	(let ((new-rt (rt-2 (deep-list-copy term)))) ;; <- Hmm. Thats really bad. Seems like a set-car! is performed
	  (hash-set! *rt-cached-funcs* key new-rt)     ;;         on term somewhere. Fix it with a deep-list-copy for now.
	  new-rt))))
(define (rt-clear-cache!)
  (set! *rt-cached-funcs* (make-hash-table 997)))

;;(define (rt-1 term)
;;  (rt-2 term))

(define *rt-local-code-environment* (the-environment))

;; rt
(define-macro (rt-compile term)
  `(begin
     ;;(set! *rt-local-code-environment* (the-environment)) ;; *rt-local-etc.* must be placed directly in the macro for hygenic reasons.
                                                          ;; For example, if placed in rt-1, "term" willl be in the environment.
     ;;   No, this one belongs in the procedure->macro block in rt-2.
     (rt-1 ',term)))
(define-macro (rt-c term)
  `(rt-compile ,term))

(define-macro (rt-func term)
  (let ((das-rt (rt-1 term)))
    (if (not das-rt)
	#f
	`(lambda ,(cadr term)
	   (set! *rt-local-code-environment* (the-environment)) ;; Should probably be moved into rt-2...
	   (,(cadr das-rt) ,@(cadr term))))))

(define-macro (define-rt2 def . body)
  `(define ,(car def) (rt-func (lambda ,(cdr def)
				 (define ,def
				   ,@body)
				 (,(car def) ,@(cdr def))))))
  
(define-macro (rt-funcall rt-func . args)
  `(begin
     (set! *rt-local-code-environment* (the-environment)) ;; Should probably be moved into rt-2...
     (cadr ,rt-func) ,@args))

(define *rt* #f)
(define *rt1* #f)
(define *rt2* #f)
(define *rt3* #f)
(define *rt4* #f)
(define-macro (<rt> rt-func)
  `(begin
     (let ((ret ((caddr (rt-compile ,rt-func)))))
       (set! *rt4* *rt3*)
       (set! *rt3* *rt2*)
       (set! *rt2* *rt1*)
       (set! *rt1* *rt*)
       (set! *rt* ret)
       ret)))


(define-macro (<rt-block> code)
  `(<rt> (lambda ()
           (block
             ,@(cddr code)))))

(define-macro (rt-play-macro play-type eval-type rest)
  (let ((start #f)
	(dur #f)
	(func (last eval-type))
	(instrument (rt-gensym2))
	(start2 (rt-gensym2))
	(keyargs (let ((ret (find-tail keyword? rest))) (if (not ret) '() ret)))
	(args (take-while (lambda (x) (not (keyword? x)))
			  rest)))
    (cond ((= 2 (length args))
	   `(let ((,instrument ,eval-type))
	      (-> ,instrument ,play-type ,(car rest) ,(cadr rest) ,@keyargs)
	      ,instrument))
	  ((= 1 (length args))
	   `(let ((,instrument ,eval-type))
	      (-> ,instrument ,play-type ,(car rest) ,@keyargs)
	      ,instrument))
	  ((= 0 (length args))
	   `(let ((,instrument ,eval-type))
	      (-> ,instrument ,play-type ,@keyargs)
	      ,instrument))
	  (else
	   (throw 'wrong-number-of-arguments-to-<rt-play>)))))


(define*2 (<rt-play-do> play-type :key (out-bus #f) (in-bus #f) :allow-other-keys :rest rest)
  `(let (,@(if in-bus `((in-bus ,in-bus)) '())
	,@(if out-bus `((out-bus ,out-bus)) '()))
     (rt-play-macro ,play-type
                    (<rt-block> ,(last rest))
                    ,(rt-fix-infix-nonrt (c-butlast rest)))))
#!
(<rt-play-do> 'play :out-bus #f :gakk 9 2 3 4)
!#

(define-macro (<rt-play> . rest)
  (apply <rt-play-do> (cons 'play rest)))

(define-macro (<rt-play-abs> . rest)
  (apply <rt-play-do> (cons 'play-abs rest)))


(define-macro (<rt-run> . rest)
  (define instrument (gensym))
  (define wait #f)
  (define code rest)
  (when (equal? (car rest) :wait)
    (set! code (cddr rest))
    (set! wait (cadr rest)))
  (if wait
      `(let ((,instrument (<rt> (lambda ()
                                  ,@code
                                  (rt_return_void)))))
         (-> ,instrument play ,wait))
      `(let ((,instrument (<rt> (lambda ()
                                  ,@code
                                  (rt_return_void)))))
         (-> ,instrument play))))

;;  `(rt-play-macro play-abs (<rt> ,(last rest)) ,(c-butlast rest)))


(define-macro (definstrument def . body)
  ;; First compile up any rt-code.
  ;;(c-display "Oh yeah")
  (let ((rt-funcs '()))
    
    (define (add-rt-func code)
      (let ((name (rt-gensym2)))
	(push! (list name code)
	       rt-funcs)
	name))
    
    (define (find-rt term)
      (cond ((null? term) term)
	    ((not (list? term)) term)
            
	    ;((eq? 'rt-compile (car term))
	    ; (let ((func (add-rt-func (cadr term))))
	    ;   `((cadr ,func))))
	    
	    ((eq? '<rt> (car term))
	     ;;(find-rt (macroexpand-1 term)))
	     (let ((func (add-rt-func (cadr term))))
	       `((caddr ,func))))
            
	    ((eq? '<rt-block> (car term))
	     (find-rt (macroexpand-1 term)))
            
	    ((eq? '<rt-run> (car term))
	     (find-rt (macroexpand-1 term)))
            
	    ((eq? '<rt-out> (car term))
	     (find-rt (macroexpand-1 term)))
            
	    ((eq? '<rt-play> (car term))
	     (find-rt (macroexpand-1 term)))
	    
	    ((eq? '<rt-play-abs> (car term))
	     (find-rt (macroexpand-1 term)))
	    
	    (else
	     (map find-rt term))))
    
    (if (or (member #:optional def)
	    (member #:rest def))
	(begin
	  (c-display "Rest or optional argument is not possible with definstrument :-(")
	  (throw 'gakk)))
    
    (let ((keymember (member #:key def)))
      (if keymember
	  (set-cdr! keymember (append (list '(out-bus *out-bus*) '(in-bus *in-bus*)) (cdr keymember)))
	  (set! def (append def (list #:key '(out-bus *out-bus*) '(in-bus *in-bus*))))))
    
    (let ((newbody (find-rt body)))
      (if (not (null? rt-funcs))
	  (if (string? (car body))
	      `(define ,(car def)
		 (let ,(map (lambda (def)
			      `(,(car def) (rt-compile ,(cadr def))))
			    rt-funcs)
		   (lambda* ,(cdr def)
			    ,@(cdr newbody))))
	      `(define ,(car def)
		 (let ,(map (lambda (def)
			      `(,(car def) (rt-compile ,(cadr def))))
			    rt-funcs)
		   (lambda* ,(cdr def)
			    ,@newbody))))
	  `(define* ,def
	     ,@body)))))
		

#!
(define (a b c)
  2 3 4)
(define a
  (let ((asdfa asdfadsf))
    (lambda (b c)
      2 3 4)))

(<rt-play> start dur
	   (lambda ()
	     (out (* (env amp-env)))))
!#

(define-macro (<rt-out> . body)
  (define start #f)
  (define len #f)
  (define in-bus #f)
  (define out-bus #f)
  (let loop ((body body))
    (cond ((keyword? (car body))
	   (cond ((eqv? :start (car body))
		  (set! start (cadr body))
		  (loop (cddr body)))
		 ((eqv? :in-bus (car body))
		  (set! in-bus (cadr body))
		  (loop (cddr body)))
		 ((eqv? :out-bus (car body))
		  (set! out-bus (cadr body))
		  (loop (cddr body)))
		 ((or (eqv? :len (car body))
		      (eqv? :length (car body)))
		  (set! len (cadr body))
		  (loop (cddr body)))
		 ((eqv? :dur (car body))
		  (set! start (cadr body))
		  (set! len (caddr body))
		  (loop (cdddr body)))
		 (else
		  (error (list "Unknown keyword" (car body) " for <rt-out>. Legal keywords are start/len/dur.")))))
	  ((and start len)
	   ;;(c-display "start/len" start len in-bus out-bus)
	   `(<rt-play> :in-bus ,in-bus :out-bus ,out-bus ,start ,len (lambda () (out ,@body))))
	  (start
	   `(<rt-play> :in-bus ,in-bus :out-bus ,out-bus ,start (lambda () (out ,@body))))
	  (len 
	   `(<rt-play> :in-bus ,in-bus :out-bus ,out-bus 0 ,len (lambda () (out ,@body))))
	  (else
	   `(<rt-play> :in-bus ,in-bus :out-bus ,out-bus (lambda () (out ,@body)))))))
#!
(<rt-out> :start 3 :len 1 (oscil (extern (make-oscil))))
(<rt-out> :dur 3 1 (oscil (extern (make-oscil))))
!#


(c-display "#:RT-Compiler loaded successfully...")



#!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Drodle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rt-2 '(lambda ()
	 (+ 2 a 35)))

(rt-2 '(lambda ()
	(call-with-current-continuation
	 (lambda (return)
	   (return 2)))))


(define a (rt-2 '(lambda ()
		   (call-with-current-continuation
		    (lambda (return)
		      (return 2)
		      5)))))
(rt-funcall a)

(define a (rt-2 '(lambda ()
		   (let* ((a (lambda ()
			       9))
			  (b a))
		     (b)))))

(define a (rt-2 '(lambda ()
		   (let ((a (lambda ()
			      5)))
		     a))))

(define a (rt-2 '(lambda ()
		   (((lambda ()
		       (lambda ()
			 5)))))))


(define a (rt-2 '(lambda ()
		   (let ((a (let ((b (lambda ()
				       9)))
			      b)))
		     (a)))))

(rt-funcall a)



;; Closure: (is not supposed to work)
(define a (rt-2 '(lambda ()
		   (let* ((a (lambda (b)
			       (lambda ()
				 (the <int> b))
			       )))
		     ((a 50))))))
;; Wrong result:
(rt-funcall a)
(rte-frames)
(-> *rt-engine* dir)

(lambda ((<struct-RT_Globals> *rt_globals))
  (let* ((b__2 <int>)
	 (rt_gen413__3 <int> (lambda ((<struct-RT_Globals> *rt_globals))
			       (return (the <int> rt_globals->b__2))))
	 (a__1 (<int> (<struct-RT_Globals-*>)) (lambda-decl ((<struct-RT_Globals> *rt_globals) (<int> b__2))))
	 (rt_gen415 (<int> (<struct-RT_Globals-*>)) (lambda ((<struct-RT_Globals> *rt_globals)
							     (<int> _rt_local_b__2))
						      (set! rt_globals->b__2 _rt_local_b__2)
						      (begin
							(begin
							  (return rt_gen413__3)))))
	 (a__1 (<int> (<struct-RT_Globals-*>)) (lambda ((<struct-RT_Globals> *rt_globals)
							(<int> _rt_local_b__2))
						 (let* ((rt_gen416 <int> rt_globals->b__2)
							(_rt_ret (<int> (<struct-RT_Globals-*>)) (rt_gen415 rt_globals _rt_local_b__2)))
						   (set! rt_globals->b__2 rt_gen416)
						   (return _rt_ret)))))
    (let* ((rt_gen414__4 (<int> (<struct-RT_Globals-*>))))
      (begin
	(begin
	  (set! rt_gen414__4 (a__1 rt_globals 50))
	  (return (rt_gen414__4 rt_globals)))))))


(rt-2 '(lambda ()
	 (begin
	   (range i 0 (mus-channels loc)
		  (rt-set-locvals loc i 5))
	   (range i 0 (mus-channels (rt-get-loc-outf loc))
		  (out i (rt-get-float-val (mus-data (rt-get-loc-outf loc)) i))))))

(rt-2 '(lambda ()
	 (begin
	   (range i 0 (mus-channels locs)
		  (rt-set-locvals locs i 5))
	   (range i 0 (mus-channels (rt-get-loc-outf locs))
		  (out i (rt-get-float-val (mus-data (rt-get-loc-outf locs)) i))))))

(macroexpand-1 '(rt-macro-locsig locs 5))

(define a (rt-2 '(lambda ()
		   (letrec ((das_func (lambda ()
				     (let ((a 2))
				       (while (< a 10)
					      (printf "ai: %d\\n" a)
					      (if (>= a 5)
						  (break))
					      (set! a (1+ a))
					      (let ((ai (lambda ()
							  (if (odd? a)
							      (continue)))))
						(ai))
					      (set! a (1+ a))
					      ;;(das_func)
					      ;;(break)
					      )
				       a))))
		     (das_func)))))
(rt-funcall a)

(define a (rt-2 '(lambda ()
		   (let ((ai (lambda (a)
			       5)))
		     (ai 2)))))

(define a (rt-2 '(lambda ()
		   (let loop ((a 5)
			      (b 6)
			      (c 7))
		     (if (< (+ a b c) 100)
			 (loop (1+ a) (1+ b) (1+ c))
			 (+ a b c))))))

(define a (rt-2 '(lambda ()
		   (range i 0 5
			  (printf "%d " i)))))

(rt-funcall a)


;; Error:
(define a (rt-2 '(lambda ()
		   (let ((a 1))
		     (letrec ((a 2)
			      (b (let ((c (lambda ()
					    a)))
				   (c))))
		       b)))))

(rt-funcall a)


(provided? 'snd-pd-external)

(rt-macroexpand '(< a 3))

!#



