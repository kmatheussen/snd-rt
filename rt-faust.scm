
;; This file is loaded by rt-compiler.scm



(define *rt-local-faust-code-environment* (the-environment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Main struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ec-struct <mus_rt_faust>
  <void*> dsp
  <void*> compute_func
  <float-**> ins
  <float-**> outs
  <int> num_inputs
  <int> num_outputs
  <void*> newDsp
  <void*> init
  <void*> handle
  <void*> conclude
  <void*> deleteDsp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Caching ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define faust-cache '())
(define (add-faust-cache source handle)
  (push! (list source handle) faust-cache))
(define (find-faust-cache source)
  (call/cc
   (lambda (return)
     (for-each (lambda (cache)
		 (call/cc
		  (lambda (next)
		    (define cache-source (car cache))
		    (if (not (= (length source)
				(length cache-source)))
			(next #f))
		    (for-each (lambda (s1 s2)
				(if (not (or (eq? s1 s2)
					     (string=? s1 s2)))
				    (next #f)))
			      source
			      cache-source)
		    (return (cadr cache)))))
	       faust-cache)
     #f)))
(define* (display-faust-source :optional (faust (-> *rt* faust)))
  (define handle (-> faust handle))
  (call/cc
   (lambda (return)
     (for-each (lambda (cache)
		 (when (equal? (cadr cache) 
			       handle)
		   (for-each c-display (car cache))
		   (return #t)));(car cache))))
	       faust-cache)
     #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Compiling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define has-compiled-faust-gui #f)

;; Don't reset this one. (Dynamic linking of c++ files seems to be very dynamic.)
;; (removed, only necessary when linking with RTLD_GLOBAL)
;;(define num-compiled-faust-files -1)

(define* (compile-faust source :key (make-gui #t))  
  (define file (generate-faust-source-file source))

  (c-display (<-> "compiling " file ".dsp"))

  (system (<-> "faust -a snd-rt-gtk.cpp -o " file ".cpp " file ".dsp"))

  ;; Uncomment line below if linking with RTLD_GLOBAL
  ;;(system (<-> "sed -i -e 's/mydsp/mydsp" (number->string (inc! num-compiled-faust-files 1)) "/g' " file ".cpp"))
  (system (<-> "g++ -O3 -ffast-math -I" snd-header-files-path
               " " (if make-gui "-DMAKE_GUI" "")
	       " " (or (getenv "CFLAGS") "") " "
	       " " *eval-c-CFLAGS* " "
	       (if make-gui
		   (<-> (string-append (string #\`) "pkg-config --cflags --libs gtk+-2.0" (string #\`)) " ")
		   " ")
	       " " (or (getenv "LDFLAGS") "") " "
	       file ".cpp -shared -fPIC -o " file ".so"))

  (for-each delete-at-exit
	    (list (<-> file ".dsp")
		  (<-> file ".cpp")
		  (<-> file ".so")))

  (let ((handle (c-dynamic-handle (<-> file ".so") :flags (RTLD_LOCAL))))
    (cond ((not handle)
	   (c-display "Unable to dynamically link " (<-> file ".so"))
	   #f)
	  (else
	   (set! has-compiled-faust-gui #t)
	   (add-faust-cache source handle)
	   handle))))


    

#!
(define test (make-faust-object "/home/kjetil/snd-run/osc"))
!#




;; Faust gui

;;(compile-faust "/home/kjetil/snd-run/dummy" :make-gui #t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Eval-C code to connect snd-rt and faust ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Faust dsp

(eval-c ""
	"#include <dlfcn.h>"
	"#include <unistd.h>"

	(shared-struct <mus_rt_faust>)

	(<void> cleanup (lambda ((<struct-mus_rt_faust-*> faust)
				 ((<void> (<void*>)) conclude)
				 ((<void> (<void*>)) deleteDsp))
			  (printf (string "Cleaning up old faust object %p\\n") faust)
			  
			  (conclude faust->dsp)
			  (deleteDsp faust->dsp)
			  
			  (for-each 0 faust->num_inputs
				    (lambda (n)
				      (free faust->ins[n])))
			  (for-each 0 faust->num_outputs
				    (lambda (n)
				      (free faust->outs[n])))
			  (free faust->ins)
			  (free faust->outs)
			  
			  ;; faust->handle is cached and can be reused.
			  ;;(dlclose faust->handle)
			  
			  (free faust)))
	
	(public
	 (<void*> faust-get-gui (lambda ((<struct-mus_rt_faust-*> faust)
					 ((<void*> (<char*> <void*>)) newGTKUI)
					 (<void*> window)
					 ((<void> (<void*> <void*>)) buildUserInterface))
				  (<void*> gtkui (newGTKUI (string "testing") window))
				  (buildUserInterface faust->dsp gtkui)
				  (return gtkui)))
	 
	 (<void> c-cleanup-faust-object (lambda ((<struct-mus_rt_faust-*> faust))
					  (cleanup faust faust->conclude faust->deleteDsp)))

	 (<void> start-faust-gui (lambda (
					  (<struct-mus_rt_faust-*> faust)
					  (<void*> gtkui)
					  ((<void> (<void*>)) runGTKUI)
					  )
				   ;;(printf (string "ui: %p\\n") gtkui)
				   (runGTKUI gtkui)
				   return
				   ))

	 ;; (-> faust init-gui) must have been called before calling faust-contains-ui?
	 (<SCM> faust-contains-ui? (lambda (
					   (<struct-mus_rt_faust-*> faust)
					   ((<int> (<void*>)) containsUI)
					   (<void*> gtkui)
					   )
				    (printf (string "contains? %d\\n" ) (containsUI gtkui))
				    (if (containsUI gtkui)
					(return SCM_BOOL_T)
					(return SCM_BOOL_F))))
	 
	 (<SCM> init-faust-c-object (lambda (
					     (<struct-mus_rt_faust-*> faust)
					     ((<void*> (<void>)) newDsp)
					     ((<int> (<void*>)) getNumInputs)
					     ((<int> (<void*>)) getNumOutputs)
					     ((<void> (<void*> <int>)) init)
                                        ;((<void*> (<char*>)) newGTKUI)
                                        ;((<void> (<void*> <void*>)) buildUserInterface)
                                        ;((<void> (<void*>)) runGTKUI)
					     )
				      ;;(printf (string "ai %p\\n") newDsp);faust)
				      (set! faust->dsp (newDsp))
				      (if (== NULL faust->dsp)
					  (return SCM_BOOL_F))

				      (init faust->dsp ,(rte-samplerate))

				      (set! faust->num_inputs (getNumInputs faust->dsp))
				      (set! faust->num_outputs (getNumOutputs faust->dsp))

				      (set! faust->ins (calloc (sizeof <float-*>) faust->num_inputs))
				      (set! faust->outs (calloc (sizeof <float-*>) faust->num_outputs))
				      
				      (for-each 0 faust->num_inputs
						(lambda (n)
						  (set! faust->ins[n] (calloc (sizeof <float>) ,rt-max-frame-size))))
				      (for-each 0 faust->num_outputs
						(lambda (n)
						  (set! faust->outs[n] (calloc (sizeof <float>) ,rt-max-frame-size))))
				      
				      (return SCM_BOOL_T)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Create faust object ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (cleanup-faust-object faust)
  (c-display "      TRYING TO CLEANUP (never seen this message, something is not wrong if this message is snown, which it is not.")
  )
;;  (c-cleanup-faust-object faust))

(define faust-gui-faust #f)

(define (make-faust-object source)

  (define handle (find-faust-cache source))
  (define first-one? (not has-compiled-faust-gui))

  (if (not handle)
      (set! handle (compile-faust source :make-gui first-one?)))

  (let ((faust (<mus_rt_faust>))
	;;(handle (c-dynamic-handle (<-> file ".so") :flags (RTLD_LOCAL)))
	)
    (if first-one?
	(set! faust-gui-faust faust))

    (-> faust handle handle)

    (cond ((not handle)
	   #f)
	  ((not (init-faust-c-object (-> faust get-c-object)
				     (c-dlsym handle "newDsp")
				     (c-dlsym handle "getNumInputs")
				     (c-dlsym handle "getNumOutputs")
				     (c-dlsym handle "init")
				     ))
	   (c-display "Unable to create new Faust Dsp object.")
	   #f)
	  (else
	   (-> faust compute_func (c-dlsym handle "compute"))
	   (-> faust add-method 'display-source
	       (lambda ()
		 (display-faust-source faust)))
	   (-> faust add-method 'init-gui
	       (lambda (dialog)
		 (let ((gtkui (faust-get-gui (-> faust get-c-object)
					     (c-dlsym (-> faust-gui-faust handle) "newGTKUI")
					     (-> dialog dialog)
					     (c-dlsym handle "buildUserInterface"))))
		   (-> faust add-method 'gtkui (lambda ()
						 gtkui)))))
	   
	   (-> faust add-method 'contains-ui? (lambda ()
						;;(-> faust init-gui)
						(faust-contains-ui? (-> faust get-c-object)
								    (c-dlsym (-> faust-gui-faust handle) "containsUI")
								    (-> faust gtkui))))
	   (-> faust add-method 'open-gui
	       (lambda (dialog)
		 ;;(define d (<dialog> "hello" #f))
		 ;(define d (-> dialog dialog))
		 ;(set-car! d "POINTER")
		 ;(c-display "dalog: " d)
		 (start-faust-gui (-> faust get-c-object)
				  (-> faust gtkui)
				  (c-dlsym (-> faust-gui-faust handle) "runGTKUI"))))

	   (-> faust newDsp (c-dlsym (-> faust handle) "newDsp"))
	   (-> faust init (c-dlsym (-> faust handle) "init"))
	   (-> faust conclude (c-dlsym (-> faust handle) "conclude"))
	   (-> faust deleteDsp (c-dlsym (-> faust handle) "deleteDsp"))
	   (add-finalizer (-> faust get-c-object) cleanup-faust-object)
	   ;;(add-finalizer faust cleanup-faust-object)
	   faust))))



#!
(define test (make-faust-object "/home/kjetil/snd-run-old4/osc"))
(-> test get-c-object)
(-> test dir)
(-> test compute_func)
(-> test dsp)
(-> test num_inputs)
(-> test num_outputs)
(-> test ins)

!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Generate faust code ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-faust-parse term)
  (set! *eval-macro-prefix* eval-faust-macro-prefix)
  (eval-parse term))

(define-faust-macro (= a b)
  (<-> (eval-faust-parse a) "= " (eval-faust-parse b) ";"))

(define-faust-macro (import something)
  (<-> "import(" (eval-parse `(string ,something)) ");"))

(define-faust-macro (out . rest)
  `(= process ,@rest))

(define-faust-macro (unquote something)
  (local-eval something *rt-local-faust-code-environment*)
  (primitive-eval something))

(define-faust-macro (unquote-splicing something)
  (apply <-> (map eval-parse (local-eval something *rt-local-faust-code-environment*))))




;; Guile doesn't like "(: etc. etc.)"
;; moved into the general infix fixer in eval-c
;;(define-faust-macro (colon . rest)
;;  (define first #t)
;;  (apply <-> (map (lambda (res)
;;		    (if first
;;			(begin
;;			  (set! first #f)
;;			  (eval-parse res) )   		
;;			(<-> ": " (eval-parse res))))
;;		  rest)))

;; Automatically add (string) around second argument for the gui objects.
(for-each (lambda (funcname)
	    (primitive-eval `(define-faust-macro (,funcname a . rest)
			       (<-> ,(symbol->string funcname)
				    "(" 
				    (eval-parse `(string ,a))
				    (apply <-> (map (lambda (s)
						      (<-> ", " s))
						    (map eval-parse rest)))
				    ")"))))

	  '(button checkbox hslider vslider nentry vgroup hgroup tgroup vbargraph hbargraph))

;; More faust translation stuff added to eval-c.scm


(define (generate-faust-source-file sourcelines)
  (fix-defines
   (define basename (tmpnam))
   (define sourcefile (<-> basename ".dsp"))
   (define fd (open-file sourcefile "w"))
   ;;(c-display "hepp" sourcelines)
   (for-each (lambda (code)
	       (write-line (<-> code "") fd))
	     sourcelines)
   (close fd)
   basename))


(define*2 (generate-faust-source :key 
				 (autoimport-libs #t)
				 :rest terms)
  (c-display "terms" terms)
  (set! terms (map eval-faust-parse terms))
  (when autoimport-libs
    (set! terms (cons "import(\"music.lib\");" terms))
    ;;(write-line "import(\"effect.lib\");" fd)
    ;;(write-line "import(\"filter.lib\");" fd)
    ;;(write-line "import(\"math.lib\");" fd)
    )
  terms)


#!
(generate-faust-source '(import "ai"))

(generate-faust-source #:autoimport-libs #f
		       "process = osc(500);")
(generate-faust-source "process = osc(500);")
(generate-faust-source "process = osc(500);")
!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; <faust> and <faust-vct> in Snd-rt ;;;;;;;;
;;;;;;;; (frame by frame processing) ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			   
(<rt-type> '<faust>
	   (lambda (faust)
	     (and (object? faust)
		  (eq? '<mus_rt_faust> (-> faust class-name))))
	   #f
	   :transformfunc
	   (lambda (faust)
	     (-> faust get-c-object))
	   :c-type '<struct-mus_rt_faust-*>)

(define-rt-ec <vct-*> rt_faust_compute
  (lambda (,rt-globalvardecl
           (<faust> faust)
           (<vct-*> input))
    (<FaustComputeFunc> compute faust->compute_func)
    (<vct-*> output (rt_alloc_vct rt_globals faust->num_outputs))
    ;;(printf (string "faust->compute_func: %p %p\\n") compute faust->dsp)
    (let* ((minin <int> (MIN faust->num_inputs input->length)))
      (for-each 0 minin
                (lambda (n)
                  (set! faust->ins[n][0] input->data[n])))
      (compute faust->dsp 1 faust->ins faust->outs)
      (for-each 0 faust->num_outputs
                (lambda (n)
                  (set! output->data[n] faust->outs[n][0])))
      (return output))))


(define-rt-macro (faust-compute faust-object input)
  `(rt_faust_compute ,faust-object ,input))


(define-macro (start-faust-gui-if-necessary faust)
  `(let ((dialog (default-rt-dialog (%delay rt-current-rt) :show #f)))
     (-> ,faust init-gui dialog)
     ;;(c-display "contains?" (-> faust contains-ui?))
     (when (-> ,faust contains-ui?)
       (-> ,faust open-gui dialog)
       (-> dialog show))))


(define*2 (rt-<faust>-do :key 
			 (in '(vct))
			 :rest code)
  ;;(c-display "input:" input)
  ;;(set! code (map eval-faust-parse code))
  (set! code (apply generate-faust-source code))
  (c-display "code" code)
  `(faust-compute (extern (let ((faust (make-faust-object ',code)))
			    (start-faust-gui-if-necessary faust)
			    faust))
		  ,in))


(define-rt-macro (<faust-vct> . rest)
  (apply rt-<faust>-do rest))

(define-rt-macro <faust>
  (labamba (:key (in '(vct)) :rest rest)
    (if (and (null? (cdr rest))
	     (symbol? (car rest)))
	`(vct-ref (faust-compute ,(car rest) ,in) 0)
	`(vct-ref (<faust-vct> :in ,in ,@rest) 0))))

;;(define-rt-macro (<faust> . rest)
;;  `(vct-ref (<faust-vct> ,@rest) 0))


#!
(<rt-out> (vct-scale! (<faust-vct> "process=osc(400),osc(500);")
		      (<slider> "vol" 0.0 0.1 1.0)))

(<rt-out> (* (<faust> "freq = vslider(\"freq\", 600, 0, 2400, 0.1);"
		      "process = vgroup(\"ai\",osc(freq));")
	     (<slider> "vol" 0.0 0.1 1.0)))

(<rt-out> (* (<faust> (= freq    (vslider "freq" 600 0 2400 0.1))
		      (= process (vgroup "ai" (osc freq))))
	     (<slider> "vol" 0.0 0.1 1.0)))

(<rt-out> (<faust> :in (vct (oscil))
		   "process=*(0.3);"))

!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; <rt-faust> in Guile ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; (block processing)  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define*2 (<rt-faust-do> :key
			 (in-bus #f)
			 (out-bus #f)
			 :allow-other-keys
			 :rest code)
  (define faust (rt-gensym))
  (define rt (rt-gensym))
  
  (set! code (apply generate-faust-source code))

  ;;(c-display "in-bus/out-bus:" in-bus out-bus)
  
  `(begin
     (set! *rt-local-faust-code-environment* (the-environment))
     (let* ((,faust (make-faust-object ',code))
            ,@(if in-bus `((in-bus ,in-bus)) '())
            ,@(if out-bus `((out-bus ,out-bus)) '())
            (,rt (<rt> (lambda () (out (in))))))
       (-> ,rt faust ,faust)
       (-> ,rt play)
       (let ((rt-current-rt ,rt))
         (start-faust-gui-if-necessary ,faust))
       ,rt)))

(define-macro (<rt-faust> . code)
  (apply <rt-faust-do> code))

(define-macro (<rt-faust-url> url)
  `(<rt-faust> (url ,url)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Allocating faust instances in RT ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-rt-ec <faust> rt_faust_make_faust
  (lambda (,rt-globalvardecl
	   (<faust> faust))
    (<struct-mus_rt_faust-*> copy (tar_alloc rt_globals->heap (sizeof <struct-mus_rt_faust>)))
    ((<void*> (<void>)) newDsp faust->newDsp)
    ((<void> (<void*> int)) init faust->init)
    (memcpy copy faust (sizeof <struct-mus_rt_faust>))
    (set! copy->dsp (newDsp))
    (init copy->dsp ,(rte-samplerate))
    (return copy)
    ))
    
(define-rt-macro (make-faust . code)
  (set! code (apply generate-faust-source code))
  `(rt_faust_make_faust (extern (let ((faust (make-faust-object ',code)))
				  (start-faust-gui-if-necessary faust)
				  (c-display "addr" (cadr (-> faust get-c-object)))
				  faust))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; <faust> in Stalin '';;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-stalin-ec <void-*> rt_faust_compute
  (lambda ((<void*> vfaust)
           (<void*> vinput))
    (<vct-*> input (cast <vct*> vinput))
    (<struct-mus_rt_faust-*> faust (cast <struct-mus_rt_faust-*> vfaust))
    (<FaustComputeFunc> compute faust->compute_func)
    (<vct-*> output (rt_alloc_vct faust->num_outputs))
    ;;(printf (string "faust->compute_func: %p %p\\n") compute faust->dsp)
    (let* ((minin <int> (MIN faust->num_inputs input->length)))
      (for-each 0 minin
                (lambda (n)
                  (set! faust->ins[n][0] input->data[n])))
      (compute faust->dsp 1 faust->ins faust->outs)
      (for-each 0 faust->num_outputs
                (lambda (n)
                  (set! output->data[n] faust->outs[n][0])))
      (return output))))

(define-stalin-macro (faust-compute faust-object input)
  `(rt_faust_compute ,faust-object ,input))


(define-stalin-ec <void*> rt_faust_make_faust
  (lambda ((<void*> vfaust))
    (<struct-mus_rt_faust-*> faust (cast <struct-mus_rt_faust-*> vfaust))
    (<struct-mus_rt_faust-*> copy (tar_alloc heap (sizeof <struct-mus_rt_faust>)))
    ((<void*> (<void>)) newDsp faust->newDsp) ;;(cast ((<void*> (<void>)) newDsp)
    ((<void> (<void*> int)) init faust->init) ;;(cast ((<void*> (<void>)) newDsp)
    (memcpy copy faust (sizeof <struct-mus_rt_faust>))
    (set! copy->dsp (newDsp))
    (init copy->dsp ,(rte-samplerate))
    (return copy)
    ))
    
(define-stalin-macro (make-faust . code)
  (set! code (apply generate-faust-source code))
  (let ((faust (make-faust-object code)))
    (start-faust-gui-if-necessary faust)
    (c-display "addr" (cadr (-> faust get-c-object)))
    `(rt_faust_make_faust (ulong_to_void_ ,(cadr (-> faust get-c-object))))))

(define-stalin-macro (<faust-vct> :key 
                                  (in '(vct))
                                  :rest code)
  (set! code (apply generate-faust-source code))
  (c-display "code" code)
  `(faust-compute (ulong_to_void_ ,(let ((faust (make-faust-object code)))
                                      (start-faust-gui-if-necessary faust)
                                      (cadr (-> faust get-c-object))))
		  ,in))

(define-stalin-macro (<faust> :key (in '(vct)) :rest rest)
  (if (and (null? (cdr rest))
           (symbol? (car rest)))
      `(vct-ref (faust-compute ,(car rest) ,in) 0)
      `(vct-ref (<faust-vct> :in ,in ,@rest) 0)))


#!
(define test (make-faust-object (generate-faust-source '(include "/home/kjetil/snd-run-old4/osc.dsp"))))
(-> test get-c-object)
(-> test dsp)
(-> test dir)
(-> test compute_func)
(-> test num_inputs)
(-> test num_outputs)
(-> test ins)

(<rt-stalin>
 (define faust-oscillator ,(cadr (-> test get-c-object)))
 (block
   (out (<faust> faust-oscillator))))


(<rt-stalin>
 (define osc (make-faust (out (* 0.29 (osc (vslider "freq" 600 0 2400 0.1))))))
 (block
   (out (<faust> osc))))

(<rt-stalin>
 (while #t
   (define osc (make-faust "process = osc(500)*0.22;"))
   (block :dur 1:-s
     (out (<faust> osc)))
   (wait 0.5:-s)))

(<rt-stalin>
 (block
   (out (<faust> "process = osc(500)*0.3;"))))


(let ((code ("import(\"music.lib\");" "process = osc(500)*0.2; ")))
  (let ((faust (make-faust-object ',code)))
    (start-faust-gui-if-necessary faust)
    (cadr (-> faust c-object))
    ))


!#






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Testing  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#!
(<rt-faust> "freq    = hslider(\"freq\", 600, 0, 2400, 0.1);"
	    "process = vgroup(\"ai\",osc(freq));")

(<rt-faust> "process = osc(500)*0.2;")

(<rt-faust> "process=osc(400);")
(<rt-faust> (out (osc 400)))

(<rt-faust> (import "math.lib")
	    (= (smooth c) (colon "*(1-c)" "+~*(c)"))
	    (= vol        (colon  (hslider "volume (db)" 0 -96 0 0.1)
				  db2linear
				  (smooth 0.99)))
	    (= freq       (hslider "freq" 1000 0 24000 0.1))
	    (out (vgroup "Osc" (* (osci freq) vol))))
(display-faust-source)


(let ((in-bus *out-bus*))
  (<rt-faust> "vmeter(x)       = attach(x, envelop(x) : vbargraph(\"dB\", -96, 10));
 	       hmeter(x)       = attach(x, envelop(x) : hbargraph(\"dB\", -96, 10));
	       envelop         = abs : max(db2linear(-96)) : linear2db : min(10)  : max ~ -(96.0/SR);
	       process         = vmeter;")
  (<rt-faust> "freq = vslider(\"freq\", 600, 0, 2400, 0.1);"
	      (= vol (|:| (hslider "volume (db)" 0 -96 0 0.1) db2linear))
	      "process = vgroup(\"ai\",osc(freq)*vol);"))
(<rt-faust> :in-bus *out-bus*
	    "vmeter(x)       = attach(x, envelop(x) : vbargraph(\"dB\", -96, 10));
 	    hmeter(x)       = attach(x, envelop(x) : hbargraph(\"dB\", -96, 10));
	    envelop         = abs : max(db2linear(-96)) : linear2db : min(10)  : max ~ -(96.0/SR);
	    process         = vmeter;")


(eval-faust-parse '(= vol (colon (hslider "volume (db)" 0 -96 0 0.1) db2linear)))
(eval-faust-parse '(= vol (|:| (hslider "volume (db)" 0 -96 0 0.1) db2linear)))
(eval-faust-parse '(= vol (seq (hslider "volume (db)" 0 -96 0 0.1) db2linear)))


;; To try the next one, go to the faust software catalog at http://faust.grame.fr/catalog.php ,
;; open the code of a program, mark the code, and evaluate expression below.
;; (requires the "xsel" commandline program)
(<rt-faust> :autoimport-libs #f	   
	    (x11-selection))

(<rt-faust> :autoimport-libs #f	   
	    (url (x11-selection)))

(<rt-faust> (url "http://faudiostream.cvs.sourceforge.net/*checkout*/faudiostream/faust/examples/tapiir.dsp"))
(<rt-faust-url> "http://faudiostream.cvs.sourceforge.net/*checkout*/faudiostream/faust/examples/karplus32.dsp")
(<rt-faust-url> "http://faudiostream.cvs.sourceforge.net/*checkout*/faudiostream/faust/tools/faust2pd-1.0.2/examples/seqdemo/organ.dsp")

(<rt-faust> (include "/home/kjetil/snd-run/osc.dsp"))

(let ((abus (make-bus)))
  (<rt-faust> "smooth(c)   = *(1-c) : +~*(c);"
	      "gain        = vslider(\" dB \", 0, -96, 4, 0.1) : db2linear : smooth(0.999);"
	      "process     = vgroup(\"fader\", *(gain));")
  (set! (-> *rt* in-bus) abus) ;; *rt* contains the result of the last evaluated <rt*> expression.
  (<rt-out> (oscil))
  (set! (-> *rt* out-bus) abus))

(let ((abus (make-bus)))
  (<rt-faust> :in-bus abus
	      "smooth(c)   = *(1-c) : +~*(c);"
	      "gain        = vslider(\" dB \", 0, -96, 4, 0.1) : db2linear : smooth(0.999);"
	      "process     = vgroup(\"fader\", *(gain));")
  (<rt-out> :out-bus abus
	    (oscil)))


(let ((abus (make-bus)))
  (<rt-faust> :in-bus abus
	      (= (smooth c) "*(1-c) : +~*(c)")
	      (= gain       (colon (vslider " dB " 0 -96 4 0.1) 
				   db2linear
				   (smooth 0.999)))
	      (out (vgroup "fader" "*(gain)")))
  (<rt-out> :out-bus abus
	    (oscil)))

(display-faust-source (-> *rt1* faust))


(define-faust-macro (oscivol freq vol)
  `(* ,vol (osci ,freq)))

(<rt-faust> (out (oscivol 500 0.2)))

(<rt-out> (<faust> :in (vct (oscil* 200))
		   (= gain    (vslider "gain" 0.2 0 1 0.01))
		   (= process (vgroup "ai" "*(gain)"))))


!#
