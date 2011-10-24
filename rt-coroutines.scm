
(define *coroutine-stacksize* 4096)


(define-ec-struct <rt_coroutine>
  <int64> time
  ;;<rt_coroutine> next
  <void*> func
  <jmp_buf> remove_me
  <int> stop_me
  <int> run_scheduler_when_removed
  <struct-rt_coroutine*> next
  <void*> stack_high
  <void*> stack_low
  <coroutine_t> co
  <struct-RT_Globals*> rt_globals)


(define rt_coroutine_elements_in_rt_globals
  (flatten '((<struct-rt_coroutine-*> current_coroutine)
	     (<struct-rt_coroutine-**> queue)
	     (<struct-rt_coroutine-**> block_queue)
	     (<struct-rt_coroutine> main_coroutine)
             (<int> remove_me)
	     (<int> queue_size)
	     (<int> block_queue_size)
	     (<int64> time)
             (<int64> block_time)
             (<int64> prev_block_time)
	     (<int>   next_scheduled_time))))



(<rt-type> '<coroutine>
	   (lambda (coroutine)
	     (and (object? coroutine)
		  (eq? '<rt_coroutine> (-> coroutine class-name))))
	   #f
	   :transformfunc
	   (lambda (coroutine)
	     (-> coroutine get-c-object))
	   :c-type '<struct-rt_coroutine-*>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Scheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; priority queue (using the common binary heap type implementation)
(define queue-max-size 1024)
;;(define queue (make-array queue-max-size :initial-element NULL))
;;(define queue-size 0)


  

;; get_first_coroutine_in_queue
;; ****************************
;;
;; Returns the first in queue, and deletes it from queue. (O(log n) efficiency)
;; Note that its an error to call this function in case the queue is empty.
(rt-ec-private-function <struct-rt_coroutine*> rt_get_first_coroutine_in_queue
			(lambda (,rt-globalvardecl)
  (<int>                   which_queue (or (== 0 rt_globals->block_queue_size)
                                           (and (> rt_globals->queue_size 0)
                                                (< rt_globals->queue[1]->time ;; shouldn't this be <= ?
                                                   rt_globals->block_queue[1]->time))))
  (<struct-rt_coroutine**> queue     (?kolon which_queue
                                             rt_globals->queue
                                             rt_globals->block_queue))
  (<struct-rt_coroutine*>  ret       queue[1])
  (<int>                   size      (?kolon which_queue
                                             (- rt_globals->queue_size 1)
                                             (- rt_globals->block_queue_size 1)))
  (<struct-rt_coroutine*>  last      queue[size+1])
  (<int64>                 last_time last->time)
  (<int>                   i         1)
  (<int>                   child)
  
;  (fprintf stderr (string "which: %d %d %d, %d %d\\n")
;           (cast <int> which_queue)
;           (== 0 rt_globals->block_queue_size)
;           (cast <int> rt_globals->queue[1]->time)
;           (cast <int> rt_globals->block_queue[1]->time))
  
  (if which_queue
      rt_globals->queue_size--
      rt_globals->block_queue_size--)
  
  ;; Code below is a common binary heap "delete min" operation.
  (while (<= (* i 2) size)
    (set! child (* i 2))
    (if (and (!= child size)
	     (< queue[child+1]->time queue[child]->time))
	child++)
    (if (> last_time queue[child]->time)
	(begin
	  (set! queue[i] queue[child])
	  (set! i child))
	break))

  (set! queue[i] last)

  (if which_queue
      (set! rt_globals->next_scheduled_time (/ queue[1]->time 4)))

  ;; Remove priority info from the time attribute.
  ;;(set! ret->time (& ret->time ~(4-1)))
  (set! ret->time (/ ret->time 4))
  
  (return ret)))

			  

;; insert_coroutine_in_queue
;; *************************
;;
;; Returns 0 in case the priority queue is full. (O(log n) efficiency)
(define-rt-ec <int> rt_insert_coroutine_in_queue
		(lambda (,rt-globalvardecl
			 (<coroutine> coroutine)
			 (<int> time)
			 (<int> priority))

  (<struct-rt_coroutine-**> queue rt_globals->queue)

  (if (>= rt_globals->queue_size
	  ,(- queue-max-size 2))
      (return 0))
  
  rt_globals->queue_size++

  ;; Not too sure about this one. Maybe it can cover up bugs.
  ;;(if (< time 0)
  ;;    (set! time 0))

  (if (or (== 1 rt_globals->queue_size)
          (< time rt_globals->next_scheduled_time))
      (set! rt_globals->next_scheduled_time time))
  
  ;; Add priority info to the time attribute. ("priority" is a 2 bit integer)
  (set! time (* time 4))
  (set! time (+ time priority))

  (set! coroutine->time time)

  ;; Code below is a common binary heap "insert" operation.
  (let* ((i    <int>   rt_globals->queue_size)
	 (newi <int>   (/ i 2)))
    (while (> queue[newi]->time time)
      (set! queue[i] queue[newi])
      (set! i newi)
      (set! newi (/ newi 2)))
    ;;(fprintf stderr (string "Inserting at pos: %d (queu-size: %d)\\n") i rt_globals->queue_size)
    (set! queue[i] coroutine))

  (return 1)))


(define-rt-ec <int> rt_insert_coroutine_in_block_queue
  (lambda (,rt-globalvardecl
           (<coroutine> coroutine)
           (<int> time)
           (<int> priority))

    ;;(fprintf stderr (string "insert block-coroutine: %p\\n") coroutine)
  (<struct-rt_coroutine-**> queue rt_globals->block_queue)

  (if (>= rt_globals->block_queue_size
	  ,(- queue-max-size 2))
      (return 0))
  
  rt_globals->block_queue_size++

  ;; Not too sure about this one. Maybe it can cover up bugs.
  ;;(if (< time 0)
  ;;    (set! time 0))

  ;; Add priority info to the time attribute. ("priority" is a 2 bit integer)
  (set! time (* time 4))
  (set! time (+ time priority))

  (set! coroutine->time time)

  ;; Code below is a common binary heap "insert" operation.
  (let* ((i    <int>   rt_globals->block_queue_size)
	 (newi <int>   (/ i 2)))
    (while (> queue[newi]->time time)
      (set! queue[i] queue[newi])
      (set! i newi)
      (set! newi (/ newi 2)))
    ;;(fprintf stderr (string "Inserting at pos: %d (queu-size: %d)\\n") i rt_globals->queue_size)
    (set! queue[i] coroutine))

  (return 1)))


(define-c-macro (rt_current_coroutine)
  "(rt_globals->current_coroutine)")
(<rt-func> 'rt_current_coroutine '<coroutine> '() :is-immediate #t)

;; current_time
;; ************
;;
(define-c-macro (rt_inc_current_time n)
  (<-> "(rt_globals->time+=" (eval-c-parse n) ")"))
(<rt-func> 'rt_inc_current_time '<void> '(<int>))

(define-c-macro (rt_set_current_time n)
  (<-> "(rt_globals->time=" (eval-c-parse n) ")"))
(<rt-func> 'rt_set_current_time '<void> '(<int>))




;; get_next_scheduled_time_in_queue
;; *********************************
;;
(define-c-macro (rt_get_next_scheduled_time)
  "(rt_globals->next_scheduled_time)")
(<rt-func> 'rt_get_next_scheduled_time '<int> '() :is-immediate #t)

			  


;; set_stack_low_value
;; *******************
;;
;; Sets the low stack pointer so that the garbage collector doesn't have to
;; scan the whole stack. (Note that this
;; will also work in case the stack grows instead of shrinks, but then the
;; name of the function is of course a bit wrong since it actually sets the
;; high value and not the low value.)
(eval-c ""
	"#include <pcl.h>"
	(shared-struct <rt_coroutine>)
	(<nonstatic-void> rt_set_stack_low_value (lambda ((<struct-rt_coroutine*> coroutine))
						   (<int> dummy)
						   (set! coroutine->stack_low &dummy))))
;;(<rt-func> 'rt_set_stack_low_value '<void> '(<coroutine) rt-readin> <int>)



;; switch_to_coroutine
;; *******************
;(rt-ec-private-function <void> rt_switch_to_coroutine
;			(lambda (,rt-globalvardecl
;				 (<struct-rt_coroutine*> coroutine))			  
;  ;;(fprintf stderr (string "Warning, nested function variables not preserved before coroutine switch.\\n"))
;  (rt_set_stack_low_value rt_globals->current_coroutine)
;  (set! rt_globals->current_coroutine coroutine)
;  (set! rt_globals->time coroutine->time)
;  (co_call coroutine->co)))



;; rt_switch_to_coroutine should only be called in run_scheduler and free_globals_func
;;
;; run_scheduler
;; *************
(define-rt-ec <void> rt_run_scheduler
  (lambda (,rt-globalvardecl)
    (<struct-rt_coroutine*> next (rt_get_first_coroutine_in_queue rt_globals))
    (if (!= next rt_globals->current_coroutine)
        (rt_switch_to_coroutine rt_globals next)
        (set! rt_globals->time next->time))
    (let* ((coroutine <struct-rt_coroutine*> rt_globals->current_coroutine))
      (cond ((and (== &rt_globals->main_coroutine coroutine)
                  (== 1 rt_globals->remove_me))
             (rt_debug (string "happ")))
            ((== 1 coroutine->stop_me)
             (begin
               (set! coroutine->stop_me 0)
               (longjmp coroutine->remove_me 1)))))))




;; yield
;; *****
(define-rt (yield)
  (rt_insert_coroutine_in_queue (rt_current_coroutine)
				(get-time)
				2)
  (rt_run_scheduler))



;; wait
;; ****
(define-rt (wait n)
  (define new-time (+ (get-time) n))
  (declare (<int> n new-time))
  (cond ((< new-time
	    (rt_get_next_scheduled_time)) ;; What if there's a block scheduled before that?
	 (rt_set_current_time new-time))  ;; No need to schedule, just increase time.
	(else
	 (rt_insert_coroutine_in_queue (rt_current_coroutine)
				       new-time
				       1)
	 (rt_run_scheduler))))
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Block (inner loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; block
;; *****
(define-rt-macro (block 
                   :key dur duration
                   :rest code)
  (define das-duration (rt-gensym))

  ;;(c-display "HEPP")

  (set! duration (or dur duration))

  (if duration
      `(let ((,das-duration ,duration))
         (declare (<int> ,das-duration))
         (while (> ,das-duration 0)
           (while (and (< (get-time)
                          (rt_get_next_scheduled_time))
                       (> ,das-duration 0))
             ,@code
             (rt_inc_current_time 1)
             (set! ,das-duration (- ,das-duration 1)))

           ;; Now do an extra-low-priority yield:
           (rt_insert_coroutine_in_block_queue (rt_current_coroutine)
                                               (get-time)
                                               3)
           (rt_run_scheduler)))
      `(while #t
         (while (and (< (get-time)
                        (rt_get_next_scheduled_time)))
           ,@code
           (rt_inc_current_time 1))
         ;; Now do an extra-low-priority yield:
         (rt_insert_coroutine_in_block_queue (rt_current_coroutine)
                                             (get-time)
                                             3)
         (rt_run_scheduler))))




;; spawn-block
;; ***********
(define-rt-macro (spawn-block . rest)
  (define start 0)
  (define duration #f)
  (define code rest)
  (when (equal? :dur (car rest))
    (set! start (nth 1 rest))
    (set! duration (- (nth 2 rest) start))
    (set! code (nth-cdr 3 rest)))
  `(spawn :wait ,start
     (block :duration ,duration
       ,@code)))

	  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Coroutine pool / starting and stopping coroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

;; pool/lowlevel coroutine access
;;   We don't want to call make-low-level-coroutine in the realtime thread, since its a quite
;;   heavy operation involving two context switches. (or something like that). Therefore a pool.


(new-rt-pool 'coroutine)

;; Important. This one needs to be recompiled whenever rt_get..,rt_swi, etc. is changed. (very inconvenient)
(eval-c "-lpcl"
	"#include <pcl.h>"

	"struct RT_Globals"

	(shared-struct <rt_coroutine>)
	
	,`(define-struct <RT_Globals>
	    <void*> freefunc
	    ,@rt_coroutine_elements_in_rt_globals
	    )

	(get-proto rt_push_to_pool)
	(get-proto rt_set_stack_low_value)
        (get-proto rt_debug)

        (<void> rt_switch_to_coroutine
                (lambda (,rt-globalvardecl
                         (<struct-rt_coroutine*> coroutine))			  
                  (rt_set_stack_low_value rt_globals->current_coroutine)
                  (set! rt_globals->current_coroutine coroutine)
                  (set! rt_globals->time coroutine->time)
                  (co_call coroutine->co)))

	,(nth 2 (rt-get-ec-function 'rt_get_first_coroutine_in_queue))
	;;,(nth 2 (rt-get-ec-function 'rt_switch_to_coroutine))
	,(nth 2 (rt-get-ec-function 'rt_run_scheduler))

;;	(<nonstatic-void> coroutine_entry (lambda ((<volatile-void*> arg))
;;				  (<volatile-struct-rt_coroutine*> coroutine arg)
	(<nonstatic-void> coroutine_entry (lambda ((<struct-rt_coroutine*> coroutine))
                                            (if (== 0 (setjmp coroutine->remove_me))
                                                (co_resume)
                                                (begin ;;throw thunk
                                                  ;;(rt_debug (string "ai1 %p") coroutine)
                                                  (rt_push_to_pool coroutine NULL ,(get-rt-pool-num 'coroutine))
                                                  ;;(rt_debug (string "ai2 %p %d %d %d")
                                                  ;;          coroutine
                                                  ;;          coroutine->run_scheduler_when_removed
                                                  ;;          coroutine->rt_globals->queue_size
                                                  ;;          coroutine->rt_globals->block_queue_size)
                                                  (if coroutine->run_scheduler_when_removed
                                                      (rt_run_scheduler coroutine->rt_globals)
                                                      (co_resume))))
                                            ;;(rt_debug (string "ai3 %p") coroutine)
                                            (set! coroutine->run_scheduler_when_removed 1)
                                            (while 1
                                              ;;(fprintf stderr (string "top while %p\\n") (co_current))
                                              ((<int> (<void*>)) func coroutine->func)
                                              ;;(rt_debug (string "ai4 %p %p") coroutine func)
                                              (set! coroutine->stack_high &func)
                                              (func coroutine->rt_globals)
                                              (rt_push_to_pool coroutine NULL ,(get-rt-pool-num 'coroutine))
                                              (rt_run_scheduler coroutine->rt_globals)
                                              )))
	(public
	 (<void> init_coroutine (lambda ((<struct-rt_coroutine*> coroutine)
					 (<int> stacksize))
				  (set! coroutine->co (co_create coroutine_entry coroutine NULL stacksize))
				  (if (== NULL coroutine->co)
				      (begin
					(fprintf stderr (string "ERROR! Creating coroutine pool failed.\\n"))
					(exit 0)))
				  (co_call coroutine->co)
				  ))))

(define* (make-coroutine :key (stacksize *coroutine-stacksize*))
  (define coroutine (<rt_coroutine>))
  (init_coroutine (-> coroutine get-c-object) stacksize)
  coroutine)

(init-rt-pool 'coroutine
	      (lambda ()
		(-> (make-coroutine) get-c-object))
	      :num-elements 1000)


(<rt-type> '<coroutine>
	   (lambda (coroutine)
	     (and (object? coroutine)
		  (eq? '<rt_coroutine> (-> coroutine class-name))))
	   #f
	   :transformfunc
	   (lambda (coroutine)
	     (-> coroutine get-c-object))
	   :c-type '<struct-rt_coroutine-*>)

(rt-ec-private-function <coroutine> rt_make_coroutine
			(lambda (,rt-globalvardecl
				 ((<void> (<struct-RT_Globals-*>)) thunk))
			  (<struct-rt_coroutine*> coroutine (rt_get_pool_element ,(get-rt-pool-num 'coroutine)))
			  ;;(<struct-rt_coroutine*> coroutine (calloc 1 (sizeof <struct-rt_coroutine)))
			  ;;(set! coroutine->co (co_create coroutine_entry coroutine NULL 4096))
			  ;;(fprintf stderr (string "Made coroutine: %p\\n") coroutine)
			  (if (== NULL coroutine)
			      (exit 10))
			  (set! coroutine->func thunk)
			  (set! coroutine->rt_globals rt_globals)
			  (return coroutine)))
(<rt-func> 'rt_make_coroutine '<coroutine> '((<void> ())) :needs-rt-globals #t)

(rt-ec-private-function <struct-rt_coroutine-*> rt_spawn
			(lambda (,rt-globalvardecl
				 ((<void> (<struct-RT_Globals-*>)) thunk)
				 (<int> wait))
			  (<struct-rt_coroutine-*> coroutine (rt_make_coroutine rt_globals thunk))
			  (rt_insert_coroutine_in_queue rt_globals
							coroutine 
							(+ rt_globals->time wait)
							1)
                          (return coroutine)))
(<rt-func> 'rt_spawn '<coroutine> '((<void> ()) <int>) :needs-rt-globals #t)

;; spawn
;; *****
(define-rt-macro (spawn . rest)
  (define coroutine (rt-gensym))
  (define wait 0)
  (define code rest)
  (when (equal? :wait (car rest))
    (set! wait (cadr rest))
    (set! code (cddr rest)))
  `(rt_spawn (lambda ()
	       ,@code
	       (rt_return_void))
	     ,wait))

(define-rt-ec <int> rt_get_stop_me
  (lambda (,rt-globalvardecl)
    (return rt_globals->current_coroutine->stop_me)))

(define-rt-ec <void> rt_stop
  (lambda ((<coroutine> coroutine))
    (set! coroutine->stop_me 1)))

(define-rt-ec <void> rt_stop_me
  (lambda (,rt-globalvardecl)
    (set! rt_globals->current_coroutine->stop_me 0) ;; In case another coroutine have sat it.
    (longjmp rt_globals->current_coroutine->remove_me 1)))

(define-rt-macro (stop . coroutine)
  (if (null? coroutine)
      `(rt_stop_me)
      `(rt_stop ,(car coroutine))))
  
;; remove-me
;; *********
;(define-rt (remove-me)
;  (throw (=> current-coroutine :remove-me)))

#!
(gc)
;; usleep probably mess with signals...
(define-rt-ec <void> my_usleep (lambda ((<int> n))
                                 (usleep n)))
(while #t
  (usleep 500)
  (if (< (nth 4 (rte-info)) 2)
      (<rt-play> (lambda ()
		   ;;(spawn (printf "hello\\n"))
		   ;;(yield)
		   ;;(wait 44100)
		   ;;(printf "hello2\\n")
		   ;;(my_usleep 10000)
		   (remove-me)))))
				
(<rt-play> (lambda ()
	     ;;(printf "hello2\\n")
	     (my_usleep 10000)
	     ;;(remove-me)
	     ))



(<rt-play> (lambda ()
	     ;(block :duration 3
	;	    (printf "time: %d\\n" (the <int> (get-time))))
	     (define phase 0.0)
	     (block :duration 44100
		    (out (sin phase))
		    (set! phase (+ phase (hz->radians 440))))
	     (spawn
               (debug "hello0\\n")
               (wait 88200)
               (debug "hello3\\n"))
	     (spawn
               (debug "hello4\\n"))
	     (yield)
	     (debug "hello-1\\n")
	     (wait 44100)
	     (debug "hello2\\n")))


(<rt-play> (lambda ()
             (spawn
               (out 0.2))
             (out 0.3)))

(<rt-play> (lambda ()
             (out 0.3)))

(<rt-play> (lambda ()
	     (block; :duration 44100
               (out (* 0.1 (oscil))))))
(rte-info)
(<rt-play> (lambda ()
	     (spawn
	      (block :duration 44100
		     (out (* 0.1 (oscil)))))
	     ;; Keep "wait" below makes it crash.
	     (wait 144100)))

(<rt-play> (lambda ()	     	     
	     (spawn-block :dur 44100 200000
		(out (* 0.1 (oscil* 200))))
	     (block :duration 100000
		(out (* 0.1 (oscil))))
	     (debug "finished\\n")))


(<rt-play> (lambda ()
	     (block :duration 44100
		    (out (* 0.2 (oscil* 200))))
	     (block :duration 44100
		    (out (* 0.2 (oscil* 300))))

	     (spawn
               (block :duration 44100
                 (out (* 0.2 (oscil* 200))))
               (debug "hello2\\n"))
	      
             (debug "hello\\n")
	     (wait 100000)))

(<rt-play> (lambda () 	     
	     (range i 1 10
                (spawn-block :dur 44100 200000
                  (out (* 0.3 (oscil* (* 1 500))))))
             (wait (+ 44100 200000))))

(define initphase 0.0)
(<rt-play> (lambda () 	     
             (range i 1 100
                    (spawn
                      (let ((phase initphase)
                            (phaseinc (* i 0.004)))
                        (declare (<float> phase initphase))
                        (block ;:duration 200000
                          (out (* 0.02 (sin phase)))
                          (set! phase (+ phase phaseinc)))))
                    (yield))))

(<rt-play> (lambda () 	     
             (range i 1 100
                    (spawn
                      (let ((i i))
                        (yield)
                        (let* ((phase 0.0)
                               (phaseinc (* i 0.004)))
                          (declare (<double> phase phaseinc))
                          (declare (<int> i))
                          (block ;:duration 200000
                            (out (* 0.02 (sin phase)))
                            (set! phase (+ phase phaseinc))))))
                    (yield))))


;; Merkelig: (ikke forandr!)
(<rt-play> (lambda ()
	     (define u 1)
	     (range iy 1 100
		    (spawn
                      (define freq (hz->radians (* (inc! u 1) 100)))
                      (debug "debug %f" (the <float> freq))
                      (define phase 0.0)
                      (declare (<double> freq phase))
                      (block     
                        ;;(out (oscil* 200))
                        (out (* 0.1 (sin phase)))
                        (set! phase (+ phase freq)))))))

(<rt-run>
 (define osc (spawn-block (out (* 0.1 (oscil)))))
 (wait (^s 2))
 (stop osc)
 (debug "got it"))

(<rt-run>
 (spawn
   (stop)))

(<rt-run>
 (spawn
   (block (out (* 0.1 (oscil* 440))))))


(<rt-run>
 (spawn
   (block (out (* 0.1 (oscil* 440)))))
 (spawn
   (block (out (* 0.1 (oscil* 220)))))
 (spawn
   (block (out (* 0.1 (oscil* 660))))))






(rt-clear-cache!)

(<rt-run> :wait 2
 (debug "hello!")
 (out (oscil*)))


(macroexpand '(<rt-play> (lambda () 1)))

(rt-macroexpand '(spawn (printf "hello\\n")))
(load-from-path "rt-coroutines.scm")

(eval-c ""
	(run-now
	 (<int64> basebase 12342344)
	 (<int64> base  (+ basebase 1024))
	 (<int64> base2 (+ basebase 44100))
	 (<int64> a (+ (<< base  2) 0))
	 (<int64> b (+ (<< base2 2) 1))
	 (printf (string "hepp: %d\\n") (> b a))))



!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Signal/event handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!
;; "event" works like a cons cell. But the "cons" function is not available in the target language yet
;; (because the language is statically typed and only recently got a garbage collector), so we need
;;  a custom structure instead:
(define-struct event
  :next
  :coroutine)

(define (wait-event event)  
  (define new-event (make-event :coroutine current-coroutine))
  (set-finalizer! new-event
		  (lambda (event)
		    (define coroutine (=> event :coroutine))
		    (when coroutine
		      (set! (=> coroutine :run-scheduler-when-removed) #f)
		      (set! current-coroutine coroutine)
		      (low-level-coroutine-switch-to (=> coroutine :low-level-coroutine)))))
  (set! (=> event :next) new-event)
  (run-scheduler)
  (if (not (=> current-coroutine :run-scheduler-when-removed))
      (throw (=> current-coroutine :remove-me)))) ;; the current coroutine was never signaled.


;; This is a first in, first out. Is that good? (does it matter?)
(define (signal-event event)
  (define next-event (=> event :next))
  (when next-event
    (set! (=> event :next (=> next-event :next)))
    (let ((coroutine (=> next-event :coroutine)))
      (set! (=> next-event :coroutine) #f) ;; To avoid finalizing
      (set! (=> coroutine :time) (current-time))
      (insert-coroutine-in-queue! :coroutine coroutine
				  :priority 1) ;; The yielded coroutine has priority 2, so this one will run before the current coroutine.
      (yield))))

;; This is also a first in, first out. Is that good? (does it matter?)
(define (broadcast-event event)
  (define temp (=> event :next))
  (when temp
    (while temp
      (let ((coroutine (=> temp :coroutine)))
	(set! (=> temp :coroutine) #f) ;; To avoid finalizing
	(set! (=> coroutine :time) (current-time))
	(insert-coroutine-in-queue! :coroutine coroutine
				    :priority 1))
      (set! temp (=> temp :next)))
    (set! (=> event :next) #f)
    (yield)))

!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!
(define (init-audio-thread main-func)
  (spawn-do main-func))

;; The audio block function. If it returns #f, it will not be called again at next audio block.
(define (audio_thread blocksize)
  (inc! (current-time) blocksize)
  (insert-coroutine-in-queue! :priority 0) ;; Highest priority to avoid any coroutines sending
  ;;                                          sound to the soundcard before its sent out its previous buffer.
  (run-scheduler) ;; init-audio-thread must be called first, so there is allways at least one coroutine waiting here.
  (> queue-size 0))

;; Some modifications, see rt-compiler.scm.
!#



