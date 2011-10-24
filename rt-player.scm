
#!
rt-player.scm
-Kjetil S. Matheussen, 2006

(load-from-path "rt-player.scm")
!#


(provide 'snd-rt-player.scm)


(if (not (provided? 'snd-rt-compiler.scm))
    (load-from-path "rt-compiler.scm"))

(if (not (defined? '*rt-reader-buffer-time*))
    (define-toplevel '*rt-reader-buffer-time* 5)) ;; Number of seconds to buffer.

(if (not (defined? '*rt-use-rt-player*))
    (define-toplevel '*rt-use-rt-player* #t))




;; RB2 (ringbuffer 2)

;; VCT caching. Really necessary because of the large amount of memory data being allocated. The garbage collector can't collect fast enough the huge amount of vct-data that
;; must be obtained when clicking play rapidly. At least, I think thats what happening. The result is something that seems very much like a swapping hell that
;; eventually can cause a freeze of the machine.
(define rb2-buffer '())
(define (rb2-return-buffer v)
  (if (and (not (null? rb2-buffer))
	   (= (vct-length v) (vct-length (car rb2-buffer))))
      (set! rb2-buffer (cons v rb2-buffer))
      (set! rb2-buffer (list v))))	  
(define (rb2-get-buffer size)
  (if (null? rb2-buffer)
      (make-vct size)
      (if (= (vct-length (car rb2-buffer)) size)
	  (let ((ret (car rb2-buffer)))
	    (set! rb2-buffer (cdr rb2-buffer))
	    ret)
	  (begin
	    (set! rb2-buffer '())
	    (make-vct size)))))

(define-rt-vector-struct rb2
  :buffer1
  :buffer2
  :read-pos
  :size
  :curr-ringbuffer-num  ;; Just finished with last position at the second vct.
  :ringbuffer
  :ringbuffer-num)  ;; 0 or 1 (buffer1 or buffer2)

(define old-make-rb2 make-rb2)

(define (make-rb2 ch size reader)
  (let ((rb2 (old-make-rb2 :buffer1 (rb2-get-buffer size) ;0
			   :buffer2 (rb2-get-buffer size) ;1
			   :read-pos (make-var size)      ;2
			   :size (make-var size)          ;3
			   :curr-ringbuffer-num (make-var 1) ;4
			   :ringbuffer (make-ringbuffer 64)  ;5
			   :ringbuffer-num (make-var 0))))   ;6
    ;;(c-display "reading num 0")
    (reader (=> rb2 :buffer1))
    (ringbuffer-get (=> rb2 :ringbuffer)
		    (lambda (ringbuf-num)
		      ;;(c-display "reading num" ch ringbuf-num)
		      (reader (vector-ref rb2 (c-integer ringbuf-num)))
		      (write-var (=> rb2 :ringbuffer-num) ringbuf-num))
		    500)
    rb2))

(define (free-rb2 rb2)
  (ringbuffer-stop (=> rb2 :ringbuffer))
  (rb2-return-buffer (=> rb2 :buffer1))
  (rb2-return-buffer (=> rb2 :buffer2)))


(define rb2-num-dropouts (make-var 0))

(define-rt (read-rb2 rb2 debug-rb)
  (let* ((read-pos-var (the <vct-*> (=> rb2 :read-pos)))
	 (read-pos (read-var read-pos-var))
	 (size (read-var (=> rb2 :size)))
	 (curr-ringbuffer-num (read-var (=> rb2 :curr-ringbuffer-num))))
    (declare (<int> curr-ringbuffer-num read-pos size))
    (if (= read-pos size)
	(let ((ringbuffer (=> rb2 :ringbuffer)))
	  (if (= (read-var (=> rb2 :ringbuffer-num))
		 curr-ringbuffer-num)
	      (begin
		(put-ringbuffer debug-rb 1)
		;;(printf "RT-PLAYER: Error, can not read from disk fast enough.\\n");; Set the variable *rt-reader-buffer-time* higher.\\n"))
		(write-var rb2-num-dropouts (1+ (read-var rb2-num-dropouts)))
		(if (> (read-var rb2-num-dropouts) 3)
		    (begin
		      (put-ringbuffer debug-rb 2)
		      ;;(printf "RT-PLAYER: Unable too read from disk fast enough. Stopping player.\\n")
		      (remove-me))
		    (if (and (< (- (read-var rt-snd-cursorupdate-inc) (read-var rt-snd-cursorupdate-dropout-num)) 4)
			     (> (get-time) (+ (read-var rt-snd-cursorupdate-lasttime) 5)))
			(begin
			  (put-ringbuffer debug-rb 3)
			  ;;(printf "RT-PLAYER: Too much cpu time spent. Stopping player. (2)\\n")
			  (remove-me)))))
	      (if (> (read-var rb2-num-dropouts) 0)
		  (write-var rb2-num-dropouts (1- (read-var rb2-num-dropouts)))))
	  (put-ringbuffer ringbuffer curr-ringbuffer-num)
	  (set! curr-ringbuffer-num (if (= 0 curr-ringbuffer-num) 1 0))
	  (write-var (=> rb2 :curr-ringbuffer-num) curr-ringbuffer-num)
	  (set! read-pos 0)))
    (let ((ret (vct-ref (vector-ref rb2 curr-ringbuffer-num) read-pos)))
      (write-var read-pos-var (1+ read-pos))
      ret)))




;;(define (make-bigint


(define *rt-loop-on-off* (make-var (if *c-islooping* 1 0)))
(-> *loop-on-off-hook* add!
    (lambda (on-off)
      (write-var *rt-loop-on-off* (if on-off 1 0))))


(eval-c (<-> "-I" snd-header-files-path)
	"#include <xen.h>"
        "#include <_sndlib.h>"
	"#include <vct.h>"
	
	"extern mus_float_t protected_next_sample(void *sf)"
	"extern mus_float_t protected_previous_sample(void *sf)"
	
	(public
	 (<void> sample-reader->vct (lambda ((<SCM> scm_reader)
					     (<SCM> scm_v)
					     (<int> direction)
					     (<int> startpos)
					     (<int> num_samples))
				      (<void-*> reader (cast <void-*> (SCM_SMOB_DATA scm_reader)))				      
				      (<vct-*> v (cast <vct-*> (SCM_SMOB_DATA scm_v)))
                                      ;;(fprintf stderr (string "v: %p, v->data: %p, reader: %p, v->length: %d\\n") v v->data reader (cast <int> v->length))
				      (if (|| (> (+ startpos num_samples) v->length)
					      (< startpos 0))
					  (printf (string "sample-reader->vct error. startpos: %d, num_samples: %d, vct-length: %d\\n") startpos num_samples (cast <int> v->length))
					  (if (== 1 direction)
					      (for-each startpos (+ startpos num_samples)
							(lambda (i)
                                                          ;;(fprintf stderr (string "i: %d\\n") i)
							  (set! v->data[i] (protected_next_sample reader))))
					      (for-each startpos (+ startpos num_samples)
							(lambda (i)
							  (set! v->data[i] (protected_previous_sample reader))))))))))

(define-rt-vct-struct rt-controls
  :amp
  :speed
  :expand
  :contrast
  :reverb-scale
  :reverb-length)


(define rt-controls (make-rt-controls))

(define (rt-set-controls! snd)
  (set! (=> rt-controls :amp) (amp-control snd))
  (set! (=> rt-controls :speed) (speed-control snd))
  (set! (=> rt-controls :expand) (expand-control snd))
  (set! (=> rt-controls :contrast) (contrast-control snd))
  (set! (=> rt-controls :reverb-scale) (reverb-control-scale snd))
  (set! (=> rt-controls :reverb-length) (reverb-control-length snd)))



(-> *c-control-hook* add!
    (lambda ()
      (let ((snd (c-selected-sound)))
	(if snd
	    (rt-set-controls! snd)))))

 (define (rt-set-defaults)
  (let ((snd (c-selected-sound)))
    (if snd
	(rt-set-controls! (c-selected-sound))
	(in 20 rt-set-defaults))))
(rt-set-defaults)
			  


#!
(< (abs (- 1.0 (vct-ref rt-control-values rt-control-speed))) 0.0000001)
(vct-ref rt-control-values rt-control-speed)

!#

(define rt-controls-on-off (make-var 0))
(->  *c-controls-on-off-hook* add!
     (lambda ()
       (let ((snd (c-selected-sound)))
	 (if snd
	     (write-var rt-controls-on-off (if (expand-control? snd) 1 0))))))


(add-hook! graph-hook
	     (lambda (snd chn y0 y1)
	       (-> *c-controls-on-off-hook* run)
	       (-> *c-control-hook* run)))
	       

(define (rt-snd-twofloats->int f1 f2)
  (let ((i1 (c-integer f1))
	(i2 (c-integer f2)))
    (+ (ash i1 7) i2)))
(define-rt (rt-snd-twofloats->int f1 f2)
  (let ((i1 (the <int> f1))
	(i2 (the <int> f2)))
    (the <int> (+ (ash i1 7) i2))))

;;(define (int->twofloats i)
;;  (let ((i1 (ash i (- num-bits))))
;;    (vct i1 (logand i (1- (expt 2 num-bits))))))

(define rt-snd-rb2s '())

(define rt-snd-bus (make-bus *rt-num-output-ports*))

(define rt-snd-rt-safety-old (rt-safety))
(set! (rt-safety) 0)


(definstrument (<snd-rt-player> snd start das_end start-pos)
  ;;(c-display start das_end start-pos)
  (define num-channels #f)
  (define end #f)
  (define positions1 (vct 0))
  (define positions2 (vct 0))
  (define direction #f)
  (define rb2s #f)
  (define vcts #f)
  (define size #f)
  (define debug-print-rb (make-ringbuffer 64))
  (define is-running (vct 0))
  (define sound-src-ratio #f)
  (define speed-gens #f)
  (define expand-gens #f)
  (define diskplaytype 0)
  (define masterouttype 1)
  (define (get-playfunc type position-in-queue fromdisk)
    (<rt-play> #:position position-in-queue
	       (lambda ()
		 (declare (<int> fromdisk num-channels end size direction start))
		 (let ((master-out (lambda ()
				     (range i 0 ,*rt-num-output-ports*
					    (out i (in i)))))
		       (diskplay (lambda ()
				   (let* ((volume (=> rt-controls :amp))
					  (speed (/ (abs (=> rt-controls :speed)) sound-src-ratio))
					  (expand (=> rt-controls :expand))
					  (is-speeding (or (not (= 1 sound-src-ratio))
							   (> (abs (- 1.0 speed)) 0.0000001)))
					  (is-expanding (read-var rt-controls-on-off))
					  (dont-read-anymore #f)
					  (removefunc (lambda ()
							(if fromdisk
							    (range i 0 num-channels
								   (let ((rb2 (vector-ref rb2s i)))
								     (ringbuffer-stop (=> rb2 :ringbuffer)))))
							(write-var is-running 0)
							(set! dont-read-anymore #t)
							(remove-me)))
					  (read-position (lambda (i)
							   (declare (<int> i))
							   (the <int> (rt-snd-twofloats->int (vct-ref positions1 i)
											     (vct-ref positions2 i)))))
					  (set-position! (lambda (i val)
							   (declare (<int> i val))
							   (let ((i (the <int> i))
								 (val (the <int> val)))
							     (vct-set! positions1 i (ash val -7))
							     (vct-set! positions2 i (logand val ,(1- (expt 2 7)))))))
					  
					  (changeposition (lambda (i)
							    (declare (<int> i))
							    (let ((i (the <int> i)))
							      (if (= 1 direction)
								  (begin
								    (set-position! i (the <int> (1+ (read-position i))))
								    (if (>= (read-position i) end)
									(if (= 0 (read-var *rt-loop-on-off*))
									    (removefunc)
									    (set-position! i start))))
								  (begin
								    (set-position! i (the <int> (1- (read-position i))))
								    (if (< (read-position i) start)
									(if (= 0 (read-var *rt-loop-on-off*))
									    (removefunc)
									    (set-position! i (the <int> (1- end))))))))))
					  (getdatafunc (lambda (i)
							 (declare (<int> i))
							 (if dont-read-anymore
							     0.0
							     (let ((ret (if fromdisk
									    (read-rb2 (vector-ref rb2s i) debug-print-rb)
									    (vct-ref (vector-ref vcts i) (- (read-position i) start)))))
							       (changeposition i)
							       ret))))
					  (speedfunc (lambda (i)
						       (declare (<int> i))
						       (src (vector-ref speed-gens i)
							    speed
							    (lambda (dir)
							      (getdatafunc i)))))
					  (expandfunc (lambda (i)
							(declare (<int> i))
							(let ((expand-gen (vector-ref expand-gens i)))
							  (setter!-mus-increment/mus_set_increment expand-gen (max 0.1 expand))
							  (granulate expand-gen
								     (lambda (dir)
								       (if is-speeding
									   (speedfunc i)
									   (getdatafunc i))))))))
				     (if (= 1 num-channels)
					 (cond (is-expanding
						(out (* volume (expandfunc 0))))
					       (is-speeding
						(out (* volume (speedfunc 0))))
					       (else
						(out (* volume (getdatafunc 0)))))
					 (cond (is-expanding
						(range i 0 num-channels
						       (out i (* volume (expandfunc i)))))
					       (is-speeding
						(range i 0 num-channels
						       (out i (* volume (speedfunc i)))))
					       (else
						(range i 0 num-channels
						       (out i (* volume (getdatafunc i)))))))))))
		   (if (= type diskplaytype)
		       (diskplay)
		       (master-out))))))

  (ringbuffer-get debug-print-rb
		  (let ((num 0))
		    (lambda (error-type)
		      (cond ((= 1 error-type) (c-display "RT-PLAYER: Error, can not read from disk fast enough. (" num ") (press 'l')") (set! num (1+ num)))
			    ((= 2 error-type) (c-display "RT-PLAYER: Unable too read from disk fast enough. Stopping player. (press 'l')"))
			    ((= 3 error-type) (c-display "RT-PLAYER: Too much cpu time spent. Stopping player. (2) (press 'l')")))))
		  500)
		    
  (if (not snd)
      (get-playfunc masterouttype 'last #f)
      (let ()
	(define (cleanup-func rt)
	  (lambda ()
	    (-> rt stop)
	    (ringbuffer-stop debug-print-rb)
	    (if rb2s
		(for-each (lambda (rb2)
			    (free-rb2 rb2))
			  ;;(ringbuffer-stop (vector-ref rb2 5)))
			  (vector->list rb2s))
		(for-each rb2-return-buffer (vector->list vcts)))))
	(set! num-channels (channels snd))
	(set! end (if das_end das_end (frames snd)))
	(set! positions1 (make-vct num-channels (ash start-pos -7)))
	(set! positions2 (make-vct num-channels (logand start-pos (1- (expt 2 7)))))
	(set! direction (if (>= (speed-control snd) 0) 1 -1))
	(set! rb2s #f)
	(set! vcts #f)
	(set! size (- end start))
	(set! is-running (make-var 1))
	(set! sound-src-ratio (/ (mus-srate) (srate snd)))
	(set! speed-gens (apply vector (append (map (lambda (ch)
						      (make-src #:srate 0.0 #:width (if (not (= 1 sound-src-ratio))
											(max 40 (sinc-width))
											(sinc-width))))
						    (iota num-channels))
					       (list snd))))
	(set! expand-gens (apply vector (map (lambda (ch)
					       (let ((ret (make-granulate  #:length 0.15 #:hop 0.03)))
						 (set! (mus-location ret) 0)
						 ret))
					     (iota num-channels))))
	
	(if (or #f
		(<= (- end start) (* 3 (mus-srate) *rt-reader-buffer-time*)))
	    (begin
	      ;;(c-display "direction" direction)
	      (set! vcts (apply vector (map (lambda (ch)
					      (let ((v (rb2-get-buffer size)))
						(sample-reader->vct (make-sample-reader start snd ch direction) v 1 0 size)
						v))
					    (iota num-channels))))
	      (let ((ret (get-playfunc diskplaytype 'first #f)))
		(-> ret add-method 'cleanup-func (cleanup-func ret))
		ret))
	    (begin
	      (set! rb2s (apply vector (map (lambda (ch)
					      (if (= 1 direction)
						  (let ((reader (make-sample-reader start-pos snd ch direction))
							(position start-pos)
							(size (c-integer (* (mus-srate) (+ (if #t
											       0
											       (+ (random 1.0) ;; To avoid all channels switching buffers at once.
												  (/ num-channels 4)))
											   *rt-reader-buffer-time*)))))
						    (make-rb2 ch size
							      (lambda (avct)
								(let ((bytesleft (- end position)))
								  (if (< bytesleft size)
								      (begin
									(sample-reader->vct reader avct direction 0 bytesleft)
									(set! reader (make-sample-reader start snd ch direction))
									(sample-reader->vct reader avct direction bytesleft (- size bytesleft))
									(set! position (+ start (- size bytesleft))))
								      (begin
									(sample-reader->vct reader avct direction 0 size)
									(set! position (+ position size))))))))
						  (let ((reader (make-sample-reader start-pos snd ch direction))
							(position start-pos)
							(size (* (mus-srate) *rt-reader-buffer-time*)))
						    (make-rb2 ch size
							      (lambda (avct) ;; "position" is placed where to start reading next time. (this was tricky)
								(let ((bytesleft (1+ (- position start))))
								  (if (< bytesleft size)
								      (begin
									(sample-reader->vct reader avct direction 0 bytesleft)
									(set! reader (make-sample-reader (1- end) snd ch direction))
									(sample-reader->vct reader avct direction bytesleft (- size bytesleft))
									(set! position (- (1- end) (- size bytesleft))))
								      (begin
									(sample-reader->vct reader avct direction 0 size)
									(set! position (- position size))))))))))
					    (iota num-channels))))
	      (let ((ret (get-playfunc diskplaytype 'first #t)))
		(-> ret add-method 'cleanup-func (cleanup-func ret))
		ret))))))


(set! (rt-safety) rt-snd-rt-safety-old)

;;(define rt-snd-master-player (<snd-rt-player> #f #f #f #f #:in-bus rt-snd-bus))


(define rt-snd-players '())

(define (rt-snd-play snd start end start-pos)
  (set! start-pos (min (frames snd) start-pos))
  (set! rt-snd-players (cons (<snd-rt-player> snd start end start-pos) ; #:out-bus rt-snd-bus)
			     rt-snd-players)))

(define (rt-snd-stop-playing)
  (for-each (lambda (player)
	      (-> player cleanup-func))
	    rt-snd-players)
  (set! rt-snd-players '())
  ;;(in 1000 gc)
  )
(define (rt-snd-stop player)
  (-> player cleanup-func)
  (set! rt-snd-players (remove (lambda (p) (eq? p player))
			      rt-snd-players))
  ;;(in 1000 gc)
  )

#!
sfdf
!#

(define (rt-snd-get-player)
  (if (null? rt-snd-players)
      #f
      (call-with-current-continuation
       (lambda (return)
	 (for-each (lambda (player)
		     (if (= 1 (read-var (-> player is-running)))
			 (return player)))
		   rt-snd-players)
	 (set! rt-snd-players '())
	 #f))))
      
(define rt-snd-is-playing? rt-snd-get-player)

(define (rt-snd-get-play-pos snd)
  (let ((ret #f))
    (for-each (lambda (player)
		(let* ((channels (-> player num-channels))
		       (player-snd (vector-ref (-> player speed-gens) channels)))
		  (if (= snd player-snd)
		      (if (or (not ret)
			      (= 1 (read-var (-> player is-running))))
			  (set! ret (rt-snd-twofloats->int (vct-ref (-> player positions1) 0)
							   (vct-ref (-> player positions2) 0)))))))
	      rt-snd-players)
    (if ret
	(c-integer ret)
	ret)))

(define rt-snd-cursorupdate-run #t)
(define rt-snd-cursorupdate-interval 50)
(define rt-snd-last-dropouts (-> *rt-engine* num_max_cpu_interrupts))
(define rt-snd-cursorupdate-inc (make-var 0))
(define rt-snd-cursorupdate-dropout-num (make-var 0))
(define rt-snd-cursorupdate-lasttime (make-var (rte-time)))

(define (rt-snd-cursorupdate)
  (let ((player (rt-snd-get-player)))
    (if player
	(begin
	  (if (not (= (if (>= (speed-control) 0) 1 -1)
		      (-> player direction)))
	      (let ((start (-> player start))
		    (end (-> player end))
		    (position (c-integer (rt-snd-twofloats->int (vct-ref (-> player positions1) 0)
								(vct-ref (-> player positions2) 0)))))
		(rt-snd-play (c-selected-sound) start end position)
		(rt-snd-stop player))
	      (begin
		(write-var rt-snd-cursorupdate-inc (1+ (read-var rt-snd-cursorupdate-inc)))
		(if (> (read-var rt-snd-cursorupdate-inc) 100000)
		    (write-var rt-snd-cursorupdate-inc 0))
		(let ((pos (rt-snd-get-play-pos (c-selected-sound))))
		  (if pos
		      (let ((newdropouts (-> *rt-engine* num_max_cpu_interrupts)))
			(if (not (= newdropouts rt-snd-last-dropouts))
			    (begin
			      (c-display "RT-PLAYER: Error, the player engine used too much CPU, so some processing was skipped. (press 'l')")
			      (if (< (- (read-var rt-snd-cursorupdate-inc) (read-var rt-snd-cursorupdate-dropout-num)) 4)
				  (begin
				    (if (> (rte-time) (+ (read-var rt-snd-cursorupdate-lasttime) 5))
					(begin
					  (c-display "RT-PLAYER: Too much cpu time spent. Stopping player. (press 'l')")
					  (rt-snd-stop-playing))))
				  (write-var rt-snd-cursorupdate-lasttime (rte-time)))
			      (write-var rt-snd-cursorupdate-dropout-num (read-var rt-snd-cursorupdate-inc))
			      (set! rt-snd-last-dropouts newdropouts))
			    (write-var rt-snd-cursorupdate-lasttime (rte-time)))
			(set! (cursor) pos)
			(c-show-times (cursor) #f)))))))))
  (if rt-snd-cursorupdate-run
      (in rt-snd-cursorupdate-interval
	  rt-snd-cursorupdate)))

(rt-snd-cursorupdate)






	 
#!
(rt-snd-play 0 317544 492639 492639)

(rt-clear-cache!)

(set! (rt-safety) 0)
(define p (<snd-rt-player> (selected-sound) 20000 #f (* 1 (mus-srate))))
(set! (rt-safety) 1)

(rte-info)
(begin
  (rte-silence!)
  (ringbuffer-stop-all))

(length rt-running-ringbuffers)

(if (>= (vct-ref rt-control-values rt-control-speed) 0.0)  1 0)

(vct-ref rt-control-values rt-control-speed)

(rt-macroexpand '(vector-ref vec 5))
(format #f "~A" '(vector))

(set! (optimization) 6)

(read-var *rt-loop-on-off*)
(read-var rt-controls-on-off)
(write-var rt-controls-on-off (if (expand-control?) 1 0))


(macroexpand '(next-sample reader))

(reverb-control-scale 0)

(vct-ref rt-control-values rt-control-speed)

(define r (make-sample-reader 200000 0 0 1))
(begin r)
(SCM_SMOB_DATA r)
(dosomething r)
(define rt-control-on-off (make-var 0))
;;max buffer-size when buffring everything:
(exact->inexact (/ (* 4 ;; bytes in a float
		      3 ;; factor set for buffring all. Must be bigger than 2.
		      *rt-reader-buffer-time*
		      (mus-srate))
		   1000000))


(string= "abc" "abcd" 0 2 0 2)
(string-drop "aiai.scm" 2)
(string-take "cache.erm" 6)

(define num-bits 8)
(define (twofloats->int f1 f2)
  (let ((i1 (c-integer f1))
	(i2 (c-integer f2)))
    (+ (ash i1 num-bits) i2)))
(twofloats->int 1 6)
(twofloats->int 2.0 9.0)

(logand #b101 #b110)
(1- (expt 2 num-bits))
(begin #b1111)

(define (int->twofloats i)
  (let ((i1 (ash i (- num-bits))))
    (vct i1 (logand i (1- (expt 2 num-bits))))))

(int->twofloats 262)


(ash 500000 23)

(let ((player (car rt-snd-players)))
  (list (rt-snd-twofloats->int (vct-ref (-> player positions1) 0)
			       (vct-ref (-> player positions2) 0))
	(vct-ref (-> player positions1) 0)
	(vct-ref (-> player positions2) 0)))

(logand 51233 (1- (expt 2 7)))

(while #t
  (c-play)
  (c-play)
  (c-play)
  (c-play)
  (c-stop)
  (sleep 1))

!#



