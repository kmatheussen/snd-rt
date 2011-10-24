

;; Instructions:
;; 1. Start the jack sound server.
;; 2. Start snd.
;; 3. Evaluate (load-from-path "rt-compiler.scm")
;; 4. Evaluate (load-from-path "rt-examples.scm")
;; 5. Browse this file and evaluate commented blocks.
;;
;; In case of stuck sounds, evaluate (rte-silence!)
;; 
;; K.Matheussen 2005.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Oscilator


(definstrument (oscillator start duration)
  (let ((osc (make-oscil))
	(vol 2/3))
    (<rt-out> :dur start duration
	      (* (oscil osc)
		 vol))))

#!
(define i (oscillator 0 10))
(set! (-> i vol) 0.3)
(set! (mus-frequency (-> i osc)) 200)
(-> i stop)
(rte-silence!)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Realtime version of bird.scm.

(load-from-path "bird.scm")

(define bigbird-org bigbird)
(definstrument (bigbird-new start dur frequency freqskew amplitude freq-envelope amp-envelope partials)
  "(bigbird start dur frequency freqskew amplitude freq-envelope amp-envelope partials)"
    
  (let* ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	 (os (make-polyshape frequency :coeffs (partials->polynomial (normalize-partials partials))))
	 (amp-env (make-env amp-envelope amplitude dur))
	 (beg (inexact->exact (round (* (mus-srate) start))))
	 (len (inexact->exact (round (* (mus-srate) dur))))
	 (end (+ beg len)))
    (ws-interrupt?)
    (<rt-out> :dur start dur
	      (* (env amp-env)
		 (polyshape os 1.0 (env gls-env))))))

(define bird-org bird)
(definstrument (bird-new start dur frequency freqskew amplitude freq-envelope amp-envelope)
  "(bird start dur frequency freqskew amplitude freq-envelope amp-envelope)"
  (let* ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	 (os (make-oscil :frequency frequency))
	 (amp-env (make-env amp-envelope amplitude dur))
	 (len (inexact->exact (round (* (mus-srate) dur))))
	 (beg (inexact->exact (round (* (mus-srate) start))))
	 (end (+ beg len)))
    (ws-interrupt?)
    (<rt-out> :dur start dur
	      (* (env amp-env)
		 (oscil os (env gls-env))))))


(define with-sound-org with-sound)
(defmacro with-sound-new (args . body)
  `(begin
     ,@body))
  
(define (make-rt-birds)
  (set! bird bird-new)
  (set! bigbird bigbird-new)
  (set! with-sound with-sound-new)

  (make-birds)
  
  (set! with-sound with-sound-org)
  (set! bird bird-org)
  (set! bigbird bigbird-org))



#!
(make-rt-birds)
(rte-info)
(rte-silence!)
(rte-restart)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI examples

(definstrument (make-osc-gui)
  (letrec* ((osc (make-oscil #:frequency 0))
	    (freq (make-glide-var 440 1))
	    (das-vol 0.4)
	    (vol (make-glide-var das-vol 0.001))
	    (instrument (<rt-out>
			 (* (read-glide-var vol)
			    (oscil osc (hz->radians (read-glide-var freq))))))
	    (exit (lambda ()
		    (-> instrument stop)
		    (-> d hide)))
	    (d (<dialog> "Hard Realtime Common Lisp Music!"  exit
			 "Close" exit
			 "Stop" (<- instrument stop)
			 "Start" (<- instrument play))))
    (<slider> d "Frequency" 50 440 20000 (lambda (val)
					   (write-glide-var freq val))
	      1)
    (<slider> d "Amplitude" 0 das-vol 2.0 (lambda (val) 
					(write-glide-var vol val))
	      1000)
    (-> d show)))


(definstrument (make-fm-gui freq)
  (letrec* ((amp 0.6)
	    (mc-ratio 2)
	    (index 4)
	    
	    (fm (make-oscil (* freq mc-ratio) :initial-phase (/ 3.14159 2.0)))
	    (carrier (make-oscil freq))
	    (fm_index (* (hz->radians freq) mc-ratio index))
	    
	    (instrument (<rt-out> (* amp
				     (oscil carrier (* fm_index
						       (oscil fm))))))
	    (exit (lambda ()
		    (-> instrument stop)
		    (-> d hide)))
	    
	    (d (<dialog> "Hard Realtime Common Lisp Music!"  exit
			 "Close" exit
			 "Stop" (<- instrument stop)
			 "Start" (<- instrument play))))

    (<slider> d "Fm Frequency"
	      2 (mus-frequency fm) 1200
	      (lambda (val) 
		(set! (mus-frequency fm) (* mc-ratio val)))
	      10)

    (<slider> d "Carrier Frequency"
	      50 (mus-frequency carrier) 1200
	      (lambda (val) 
		(set! (mus-frequency carrier) val))
	      10)
    
    (<slider> d "Index"
	      0 (-> instrument fm_index) 50.0
	      (lambda (val) 
		(set! (-> instrument fm_index) (* (hz->radians freq) mc-ratio val)))
	      100)
    
    (<slider> d "Amplitude" 
	      0 (-> instrument amp) 1.0
	      (lambda (val) 
		(set! (-> instrument amp) val))
	      1000)
    
    (-> d show)))


#!
;; gui.scm requires gtk:
;;(c-load-from-path gui)

(make-osc-gui)
(make-fm-gui 200)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fileplayers

(definstrument (play-once filename)
  (let* ((rs (make-readin filename)))
    (<rt-play> (lambda ()
		 (if (>= (mus-location rs) (mus-length rs))
		     (remove-me)
		     (out (readin rs)))))))

(definstrument (loopplay filename)
  (let* ((rs (make-readin filename)))
    (<rt-play> (lambda ()
		 (if (>= (mus-location rs) (mus-length rs))
		     (set! (mus-location rs) 0))
		 (out (readin rs))))))

(definstrument (backandforth-stereo filename pan)
  (let* ((read0 (make-readin filename #:channel 0))
	 (read1 (make-readin filename #:channel 1)))
    (<rt-play> (lambda ()
		 (let ((readfunc (lambda (read)
				   (if (>= (mus-location read) (mus-length read))
				       (set! (mus-increment read) -1)
				       (if (<= (mus-location read) 0)
					   (set! (mus-increment read) 1)))
				   (readin read))))
		   ;; (Very stupid panner)
		   (out (vct (* (readfunc read0)
				(if (< pan 0)
				    1
				    (- 1 pan)))
			     (* (readfunc read1)
				(if (> pan 0)
				    1
				    (+ 1 pan))))))))))


(definstrument (play-once-st filename)
  (let* ((rs-A (make-readin #:file filename #:channel 0))
         (rs-B (make-readin #:file filename #:channel 1)))
    (<rt-play> (lambda ()
                 (if (>= (mus-location rs-A) (mus-length rs-A))
                     (remove-me)
                     (out (vct (readin rs-A)
                               (readin rs-B))))))))


#!
(define filename "/gammelhd/home/kjetil/cemb2.wav")

(play-once filename)

(define p (loopplay filename))
(-> p stop)

(define p (backandforth-stereo filename 0))
(set! (-> p pan) -1)
(set! (-> p pan) 0)
(set! (-> p pan) 1)
(-> p stop)

(define p (play-once-st filename))
(rte-silence!)

!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extremely Simple delay.
;;

(definstrument (extremely-simple-delay latency mix #:key (max-latency-in-seconds 2))
  (let* ((das-vct-length (c-integer (* (rte-samplerate) max-latency-in-seconds)))
	 (das-vct (make-vct das-vct-length))
	 (pos 0))
    (<rt-play> (lambda ()
		 (declare (<int> pos))
		 
		 (let* ((frame-latency (the <int> (* (mus-srate) latency)))
			(write-pos (remainder (+ pos frame-latency)
					      das-vct-length))
			(read-pos pos)
			(next-pos (remainder (1+ pos)
					     das-vct-length))
			(sample (in 0))
			(delayed-sample (vct-ref das-vct read-pos)))
		   
		   (vct-set! das-vct write-pos sample)
		   (out (+ sample (* mix delayed-sample)))
		   (set! pos next-pos))))))

	     
#!

(define bus (make-bus 2))
(loopplay filename #:out-bus bus)
(define p (extremely-simple-delay 1 0.5 #:in-bus bus))

(set! (-> p mix) 1)
(set! (-> p latency) 1.5)
(rte-info)
(rte-silence!)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fm violin.
;;
;; Copied from v.scm.

(definstrument (fm-violin startime dur frequency amplitude #:key
	    (fm-index 1.0)
	    (amp-env '(0 0  25 1  75 1  100 0))
	    (periodic-vibrato-rate 5.0) 
	    (random-vibrato-rate 16.0)
	    (periodic-vibrato-amplitude 0.0025) 
	    (random-vibrato-amplitude 0.005)
	    (noise-amount 0.0) 
	    (noise-freq 1000.0)
	    (ind-noise-freq 10.0) 
	    (ind-noise-amount 0.0)
	    (amp-noise-freq 20.0) 
	    (amp-noise-amount 0.0)
	    (gliss-env '(0 0  100 0)) 
	    (glissando-amount 0.0) 
	    (fm1-env '(0 1  25 .4  75 .6  100 0))  
	    (fm2-env '(0 1  25 .4  75 .6  100 0)) 
	    (fm3-env '(0 1  25 .4  75 .6  100 0))
	    (fm1-rat 1.0) 
	    (fm2-rat 3.0)	 
	    (fm3-rat 4.0)                    
	    (fm1-index #f) 
	    (fm2-index #f) 
	    (fm3-index #f)
	    (degree 0)
	    (distance 1.0)
	    (reverb-amount 0.01)
	    (base 1.0)
	    #:allow-other-keys)

    "(fm-violin startime dur frequency amplitude #:key 
   (fm-index 1.0) (amp-env '(0 0  25 1  75 1  100 0)) 
   (periodic-vibrato-rate 5.0) (random-vibrato-rate 16.0) 
   (periodic-vibrato-amplitude 0.0025) (random-vibrato-amplitude 0.005) 
   (noise-amount 0.0) (noise-freq 1000.0) (ind-noise-freq 10.0) 
   (ind-noise-amount 0.0) (amp-noise-freq 20.0) 
   (amp-noise-amount 0.0) (gliss-env '(0 0  100 0)) 
   (glissando-amount 0.0) (fm1-env '(0 1  25 .4  75 .6  100 0)) 
   (fm2-env '(0 1  25 .4  75 .6  100 0)) (fm3-rat 4.0) 
   (fm3-env '(0 1  25 .4  75 .6  100 0)) (fm1-rat 1.0) 
   (fm2-rat 3.0) (fm1-index #f) (fm2-index #f) 
   (fm3-index #f) (degree 0) (distance 1.0) 
   (reverb-amount 0.01) (base 1.0)) 
This version of the fm-violin assumes it is running within with-sound (where *output* and *reverb* are defined).
  (with-sound () (fm-violin 0 1 440 .1))"

    (let* ((pi 3.141592653589793)
	   (beg (inexact->exact (floor (* startime (mus-srate)))))
	   (len (inexact->exact (floor (* dur (mus-srate)))))
	   (end (+ beg len))
	   (frq-scl (hz->radians frequency))
	   (modulate (not (zero? fm-index)))
	   (maxdev (* frq-scl fm-index))
	   (logfreq (log frequency))
	   (sqrtfreq (sqrt frequency))
	   (index1 (or fm1-index (min pi (* maxdev (/ 5.0 logfreq)))))
	   (index2 (or fm2-index (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001)))))))
	   (index3 (or fm3-index (min pi (* maxdev (/ 4.0 sqrtfreq)))))
	   (easy-case (and (zero? noise-amount)
			   (equal? fm1-env fm2-env)
			   (equal? fm1-env fm3-env)
			   (= fm1-rat (floor fm1-rat))
			   (= fm2-rat (floor fm2-rat))
			   (= fm3-rat (floor fm3-rat))
			   (integer? (inexact->exact (/ fm2-rat fm1-rat))) ; might be 2=2 but 1=3 or whatever
			   (integer? (inexact->exact (/ fm3-rat fm1-rat)))))
	   (norm (or (and easy-case modulate 1.0) index1))
	   (carrier (make-oscil frequency))
	   (poly-fmosc1  (if modulate 
			     (if easy-case 
				 (make-polyshape :frequency (* fm1-rat frequency) 
						 :coeffs (partials->polynomial (list (inexact->exact fm1-rat) index1
										     (inexact->exact (floor (/ fm2-rat fm1-rat))) index2
										     (inexact->exact (floor (/ fm3-rat fm1-rat))) index3)))
				 (make-polyshape))
			     (make-polyshape)))
	   (fmosc1  (if modulate 
			(if easy-case 
			    #f
			    (make-oscil (* fm1-rat frequency)))
			#f))
	   (fmosc2  (and modulate (or easy-case (make-oscil (* fm2-rat frequency)))))
	   (fmosc3  (and modulate (or easy-case (make-oscil (* fm3-rat frequency)))))
	   (ampf  (make-env amp-env :scaler amplitude :base base :duration dur))
	   (indf1 (and modulate (make-env fm1-env norm :duration dur)))
	   (indf2 (and modulate (or easy-case (make-env fm2-env index2 :duration dur))))
	   (indf3 (and modulate (or easy-case (make-env fm3-env index3 :duration dur))))
	   (frqf (make-env gliss-env (* glissando-amount frq-scl) :duration dur))
	   (pervib (make-triangle-wave periodic-vibrato-rate (* periodic-vibrato-amplitude frq-scl)))
	   (ranvib (make-rand-interp random-vibrato-rate (* random-vibrato-amplitude frq-scl)))
	   (fm-noi (if (not (= 0.0 noise-amount))
		       (make-rand noise-freq (* pi noise-amount))
		       #f))
	   (ind-noi (if (and (not (= 0.0 ind-noise-amount)) (not (= 0.0 ind-noise-freq)))
			(make-rand-interp ind-noise-freq ind-noise-amount)
			#f))
	   (amp-noi (if (and (not (= 0.0 amp-noise-amount)) (not (= 0.0 amp-noise-freq)))
			(make-rand-interp amp-noise-freq amp-noise-amount)
			#f))
	   (ind-noi? (if ind-noi #t #f))
	   (amp-noi? (if amp-noi #t #f))

	   ;;(locs (make-locsig degree distance reverb-amount *output* *reverb* (mus-channels *output*)))
	   (locs (make-locsig degree distance reverb-amount *output* *reverb* 2))
	   (vib 0.0) 
	   (modulation 0.0)
	   (fuzz 0.0)
	   (ind-fuzz 1.0)
	   (amp-fuzz 1.0))
      
      (ws-interrupt?)

      (if (not ind-noi)
	  (set! ind-noi (make-rand-interp)))
      (if (not amp-noi)
	  (set! amp-noi (make-rand-interp)))
      (if (not fm-noi)
	  (set! fm-noi (make-rand)))
      (if (not (oscil? fmosc1))
	  (set! fmosc1 (make-oscil)))
      (if (not (oscil? fmosc2))
	  (set! fmosc2 (make-oscil)))
      (if (not (oscil? fmosc3))
	  (set! fmosc3 (make-oscil)))
      (if (not (env? indf1))
	  (set! indf1 (make-env '(0 2) )))
      (if (not (env? indf2))
	  (set! indf2 (make-env '(0 2))))
      (if (not (env? indf3))
	  (set! indf3 (make-env '(0 2))))


      (if (or (not easy-case) ind-noi amp-noi (> noise-amount 0.0) (not modulate))
	  (<rt-play> startime dur
		     (lambda ()
		       (if (not (= 0.0 noise-amount))
			   (set! fuzz (rand fm-noi)))
		       (set! vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib)))
		       (if ind-noi? (set! ind-fuzz (+ 1.0 (rand-interp ind-noi))))
		       (if amp-noi? (set! amp-fuzz (+ 1.0 (rand-interp amp-noi))))
		       (if modulate
			   (if easy-case
			       (set! modulation
				     (* (env indf1) 
					(polyshape poly-fmosc1 1.0 vib)))
			       (set! modulation
				     (+ (* (env indf1) (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
					(* (env indf2) (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
					(* (env indf3) (oscil fmosc3 (+ (* fm3-rat vib) fuzz)))))))
		       (locsig locs (* (env ampf) amp-fuzz
				       (oscil carrier (+ vib (* ind-fuzz modulation)))))))
	  (<rt-play> startime dur
		     (lambda () 
		       (let* ((vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib))))
			 (locsig locs (* (env ampf) 
					 (oscil carrier (+ vib (* (env indf1) 
								  (polyshape fmosc1 1.0 vib))))))))))))


#!
(c-for 0 < 60 1
       (lambda (i)
	 (c-display i)
	 (fm-violin i (max 0.5 (random 15))
		    (1+ (random (1+ (* i 20))))
		    (max 0.02 (random .3))
		    :degree (random 90)
		    :noise-amount (* (random 8) (random 0.4))
		    :fm-index (+ i 1))))
(rte-info)
(rte-silence!)
(rte-restart)

!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ladspa

(definstrument (ladspatest)
  (let ((am-pitchshift (make-ladspa "am_pitchshift_1433" "amPitchshift")))
    (<rt-play> (lambda ()
		 (out (ladspa-run am-pitchshift (vct (in 0))))))))

(definstrument (ladspatest-st)
  (let ((am-pitchshift (make-ladspa "mbeq_1197" "mbeq"))
	(am-pitchshift2 (make-ladspa "mbeq_1197" "mbeq")))
    (<rt-play> (lambda ()
		 (out (vct (vct-ref (ladspa-run am-pitchshift
						(vct (in 0)))
				    0)
			   (vct-ref (ladspa-run am-pitchshift2
						(vct (in 1)))
				    0)))))))



#!
(define l (ladspatest))
(ladspa-set! (-> l am-pitchshift) 0 1.5)
(make-ladspa-gui (-> l am-pitchshift))
;;(make-ladspa-gui (-> l am-pitchshift2))
(-> l stop)
(rte-info)
(rte-silence!)
(rte-restart)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shepard tones

(definstrument (risset startpitch endpitch num-oscs loop-duration)
  (define starttime (+ 1 (rte-time))) ;; Ensure all starts simultaniously by scheduling one second into the future.
  (for-each (lambda (n)
	      (let ((osc (make-oscil #:frequency 0))
		    (e-p (make-env `(0 ,startpitch 1 ,endpitch) #:duration loop-duration #:base (expt 2 num-oscs)))
		    (e-a (make-env `(0 0 1 0.25 2 0) #:duration loop-duration #:base 100)) ;; Linear amplitude-change didn't sound very nice.
		    (start-location (* (/ (* (mus-srate) loop-duration)
					  num-oscs)
				       n)))
		(set! (mus-location e-p) start-location)
		(set! (mus-location e-a) start-location)
		(<rt-play-abs> starttime
			       (lambda ()
				 (if (>= (mus-location e-p) (mus-length e-p))
				     (begin
				       (set! (mus-location e-p) 0)
				       (set! (mus-location e-a) 0)))
				 (out (* (env e-a)
					 (oscil osc (hz->radians (env e-p)))))))))
	    (iota num-oscs)))

    
#!
;; When evaluating the three following lines simulatniously, There are some occasional pops. Why?
(risset 50 800 4 50)
(risset 800 50 4 50)
(risset 150 200 4 10)

(rte-silence!)
(rte-restart)
(rte-info)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hyper-simple alsa midi softsynth.
;; (Lack of list-creation functions makes it a bit unelegant and hard to extend...)

(definstrument (midisoftsynth num-voices)
  (let* ((num-playing 0)
	 (freqs (make-vct (1+ num-voices)))
	 (amps (make-vct (1+ num-voices)))
	 (oscs (apply vector (map (lambda (i) (make-oscil #:frequency 0)) (iota (1+ num-voices))))))
    (<rt-play> (lambda ()
		 (receive-midi (lambda (control data1 data2)
				 (set! control (logand #xf0 control))
				 ;;(printf "gakk! %x %x %x\\n" control data1 data2)
				 (if (and (< num-playing num-voices)
					  (= control #x90)
					  (> data2 0))
				     (begin
				       (vct-set! freqs num-playing data1)
				       (vct-set! amps num-playing (/ data2 #x7f))
				       (set! (mus-phase (vector-ref oscs num-playing)) 0)
				       (set! num-playing (1+ num-playing)))
				     (if (or (= control #x80)
					     (and (= control #x90)
						  (= data2 0)))
					 (let ((foundit 0))
					   (range i 0 num-playing
						  (if (and (not foundit)
							   (= (vct-ref freqs i) data1))
						      (begin
							(set! foundit 1)
							(set! num-playing (1- num-playing))))
						  (if foundit
						      (begin
							(set! (mus-phase (vector-ref oscs i))
							      (mus-phase (vector-ref oscs (1+ i))))
							(vct-set! freqs i (vct-ref freqs (1+ i)))
							(vct-set! amps i (vct-ref amps (1+ i)))))))))))
		 (range i 0 num-playing
			(out (* (oscil (vector-ref oscs i)
				       (hz->radians (midi-to-freq (vct-ref freqs i))))
				(vct-ref amps i))))))))


#!
(define i (midisoftsynth 128))

(rte-info)
(rte-silence!)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Another midi sofsynth, using samples. This one is a bit more advanced. Actually, it
;; sounds very nice. However, it uses Guile for reading midi, and Guile is not allways
;; able to keep up (turning off the garbage collector might help, but that is dangerous!),
;; sometimes making the sounds lag.

(definstrument (midisoftsampler filename srcval vol)
  (let* ((read (make-readin filename))
	 (sr (make-src #:srate srcval #:width 5))
	 (attack (make-env `(0 0    0.7 ,(* 1.9 vol)    1.0 ,vol) #:duration 0.1))
	 (release (make-env `(0 ,vol    1 0) #:duration 0.5))
	 (is-attacking #t)
	 (is-releasing #f))
    (<rt-play> (lambda ()
		 (if (>= (mus-location read) (mus-length read))
		     (set! (mus-increment read) -1)
		     (if (<= (mus-location read) 0)
			 (set! (mus-increment read) 1)))
		 (let ((outval (src sr 0.0 (lambda (direction)
					     (readin read)))))
		   (cond ((and is-releasing
			       (not is-attacking))
			  (out (* (env release) outval))
			  (if (>= (mus-location release) (mus-length release))
			      (remove-me)))
			 (is-attacking
			  (out (* (env attack) outval))
			  (if (>= (mus-location attack) (mus-length attack))
			      (set! is-attacking #f)))
			 (else
			  (out (* vol outval)))))))))


(define (start-synth filename middlenote)
  (define synths '())
  (define (play note vol)
    (let ((middlenote (midi-to-freq middlenote))
	  (srcval (midi-to-freq note))) 
      (set! srcval (- srcval middlenote))
      (set! srcval (/ srcval middlenote))
      (set! srcval (1+ srcval))
      (set! synths (cons (list note
			       (midisoftsampler filename srcval (/ vol 256)))
			 synths))))
  (define (stop note)
    (let ((synth (any (lambda (s)
			(if (= note (car s))
			    s
			    #f))
		      synths)))
      (if (not synth)
	  (c-display "Error, unable to remove" note)
	  (begin
	    (set! (-> (cadr synth) is-releasing) #t)
	    (set! synths (delete! synth synths eq?))))))
  (receive-midi (lambda (control data1 data2)
		  (set! control (logand #xf0 control))
		  (cond ((and (= #x90 control)
			      (not (= 0 data2)))
			 (play data1 data2))
			((or (= control #x80)
			     (and (= control #x90)
				  (= data2 0)))
			 (stop data1))))))
  
#!
(define filename "/gammelhd/home/kjetil/flute2.wav")
(start-synth filename 60)
(start-synth filename 68)
(start-synth filename 78)
(start-synth filename 90)

(begin
  (stop-receiving-midi!)
  (rte-silence!))

#!




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Same midisoftsynth as the one above. But now properly
;;; made, reading midi inside the realtime thread.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-rt-vct-struct status
  :is-playing
  :note 1 ;;??
  :is-attacking
  :is-releasing
  :src-val
  :vol)

(define-rt-vector-struct synth
  :status (make-status)
  :read
  :sr
  :attack-env
  :release-env)

(definstrument (midisoftsampler filename middlenote #:key (attack 1.5) (release 0.5) (src-width 5))
  (let* ((num-synths 16)
	 (num-playing 0)
	 (synths (apply vector (map (lambda (n)
				      (make-synth :read (make-rt-readin filename)
						  :sr (make-src #:srate 0 #:width src-width)
						  :attack-env (make-env `(0 0    0.7 1.9    1.0 1.0) #:duration attack)
						  :release-env (make-env `(0 1    1 0) #:duration release)))
				    (iota num-synths)))))
    (<rt-play> (lambda ()
		 (receive-midi (lambda (control data1 data2)
				 (set! control (logand #xf0 control))
				 (if (and (< num-playing num-synths)
					  (= control #x90)
					  (> data2 0))
				     (let loop ((i 0))
				       (let* ((synth (vector-ref synths i))
					      (status (=> synth :status)))
					 (if (not (=> status :is-playing))
					     (begin
					       (set! (mus-location (the  <rt-readin> (=> synth :read))) 0)
					       (mus-reset (=> synth :sr))
					       (mus-reset (=> synth :attack-env))
					       (mus-reset (=> synth :release-env))
					       ;;(set! status :is-playing #t)
					       (set! (=> status :is-playing) #t)
					       (set! (=> status :is-playing) #t)
					       (set! (=> status :is-playing) #t)
					       (set! (=> status :note) data1)
					       (set! (=> status :is-attacking) #t)
					       (set! (=> status :is-releasing) #f)
					       (set! (=> status :src-val) ((lambda (middlenote srcval)
									     (set! srcval (- srcval middlenote))
									     (set! srcval (/ srcval middlenote))
									     (1+ srcval))
									   (midi-to-freq middlenote)
									   (midi-to-freq data1)))
					       (set! (=> status :vol) (/ data2 256))
					       (set! num-playing (1+ num-playing)))
					     (if (< i (1- num-synths))
						 (loop (1+ i))))))
				     (if (or (= control #x80)
					     (and (= control #x90)
						  (= data2 0)))
					 (let loop ((i 0))
					   (let* ((synth (vector-ref synths i))
						  (status (=> synth :status)))
					     (if (and (= (the <int> (=> status :note))
							 data1)
						      (=> status :is-playing)
						      (not (=> status :is-releasing)))
						 (begin
						   (set! (=> status :is-releasing) #t))
						 (if (< i (1- num-synths))
						     (loop (1+ i))))))))))
		 (range i 0 num-synths
			(let* ((synth (vector-ref synths i))
			       (status (=> synth :status)))
			  (if (=> status :is-playing)
			      (let ((read (the <rt-readin> (=> synth :read)))
				    (sr (=> synth :sr))
				    (attack (=> synth :attack-env))
				    (release (=> synth :release-env))
				    (is-attacking (=> status :is-attacking))
				    (is-releasing (=> status :is-releasing))
				    (src-val (=> status :src-val))
				    (vol (=> status :vol)))
				(if (>= (mus-location read) (mus-length read))
				    (set! (mus-increment read) -1)
				    (if (<= (mus-location read) 0)
					(set! (mus-increment read) 1)))
				(let ((outval (src sr src-val (lambda (direction)
								(readin read)))))
				  (cond ((and is-releasing
					      (not is-attacking))
					 (out (* vol (env release) outval))
					 (if (>= (mus-location release) (mus-length release))
					     (begin
					       (set! (=> status :is-playing) #f)
					       (set! num-playing (1- num-playing)))))
					(is-attacking
					 (out (* vol (env attack) outval))
					 (if (>= (mus-location attack) (mus-length attack))
					     (set! (=> status :is-attacking) #f)))
					(else
					 (out (* vol outval)))))))))))))
  

#!
(define filename "/gammelhd/home/kjetil/flute2.wav")
(define filename "/hom/kjetism/Blub_mono16.wav")
(define filename "/home/kjetil/Blub_mono16.wav")
(midisoftsampler filename 60)
(midisoftsampler filename 68)
(midisoftsampler filename 78)
(midisoftsampler filename 90)
(rte-silence!)
(rte-info)
!#


