;; adapted from snd-scheme version of
;; CLM implementation of Xenakis' Dynamic Stochastic Synthesis as heard in
;; his GENDY3, S.709, Legende d'Eer, etc.
;; 12/17/03
;; revised 02/07/06
;; Bill Sack wsack@buffalo.edu

;; to do: - load lists for array dynamically and/or through arguments

(letrec*
 ((amp .9) (bits 16) (xmin 1) (xmax 20) (xwig 0) (xstep 1) (ywig 0) (xfb 0)
  ;;make vct to hold x,y breakpoints
  (xy-array (vct 10 0 10 0 10 0 10 0 10 0 10 0 10 0 10 0))
  ;;some explanation of the parameters:
  ;;amp - scales overall amplitude
  ;;bits - the resolution of the wave's amplitude dimension: from 2 -> whatever
  ;;xmin - minimum number of samples between time breakpoints. must be equal to or greater than 1
  ;;xmax - maximum number of samples between time breakpoints.
  ;;xwig - amplitude applied to random walk function in time dimension
  ;;xstep - quantization of freedom in time dimension, in samples. minimum of 1
  ;;ywig - amplitude applied to random walk function in amplitude dimension, in 
  ;;       percent of overall possible amplitude
  ;;xfb - an attempt at an FIR low-pass filter - the old (n + (x * n-1)) trick,
  ;;      not really that useful
  ;;init-array - initial x and y breakpoints for wave. x values must be 
  ;;             integers and 1 or greater, y values between -(2^bits / 2) and (2^bits / 2)
  (y 0) (dx 0) (prev-dx 0) (dy 0)
  (j 0) (m 0) (dt 0) (output 0) 
  (oldy 0) (xdev 0) (ydev 0)
  (b (expt 2 (- bits 1))); because we use signed ints - see (- b) below
  (xy-array-l (inexact->exact (vct-length xy-array)));length of vct - used in modulo below
  (instrument (<rt-play>
	       (lambda ()
		 (if (= dx dt);;when current sample is a breakpoint
		     (begin
		       (set! dx (vct-ref xy-array (modulo m xy-array-l)))
		       (set! y (vct-ref xy-array (+ (modulo m xy-array-l) 1)))
		       (set! prev-dx (vct-ref xy-array (modulo (- m 2) xy-array-l)))
		       (set! dy (- y oldy))
		       (set! oldy y)
		       ;;straight uniform distribution for y
		       (set! ydev (inexact->exact (round (* (- 1.0 (random 2.0)) (* .01 b ywig)))))
		       ;;gaussian distribution for x
		       (set! xdev 
			     (* xstep (inexact->exact (round 
						       (* xwig 
							  (* (sqrt (* -2.0 (log (- 1 (random 1.0)))))
							     (cos (* 6.283185307179586 (random 1.0)))))))))
		       (vct-set! xy-array (modulo m xy-array-l)
				 ;;mirror stuff for x
				 (cond ((or  (< (round xmax) (+ dx xdev))
					     (> (round xmin)(+ dx xdev)))
					(max (min ;;this mirror is attentuated
					      (inexact->exact (round (+ (* xfb prev-dx) (* (- 1  xfb) (+ dx (- xdev))))))
					      (inexact->exact (round xmax))) (inexact->exact (round xmin))))
				       (else (inexact->exact (round (+ (* xfb prev-dx)
								       (* (- 1  xfb) (+ dx xdev))))))))
		       (vct-set! xy-array (+ (modulo m xy-array-l) 1)
				 ;;mirror stuff for y 
				 (cond ((or (< b (+ y ydev)) (> (- b) (+ y ydev)))
					(max (min (+ y (- ydev)) b) (- b)))
				       (else (+ y ydev))))
		       (set! m (+ m 2))
		       (set! dt 0)))
		 (set! dt (+ 1 dt))
		 (set! j (+ j (/ dy dx)));linear interpolation
		 (set! output (/ j b));normalization -1 to 1
		 (out 0 (* amp output))))))
 (pd-inlet 0 'Amplitude
	   (lambda (val) 
	     (set! (-> instrument amp) val)))
 (pd-inlet 0 'xwig
	   (lambda (val) 
	     (set! (-> instrument xwig) val)))
 (pd-inlet 0 'ywig
	   (lambda (val) 
	     (set! (-> instrument ywig) val)))
 (pd-inlet 0 'xstep
	   (lambda (val) 
	     (set! (-> instrument xstep) val)))
 (pd-inlet 0 'xmin
	   (lambda (val) 
	     (set! (-> instrument xmin) val)))
 (pd-inlet 0 'xmax
	   (lambda (val) 
	     (set! (-> instrument xmax) val))))
