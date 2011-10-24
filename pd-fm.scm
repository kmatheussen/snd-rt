
(let ((amp 0.6)
      (mc-ratio 2)
      (index 4)
      (freq 300))
       
  (define fm 
    (make-oscil (* freq mc-ratio)
		:initial-phase (/ 3.14159 2.0)))

  (define carrier 
    (make-oscil freq))

  (define fm_index 
    (* (hz->radians freq) mc-ratio index))

  (define instrument
    (<rt-out> 0 (* amp
		   (oscil carrier
			  (* fm_index
			     (oscil fm))))))
  
  (pd-inlet 0 'mc-ratio
	    (lambda (val)
	      (set! mc-ratio val)))
  
  (pd-inlet 0 'Fm-Frequency
	    (lambda (val) 
	      (set! (mus-frequency fm)
		    (* mc-ratio val))))
	    
  (pd-inlet 0 'Carrier-Frequency
	    (lambda (val) 
	      (set! (mus-frequency carrier)
		    val)))
  
  (pd-inlet 0 'Index
	    (lambda (val) 
	      (set! (-> instrument fm_index)
		    (* (hz->radians freq)
		       mc-ratio val))))
  
  (pd-inlet 0 'Amplitude
	    (lambda (val) 
		(set! (-> instrument amp)
		      val))))

