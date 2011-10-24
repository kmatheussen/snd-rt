
(define-stalin (Comb damp1 damp2 feedback bufsize)
  (define filterstore 0.0)
  (Buffer bufsize
	  (Lambda (input old-val)
		  (begin
		    (set! filterstore (+ (* old-val damp2) (* filterstore damp1)))
		    (+ input (* filterstore feedback)))
		  old-val)))

(define-stalin (Allpass feedback bufsize)
  (Buffer bufsize
          (Lambda (input old-val)
             (+ input (* old-val feedback))
             (- old-val input))))

(define-stalin (Mono-freeverb fb1 fb2 damp spread)
  (Seq (Sum (Comb (+ 1116 spread) fb1 damp)
	    (Comb (+ 1188 spread) fb1 damp)
	    (Comb (+ 1277 spread) fb1 damp)
	    (Comb (+ 1356 spread) fb1 damp)
	    (Comb (+ 1422 spread) fb1 damp)
	    (Comb (+ 1491 spread) fb1 damp)
	    (Comb (+ 1557 spread) fb1 damp)
	    (Comb (+ 1617 spread) fb1 damp))
       (Allpass (+ 556 spread) fb2)
       (Allpass (+ 441 spread) fb2)
       (Allpass (+ 341 spread) fb2)
       (Allpass (+ 225 spread) fb2)))

(define-stalin (Stereo-freeverb fb1 fb2 damp spread)
  (Split (Sum Identity Identity)
	 (Mono-freeverb fb1 fb2 damp 0)
	 (Mono-freeverb fb1 fb2 damp spread)))


(define-stalin (Fx-ctrl g w Fx)
  (Sum (Seq (Par (* g)
		 (* g))
	    Fx
	    (Par (* w)
		 (* w)))
       (Par (* (1- w))
	    (* (1- w)))))

(define-stalin (Fx-ctrl clean wet Fx)
  (Par (Sum (* clean)
	    (Seq Fx
		 (* wet)))
       (Sum (* clean)
	    (Seq Fx
		 (* wet)))))

  (Sum (Seq (Par (* g)
		 (* g))
	    Fx
	    (Par (* w)
		 (* w)))
       (Par (* (1- w))
	    (* (1- w)))))

#!
(<rt-stalin>
  (gen-sound
   (Seq (Split (oscil :freq 440)
	       Identity
	       Identity)
	(Fx-ctrl 0.5 0.5 (Stereo-freeverb 2 3 4 5)))))
!#
