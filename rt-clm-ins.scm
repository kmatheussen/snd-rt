;;; CLM Snd/Scheme instruments translated to run in realtime using the realtime extension.
;;;


(use-modules (ice-9 optargs))

(provide 'snd-clm-ins.scm)

(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))
(if (not (provided? 'snd-env.scm)) (load-from-path "env.scm"))


(define *rt-num-channels* 2)


;;; -------- pluck
;;;
;;; The Karplus-Strong algorithm as extended by David Jaffe and Julius Smith -- see 
;;;  Jaffe and Smith, "Extensions of the Karplus-Strong Plucked-String Algorithm"
;;;  CMJ vol 7 no 2 Summer 1983, reprinted in "The Music Machine".
;;;  translated from CLM's pluck.ins

(definstrument (pluck start dur freq amp-1 :key (weighting .5) (lossfact .9))
  "(pluck start dur freq amp weighting lossfact) implements the Jaffe-Smith plucked string physical model. 
'weighting' is the ratio of the once-delayed to the twice-delayed samples.  It defaults to .5=shortest decay. 
Anything other than .5 = longer decay.  Must be between 0 and less than 1.0. 
'lossfact' can be used to shorten decays.  Most useful values are between .8 and 1.0. (pluck 0 1 330 .3 .95 .95)"

  (let ((amp amp-1)) ; make new Guile happy

    (define (getOptimumC S o p)
      (let* ((pa (* (/ 1.0 o) (atan (* S (sin o)) (+ (- 1.0 S) (* S (cos o))))))
	     (tmpInt (inexact->exact (floor (- p pa))))
	     (pc (- p pa tmpInt)))
	(if (< pc .1)
	    (do ()
		((>= pc .1))
	      (set! tmpInt (- tmpInt 1))
	      (set! pc (+ pc 1.0))))
	(list tmpInt (/ (- (sin o) (sin (* o pc))) (sin (+ o (* o pc)))))))
    
    (define (tuneIt f s1)
      (let* ((p (/ (mus-srate) f))	;period as float
	     (s (if (= s1 0.0) 0.5 s1))
	     (o (hz->radians f))
	     (vals (getOptimumC s o p))
	     (T1 (car vals))
	     (C1 (cadr vals))
	     (vals1 (getOptimumC (- 1.0 s) o p))
	     (T2 (car vals1))
	     (C2 (cadr vals1)))
	(if (and (not (= s .5))
		 (< (abs C1) (abs C2)))
	    (list (- 1.0 s) C1 T1)
	    (list s C2 T2))))
    
    (let* ((vals (tuneIt freq weighting))
	   (wt0 (car vals))
	   (c (cadr vals))
	   (dlen (caddr vals))
					;(amp amp-1)
	   (beg (inexact->exact (floor (* start (mus-srate)))))
	   (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	   (lf (if (= lossfact 0.0) 1.0 (min 1.0 lossfact)))
	   (wt (if (= wt0 0.0) 0.5 (min 1.0 wt0)))
	   (tab (make-vct dlen))
	   ;; get initial waveform in "tab" -- here we can introduce 0's to simulate different pick
	   ;; positions, and so on -- see the CMJ article for numerous extensions.  The normal case
	   ;; is to load it with white noise (between -1 and 1).
	   (allp (make-one-zero (* lf (- 1.0 wt)) (* lf wt)))
	   (feedb (make-one-zero c 1.0)) ;or (feedb (make-one-zero 1.0 c))
	   (ctr 0))
      
      (do ((i 0 (1+ i)))
	  ((= i dlen))
	(vct-set! tab i (- 1.0 (random 2.0))))
      (ws-interrupt?)
      (<rt-play> start dur
		 (lambda ()
		   (let ((val (vct-ref tab ctr)))	;current output value
		     (vct-set! tab ctr (* (- 1.0 c) 
					  (one-zero feedb 
						    (one-zero allp val))))
		     (set! ctr (+ ctr 1))
		     (if (>= ctr dlen) (set! ctr 0))
		     (out (* amp val))))))))

#!
(define (between min max) (+ min (random (- max min))))
(c-for 0 < 60 (between 0.1 1.2)
       (lambda (i)
	 (pluck i 5 (between 50 400) 0.5 :weighting 0.3 :lossfact 0.99)))
!#

;;; -------- mlbvoi
;;;
;;; translation from MUS10 of Marc LeBrun's waveshaping voice instrument (using FM here)
;;; this version translated (and simplified slightly) from CLM's mlbvoi.ins

(definstrument (vox beg dur freq amp ampfun freqfun freqscl phonemes formant-amps formant-indices :key (vibscl .1) (deg 0) (pcrev 0))  
  (let ((formants
	 '((I 390 1990 2550)  (E 530 1840 2480)  (AE 660 1720 2410)
	   (UH 520 1190 2390) (A 730 1090 2440)  (OW 570 840 2410)
	   (U 440 1020 2240)  (OO 300 870 2240)  (ER 490 1350 1690)
	   (W 300 610 2200)   (LL 380 880 2575)  (R 420 1300 1600)
	   (Y 300 2200 3065)  (EE 260 3500 3800) (LH 280 1450 1600)
	   (L 300 1300 3000)  (I2 350 2300 3340) (B 200 800 1750)
	   (D 300 1700 2600)  (G 250 1350 2000)  (M 280 900 2200)
	   (N 280 1700 2600)  (NG 280 2300 2750) (P 300 800 1750)
	   (T 200 1700 2600)  (K 350 1350 2000)  (F 175 900 4400)
	   (TH 200 1400 2200) (S 200 1300 2500)  (SH 200 1800 2000)
	   (V 175 1100 2400)  (THE 200 1600 2200)(Z 200 1300 2500)
	   (ZH 175 1800 2000) (ZZ 900 2400 3800) (VV 565 1045 2400))))
	;;formant center frequencies for a male speaker

    (define (find-phoneme phoneme forms)
      (if (eq? phoneme (car (car forms)))
	  (cdr (car forms))
	(find-phoneme phoneme (cdr forms))))

    (define (vox-fun phons which)
      (let ((f1 '())
	    (len (length phons)))
	(do ((i 0 (+ i 2)))
	    ((>= i len))
	  (set! f1 (cons (list-ref phons i) f1))
	  (set! f1 (cons (list-ref (find-phoneme (list-ref phons (+ i 1)) formants) which) f1)))
	(reverse f1)))

  (let* ((start (inexact->exact (floor (* (mus-srate) beg))))
	 (end (+ start (inexact->exact (floor (* (mus-srate) dur)))))
	 (car-os (make-oscil :frequency 0))
	 (fs (length formant-amps))
	 (evens (make-vector fs))
	 (odds (make-vector fs))
	 (amps (make-vct fs))
	 (indices (make-vct fs))
	 (frmfs (make-vector fs))
	 (ampf (make-env ampfun :scaler amp :duration dur))
	 (freqf (make-env freqfun :duration dur :scaler (* freqscl freq) :offset freq))
	 (frq 0.0)
	 (carrier 0.0)
	 (frm 0.0)
	 (frm-int 0)
	 (frm0 0.0)
	 (even-amp 0.0)
	 (odd-amp 0.0)
	 (even-freq 0.0)
	 (odd-freq 0.0)
	 (sum 0.0)
	 (loc (make-locsig deg 1.0 pcrev *output* *reverb* *rt-num-channels*));;(mus-channels *output*)))
	 (per-vib (make-triangle-wave :frequency 6 :amplitude (* freq vibscl)))
	 (ran-vib (make-rand-interp :frequency 20 :amplitude (* freq .5 vibscl))))
    (do ((i 0 (1+ i)))
	((= i fs))
      (let ((amp (list-ref formant-amps i))
	    (index (list-ref formant-indices i)))
	(vector-set! evens i (make-oscil :frequency 0))
	(vector-set! odds i (make-oscil :frequency 0))
	(vct-set! amps i amp)
	(vct-set! indices i index)
	(vector-set! frmfs i (make-env :envelope (vox-fun phonemes i) :duration dur))))
    (<rt-play> beg dur
     (lambda ()
       (declare (<int> frm-int))
	 (set! frq (+ (env freqf) (triangle-wave per-vib) (rand-interp ran-vib)))
	 (set! carrier (oscil car-os (hz->radians frq)))
	 (set! sum 0.0)
	 (do ((k 0 (1+ k))) ((= k fs))
	   (set! frm (env (vector-ref frmfs k)))
	   (set! frm0 (/ frm frq))
	   (set! frm-int (inexact->exact (floor frm0)))
	   (if (even? frm-int)
	       (begin
		 (set! even-freq (hz->radians (* frm-int frq)))
		 (set! odd-freq (hz->radians (* (+ frm-int 1) frq)))
		 (set! odd-amp (- frm0 frm-int))
		 (set! even-amp (- 1.0 odd-amp)))
	       (begin
		 (set! odd-freq (hz->radians (* frm-int frq)))
		 (set! even-freq (hz->radians (* (+ frm-int 1) frq)))
		 (set! even-amp (- frm0 frm-int))
		 (set! odd-amp (- 1.0 even-amp))))
	   (set! sum (+ sum 
			(* (vct-ref amps k) 
			   (+ (* even-amp (oscil (vector-ref evens k) 
						 (+ even-freq (* (vct-ref indices k) carrier))))
			      (* odd-amp (oscil (vector-ref odds k) 
						(+ odd-freq (* (vct-ref indices k) carrier)))))))))
	 (locsig loc (* (env ampf) sum)))))))

#!
(define-macro (either . rest)
  (define i -1)
  (define sel (gensym))
  `(let ((,sel (random ,(length rest))))
     (cond ,@(map (lambda (b)
		    (set! i (1+ i))
		    `((= ,i ,sel)
		      ,b))
		  rest))))

(c-for 0 < 10 (between 0.5 3.2)
       (lambda (i)
	 (either
	  (vox i 2 170 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1
	       '(0 E 25 AE 35 ER 65 ER 75 I 100 UH) '(.8 .15 .05) '(.005 .0125 .025) .05 .1 :deg (between 0 90))
	  (vox i 2 170 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1
	       '(0 E 25 AE 35 ER 65 ER 75 I 100 UH) '(.8 .15 .05) '(.005 .0125 .025) .05 .1 :deg (between 0 90))
	  (vox i 2 300 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1
	       '(0 I 5 OW 10 I 50 AE 100 OO) '(.8 .15 .05) '(.05 .0125 .025) .02 .1 :deg (between 0 90))
	  (vox i 5 600 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1
	       '(0 I 5 OW 10 I 50 AE 100 OO) '(.8 .16 .04) '(.01 .01 .1) .01 .1 :deg (between 0 90))
	  )))
(rte-info)
!#



;;; -------- FOF example

(definstrument (fofins beg dur frq amp vib f0 a0 f1 a1 f2 a2 :key (ae '(0 0 25 1 75 1 100 0)) ve)
  "(fofins beg dur frq amp vib f0 a0 f1 a1 f2 a2 :optional (ampenv '(0 0 25 1 75 1 100 0)) vibenv) produces FOF 
synthesis: (fofins 0 1 270 .2 .001 730 .6 1090 .3 2440 .1)"
    (let* ((two-pi (* 2 pi))
	   (start (inexact->exact (floor (* beg (mus-srate)))))
	   (len (inexact->exact (floor (* dur (mus-srate)))))
	   (end (+ start len))
	   (ampf (make-env :envelope ae :scaler amp :duration dur))
	   (vibf (make-env :envelope (or ve (list 0 1 100 1)) :scaler vib :duration dur))
	   (frq0 (hz->radians f0))
	   (frq1 (hz->radians f1))
	   (frq2 (hz->radians f2))
	   (foflen (if (= (mus-srate) 22050) 100 200))
	   (vibr (make-oscil :frequency 6))
	   (win-freq (/ two-pi foflen))
	   (wt0 (make-wave-train :size foflen :frequency frq))
	   (foftab (mus-data wt0)))
      (do ((i 0 (1+ i)))
	  ((= i foflen))
	(vct-set! foftab i (* (+ (* a0 (sin (* i frq0)))
				 (* a1 (sin (* i frq1)))
				 (* a2 (sin (* i frq2))))
			      .5 (- 1.0 (cos (* i win-freq))))))
      (ws-interrupt?)
      (<rt-play> beg dur
		 (lambda ()
		   (out (* (env ampf) 
			   (wave-train wt0 (* (env vibf) 
					      (oscil vibr)))) )))))

#!
(fofins 0 1 270 .2 .001 730 .6 1090 .3 2440 .1)
!#


;;; FM TRUMPET ---------------------------------------------------
;;; Dexter Morrill's FM-trumpet:
;;; from CMJ feb 77 p51

(definstrument (fm-trumpet startime dur
			   :key (frq1 250.0)
			   (frq2 1500.0)
			   (amp1 0.5)
			   (amp2 0.1)
			   (ampatt1 0.03)
			   (ampdec1 0.35)
			   (ampatt2 0.03)
			   (ampdec2 0.3)
			   (modfrq1 250.0)
			   (modind11 0.0)
			   (modind12 2.66)
			   (modfrq2 250.0)
			   (modind21 0.0)
			   (modind22 1.8)
			   (rvibamp 0.007)
			   (rvibfrq 125.0)
			   (vibamp 0.007)
			   (vibfrq 7.0)
			   (vibatt 0.6)
			   (vibdec 0.2)
			   (frqskw 0.03)
			   (frqatt 0.06)
			   (ampenv1 '(0 0  25 1  75 .9  100 0))
			   (ampenv2 '(0 0  25 1  75 .9  100 0))
			   (indenv1 '(0 0  25 1  75 .9  100 0))
			   (indenv2 '(0 0  25 1  75 .9  100 0))
			   (degree 0.0)
			   (distance 1.0)
			   (reverb-amount 0.005))
  (let* ((beg (inexact->exact (floor (* startime (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*));;(mus-channels *output*)))
	 (per-vib-f (make-env :envelope (stretch-envelope '(0 1  25 .1  75 0  100 0)
							  25 (min (* 100 (/ vibatt dur)) 45)
							  75 (max (* 100 (- 1.0 (/ vibdec dur))) 55))
			      :scaler vibamp :duration dur))
	 (ran-vib (make-rand-interp :frequency rvibfrq :amplitude rvibamp))
	 (per-vib (make-oscil :frequency vibfrq))
	 (dec-01 (max 75 (* 100 (- 1.0 (/ .01 dur)))))
	 (frq-f (make-env :envelope (stretch-envelope '(0 0  25 1  75 1  100 0)
						      25 (min 25 (* 100 (/ frqatt dur)))
						      75 dec-01)
			  :scaler frqskw :duration dur))
	 (ampattpt1 (min 25 (* 100 (/ ampatt1 dur))))
	 (ampdecpt1 (max 75 (* 100 (- 1.0 (/ ampdec1 dur)))))
	 (ampattpt2 (min 25 (* 100 (/ ampatt2 dur))))
	 (ampdecpt2 (max 75 (* 100 (- 1.0 (/ ampdec2 dur)))))
	 
	 (mod1-f (make-env :envelope (stretch-envelope indenv1 25 ampattpt1 75 dec-01)
			   :scaler (* modfrq1 (- modind12 modind11)) :duration dur))
	 (mod1 (make-oscil :frequency 0.0))
	 (car1 (make-oscil :frequency 0.0))
	 ;; set frequency to zero here because it is handled multiplicatively below
	 (car1-f (make-env :envelope (stretch-envelope ampenv1 25 ampattpt1 75 ampdecpt1)
			   :scaler amp1 :duration dur))
	 
	 (mod2-f (make-env :envelope (stretch-envelope indenv2 25 ampattpt2 75 dec-01)
			   :scaler (* modfrq2 (- modind22 modind21)) :duration dur))
	 (mod2 (make-oscil :frequency 0.0))
	 (car2 (make-oscil :frequency 0.0))
	 (car2-f (make-env :envelope (stretch-envelope ampenv2 25 ampattpt2 75 ampdecpt2)
			   :scaler amp2 :duration dur)))
    (ws-interrupt?)
    (<rt-play> startime dur
     (lambda ()
	 (let ((frq-change (hz->radians (* (+ 1.0 (rand-interp ran-vib))
					   (+ 1.0 (* (env per-vib-f) (oscil per-vib)))
					   (+ 1.0 (env frq-f))))))
	   (locsig loc (+ (* (env car1-f) 
			     (oscil car1 (* frq-change 
					    (+ frq1 (* (env mod1-f) 
						       (oscil mod1 (* modfrq1 frq-change)))))))
			  (* (env car2-f) 
			     (oscil car2 (* frq-change 
					    (+ frq2 (* (env mod2-f) 
						       (oscil mod2 (* modfrq2 frq-change))))))))))))))

#!
(fm-trumpet 0 2)
!#


;;; -------- PQWVOX
;;; translation of CLM pqwvox.ins (itself translated from MUS10 of MLB's waveshaping voice instrument (using phase quadrature waveshaping))

(definstrument (pqw-vox beg dur freq spacing-freq amp ampfun freqfun freqscl phonemes formant-amps formant-shapes)
  "(pqw-vox beg dur freq spacing-freq amp ampfun freqfun freqscl phonemes formant-amps formant-shapes) produces 
vocal sounds using phase quadrature waveshaping"

  (define formants
    '((I 390 1990 2550)  (E 530 1840 2480)  (AE 660 1720 2410)
      (UH 520 1190 2390) (A 730 1090 2440)  (OW 570 840 2410)
      (U 440 1020 2240)  (OO 300 870 2240)  (ER 490 1350 1690)
      (W 300 610 2200)   (LL 380 880 2575)  (R 420 1300 1600)
      (Y 300 2200 3065)  (EE 260 3500 3800) (LH 280 1450 1600)
      (L 300 1300 3000)  (I2 350 2300 3340) (B 200 800 1750)
      (D 300 1700 2600)  (G 250 1350 2000)  (M 280 900 2200)
      (N 280 1700 2600)  (NG 280 2300 2750) (P 300 800 1750)
      (T 200 1700 2600)  (K 350 1350 2000)  (F 175 900 4400)
      (TH 200 1400 2200) (S 200 1300 2500)  (SH 200 1800 2000)
      (V 175 1100 2400)  (THE 200 1600 2200)(Z 200 1300 2500)
      (ZH 175 1800 2000) (ZZ 900 2400 3800) (VV 565 1045 2400)))
                   ;;formant center frequencies for a male speaker

  (define (find-phoneme phoneme form)
    (if (eq? (car (car form)) phoneme)
	(cdr (car form))
	(find-phoneme phoneme (cdr form))))

  (define (vox-fun phons which newenv)
    ;; make an envelope from which-th entry of phoneme data referred to by phons
    (if (null? phons)
	newenv
      (vox-fun (cddr phons) which
	       (append newenv
		       (list (car phons)
			     (list-ref (find-phoneme (cadr phons) formants) which))))))
  (let* ((start (inexact->exact (floor (* (mus-srate) beg))))
	 (samps (inexact->exact (floor (* (mus-srate) dur))))
	 (end (+ start samps))
	 (car-sin (make-oscil :frequency 0))
	 (car-cos (make-oscil :frequency 0 :initial-phase (/ pi 2.0)))
	 (frq-ratio (/ spacing-freq freq))
	 (fs (length formant-amps))
	 (sin-evens (make-vector fs))
	 (cos-evens (make-vector fs))
	 (sin-odds (make-vector fs))
	 (cos-odds (make-vector fs))
	 (amps (make-vector fs))
	 (frmfs (make-vector fs))
	 (sin-coeffs (make-vector fs))
	 (cos-coeffs (make-vector fs))
	 (ampf (make-env :envelope ampfun :scaler amp :duration dur))
	 (freqf (make-env :envelope freqfun :duration dur :scaler (* freqscl freq) :offset freq))
	 (per-vib (make-triangle-wave :frequency 6.0 :amplitude (* freq .1)))
	 (ran-vib (make-rand-interp :frequency 20.0 :amplitude (* freq .05))))
    (do ((i 0 (1+ i)))
	((= i fs))
      (let* ((amp (list-ref formant-amps i))
	     (fshape (list-ref formant-shapes i))
	     (shape (normalize-partials fshape)))
	(vector-set! sin-evens i (make-oscil :frequency 0))
	(vector-set! sin-odds i (make-oscil :frequency 0))
	(vector-set! cos-evens i (make-oscil :frequency 0 :initial-phase (/ pi 2.0)))
	(vector-set! cos-odds i (make-oscil :frequency 0 :initial-phase (/ pi 2.0)))
	(vector-set! amps i amp)
	(vector-set! cos-coeffs i (partials->polynomial shape mus-chebyshev-first-kind))
	(vector-set! sin-coeffs i (partials->polynomial shape mus-chebyshev-second-kind))
	(vector-set! frmfs i (make-env :envelope (vox-fun phonemes i '()) :duration dur))))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (let* ((frq (+ (env freqf) (triangle-wave per-vib) (rand-interp ran-vib)))
		(frqscl (hz->radians (* frq frq-ratio)))
		(carsin (oscil car-sin frqscl))
		(carcos (oscil car-cos frqscl))
		(even-amp 0.0)
		(odd-amp 0.0)
		(even-freq 0.0)
		(odd-freq 0.0)
		(sum 0.0))
	   (do ((k 0 (1+ k)))
	       ((= k fs))
	     (let* ((frm (env (vector-ref frmfs k)))
		    (frm0 (/ frm frq))
		    (frm-int (inexact->exact (floor frm0))))
	       (if (even? frm-int)
		   (begin
		     (set! even-freq (hz->radians (* frm-int frq)))
		     (set! odd-freq (hz->radians (* (+ frm-int 1) frq)))
		     (set! odd-amp (- frm0 frm-int))
		     (set! even-amp (- 1.0 odd-amp)))
		   (begin
		     (set! odd-freq (hz->radians (* frm-int frq)))
		     (set! even-freq (hz->radians (* (+ frm-int 1) frq)))
		     (set! even-amp (- frm0 frm-int))
		     (set! odd-amp (- 1.0 even-amp))))
	       (let* ((fax (polynomial (vector-ref cos-coeffs k) carcos))
		      (yfax (* carsin (polynomial (vector-ref sin-coeffs k) carcos))))
		 (set! sum (+ sum (* (vector-ref amps k)
				     (+ (* even-amp (- (* yfax (oscil (vector-ref sin-evens k) even-freq))
						       (* fax (oscil (vector-ref cos-evens k) even-freq))))
					(* odd-amp (- (* yfax (oscil (vector-ref sin-odds k) odd-freq))
						      (* fax (oscil (vector-ref cos-odds k) odd-freq)))))))))))
	   (out (* (env ampf) sum)))))))

#!
(pqw-vox 0 1 300 300 .1 '(0 0 50 1 100 0) '(0 0 100 0) 0 '(0 L 100 L) '(.33 .33 .33) '((1 1 2 .5) (1 .5 2 .5 3 1) (1 1 4 .5)))
(a test to see if the cancellation is working -- sounds like a mosquito)

(pqw-vox 0 2 200 200 .1 '(0 0 50 1 100 0) '(0 0 100 1) .1 '(0 UH 100 ER) '(.8 .15 .05) '((1 1 2 .5) (1 1 2 .5 3 .2 4 .1) (1 1 3 .1 4 .5)))
(pqw-vox 0 2 100 314 .1 '(0 0 50 1 100 0) '(0 0 100 1) .1 '(0 UH 100 ER) '(.8 .15 .05) '((1 1 2 .5) (1 1 2 .5 3 .2 4 .1) (1 1 3 .1 4 .5)))
(pqw-vox 0 2 200 314 .1 '(0 0 50 1 100 0) '(0 0 100 1) .01 '(0 UH 100 ER) '(.8 .15 .05) '((1 1 2 .5) (1 1 4 .1) (1 1 2 .1 4 .05)))
(pqw-vox 0 2 100 414 .2 '(0 0 50 1 100 0) '(0 0 100 1) .01 '(0 OW 50 E 100 ER) '(.8 .15 .05) '((1 1 2 .5 3 .1 4 .01) (1 1 4 .1) (1 1 2 .1 4 .05)))
!#



;;; -------- STEREO-FLUTE
(definstrument (stereo-flute start dur freq flow 
			     :key
			     (flow-envelope '(0  1 100 1))
			     (decay 0.01) 		; additional time for instrument to decay
			     (noise 0.0356) 
			     (embouchure-size 0.5)
			     (fbk-scl1 0.5)		; these two are crucial for good results
			     (fbk-scl2 0.55)
			     (offset-pos 0.764264) ; from 0.0 to 1.0 along the bore
			     (out-scl 1.0)
			     (a0 0.7) (b1 -0.3)	 ; filter coefficients
			     (vib-rate 5) (vib-amount 0.03)
			     (ran-rate 5) (ran-amount 0.03))
  "(stereo-flute dur freq flow 
     :key (flow-envelope '(0  1 100 1)) (decay 0.01)
	   (noise 0.0356) (embouchure-size 0.5) (fbk-scl1 0.5)
	   (fbk-scl2 0.55) (offset-pos 0.764264) (out-scl 1.0)
	   (a0 0.7) (b1 -0.3) (vib-rate 5) (vib-amount 0.03)
           (ran-rate 5) (ran-amount 0.03))
is a physical model of a flute:
  (stereo-flute 0 1 440 .55 :flow-envelope '(0 0 1 1 2 1 3 0))"

  (let* ((current-excitation 0.0)
	 (current-difference 0.0)
	 (current-flow 0.0)
	 (out-sig 0.0)
	 (tap-sig 0.0)
	 (previous-out-sig 0.0)
	 (previous-tap-sig 0.0)
	 (dc-blocked-a 0.0)
	 (dc-blocked-b 0.0)
	 (previous-dc-blocked-a 0.0)
	 (previous-dc-blocked-b 0.0) 
	 (delay-sig 0.0)
	 (emb-sig 0.0)
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (len (inexact->exact (floor (* dur (mus-srate)))))
	 (end (+ beg len))
	 (chns *rt-num-channels*);;(mus-channels *output*))
	 (flowf (make-env :envelope flow-envelope 
			  :scaler flow 
			  :duration (- dur decay)))
	 (periodic-vibrato (make-oscil :frequency vib-rate))
	 (random-vibrato (make-rand-interp :frequency ran-rate))
	 (breath (make-rand :frequency (/ (mus-srate) 2) :amplitude 1))
	 (period-samples (inexact->exact (floor (/ (mus-srate) freq))))
	 (embouchure-samples (inexact->exact (floor (* embouchure-size period-samples))))
	 (embouchure (make-delay embouchure-samples :initial-element 0.0))
	 (bore (make-delay period-samples))
	 (offset (inexact->exact (floor (* period-samples offset-pos))))
	 (reflection-lowpass-filter (make-one-pole a0 b1)))
    (ws-interrupt?)
    (<rt-play> start dur
      (lambda ()
	 (set! delay-sig (delay bore out-sig))
	 (set! emb-sig (delay embouchure current-difference))
	 (set! current-flow (+ (* vib-amount (oscil periodic-vibrato)) 
			       (* ran-amount (rand-interp random-vibrato)) 
			       (env flowf)))
	 (set! current-difference 
	       (+  (+ current-flow (* noise (* current-flow (rand breath))))
		   (* fbk-scl1 delay-sig)))
	 ;(set! current-excitation (cubic-polynomial emb-sig))
	 (set! current-excitation (- emb-sig (* emb-sig emb-sig emb-sig)))
	 (set! out-sig (one-pole reflection-lowpass-filter 
				 (+ current-excitation (* fbk-scl2 delay-sig))))
	 (set! tap-sig (tap bore offset))
	 ;; NB the DC blocker is not in the cicuit. It is applied to the out-sig 
	 ;; but the result is not fed back into the system.
	 (set! dc-blocked-a (+ (- out-sig previous-out-sig) (* 0.995 previous-dc-blocked-a)))
	 (set! dc-blocked-b (+ (- tap-sig previous-tap-sig) (* 0.995 previous-dc-blocked-b)))
	 (out 0 (* out-scl dc-blocked-a))
	 (if (> chns 1)
	     (out 1 (* out-scl dc-blocked-b) *output*))
	 (set! previous-out-sig out-sig)
	 (set! previous-dc-blocked-a dc-blocked-a)
	 (set! previous-tap-sig tap-sig)
	 (set! previous-dc-blocked-b dc-blocked-b)))))

#!
(stereo-flute 0 1 440 .55 :flow-envelope '(0 0 1 1 2 1 3 0))
!#

	  
;;; -------- FM-BELL
(definstrument (fm-bell startime dur frequency amplitude :key amp-env index-env index)
  "(fm-bell startime dur frequency amplitude :optional amp-env index-env index) mixes in one fm bell note"
  (let* ((beg (inexact->exact (floor (* startime (mus-srate)))))
	 (len (inexact->exact (floor (* dur (mus-srate)))))
	 (end (+ beg len))
	 (fmInd1 (hz->radians (* 32.0 frequency)))
	 (fmInd2 (hz->radians (* 4.0 (- 8.0 (/ frequency 50.0)))))
	 (fmInd3 (* fmInd2 0.705 (- 1.4 (/ frequency 250.0))))  
	 (fmInd4 (hz->radians (* 32.0 (- 20 (/ frequency 20)))))
	 (mod1 (make-oscil :frequency (* frequency 2)))
	 (mod2 (make-oscil :frequency (* frequency 1.41)))
	 (mod3 (make-oscil :frequency (* frequency 2.82)))
	 (mod4 (make-oscil :frequency (* frequency 2.4)))
	 (car1 (make-oscil :frequency frequency))
	 (car2 (make-oscil :frequency frequency))
	 (car3 (make-oscil :frequency (* frequency 2.4)))
	 (indf (make-env (or index-env 
			     (list 0 1 2 1.1 25 .75 75 .5 100 .2))
			 (or index 1.0) dur))
	 (ampf (make-env (or amp-env 
			     (list 0 0 .1 1 10 .6 25 .3 50 .15 90 .1 100 0))
			 amplitude dur)))
    (ws-interrupt?)
    (<rt-play> startime dur
     (lambda ()
	 (let ((fmenv (env indf)))
	   (out (* (env ampf)
		      (+ (oscil car1 (* fmenv fmInd1 (oscil mod1)))
			 (* .15 (oscil car2 (* fmenv 
					       (+ (* fmInd2 (oscil mod2))
						  (* fmInd3 
						     (oscil mod3))))))
			 (* .15 (oscil car3 (* fmenv 
					       fmInd4 
					       (oscil mod4))))))))))))


#!
(define fbell '(0 1 2 1.1000 25 .7500 75 .5000 100 .2000 ))
(define abell '(0 0 .1000 1 10 .6000 25 .3000 50 .1500 90 .1000 100 0 ))
(fm-bell 0.0 1.0 220.0 .5 abell fbell 1.0)
!#


;;; -------- FM_INSECT
(definstrument (fm-insect startime dur frequency amplitude amp-env 
			  mod-freq mod-skew mod-freq-env mod-index mod-index-env 
			  fm-index fm-ratio
			  :key (degree 45.0)
		     	       (distance 1.0)
		               (reverb-amount 0.005))
  (let* ((beg (inexact->exact (floor (* startime (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*));;(mus-channels *output*)))
	 (carrier (make-oscil :frequency frequency))
	 (fm1-osc (make-oscil :frequency mod-freq))
	 (fm2-osc (make-oscil :frequency (* fm-ratio frequency)))
	 (ampf (make-env :envelope amp-env :scaler amplitude :duration dur))
	 (indf (make-env :envelope mod-index-env :scaler (hz->radians mod-index) :duration dur))
	 (modfrqf (make-env :envelope mod-freq-env :scaler (hz->radians mod-skew) :duration dur))
	 (fm2-amp (hz->radians (* fm-index fm-ratio frequency))))
    (ws-interrupt?)
    (<rt-play> startime dur
     (lambda ()
	 (let* ((garble-in (* (env indf)
			      (oscil fm1-osc (env modfrqf))))
		(garble-out (* fm2-amp (oscil fm2-osc garble-in))))
	   (locsig loc (* (env ampf) 
			  (oscil carrier (+ garble-out garble-in)))))))))

#|
(let ((locust '(0 0 40 1 95 1 100 .5))
      (bug_hi '(0 1 25 .7 75 .78 100 1))
      (amp    '(0 0 25 1 75 .7 100 0)))
  (fm-insect 0      1.699  4142.627  .015 amp 60 -16.707 locust 500.866 bug_hi  .346  .500)
  (fm-insect 0.195   .233  4126.284  .030 amp 60 -12.142 locust 649.490 bug_hi  .407  .500)
  (fm-insect 0.217  2.057  3930.258  .045 amp 60 -3.011  locust 562.087 bug_hi  .591  .500)
  (fm-insect 2.100  1.500   900.627  .06  amp 40 -16.707 locust 300.866 bug_hi  .346  .500)
  (fm-insect 3.000  1.500   900.627  .06  amp 40 -16.707 locust 300.866 bug_hi  .046  .500)
  (fm-insect 3.450  1.500   900.627  .09  amp 40 -16.707 locust 300.866 bug_hi  .006  .500)
  (fm-insect 3.950  1.500   900.627  .12  amp 40 -10.707 locust 300.866 bug_hi  .346  .500)
  (fm-insect 4.300  1.500   900.627  .09  amp 40 -20.707 locust 300.866 bug_hi  .246  .500))

|#


;;; -------- FM-DRUM
;;; Jan Mattox's fm drum:

(definstrument (fm-drum start-time duration frequency amplitude index 
			:key (high #f) (degree 0.0) (distance 1.0) (reverb-amount 0.01))
  (let* ((beg (inexact->exact (floor (* start-time (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* duration (mus-srate))))))
	 ;; many of the following variables were originally passed as arguments
	 (casrat (if high 8.525 3.515))
	 (fmrat (if high 3.414 1.414))
	 (glsfun '(0 0  25 0  75 1  100 1))
	 (glsf (make-env :envelope glsfun :scaler (if high (hz->radians 66) 0.0) :duration duration))
	 (ampfun '(0 0  3 .05  5 .2  7 .8  8 .95  10 1.0  12 .95  20 .3  30 .1  100 0))
	 (atdrpt (* 100 (/ (if high .01 .015) duration)))
	 (ampf (make-env :envelope (stretch-envelope ampfun 
					   10 atdrpt 
					   15 (max (+ atdrpt 1) 
						   (- 100 (* 100 (/ (- duration .2) duration)))))
		  :scaler amplitude :duration duration))
	 (indxfun '(0  0     5  .014  10 .033  15 .061  20 .099  
		    25 .153  30 .228  35 .332  40 .477  
		    45 .681  50 .964  55 .681  60 .478  65 .332  
		    70 .228  75 .153  80 .099  85 .061  
		    90 .033  95 .0141 100 0))
	 (indxpt (- 100 (* 100 (/ (- duration .1) duration))))
	 (divindxf (stretch-envelope indxfun 50 atdrpt 65 indxpt))
	 (indxf (make-env :envelope divindxf :scaler (min (hz->radians (* index fmrat frequency)) pi) :duration duration))
	 (mindxf (make-env :envelope divindxf :scaler (min (hz->radians (* index casrat frequency)) pi) :duration duration))
	 (devf (make-env :envelope (stretch-envelope ampfun 
					   10 atdrpt 
					   90 (max (+ atdrpt 1) 
						   (- 100 (* 100 (/ (- duration .05) duration)))))
			 :scaler (min pi (hz->radians 7000)) :duration duration))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	 (rn (make-rand :frequency 7000 :amplitude 1.0))
	 (carrier (make-oscil :frequency frequency))
	 (fmosc (make-oscil :frequency (* frequency fmrat)))
	 (cascade (make-oscil :frequency (* frequency casrat))))
    (ws-interrupt?)
    (<rt-play> start-time duration
     (lambda ()
	 (let ((gls (env glsf)))
	   (locsig loc (* (env ampf) 
			  (oscil carrier 
				 (+ gls 
				    (* (env indxf)
				       (oscil fmosc 
					      (+ (* gls fmrat)
						 (* (env mindxf) 
						    (oscil cascade 
							   (+ (* gls casrat)
							      (* (env devf) (rand rn)))))))))))))))))

#|
(begin
  (fm-drum 0 1.5 55 .3 5 #f)
  (fm-drum 2 1.5 66 .3 4 #t))
|#


;;; -------- FM-GONG
;;; Paul Weineke's gong.

(definstrument (gong start-time duration frequency amplitude
		     :key (degree 0.0) (distance 1.0) (reverb-amount 0.005))
  (let* ((mfq1 (* frequency 1.16))
	 (mfq2 (* frequency 3.14))
	 (mfq3 (* frequency 1.005))
	 (indx01 (hz->radians (* .01 mfq1)))
	 (indx11 (hz->radians (* .30 mfq1)))
	 (indx02 (hz->radians (* .01 mfq2)))
	 (indx12 (hz->radians (* .38 mfq2)))
	 (indx03 (hz->radians (* .01 mfq3)))
	 (indx13 (hz->radians (* .50 mfq3)))
	 (atpt 5)
	 (atdur (* 100 (/ .002 duration)))
	 (expf '(0 0  3 1  15 .5  27 .25  50 .1  100 0))  
	 (rise '(0 0  15 .3  30 1.0  75 .5  100 0))
	 (fmup '(0 0  75 1.0  98 1.0  100 0))
	 (fmdwn '(0 0  2 1.0  100 0))
	 (ampfun (make-env :envelope (stretch-envelope expf atpt atdur)
			   :scaler amplitude :duration duration))
	 (indxfun1 (make-env :envelope fmup :duration duration
			     :scaler (- indx11 indx01) :offset indx01))
	 (indxfun2 (make-env :envelope fmdwn :duration duration
			     :scaler (- indx12 indx02) :offset indx02))
	 (indxfun3 (make-env :envelope rise :duration duration
			     :scaler (- indx13 indx03) :offset indx03))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	 (carrier (make-oscil :frequency frequency))
	 (mod1 (make-oscil :frequency mfq1))
	 (mod2 (make-oscil :frequency mfq2))
	 (mod3 (make-oscil :frequency mfq3))
	 (beg (inexact->exact (floor (* start-time (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* duration (mus-srate)))))))
    (ws-interrupt?)
    (<rt-play> start-time duration
     (lambda ()
	 (locsig loc (* (env ampfun) 
			  (oscil carrier (+ (* (env indxfun1) (oscil mod1))
					    (* (env indxfun2) (oscil mod2))
					    (* (env indxfun3) (oscil mod3))))))))))

#!
(gong 0 3 261.61 .6))
!#


(definstrument (attract beg dur amp c-1) ;c from 1 to 10 or so
  ;; by James McCartney, from CMJ vol 21 no 3 p 6
  (let* ((st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate))))))
	 (c c-1) (a .2) (b .2) (dt .04)
	 (scale (/ (* .5 amp) c))
	 (x -1.0) (y 0.0) (z 0.0))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (let ((x1 (- x (* dt (+ y z)))))
	   (set! y (+ y (* dt (+ x (* a y)))))
	   (set! z (+ z (* dt (- (+ b (* x z)) (* c z)))))
	   (set! x x1)
	   (out (* scale x)))))))
#!
(c-for 0.9 <= 15 (between 0.1 2)
       (lambda (i)
	 (attract i 2 0.5 i)))
!#


;;; -------- PQW
(definstrument (pqw start dur spacing-freq carrier-freq amplitude ampfun indexfun partials
		    :key (degree 0.0)
			 (distance 1.0)
			 (reverb-amount 0.005))
  ;; phase-quadrature waveshaping used to create asymmetric (i.e. single side-band) spectra.
  ;; The basic idea here is a variant of sin x sin y - cos x cos y = cos (x + y)
  (let* ((normalized-partials (normalize-partials partials))
	 (spacing-cos (make-oscil :frequency spacing-freq :initial-phase (/ pi 2.0)))
	 (spacing-sin (make-oscil :frequency spacing-freq))
	 (carrier-cos (make-oscil :frequency carrier-freq :initial-phase (/ pi 2.0)))
	 (carrier-sin (make-oscil :frequency carrier-freq))
	 (sin-coeffs (partials->polynomial normalized-partials mus-chebyshev-second-kind))
	 (cos-coeffs (partials->polynomial normalized-partials mus-chebyshev-first-kind))
	 (amp-env (make-env :envelope ampfun :scaler amplitude :duration dur))
	 (ind-env (make-env :envelope indexfun :duration dur))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	 (r (/ carrier-freq spacing-freq))
	 (tr (make-triangle-wave :frequency 5 :amplitude (hz->radians (* .005 spacing-freq))))
	 (rn (make-rand-interp :frequency 12 :amplitude (hz->radians (* .005 spacing-freq))))
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate)))))))
    (ws-interrupt?)
    (<rt-play> start dur
     (lambda ()
	 (let* ((vib (+ (triangle-wave tr) (rand-interp rn)))
		(ax (* (min 1.0 (env ind-env)) (oscil spacing-cos vib)))
		(fax (polynomial cos-coeffs ax))
		(yfax (* (oscil spacing-sin vib) (polynomial sin-coeffs ax))))
	   (locsig loc  (* (env amp-env)
			    (- (* (oscil carrier-sin (* vib r)) yfax) 
			       (* (oscil carrier-cos (* vib r)) fax)))))))))

#!
(pqw 0 .5 200 1000 .2 '(0 0 25 1 100 0) '(0 1 100 0) '(2 .1 3 .3 6 .5))
!#
; to see the asymmetric spectrum most clearly, set the index function above to '(0 1 100 1)


;;; taken from Perry Cook's stkv1.tar.Z (Synthesis Toolkit), but I was
;;; in a bit of a hurry and may not have made slavishly accurate translations.
;;; Please let me (bil@ccrma.stanford.edu) know of any serious (non-envelope) errors.

(definstrument (tubebell beg dur freq amp :key (base 32.0))
  ;; from Perry Cook's TubeBell.cpp
  (let* ((osc0 (make-oscil (* freq 0.995)))
	 (osc1 (make-oscil (* freq 1.414 0.995)))
	 (osc2 (make-oscil (* freq 1.005)))
	 (osc3 (make-oscil (* freq 1.414)))
	 (ampenv1 (make-env (list 0 0 .005 1 dur 0) :base base :duration dur))
	 (ampenv2 (make-env (list 0 0 .001 1 dur 0) :base (* 2 base) :duration dur))
	 (ampmod (make-oscil 2.0))
	 (g0 (* .5 amp .707))
	 (g1 .203)
	 (g2 (* .5 amp 1.0))
	 (g3 .144)
	 (st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate)))))))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
       (out (* (+ (* .007 (oscil ampmod)) .993)
		    (+ (* g0 (env ampenv1) (oscil osc0 (* g1 (oscil osc1))))
		       (* g2 (env ampenv2) (oscil osc2 (* g3 (oscil osc3)))))))))))

#!
(tubebell 0 2 200 0.5)
!#


(definstrument (wurley beg dur freq amp)
  ;; from Perry Cook's Wurley.cpp
  (let* ((osc0 (make-oscil freq))
	 (osc1 (make-oscil (* freq 4.0)))
	 (osc2 (make-oscil 510.0))
	 (osc3 (make-oscil 510.0))
	 (ampmod (make-oscil 8.0))
	 (g0 (* .5 amp))
	 (g1 .307)
	 (g2 (* .5 amp .307))
	 (g3 .117)
	 (ampenv (make-env '(0 0 1 1 9 1 10 0) :duration dur))
	 (indenv (make-env (list 0 0 .001 1 .15 0 (max dur .16) 0) :duration dur))
	 (resenv (make-env (list 0 0 .001 1 .25 0 (max dur .26) 0) :duration dur))
	 (st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate)))))))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (out (* (env ampenv)
		    (+ 1.0 (* .007 (oscil ampmod)))
		    (+ (* g0 (oscil osc0 (* g1 (oscil osc1))))
		       (* (env resenv) g2 (oscil osc2 (* g3 (env indenv) (oscil osc3)))))))))))

#!
(wurley 0 2 200 0.5)
!#


(definstrument (rhodey beg dur freq amp :key (base .5))
  ;; from Perry Cook's Rhodey.cpp
  (let* ((osc0 (make-oscil freq))
	 (osc1 (make-oscil (* freq 0.5)))
	 (osc2 (make-oscil freq))
	 (osc3 (make-oscil (* freq 15.0)))
	 (ampenv1 (make-env (list 0 0 .005 1 dur 0) :base base :duration dur))
	 (ampenv2 (make-env (list 0 0 .001 1 dur 0) :base (* base 1.5) :duration dur))
	 (ampenv3 (make-env (list 0 0 .001 1 .25 0 (max dur .26) 0) :base (* base 4) :duration dur))
	 (g0 (* .5 amp))
	 (g1 .535)
	 (g2 (* .5 amp))
	 (g3 .109)
	 (st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate)))))))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (out (+ (* g0 (env ampenv1) (oscil osc0 (* g1 (oscil osc1))))
		  (* g2 (env ampenv2) (oscil osc2 (* (env ampenv3) g3 (oscil osc3))))))))))

#!
(rhodey 0 2 200 0.5)
!#


(definstrument (hammondoid beg dur freq amp)
  ;; from Perry Cook's BeeThree.cpp
  (let* ((osc0 (make-oscil (* freq 0.999)))
	 (osc1 (make-oscil (* freq 1.997)))
	 (osc2 (make-oscil (* freq 3.006)))
	 (osc3 (make-oscil (* freq 6.009)))
	 (ampenv1 (make-env (list 0 0 .005 1 (- dur .008) 1 dur 0) :duration dur))
	 (ampenv2 (make-env (list 0 0 .005 1 dur 0) :duration dur))
	 (g0 (* .25 .75 amp))
	 (g1 (* .25 .75 amp))
	 (g2 (* .5 amp))
	 (g3 (* .5 .75 amp))
	 (st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate)))))))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (out (+ (* (env ampenv1)
		       (+ (* g0 (oscil osc0))
			  (* g1 (oscil osc1))
			  (* g2 (oscil osc2))))
		    (* (env ampenv2) g3 (oscil osc3))))))))


#!
(hammondoid 0 2 200 0.5)
!#


(definstrument (metal beg dur freq amp)
  ;; from Perry Cook's HeavyMtl.cpp
  (let* ((osc0 (make-oscil freq))
	 (osc1 (make-oscil (* freq 4.0 0.999)))
	 (osc2 (make-oscil (* freq 3.0 1.001)))
	 (osc3 (make-oscil (* freq 0.50 1.002)))
	 (ampenv0 (make-env (list 0 0 .001 1 (- dur .002) 1 dur 0) :duration dur))
	 (ampenv1 (make-env (list 0 0 .001 1 (- dur .011) 1 dur 0) :duration dur))
	 (ampenv2 (make-env (list 0 0 .01 1 (- dur .015) 1 dur 0) :duration dur))
	 (ampenv3 (make-env (list 0 0 .03 1 (- dur .040) 1 dur 0) :duration dur))
	 (g0 (* .615 amp))
	 (g1 .202)
	 (g2 .574)
	 (g3 .116)
	 (st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate)))))))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (out (* g0 (env ampenv0) 
		    (oscil osc0 
			   (+ (* g1 (env ampenv1) 
				 (oscil osc1 
					(* g2 (env ampenv2) 
					   (oscil osc2))))
			      (* g3 (env ampenv3) 
				 (oscil osc3))))))))))

#!
(begin
  (metal 0 0.5 200 0.5)
  (metal 0 0.5 333 0.5)
  (metal 0.5 2 225 0.5)
  (metal 0.5 2 375 0.5))

(metal 0 2 225 0.5)

(c-for 0 < 10 (between 0.1 0.3)
       (lambda (i)
	 (metal i 2 (between 100 400) 0.25)))

!#


(definstrument (drone startime dur frequency amp-1 ampfun synth ampat ampdc amtrev deg dis rvibamt rvibfreq)
  (let* ((beg (inexact->exact (floor (* startime (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (waveform (partials->wave synth))
	 (amplitude (* amp-1 .25))
	 (freq (hz->radians frequency))
	 (s (make-table-lookup :frequency frequency :wave waveform))
	 (amp-env (make-env :envelope (stretch-envelope ampfun 25 (* 100 (/ ampat dur)) 75 (- 100 (* 100 (/ ampdc dur))))
			    :scaler amplitude :duration dur))
	 (ran-vib (make-rand :frequency rvibfreq 
			     :amplitude (* rvibamt freq)))
	 (loc (make-locsig deg dis amtrev *output* *reverb* *rt-num-channels*)))
    (ws-interrupt?)
    (<rt-play> startime dur
     (lambda ()
	 (locsig loc (* (env amp-env) (table-lookup s (+ (rand ran-vib)))))))))

#!
?
!#

(definstrument (canter beg dur pitch amp-1 deg dis pcrev ampfun ranfun skewfun
		       skewpc ranpc ranfreq indexfun atdr dcdr
		       ampfun1 indfun1 fmtfun1
		       ampfun2 indfun2 fmtfun2
		       ampfun3 indfun3 fmtfun3
		       ampfun4 indfun4 fmtfun4)
  (let* ((start (inexact->exact (floor (* beg (mus-srate)))))
	 (end (+ start (inexact->exact (floor (* dur (mus-srate))))))
	 (amp (* amp-1 .25))		;pvc's amplitudes in bag.clm are very high (overflows)
	 (rangetop 910.0)
	 (rangebot 400.0)
	 (k (inexact->exact (floor (* 100 (/ (log (/ pitch rangebot)) (log (/ rangetop rangebot)))))))
	 (mfq pitch)
	 (atpt (* 100 (/ atdr dur)))
	 (dcpt (- 100 (* 100 (/ dcdr dur))))
	 (lfmt1 (envelope-interp k fmtfun1))
	 (harm1 (inexact->exact (floor (+ .5 (/ lfmt1 pitch)))))
	 (dev11 (hz->radians (* (envelope-interp k indfun1) mfq)))
	 (dev01 (* dev11 .5))
	 (lamp1 (* (envelope-interp k ampfun1) amp (- 1 (abs (- harm1 (/ lfmt1 pitch))))))
	 (lfmt2 (envelope-interp k fmtfun2))
	 (harm2 (inexact->exact (floor (+ .5 (/ lfmt2 pitch)))))
	 (dev12 (hz->radians (* (envelope-interp k indfun2) mfq)))
	 (dev02 (* dev12 .5))
	 (lamp2 (* (envelope-interp k ampfun2) amp (- 1 (abs (- harm2 (/ lfmt2 pitch))))))
	 (lfmt3 (envelope-interp k fmtfun3))
	 (harm3 (inexact->exact (floor (+ .5 (/ lfmt3 pitch)))))
	 (dev13 (hz->radians (* (envelope-interp k indfun3) mfq)))
	 (dev03 (* dev13 .5))
	 (lamp3 (* (envelope-interp k ampfun3) amp (- 1 (abs (- harm3 (/ lfmt3 pitch))))))
	 (lfmt4 (envelope-interp k fmtfun4))
	 (harm4 (inexact->exact (floor (+ .5 (/ lfmt4 pitch)))))
	 (dev14 (hz->radians (* (envelope-interp k indfun4) mfq)))
	 (dev04 (* dev14 .5))
	 (lamp4 (* (envelope-interp k ampfun4) amp (- 1 (abs (- harm4 (/ lfmt4 pitch))))))
	 (tampfun (make-env :envelope (stretch-envelope ampfun 25 atpt 75 dcpt) :duration dur))
	 (tskwfun (make-env :envelope (stretch-envelope skewfun 25 atpt 75 dcpt) :scaler (hz->radians (* pitch skewpc)) :duration dur))
	 (tranfun (make-env :envelope (stretch-envelope ranfun 25 atpt 75 dcpt) :duration dur))
	 (tidxfun (make-env :envelope (stretch-envelope indexfun 25 atpt 75 dcpt) :duration dur))
	 (modgen (make-oscil :frequency pitch))
	 (gen1 (make-oscil :frequency (* pitch harm1)))
	 (gen2 (make-oscil :frequency (* pitch harm2)))
	 (gen3 (make-oscil :frequency (* pitch harm3)))
	 (gen4 (make-oscil :frequency (* pitch harm4)))
	 (ranvib (make-rand :frequency ranfreq :amplitude (hz->radians (* ranpc pitch))))
	 (loc (make-locsig deg dis pcrev *output* *reverb* *rt-num-channels*)))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (let* ((frqval (+ (env tskwfun) (* (env tranfun) (rand ranvib))))
		(modval (oscil modgen frqval))
		(ampval (env tampfun))
		(indval (env tidxfun)))
	   (locsig loc
		   (+ (* lamp1 ampval (oscil gen1 (* (+ (* (+ dev01 (* indval dev11)) modval) frqval) harm1)))
		      (* lamp2 ampval (oscil gen2 (* (+ (* (+ dev02 (* indval dev12)) modval) frqval) harm2)))
		      (* lamp3 ampval (oscil gen3 (* (+ (* (+ dev03 (* indval dev13)) modval) frqval) harm3)))
		      (* lamp4 ampval (oscil gen4 (* (+ (* (+ dev04 (* indval dev14)) modval) frqval) harm4))))))))))

;;; NREV (the most popular Samson box reverb)

(definstrument (nrev :key (reverb-factor 1.09) (lp-coeff 0.7) (volume-1 1.0) (das-length 256))
  ;; reverb-factor controls the length of the decay -- it should not exceed (/ 1.0 .823)
  ;; lp-coeff controls the strength of the low pass filter inserted in the feedback loop
  ;; output-scale can be used to boost the reverb output
  (define (prime? val)
    (or (= val 2)
	(and (odd? val)
	     (do ((i 3 (+ i 2))
		  (lim (sqrt val)))
		 ((or (= 0 (modulo val i)) (> i lim))
		  (> i lim))))))
  (define (next-prime val)
    (if (prime? val)
	val
	(next-prime (+ val 2))))
       
  (let* ((srscale (/ (mus-srate) 25641))
	 (val 0)
	 (dly-len (list 1433 1601 1867 2053 2251 2399 347 113 37 59 53 43 37 29 19)))
    (do ((i 0 (1+ i)))
	((= i 15))
      (let ((val (inexact->exact (floor (* srscale (list-ref dly-len i))))))
	(if (even? val) (set! val (1+ val)))
	(list-set! dly-len i (next-prime val))))

    (let* ((volume volume-1)
	   (len (+ (mus-srate) das-length)) ;;(mus-length *reverb*)))
	   (comb1 (make-comb (* .822 reverb-factor) (list-ref dly-len 0)))
	   (comb2 (make-comb (* .802 reverb-factor) (list-ref dly-len 1)))
	   (comb3 (make-comb (* .773 reverb-factor) (list-ref dly-len 2)))
	   (comb4 (make-comb (* .753 reverb-factor) (list-ref dly-len 3)))
	   (comb5 (make-comb (* .753 reverb-factor) (list-ref dly-len 4)))
	   (comb6 (make-comb (* .733 reverb-factor) (list-ref dly-len 5)))
	   (low (make-one-pole lp-coeff (- lp-coeff 1.0)))
	   (chan2 (> *rt-num-channels* 1))
	   (chan4 (= *rt-num-channels* 4))
	   (allpass1 (make-all-pass -0.700 0.700 (list-ref dly-len 6)))
	   (allpass2 (make-all-pass -0.700 0.700 (list-ref dly-len 7)))
	   (allpass3 (make-all-pass -0.700 0.700 (list-ref dly-len 8)))
	   (allpass4 (make-all-pass -0.700 0.700 (list-ref dly-len 9))) ; 10 for quad
	   (allpass5 (make-all-pass -0.700 0.700 (list-ref dly-len 11)))
	   (allpass6 (make-all-pass -0.700 0.700 (list-ref dly-len 12)))
	   (allpass7 (make-all-pass -0.700 0.700 (list-ref dly-len 13)))
	   (allpass8 (make-all-pass -0.700 0.700 (list-ref dly-len 14))))
      (ws-interrupt?)
      (<rt-play>
       (lambda ()
	   (let* ((rev (* volume (in 0)))
		  (outrev (all-pass allpass4
				    (one-pole low
					      (all-pass allpass3
							(all-pass allpass2
								  (all-pass allpass1
									    (+ (comb comb1 rev)
									       (comb comb2 rev)
									       (comb comb3 rev)
									       (comb comb4 rev)
									       (comb comb5 rev)
									       (comb comb6 rev)))))))))
	     (out 0 (all-pass allpass5 outrev))
	     (if chan2 (out 1 (all-pass allpass6 outrev)))
	     (if chan4 (out 2 (all-pass allpass7 outrev)))
	     (if chan4 (out 3 (all-pass allpass8 outrev)))))))))

#!
(define bus (make-bus))
(nrev :in-bus bus)
(metal 0 0.1 225 0.2 :out-bus bus)
(metal 0 0.2 425 0.2 :out-bus bus)
(rte-silence!)
!#


(definstrument (reson startime dur pitch amp numformants indxfun skewfun pcskew skewat skewdc
		      vibfreq vibpc ranvibfreq ranvibpc degree distance reverb-amount data)
  ;; data is a list of lists of form '(ampf resonfrq resonamp ampat ampdc dev0 dev1 indxat indxdc)
  (let* ((beg (inexact->exact (floor (* startime (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (carriers (make-vector numformants))
	 (modulator (make-oscil :frequency pitch))
	 (ampfs (make-vector numformants))
	 (indfs (make-vector numformants))
	 (c-rats (make-vector numformants))
	 (frqf (make-env :envelope (stretch-envelope skewfun 25 (* 100 (/ skewat dur)) 75 (- 100 (* 100 (/ skewdc dur))))
			 :scaler (hz->radians (* pcskew pitch)) :duration dur))
	 (totalamp 0.0)
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	 (pervib (make-triangle-wave :frequency vibfreq
				     :amplitude (hz->radians (* vibpc pitch))))
	 (ranvib (make-rand-interp :frequency ranvibfreq
				   :amplitude (hz->radians (* ranvibpc pitch)))))
    ;; initialize the "formant" generators
    (do ((i 0 (1+ i)))
	((= i numformants))
      (set! totalamp (+ totalamp (list-ref (list-ref data i) 2))))
    (do ((i 0 (1+ i)))
	((= i numformants))
      (let* ((frmdat (list-ref data i))
	     (freq (cadr frmdat))
	     (ampf (car frmdat))
	     (rfamp  (list-ref frmdat 2))
	     (ampat (* 100 (/ (list-ref frmdat 3) dur)))
	     (ampdc (- 100 (* 100 (/ (list-ref frmdat 4) dur))))
	     (dev0 (hz->radians (* (list-ref frmdat 5) freq)))
	     (dev1 (hz->radians (* (list-ref frmdat 6) freq)))
	     (indxat (* 100 (/ (list-ref frmdat 7) dur)))
	     (indxdc (- 100 (* 100 (/ (list-ref frmdat 8) dur))))
	     (harm (inexact->exact (round (/ freq pitch))))
	     (rsamp (- 1.0 (abs (- harm (/ freq pitch)))))
	     (cfq (* pitch harm)))
	(if (zero? ampat) (set! ampat 25))
	(if (zero? ampdc) (set! ampdc 75))
	(if (zero? indxat) (set! indxat 25))
	(if (zero? indxdc) (set! indxdc 75))
	(vector-set! indfs i (make-env :envelope (stretch-envelope indxfun 25 indxat 75 indxdc) :duration dur
				       :scaler (- dev1 dev0) :offset dev0))
	(vector-set! ampfs i (make-env :envelope (stretch-envelope ampf 25 ampat 75 ampdc) :duration dur
				       :scaler (* rsamp amp (/ rfamp totalamp))))
	(vector-set! c-rats i harm)
	(vector-set! carriers i (make-oscil :frequency cfq))))
    (ws-interrupt?)
    (<rt-play> startime dur
     (lambda ()
	 (let* ((outsum 0.0)
		(vib (+ (triangle-wave pervib) (rand-interp ranvib) (env frqf)))
		(modsig (oscil modulator vib)))
	   (do ((k 0 (1+ k)))
	       ((= k numformants))
	     (set! outsum (+ outsum
			     (* (env (vector-ref ampfs k))
				(oscil (vector-ref carriers k) 
				       (+ (* vib (vector-ref c-rats k))
					  (* (env (vector-ref indfs k)) modsig)))))))
	   (locsig loc outsum))))))


;;; STK's feedback-fm instrument named CelloN in Sambox-land

(definstrument (cellon beg dur pitch0 amp ampfun betafun 
		       beta0 beta1 betaat betadc ampat ampdc dis pcrev deg
		       pitch1 glissfun glissat glissdc
		       pvibfreq pvibpc pvibfun pvibat pvibdc
		       rvibfreq rvibpc rvibfun)
  (let* ((st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate))))))
	 (pit1 (if (zero? pitch1) pitch0 pitch1))
	 (loc (make-locsig deg dis pcrev *output* *reverb* *rt-num-channels*))
	 (car (make-oscil :frequency pitch0))
	 (low (make-one-zero .5 -.5))
	 (fm 0.0)
	 (fmosc (make-oscil :frequency pitch0))
	 (pvib (make-triangle-wave :frequency pvibfreq :amplitude 1.0))
	 (rvib (make-rand-interp :frequency rvibfreq :amplitude 1.0))
	 (ampap (if (> ampat 0.0) (* 100 (/ ampat dur)) 25))
	 (ampdp (if (> ampdc 0.0) (* 100 (- 1.0 (/ ampdc dur))) 75))
	 (glsap (if (> glissat 0.0) (* 100 (/ glissat dur)) 25))
	 (glsdp (if (> glissdc 0.0) (* 100 (- 1.0 (/ glissdc dur))) 75))
	 (betap (if (> betaat 0.0) (* 100 (/ betaat dur)) 25))
	 (betdp (if (> betadc 0.0) (* 100 (- 1.0 (/ betadc dur))) 75))
	 (pvbap (if (> pvibat 0.0) (* 100 (/ pvibat dur)) 25))
	 (pvbdp (if (> pvibdc 0.0) (* 100 (- 1.0 (/ pvibdc dur))) 75))
	 (pvibenv (make-env :envelope (stretch-envelope (or pvibfun '(0 1 100 1)) 25 pvbap 75 pvbdp) :duration dur
			     :scaler (hz->radians (* pvibpc pitch0))))
	 (rvibenv (make-env :envelope (or rvibfun '(0 1 100 1)) :duration dur
			     :scaler (hz->radians (* rvibpc pitch0))))
	 (glisenv (make-env :envelope (stretch-envelope (or glissfun '(0 0 100 0)) 25 glsap 75 glsdp) :duration dur
			     :scaler (hz->radians (- pit1 pitch0))))
	 (amplenv (make-env :envelope (stretch-envelope ampfun 25 ampap 75 ampdp) :scaler amp :duration dur))
	 (betaenv (make-env :envelope (stretch-envelope betafun 25 betap 75 betdp) :duration dur
			     :scaler (- beta1 beta0) :offset beta0)))
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (let* ((vib (+ (* (env pvibenv) (triangle-wave pvib))
			(* (env rvibenv) (rand-interp rvib))
			(env glisenv))))
	   (set! fm (one-zero low (* (env betaenv) (oscil fmosc (+ fm vib)))))
	   (locsig loc (* (env amplenv) 
			  (oscil car (+ fm vib)))))))))

(definstrument (jl-reverb :key (decay 3.0) (das-length 256))
  (let* ((allpass1 (make-all-pass -0.700 0.700 2111))
	 (allpass2 (make-all-pass -0.700 0.700  673))
	 (allpass3 (make-all-pass -0.700 0.700  223))
	 (comb1 (make-comb 0.742 9601))
	 (comb2 (make-comb 0.733 10007))
	 (comb3 (make-comb 0.715 10799))
	 (comb4 (make-comb 0.697 11597))
	 (chns *rt-num-channels*)
	 (outdel1 (make-delay (inexact->exact (round (* .013 (mus-srate))))))
	 (outdel2 (make-delay (inexact->exact (round (* .011 (mus-srate))))))
	 (comb-sum 0.0)
	 (comb-sum-1 0.0)
	 (comb-sum-2 0.0)
	 (all-sums 0.0)
	 (delA 0.0)
	 (delB 0.0)
	 (decay-dur (* decay (mus-srate)))
	 (len (+ decay-dur das-length)))
    (ws-interrupt?)
    (<rt-play>
     (lambda ()
       (let ((allpass-sum (all-pass allpass3 (all-pass allpass2 (all-pass allpass1 (in 0))))))
	   (set! comb-sum 
		 (+ (comb comb1 allpass-sum)
		    (comb comb2 allpass-sum)
		    (comb comb3 allpass-sum)
		    (comb comb4 allpass-sum)))
	   (out 0 (delay outdel1 comb-sum))
	   (if (> chns 1) (out 1 (delay outdel2 comb-sum))))))))


#!
(define bus (make-bus))
(jl-reverb :in-bus bus)
(metal 0 0.1 225 0.2 :out-bus bus)
(metal 0 0.2 425 0.2 :out-bus bus)
(rte-silence!)
(rte-info)
!#


(definstrument (gran-synth start-time duration audio-freq grain-dur grain-interval amp)
  (let* ((beg (inexact->exact (floor (* start-time (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* duration (mus-srate))))))
	 (grain-env (make-env :envelope '(0 0 25 1 75 1 100 0) :duration grain-dur))
	 (carrier (make-oscil :frequency audio-freq))
	 (grain-size (inexact->exact (ceiling (* (max grain-dur grain-interval) (mus-srate)))))
	 (grains (make-wave-train :size grain-size :frequency (/ 1.0 grain-interval)))
	 (grain (mus-data grains)))
    (do ((i 0 (1+ i)))
	((= i grain-size))
      (vct-set! grain i (* (env grain-env) (oscil carrier))))
    (ws-interrupt?)
    (<rt-play> start-time duration
     (lambda ()
       (out (* amp (wave-train grains)))))))

#!
(gran-synth 0 2 100 .0189 .02 .4)
(gran-synth 0 2 400 .0189 .02 0.4)
!#


(definstrument (touch-tone start telephone-number)
  (let ((touch-tab-1 '(0 697 697 697 770 770 770 852 852 852 941 941 941))
	(touch-tab-2 '(0 1209 1336 1477 1209 1336 1477 1209 1336 1477 1209 1336 1477)))
    (do ((i 0 (1+ i)))
	((= i (length telephone-number)))
      (let* ((k (list-ref telephone-number i))
	     (beg (inexact->exact (floor (* (+ start (* i .4)) (mus-srate)))))
	     (end (inexact->exact (floor (+ beg (* .3 (mus-srate))))))
	     (i (if (number? k)
		    (if (not (= 0 k))
			k 
			11)
		    (if (eq? k '*) 
			10
			12)))
	     (frq1 (make-oscil :frequency (list-ref touch-tab-1 i)))
	     (frq2 (make-oscil :frequency (list-ref touch-tab-2 i))))
	(ws-interrupt?)
	(c-display beg)
	(<rt-play> (/ beg (mus-srate)) (/ (- end beg) (mus-srate))
	 (lambda ()
	   (out (* 0.1 (+ (oscil frq1) (oscil frq2))))))))))

#!
(touch-tone 0.0 '(7 2 3 4 9 7 1))
!#
;;; (with-sound () (touch-tone 0.0 '(7 2 3 4 9 7 1))
;;; I think the dial tone is 350 + 440
;;; http://www.hackfaq.org/telephony/telephone-tone-frequencies.shtml

(definstrument (spectra start-time duration frequency amplitude
			:key (partials '(1 1 2 0.5))
			(amp-envelope '(0 0 50 1 100 0))
			(vibrato-amplitude 0.005)
			(vibrato-speed 5.0)
			(degree 0.0)
			(distance 1.0)
			(reverb-amount 0.005))
  (let* ((beg (inexact->exact (floor (* start-time (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* duration (mus-srate))))))
	 (waveform (partials->wave partials))
	 (freq (hz->radians frequency))
	 (s (make-table-lookup :frequency frequency :wave waveform))
	 (amp-env (make-env :envelope amp-envelope :scaler amplitude :duration duration))
	 (per-vib (make-triangle-wave :frequency vibrato-speed
				      :amplitude (* vibrato-amplitude freq)))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	 (ran-vib (make-rand-interp :frequency (+ vibrato-speed 1.0)
				    :amplitude (* vibrato-amplitude freq))))
    (ws-interrupt?)
    (<rt-play> start-time duration
     (lambda ()
       (locsig loc (* (env amp-env) 
		      (table-lookup s (+ (triangle-wave per-vib)
					 (rand-interp ran-vib)))))))))

#!
(spectra 0 1 440.0 .8 '(1.0 .4 2.0 .2 3.0 .2 4.0 .1 6.0 .1) 
	 '(0.0 0.0 1.0 1.0 5.0 0.9 12.0 0.5 25.0 0.25 100.0 0.0))
!#


;;; interpolate between two waveforms (this could be extended to implement all the various
;;; wavetable-based synthesis techniques).

(definstrument (two-tab start-time duration frequency amplitude
		        :key (partial-1 '(1.0 1.0 2.0 0.5))
			          (partial-2 '(1.0 0.0 3.0 1.0))
			          (amp-envelope '(0 0 50 1 100 0))
			          (interp-func '(0 1 100 0))
			          (vibrato-amplitude 0.005)
			          (vibrato-speed 5.0)
			          (degree 0.0)
			          (distance 1.0)
			          (reverb-amount 0.005))
  (let* ((beg (inexact->exact (floor (* start-time (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* duration (mus-srate))))))
	 (waveform-1 (partials->wave partial-1))
	 (waveform-2 (partials->wave partial-2))
	 (freq (hz->radians frequency))
	 (s-1 (make-table-lookup :frequency frequency :wave waveform-1))
	 (s-2 (make-table-lookup :frequency frequency :wave waveform-2))
	 (amp-env (make-env :envelope amp-envelope :scaler amplitude :duration duration))
	 (interp-env (make-env :envelope interp-func :duration duration))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	 (per-vib (make-triangle-wave :frequency vibrato-speed
				      :amplitude (* vibrato-amplitude freq)))
	 (ran-vib (make-rand-interp :frequency (+ vibrato-speed 1.0)
				    :amplitude (* vibrato-amplitude freq))))
    (ws-interrupt?)
    (<rt-play> start-time duration
     (lambda ()
	 (let ((vib (+ (triangle-wave per-vib) 
		       (rand-interp ran-vib)))
	       (intrp (env interp-env)))
	   (locsig loc (* (env amp-env) 
			    (+ (* intrp (table-lookup s-1 vib))
			       (* (- 1.0 intrp) 
				  (table-lookup s-2 vib))))))))))

#!
(two-tab 0 2 225 0.7)
!#



(definstrument (lbj-piano begin-time duration frequency amplitude :key (pfreq frequency)
			  (degree 45) (reverb-amount 0) (distance 1))
  (let ((piano-spectra (list

               (list  1.97 .0326  2.99 .0086  3.95 .0163  4.97 .0178  5.98 .0177  6.95 .0315  8.02 .0001
		  8.94 .0076  9.96 .0134 10.99 .0284 11.98 .0229 13.02 .0229 13.89 .0010 15.06 .0090 16.00 .0003
		 17.08 .0078 18.16 .0064 19.18 .0129 20.21 .0085 21.27 .0225 22.32 .0061 23.41 .0102 24.48 .0005
		 25.56 .0016 26.64 .0018 27.70 .0113 28.80 .0111 29.91 .0158 31.06 .0093 32.17 .0017 33.32 .0002
		 34.42 .0018 35.59 .0027 36.74 .0055 37.90 .0037 39.06 .0064 40.25 .0033 41.47 .0014 42.53 .0004
		 43.89 .0010 45.12 .0039 46.33 .0039 47.64 .0009 48.88 .0016 50.13 .0006 51.37 .0010 52.70 .0002
		 54.00 .0004 55.30 .0008 56.60 .0025 57.96 .0010 59.30 .0012 60.67 .0011 61.99 .0003 62.86 .0001
		 64.36 .0005 64.86 .0001 66.26 .0004 67.70 .0006 68.94 .0002 70.10 .0001 70.58 .0002 72.01 .0007
		 73.53 .0006 75.00 .0002 77.03 .0005 78.00 .0002 79.57 .0006 81.16 .0005 82.70 .0005 84.22 .0003
		 85.41 .0002 87.46 .0001 90.30 .0001 94.02 .0001 95.26 .0002 109.39 .0003)

               (list  1.98 .0194  2.99 .0210  3.97 .0276  4.96 .0297  5.96 .0158  6.99 .0207  8.01 .0009
		  9.00 .0101 10.00 .0297 11.01 .0289 12.02 .0211 13.04 .0127 14.07 .0061 15.08 .0174 16.13 .0009
		 17.12 .0093 18.16 .0117 19.21 .0122 20.29 .0108 21.30 .0077 22.38 .0132 23.46 .0073 24.14 .0002
		 25.58 .0026 26.69 .0035 27.77 .0053 28.88 .0024 30.08 .0027 31.13 .0075 32.24 .0027 33.36 .0004
		 34.42 .0004 35.64 .0019 36.78 .0037 38.10 .0009 39.11 .0027 40.32 .0010 41.51 .0013 42.66 .0019
		 43.87 .0007 45.13 .0017 46.35 .0019 47.65 .0021 48.89 .0014 50.18 .0023 51.42 .0015 52.73 .0002
		 54.00 .0005 55.34 .0006 56.60 .0010 57.96 .0016 58.86 .0005 59.30 .0004 60.75 .0005 62.22 .0003
		 63.55 .0005 64.82 .0003 66.24 .0003 67.63 .0011 69.09 .0007 70.52 .0004 72.00 .0005 73.50 .0008
		 74.95 .0003 77.13 .0013 78.02 .0002 79.48 .0004 82.59 .0004 84.10 .0003)

               (list  2.00 .0313  2.99 .0109  4.00 .0215  5.00 .0242  5.98 .0355  7.01 .0132  8.01 .0009
		  9.01 .0071 10.00 .0258 11.03 .0221 12.02 .0056 13.06 .0196 14.05 .0160 15.11 .0107 16.11 .0003
		 17.14 .0111 18.21 .0085 19.23 .0010 20.28 .0048 21.31 .0128 22.36 .0051 23.41 .0041 24.05 .0006
		 25.54 .0019 26.62 .0028 27.72 .0034 28.82 .0062 29.89 .0039 30.98 .0058 32.08 .0011 33.21 .0002
		 34.37 .0008 35.46 .0018 36.62 .0036 37.77 .0018 38.92 .0042 40.07 .0037 41.23 .0011 42.67 .0003
		 43.65 .0018 44.68 .0025 45.99 .0044 47.21 .0051 48.40 .0044 49.67 .0005 50.88 .0019 52.15 .0003
		 53.42 .0008 54.69 .0010 55.98 .0005 57.26 .0013 58.53 .0027 59.83 .0011 61.21 .0027 62.54 .0003
		 63.78 .0003 65.20 .0001 66.60 .0006 67.98 .0008 69.37 .0019 70.73 .0007 72.14 .0004 73.62 .0002
		 74.40 .0003 76.52 .0006 77.97 .0002 79.49 .0004 80.77 .0003 81.00 .0001 82.47 .0005 83.97 .0001
		 87.27 .0002)

               (list  2.00 .0257  2.99 .0142  3.97 .0202  4.95 .0148  5.95 .0420  6.95 .0037  7.94 .0004
		  8.94 .0172  9.95 .0191 10.96 .0115 11.97 .0059 12.98 .0140 14.00 .0178 15.03 .0121 16.09 .0002
		 17.07 .0066 18.08 .0033 19.15 .0022 20.18 .0057 21.22 .0077 22.29 .0037 23.33 .0066 24.97 .0002
		 25.49 .0019 26.55 .0042 27.61 .0043 28.73 .0038 29.81 .0084 30.91 .0040 32.03 .0025 33.14 .0005
		 34.26 .0003 35.38 .0019 36.56 .0037 37.68 .0049 38.86 .0036 40.11 .0011 41.28 .0008 42.50 .0004
		 43.60 .0002 44.74 .0022 45.99 .0050 47.20 .0009 48.40 .0036 49.68 .0004 50.92 .0009 52.17 .0005
		 53.46 .0007 54.76 .0006 56.06 .0005 57.34 .0011 58.67 .0005 59.95 .0015 61.37 .0008 62.72 .0004
		 65.42 .0009 66.96 .0003 68.18 .0003 69.78 .0003 71.21 .0004 72.45 .0002 74.22 .0003 75.44 .0001
		 76.53 .0003 78.31 .0004 79.83 .0003 80.16 .0001 81.33 .0003 82.44 .0001 83.17 .0002 84.81 .0003
		 85.97 .0003 89.08 .0001 90.70 .0002 92.30 .0002 95.59 .0002 97.22 .0003 98.86 .0001 108.37 .0001
		 125.54 .0001)

               (list  1.99 .0650  3.03 .0040  4.03 .0059  5.02 .0090  5.97 .0227  6.98 .0050  8.04 .0020
		  9.00 .0082  9.96 .0078 11.01 .0056 12.01 .0095 13.02 .0050 14.04 .0093 15.08 .0064 16.14 .0017
		 17.06 .0020 18.10 .0025 19.14 .0023 20.18 .0015 21.24 .0032 22.29 .0029 23.32 .0014 24.37 .0005
		 25.43 .0030 26.50 .0022 27.60 .0027 28.64 .0024 29.76 .0035 30.81 .0136 31.96 .0025 33.02 .0003
		 34.13 .0005 35.25 .0007 36.40 .0014 37.51 .0020 38.64 .0012 39.80 .0019 40.97 .0004 42.09 .0003
		 43.24 .0003 44.48 .0002 45.65 .0024 46.86 .0005 48.07 .0013 49.27 .0008 50.49 .0006 52.95 .0001
		 54.23 .0005 55.45 .0004 56.73 .0001 58.03 .0003 59.29 .0002 60.59 .0003 62.04 .0002 65.89 .0002
		 67.23 .0002 68.61 .0002 69.97 .0004 71.36 .0005 85.42 .0001)

               (list  1.98 .0256  2.96 .0158  3.95 .0310  4.94 .0411  5.95 .0238  6.94 .0152  7.93 .0011
		  8.95 .0185  9.92 .0166 10.93 .0306 11.94 .0258 12.96 .0202 13.97 .0403 14.95 .0228 15.93 .0005
		 17.01 .0072 18.02 .0034 19.06 .0028 20.08 .0124 21.13 .0137 22.16 .0102 23.19 .0058 23.90 .0013
		 25.30 .0039 26.36 .0039 27.41 .0025 28.47 .0071 29.64 .0031 30.60 .0027 31.71 .0021 32.84 .0003
		 33.82 .0002 35.07 .0019 36.09 .0054 37.20 .0038 38.33 .0024 39.47 .0055 40.55 .0016 41.77 .0006
		 42.95 .0002 43.27 .0018 44.03 .0006 45.25 .0019 46.36 .0033 47.50 .0024 48.87 .0012 50.03 .0016
		 51.09 .0004 53.52 .0017 54.74 .0012 56.17 .0003 57.40 .0011 58.42 .0020 59.70 .0007 61.29 .0008
		 62.56 .0003 63.48 .0002 64.83 .0002 66.12 .0012 67.46 .0017 68.81 .0003 69.13 .0003 70.53 .0002
		 71.84 .0001 73.28 .0002 75.52 .0010 76.96 .0005 77.93 .0003 78.32 .0003 79.73 .0003 81.69 .0002
		 82.52 .0001 84.01 .0001 84.61 .0002 86.88 .0001 88.36 .0002 89.85 .0002 91.35 .0003 92.86 .0002
		 93.40 .0001 105.28 .0002 106.22 .0002 107.45 .0001 108.70 .0003 122.08 .0002)

               (list  1.97 .0264  2.97 .0211  3.98 .0234  4.98 .0307  5.96 .0085  6.94 .0140  7.93 .0005
		  8.96 .0112  9.96 .0209 10.98 .0194 11.98 .0154 12.99 .0274 13.99 .0127 15.01 .0101 15.99 .0002
		 17.04 .0011 18.08 .0032 19.14 .0028 20.12 .0054 21.20 .0053 22.13 .0028 23.22 .0030 24.32 .0006
		 25.24 .0004 26.43 .0028 27.53 .0048 28.52 .0039 29.54 .0047 30.73 .0044 31.82 .0007 32.94 .0008
		 34.04 .0012 35.13 .0018 36.29 .0007 37.35 .0075 38.51 .0045 39.66 .0014 40.90 .0004 41.90 .0002
		 43.08 .0002 44.24 .0017 45.36 .0013 46.68 .0020 47.79 .0015 48.98 .0010 50.21 .0012 51.34 .0001
		 53.82 .0003 55.09 .0004 56.23 .0005 57.53 .0004 58.79 .0005 59.30 .0002 60.03 .0002 61.40 .0003
		 62.84 .0001 66.64 .0001 67.97 .0001 69.33 .0001 70.68 .0001 73.57 .0002 75.76 .0002 76.45 .0001
		 79.27 .0001 80.44 .0002 81.87 .0002)

               (list  2.00 .0311  2.99 .0086  3.99 .0266  4.97 .0123  5.98 .0235  6.97 .0161  7.97 .0008
		  8.96 .0088  9.96 .0621 10.99 .0080 11.99 .0034 12.99 .0300 14.03 .0228 15.04 .0105 16.03 .0004
		 17.06 .0036 18.09 .0094 18.95 .0009 20.17 .0071 21.21 .0161 22.25 .0106 23.28 .0104 24.33 .0008
		 25.38 .0030 26.46 .0035 27.50 .0026 28.59 .0028 29.66 .0128 30.75 .0139 31.81 .0038 32.93 .0006
		 34.04 .0004 35.16 .0005 36.25 .0023 37.35 .0012 38.46 .0021 39.59 .0035 40.71 .0006 41.86 .0007
		 42.42 .0001 43.46 .0003 44.17 .0032 45.29 .0013 46.57 .0004 47.72 .0011 48.79 .0005 50.11 .0005
		 51.29 .0003 52.47 .0002 53.68 .0004 55.02 .0005 56.18 .0003 57.41 .0003 58.75 .0007 59.33 .0009
		 60.00 .0004 61.34 .0001 64.97 .0003 65.20 .0002 66.48 .0002 67.83 .0002 68.90 .0003 70.25 .0003
		 71.59 .0002 73.68 .0001 75.92 .0001 77.08 .0002 78.45 .0002 81.56 .0002 82.99 .0001 88.39 .0001)

               (list   .97 .0059  1.98 .0212  2.99 .0153  3.99 .0227  4.96 .0215  5.97 .0153  6.98 .0085
		  7.98 .0007  8.97 .0179  9.98 .0512 10.98 .0322 12.00 .0098 13.02 .0186 14.00 .0099 15.05 .0109
		 15.88 .0011 17.07 .0076 18.11 .0071 19.12 .0045 20.16 .0038 21.23 .0213 22.27 .0332 23.34 .0082
		 24.34 .0014 25.42 .0024 26.47 .0012 27.54 .0014 28.60 .0024 29.72 .0026 30.10 .0008 31.91 .0021
		 32.13 .0011 33.02 .0007 34.09 .0014 35.17 .0007 36.27 .0024 37.39 .0029 38.58 .0014 39.65 .0017
		 40.95 .0012 41.97 .0004 42.43 .0002 43.49 .0001 44.31 .0012 45.42 .0031 46.62 .0017 47.82 .0013
		 49.14 .0013 50.18 .0010 51.54 .0003 53.90 .0006 55.06 .0010 56.31 .0003 57.63 .0001 59.02 .0003
		 60.09 .0004 60.35 .0004 61.62 .0009 63.97 .0001 65.19 .0001 65.54 .0002 66.92 .0002 67.94 .0002
		 69.17 .0003 69.60 .0004 70.88 .0002 72.24 .0002 76.12 .0001 78.94 .0001 81.75 .0001 82.06 .0001
		 83.53 .0001 90.29 .0002 91.75 .0001 92.09 .0002 93.28 .0001 97.07 .0001)

               (list  1.98 .0159  2.98 .1008  3.98 .0365  4.98 .0133  5.97 .0101  6.97 .0115  7.97 .0007
		  8.99 .0349 10.01 .0342 11.01 .0236 12.00 .0041 13.02 .0114 14.05 .0137 15.06 .0100 16.05 .0007
		 17.04 .0009 18.12 .0077 19.15 .0023 20.12 .0017 21.24 .0113 22.26 .0126 23.30 .0093 24.36 .0007
		 25.43 .0007 26.47 .0009 27.55 .0013 28.59 .0025 29.61 .0010 30.77 .0021 31.86 .0023 32.96 .0003
		 34.03 .0007 35.06 .0005 36.20 .0006 37.34 .0006 38.36 .0009 39.60 .0016 40.69 .0005 41.77 .0002
		 42.92 .0002 44.02 .0003 45.24 .0006 46.33 .0004 47.50 .0007 48.71 .0007 49.87 .0002 51.27 .0002
		 53.42 .0003 55.88 .0003 57.10 .0004 58.34 .0002 59.86 .0003 61.13 .0003 67.18 .0001 68.50 .0001
		 71.17 .0001 83.91 .0001 90.55 .0001)

               (list   .98 .0099  2.00 .0181  2.99 .0353  3.98 .0285  4.97 .0514  5.96 .0402  6.96 .0015
		  7.98 .0012  8.98 .0175  9.98 .0264 10.98 .0392 11.98 .0236 13.00 .0153 14.04 .0049 15.00 .0089
		 16.01 .0001 17.03 .0106 18.03 .0028 19.05 .0024 20.08 .0040 21.11 .0103 22.12 .0104 23.20 .0017
		 24.19 .0008 25.20 .0007 26.24 .0011 27.36 .0009 27.97 .0030 29.40 .0044 30.37 .0019 31.59 .0017
		 32.65 .0008 33.59 .0005 34.79 .0009 35.75 .0027 36.88 .0035 37.93 .0039 39.00 .0031 40.08 .0025
		 41.16 .0010 43.25 .0004 44.52 .0012 45.62 .0023 45.85 .0012 47.00 .0006 47.87 .0008 48.99 .0003
		 50.48 .0003 51.62 .0001 52.43 .0001 53.56 .0002 54.76 .0002 56.04 .0002 56.68 .0006 57.10 .0003
		 58.28 .0005 59.47 .0003 59.96 .0002 60.67 .0001 63.08 .0002 64.29 .0002 66.72 .0001 67.97 .0001
		 68.65 .0001 70.43 .0001 79.38 .0001 80.39 .0001 82.39 .0001)

               (list  1.00 .0765  1.99 .0151  2.99 .0500  3.99 .0197  5.00 .0260  6.00 .0145  6.98 .0128
		  7.97 .0004  8.98 .0158  9.99 .0265 11.02 .0290 12.02 .0053 13.03 .0242 14.03 .0103 15.06 .0054
		 16.04 .0006 17.08 .0008 18.10 .0058 19.16 .0011 20.16 .0055 21.18 .0040 22.20 .0019 23.22 .0014
		 24.05 .0005 25.31 .0019 26.38 .0018 27.44 .0022 28.45 .0024 29.57 .0073 30.58 .0032 31.66 .0071
		 32.73 .0015 33.85 .0005 34.96 .0003 36.00 .0020 37.11 .0018 38.18 .0055 39.23 .0006 40.33 .0004
		 41.52 .0003 43.41 .0028 45.05 .0003 45.99 .0002 47.07 .0003 48.52 .0002 49.48 .0003 50.63 .0003
		 51.81 .0002 54.05 .0002 55.24 .0001 56.62 .0001 57.81 .0004 59.16 .0013 60.23 .0003 66.44 .0001
		 68.99 .0004 75.49 .0001 87.56 .0004)

               (list   .98 .0629  1.99 .0232  2.98 .0217  4.00 .0396  4.98 .0171  5.97 .0098  6.99 .0167
		  7.99 .0003  8.98 .0192  9.98 .0266 10.99 .0256 12.01 .0061 13.02 .0135 14.02 .0062 15.05 .0158
		 16.06 .0018 17.08 .0101 18.09 .0053 19.11 .0074 20.13 .0020 21.17 .0052 22.22 .0077 23.24 .0035
		 24.00 .0009 25.32 .0016 26.40 .0022 27.43 .0005 28.55 .0026 29.60 .0026 30.65 .0010 31.67 .0019
		 32.77 .0008 33.81 .0003 34.91 .0003 36.01 .0005 37.11 .0010 38.20 .0014 39.29 .0039 40.43 .0012
		 41.50 .0006 43.38 .0017 43.75 .0002 44.94 .0005 46.13 .0002 47.11 .0003 48.28 .0005 48.42 .0005
		 49.44 .0003 50.76 .0004 51.93 .0002 54.15 .0003 55.31 .0005 55.50 .0003 56.98 .0003 57.90 .0004
		 60.33 .0002 61.39 .0001 61.59 .0001 65.09 .0002 66.34 .0001 68.85 .0001 70.42 .0002 71.72 .0001
		 73.05 .0003 79.65 .0001 85.28 .0002 93.52 .0001)

               (list  1.02 .0185  1.99 .0525  2.98 .0613  3.99 .0415  4.98 .0109  5.97 .0248  6.99 .0102
		  7.98 .0005  8.98 .0124  9.99 .0103 10.99 .0124 12.00 .0016 13.01 .0029 14.03 .0211 15.04 .0128
		 16.07 .0021 17.09 .0009 18.09 .0043 19.14 .0022 20.13 .0016 21.20 .0045 22.21 .0088 23.26 .0046
		 24.29 .0013 25.35 .0009 26.39 .0028 27.49 .0009 28.51 .0006 29.58 .0012 30.70 .0010 31.74 .0019
		 32.75 .0002 33.85 .0001 34.95 .0005 36.02 .0003 37.16 .0009 38.25 .0018 39.35 .0008 40.54 .0004
		 41.61 .0002 43.40 .0004 43.74 .0003 45.05 .0001 46.11 .0003 47.40 .0002 48.36 .0004 49.55 .0004
		 50.72 .0002 52.00 .0001 55.58 .0002 57.02 .0001 57.98 .0002 59.13 .0003 61.56 .0001 66.56 .0001
		 87.65 .0002)

               (list  1.00 .0473  1.99 .0506  2.99 .0982  3.99 .0654  5.00 .0196  5.99 .0094  6.99 .0118
		  7.93 .0001  8.99 .0057 10.01 .0285 11.01 .0142 12.03 .0032 13.03 .0056 14.06 .0064 15.06 .0059
		 16.11 .0005 17.09 .0033 18.14 .0027 19.15 .0014 20.17 .0010 21.21 .0059 22.26 .0043 23.31 .0031
		 24.31 .0018 25.33 .0009 26.41 .0005 27.47 .0015 28.53 .0015 29.58 .0041 30.65 .0025 31.73 .0011
		 32.83 .0010 34.98 .0003 36.07 .0009 37.23 .0001 38.26 .0020 39.41 .0014 40.53 .0005 41.40 .0003
		 42.80 .0002 43.48 .0028 43.93 .0001 45.03 .0003 46.18 .0007 47.41 .0001 48.57 .0002 49.67 .0001
		 50.83 .0002 54.39 .0001 55.58 .0002 57.97 .0005 58.11 .0002 59.21 .0001 60.42 .0002 61.66 .0001)

               (list  1.00 .0503  2.00 .0963  2.99 .1304  3.99 .0218  4.98 .0041  5.98 .0292  6.98 .0482
		  7.99 .0005  8.99 .0280 10.00 .0237 11.00 .0152 12.02 .0036 12.95 .0022 14.06 .0111 15.07 .0196
		 16.08 .0016 17.11 .0044 18.13 .0073 19.17 .0055 20.19 .0028 21.20 .0012 22.27 .0068 23.30 .0036
		 24.35 .0012 25.35 .0002 26.46 .0005 27.47 .0005 28.59 .0009 29.65 .0021 30.70 .0020 31.78 .0012
		 32.89 .0010 35.06 .0005 36.16 .0008 37.27 .0010 38.36 .0010 39.47 .0014 40.58 .0004 41.43 .0007
		 41.82 .0003 43.48 .0008 44.53 .0001 45.25 .0003 46.43 .0002 47.46 .0002 48.76 .0005 49.95 .0004
		 50.96 .0002 51.12 .0002 52.33 .0001 54.75 .0001 55.75 .0002 56.90 .0002 58.17 .0002 59.40 .0004
		 60.62 .0002 65.65 .0001 66.91 .0002 69.91 .0001 71.25 .0002)

               (list  1.00 .1243  1.98 .1611  3.00 .0698  3.98 .0390  5.00 .0138  5.99 .0154  7.01 .0287
		  8.01 .0014  9.01 .0049 10.00 .0144 11.01 .0055 12.05 .0052 13.01 .0011 14.05 .0118 15.07 .0154
		 16.12 .0028 17.14 .0061 18.25 .0007 19.22 .0020 20.24 .0011 21.27 .0029 22.30 .0046 23.34 .0049
		 24.35 .0004 25.45 .0003 26.47 .0007 27.59 .0008 28.16 .0009 29.12 .0002 29.81 .0006 30.81 .0009
		 31.95 .0004 33.00 .0011 34.12 .0005 35.18 .0003 36.30 .0008 37.38 .0003 38.55 .0003 39.64 .0006
		 40.77 .0007 41.52 .0006 41.89 .0006 43.04 .0011 43.60 .0009 44.31 .0002 45.68 .0002 46.56 .0003
		 47.60 .0001 48.83 .0006 50.01 .0003 51.27 .0003 56.04 .0005 57.21 .0003 58.56 .0004 59.83 .0003
		 61.05 .0001 62.20 .0001 67.37 .0002 76.53 .0001)

               (list   .99 .0222  1.99 .0678  2.99 .0683  4.00 .0191  5.00 .0119  6.01 .0232  6.98 .0336
		  7.99 .0082  9.01 .0201 10.01 .0189 11.01 .0041 12.01 .0053 13.05 .0154 14.04 .0159 15.06 .0092
		 16.11 .0038 17.12 .0014 18.15 .0091 19.16 .0006 20.30 .0012 21.25 .0061 22.28 .0099 23.34 .0028
		 24.38 .0012 25.43 .0016 26.49 .0048 27.55 .0025 28.62 .0015 29.71 .0032 30.78 .0077 31.88 .0011
		 32.97 .0007 34.08 .0006 35.16 .0008 36.28 .0004 37.41 .0006 38.54 .0005 39.62 .0002 40.80 .0003
		 41.93 .0001 43.06 .0002 44.21 .0003 45.38 .0002 46.54 .0007 47.78 .0003 48.95 .0004 50.10 .0003
		 51.37 .0002 53.79 .0003 56.20 .0001 58.71 .0002 66.47 .0003)

               (list  1.01 .0241  1.99 .1011  2.98 .0938  3.98 .0081  4.99 .0062  5.99 .0291  6.99 .0676
		  7.59 .0004  8.98 .0127  9.99 .0112 10.99 .0142 12.00 .0029 13.02 .0071 14.02 .0184 15.03 .0064
		 16.07 .0010 17.09 .0011 18.11 .0010 19.15 .0060 20.19 .0019 21.24 .0025 22.29 .0013 23.31 .0050
		 25.41 .0030 26.50 .0018 27.53 .0006 28.63 .0012 29.66 .0013 30.77 .0020 31.84 .0006 34.04 .0001
		 35.14 .0001 36.32 .0004 37.41 .0007 38.53 .0007 39.67 .0009 40.85 .0003 45.49 .0002 46.65 .0001
		 47.81 .0004 49.01 .0002 53.91 .0002 55.14 .0002 57.69 .0002)

               (list  1.00 .0326  2.00 .1066  2.99 .1015  4.00 .0210  4.97 .0170  5.99 .0813  6.98 .0820
		  7.96 .0011  8.99 .0248 10.03 .0107 11.01 .0126 12.01 .0027 13.01 .0233 14.04 .0151 15.05 .0071
		 16.04 .0002 17.10 .0061 18.12 .0059 19.15 .0087 20.23 .0005 21.25 .0040 22.30 .0032 23.35 .0004
		 24.40 .0001 25.45 .0030 26.54 .0022 27.60 .0003 28.70 .0009 29.80 .0029 30.85 .0006 31.97 .0006
		 34.19 .0004 35.30 .0003 36.43 .0007 37.56 .0005 38.68 .0019 39.88 .0013 41.00 .0003 43.35 .0003
		 44.51 .0002 45.68 .0006 46.93 .0010 48.11 .0006 49.29 .0003 55.58 .0002)

               (list   .98 .0113  1.99 .0967  3.00 .0719  3.98 .0345  4.98 .0121  6.00 .0621  7.00 .0137
		  7.98 .0006  9.01 .0314 10.01 .0171 11.02 .0060 12.03 .0024 13.05 .0077 14.07 .0040 15.12 .0032
		 16.13 .0004 17.15 .0011 18.20 .0028 19.18 .0003 20.26 .0003 21.31 .0025 22.35 .0021 23.39 .0005
		 25.55 .0002 26.62 .0014 27.70 .0003 28.78 .0005 29.90 .0030 31.01 .0011 32.12 .0005 34.31 .0001
		 35.50 .0002 36.62 .0002 37.76 .0005 38.85 .0002 40.09 .0004 43.60 .0001 44.73 .0002 46.02 .0002
		 47.25 .0004 48.44 .0004)

               (list   .99 .0156  1.98 .0846  2.98 .0178  3.98 .0367  4.98 .0448  5.98 .0113  6.99 .0189
		  8.00 .0011  9.01 .0247 10.02 .0089 11.01 .0184 12.03 .0105 13.00 .0039 14.07 .0116 15.09 .0078
		 16.13 .0008 17.14 .0064 18.19 .0029 19.22 .0028 20.25 .0017 21.32 .0043 22.37 .0055 23.42 .0034
		 24.48 .0004 25.54 .0002 26.61 .0017 27.70 .0011 28.80 .0002 29.89 .0019 30.97 .0028 32.09 .0007
		 34.30 .0002 35.44 .0003 36.55 .0001 37.69 .0004 38.93 .0002 40.05 .0005 41.20 .0005 42.37 .0002
		 43.54 .0003 44.73 .0001 45.95 .0002 47.16 .0001 48.43 .0005 49.65 .0004 55.90 .0002 59.81 .0004)

               (list  1.01 .0280  2.00 .0708  2.99 .0182  3.99 .0248  4.98 .0245  5.98 .0279  6.98 .0437
		  7.99 .0065  8.99 .0299 10.00 .0073 10.99 .0011 12.03 .0122 13.03 .0028 14.08 .0044 15.11 .0097
		 16.15 .0010 17.17 .0025 18.19 .0017 19.24 .0008 20.28 .0040 21.32 .0024 22.38 .0008 23.46 .0032
		 24.52 .0010 25.59 .0008 26.68 .0009 27.76 .0012 28.88 .0003 29.95 .0005 31.05 .0017 32.14 .0002
		 33.29 .0003 37.88 .0002 39.03 .0002 40.19 .0004 41.37 .0003 43.74 .0002 46.20 .0001 48.68 .0001
		 49.93 .0001 51.19 .0002)

               (list  1.00 .0225  1.99 .0921  2.98 .0933  3.99 .0365  4.99 .0100  5.98 .0213  6.98 .0049
		  7.98 .0041  8.98 .0090  9.99 .0068 11.01 .0040 12.03 .0086 13.02 .0015 14.04 .0071 15.09 .0082
		 16.14 .0011 17.15 .0014 18.18 .0010 19.26 .0013 20.26 .0005 21.33 .0006 22.36 .0011 23.46 .0016
		 24.52 .0004 25.59 .0002 26.70 .0006 27.78 .0007 28.87 .0002 30.03 .0008 31.14 .0010 32.24 .0006
		 33.37 .0002 35.67 .0003 37.99 .0004 39.17 .0004 40.35 .0005 41.53 .0001 46.42 .0001)

               (list  1.00 .0465  1.99 .0976  2.98 .0678  4.00 .0727  4.99 .0305  5.98 .0210  6.98 .0227
		  8.00 .0085  9.01 .0183 10.02 .0258 11.05 .0003 12.06 .0061 13.05 .0021 14.10 .0089 15.12 .0077
		 16.16 .0016 17.21 .0061 18.23 .0011 19.29 .0031 20.36 .0031 21.41 .0007 22.48 .0013 23.55 .0020
		 24.64 .0004 25.74 .0005 26.81 .0006 27.95 .0006 29.03 .0001 30.22 .0010 31.30 .0004 32.48 .0001
		 33.60 .0002 38.30 .0003)

               (list  1.00 .0674  1.99 .0841  2.98 .0920  3.99 .0328  4.99 .0368  5.98 .0206  6.99 .0246
		  8.01 .0048  9.01 .0218 10.03 .0155 11.05 .0048 12.06 .0077 13.00 .0020 14.10 .0083 15.15 .0084
		 16.18 .0015 17.22 .0039 18.27 .0032 19.34 .0026 20.40 .0012 21.47 .0009 22.54 .0008 23.62 .0016
		 24.71 .0005 25.82 .0004 26.91 .0002 28.03 .0008 29.17 .0002 30.32 .0028 31.45 .0004 32.61 .0005
		 33.77 .0001 36.14 .0003 37.32 .0002 38.54 .0005 39.75 .0002 42.23 .0002 48.65 .0001)

               (list  1.01 .0423  1.99 .0240  2.98 .0517  4.00 .0493  5.00 .0324  6.00 .0094  6.99 .0449
		  7.99 .0050  9.00 .0197 10.03 .0132 11.03 .0009 12.07 .0017 13.08 .0023 14.12 .0094 15.16 .0071
		 16.21 .0020 17.25 .0005 18.30 .0027 19.04 .0004 20.43 .0022 21.51 .0002 22.59 .0006 23.72 .0018
		 24.80 .0002 25.88 .0002 27.03 .0002 28.09 .0006 29.31 .0002 30.46 .0004 31.61 .0007 32.78 .0005
		 33.95 .0001 36.34 .0002 37.56 .0001 38.80 .0001 40.02 .0001 44.14 .0001)

               (list  1.00 .0669  1.99 .0909  2.99 .0410  3.98 .0292  4.98 .0259  5.98 .0148  6.98 .0319
		  7.99 .0076  9.01 .0056 10.02 .0206 11.04 .0032 12.05 .0085 13.08 .0040 14.12 .0037 15.16 .0030
		 16.20 .0013 17.24 .0021 18.30 .0010 19.36 .0015 20.44 .0013 21.50 .0009 22.60 .0015 23.69 .0014
		 24.80 .0006 25.87 .0002 27.02 .0006 28.12 .0002 29.28 .0003 30.43 .0002 31.59 .0007 32.79 .0001
		 35.14 .0001 37.57 .0001 40.03 .0002 41.28 .0004 44.10 .0001)

               (list   .99 .0421  1.99 .1541  2.98 .0596  3.98 .0309  4.98 .0301  5.99 .0103  7.00 .0240
		  8.01 .0073  9.01 .0222 10.04 .0140 11.05 .0033 12.08 .0045 13.13 .0009 14.13 .0015 15.21 .0026
		 16.24 .0003 17.30 .0004 18.35 .0010 19.39 .0003 20.50 .0015 21.57 .0003 22.68 .0011 23.80 .0005
		 24.90 .0008 26.02 .0002 27.16 .0001 28.30 .0006 29.48 .0002 31.81 .0005 33.00 .0003 34.21 .0001
		 37.89 .0001)

               (list   .99 .0389  2.00 .2095  3.00 .0835  3.99 .0289  5.00 .0578  5.99 .0363  7.01 .0387
		  8.01 .0056  9.04 .0173 10.05 .0175 11.08 .0053 12.10 .0056 13.15 .0064 14.19 .0036 15.22 .0019
		 16.29 .0010 17.36 .0017 18.43 .0018 19.51 .0004 20.60 .0011 21.70 .0003 22.82 .0003 23.95 .0001
		 25.05 .0004 26.17 .0001 28.50 .0003 29.68 .0001 32.07 .0003 33.28 .0004 34.52 .0001)

               (list  1.00 .1238  1.99 .2270  3.00 .0102  3.99 .0181  4.98 .0415  6.00 .0165  7.01 .0314
		  8.02 .0148  9.04 .0203 10.05 .0088 11.07 .0062 12.11 .0070 13.14 .0054 14.19 .0028 15.24 .0044
		 16.30 .0029 17.38 .0009 18.45 .0026 19.56 .0003 20.65 .0025 21.74 .0014 22.87 .0013 23.99 .0007
		 25.15 .0002 27.46 .0004 28.39 .0006 28.65 .0004 29.85 .0001 31.05 .0002 32.27 .0003 33.52 .0002
		 34.76 .0003)

               (list  1.00 .1054  2.00 .2598  2.99 .0369  3.98 .0523  4.99 .0020  5.99 .0051  7.00 .0268
		  8.01 .0027  9.04 .0029 10.05 .0081 11.08 .0047 12.12 .0051 13.16 .0091 14.19 .0015 15.27 .0030
		 16.34 .0017 17.42 .0006 18.51 .0003 19.61 .0007 20.72 .0003 21.84 .0001 22.99 .0010 24.13 .0001
		 28.44 .0001 30.09 .0001)

               (list   .99 .0919  2.00 .0418  2.99 .0498  3.99 .0135  4.99 .0026  6.00 .0155  7.01 .0340
		  8.02 .0033  9.04 .0218 10.08 .0084 11.11 .0057 12.15 .0051 13.21 .0043 14.25 .0015 15.31 .0023
		 16.40 .0008 17.48 .0004 18.59 .0016 19.71 .0010 20.84 .0018 21.98 .0002 23.11 .0013 24.26 .0003
		 26.67 .0002 29.12 .0002 30.37 .0002 31.62 .0003 32.92 .0001)

               (list   .99 .1174  1.99 .1126  2.99 .0370  3.99 .0159  5.01 .0472  6.01 .0091  7.03 .0211
		  8.05 .0015  9.07 .0098 10.11 .0038 11.15 .0042 12.20 .0018 13.24 .0041 14.32 .0033 15.41 .0052
		 16.49 .0001 17.61 .0004 18.71 .0004 19.84 .0004 20.99 .0002 22.14 .0006 23.31 .0006 24.50 .0004
		 25.70 .0002 28.09 .0002 28.66 .0002 32.00 .0001)

               (list  1.00 .1085  2.00 .1400  2.99 .0173  3.99 .0229  5.00 .0272  6.02 .0077  7.03 .0069
		  8.04 .0017  9.08 .0045 10.10 .0030 11.15 .0040 12.20 .0007 13.25 .0019 14.32 .0008 15.42 .0024
		 16.50 .0002 17.59 .0005 18.71 .0003 19.83 .0002 20.98 .0005 23.29 .0008)

               (list  1.00 .0985  2.00 .1440  2.99 .0364  3.99 .0425  5.00 .0190  6.01 .0089  7.03 .0278
		  8.04 .0006  9.07 .0083 10.10 .0021 11.14 .0050 12.18 .0005 13.26 .0036 14.33 .0005 15.41 .0026
		 17.62 .0004 18.75 .0004 19.89 .0003 21.04 .0012 22.21 .0002 23.38 .0004 27.04 .0001)

               (list   .99 .1273  2.00 .1311  2.99 .0120  4.00 .0099  5.00 .0235  6.02 .0068  7.03 .0162
		  8.06 .0009  9.08 .0083 10.12 .0014 11.17 .0050 12.24 .0010 13.29 .0013 14.39 .0022 15.48 .0011
		 16.59 .0002 17.70 .0003 18.84 .0010 20.00 .0003 21.17 .0003 23.56 .0004 28.79 .0003)

               (list  1.00 .1018  2.00 .1486  3.00 .0165  4.00 .0186  5.01 .0194  6.02 .0045  7.04 .0083
		  8.06 .0012  9.10 .0066 10.15 .0009 11.19 .0008 12.26 .0011 13.34 .0028 14.45 .0006 15.53 .0009
		 16.66 .0002 17.79 .0006 18.94 .0005 20.11 .0003 21.29 .0005 22.49 .0003 23.73 .0005 26.22 .0001
		 27.52 .0001 28.88 .0002)

               (list  1.00 .1889  1.99 .1822  3.00 .0363  4.00 .0047  5.01 .0202  6.03 .0053  7.05 .0114
		  8.01 .0002  9.13 .0048 10.17 .0010 11.23 .0033 12.30 .0010 13.38 .0006 14.50 .0002 15.62 .0010
		 20.27 .0001 21.47 .0001)

               (list  1.00 .0522  1.99 .0763  2.99 .0404  4.00 .0139  5.01 .0185  6.01 .0021  7.06 .0045
		  8.09 .0002  9.11 .0003 10.17 .0006 11.25 .0004 12.32 .0005 13.40 .0003 14.53 .0003 15.65 .0007
		 16.80 .0001 17.95 .0002 19.14 .0006 20.34 .0002 21.56 .0003)

               (list   .99 .1821  1.99 .0773  3.00 .0125  4.01 .0065  5.01 .0202  6.03 .0071  7.05 .0090
		  8.08 .0006  9.13 .0008 10.18 .0013 11.25 .0010 12.33 .0012 13.42 .0006 14.54 .0005 15.65 .0004
		 17.97 .0002 19.15 .0001)

               (list  1.00 .1868  2.00 .0951  3.00 .0147  4.01 .0134  5.02 .0184  6.04 .0132  7.06 .0011
		  8.11 .0008  9.15 .0010 10.22 .0012 11.30 .0011 12.40 .0003 13.11 .0004 13.49 .0002 14.62 .0003
		 15.77 .0001)

               (list  1.00 .1933  2.00 .0714  3.00 .0373  4.00 .0108  5.02 .0094  6.02 .0010  7.07 .0022
		  8.11 .0002  9.16 .0065 10.23 .0015 11.31 .0023 12.40 .0003 13.53 .0014 14.66 .0002 15.81 .0011
		 18.20 .0002 19.41 .0001)

               (list   .99 .2113  1.99 .0877  3.00 .0492  4.01 .0094  5.02 .0144  6.04 .0103  7.07 .0117
		  8.12 .0006  9.19 .0019 10.25 .0007 11.35 .0017 12.45 .0010 13.58 .0003 14.74 .0003 15.91 .0003
		 19.57 .0002)

               (list   .99 .2455  1.99 .0161  3.00 .0215  4.01 .0036  5.03 .0049  6.04 .0012  7.09 .0036
		  8.14 .0011  9.21 .0009 10.30 .0001 11.40 .0012 12.50 .0001 13.66 .0005 14.84 .0001)

               (list  1.00 .1132  2.00 .0252  3.00 .0292  4.01 .0136  5.03 .0045  6.06 .0022  7.11 .0101
		  8.17 .0004  9.23 .0010 10.33 .0012 11.44 .0013 12.58 .0011 13.75 .0002 14.93 .0005 16.14 .0002)

               (list  1.00 .1655  2.00 .0445  3.00 .0120  4.00 .0038  5.02 .0015  6.07 .0038  7.11 .0003
		  8.19 .0002  9.25 .0010 10.36 .0011 11.48 .0005 12.63 .0002 13.79 .0003 16.24 .0002)

               (list   .99 .3637  1.99 .0259  3.01 .0038  4.01 .0057  5.03 .0040  6.07 .0067  7.12 .0014
		  8.19 .0004  9.27 .0003 10.38 .0002 12.67 .0001)

               (list  1.00 .1193  2.00 .0230  3.00 .0104  4.01 .0084  5.04 .0047  6.08 .0035  7.13 .0041
		  8.20 .0002  9.29 .0005 10.40 .0005 11.53 .0003 12.70 .0002 13.91 .0002)

               (list  1.00 .0752  2.00 .0497  3.00 .0074  4.02 .0076  5.05 .0053  6.09 .0043  7.15 .0024
		  8.22 .0001  9.32 .0006 10.45 .0002 11.58 .0001 12.78 .0001 15.22 .0001)

               (list  1.00 .2388  2.00 .0629  3.01 .0159  4.04 .0063  5.07 .0051  6.12 .0045  7.19 .0026
		  8.29 .0015  9.43 .0001 11.75 .0002)

               (list  1.00 .1919  2.01 .0116  3.01 .0031  4.03 .0090  5.07 .0061  6.13 .0036  7.19 .0013
		  8.30 .0016  9.13 .0001 10.59 .0002 11.78 .0002)

               (list  1.00 .1296  2.00 .0135  3.01 .0041  4.04 .0045  5.09 .0028  6.14 .0046  7.23 .0007
		  8.32 .0007  9.50 .0001)

               (list  1.00 .0692  2.00 .0209  3.02 .0025  4.05 .0030  5.09 .0047  6.17 .0022  7.25 .0015
		  8.36 .0015  9.53 .0010 10.69 .0001 13.40 .0001)

               (list  1.00 .1715  2.00 .0142  3.01 .0024  4.03 .0015  5.07 .0017  6.13 .0018  7.22 .0009
		  8.33 .0014  9.51 .0007 10.69 .0002)

               (list  1.00 .1555  2.01 .0148  3.02 .0007  4.06 .0006  5.10 .0005  6.16 .0008  7.26 .0009
		  8.39 .0008  9.58 .0002)

               (list  1.00 .1357  2.00 .0116  3.02 .0026  4.04 .0009  5.09 .0004  6.17 .0005  7.27 .0002
		  8.40 .0001)

               (list  1.00 .2185  2.01 .0087  3.03 .0018  4.06 .0025  5.11 .0020  6.20 .0012  7.32 .0005
		  8.46 .0001  9.66 .0003)

               (list  1.00 .2735  2.00 .0038  3.02 .0008  4.06 .0012  5.12 .0008  6.22 .0011  7.35 .0003
		  8.50 .0002)

               (list  1.00 .1441  1.99 .0062  3.01 .0023  4.05 .0011  5.11 .0012  6.20 .0003  7.33 .0004
		  8.50 .0001)

               (list  1.00 .0726  2.01 .0293  3.03 .0022  5.14 .0005  6.26 .0011  7.41 .0002  8.63 .0002)

               (list  1.00 .0516  2.00 .0104  3.02 .0029  5.15 .0002  6.27 .0001)

               (list  1.00 .0329  2.00 .0033  3.03 .0013  4.10 .0005  5.19 .0004  6.32 .0002)

               (list  1.00 .0179  1.99 .0012  3.04 .0005  4.10 .0017  5.20 .0005  6.35 .0001)

               (list  1.00 .0334  2.01 .0033  3.04 .0011  4.13 .0003  5.22 .0003)

               (list   .99 .0161  2.01 .0100  3.04 .0020  4.13 .0003)

               (list  1.00 .0475  1.99 .0045  3.03 .0035  4.12 .0011)

               (list  1.00 .0593  2.00 .0014  4.17 .0002)

               (list  1.00 .0249  2.01 .0016)

               (list  1.00 .0242  2.00 .0038  4.19 .0002)

               (list  1.00 .0170  2.02 .0030)

               (list  1.00 .0381  2.00 .0017  3.09 .0002)

               (list  1.00 .0141  2.03 .0005  3.11 .0003  4.26 .0001)

               (list  1.00 .0122  2.03 .0024)

               (list  1.00 .0107  2.07 .0007  3.12 .0004)

               (list  1.00 .0250  2.02 .0026  3.15 .0002)

               (list  1.01 .0092)

               (list  1.01 .0102  2.09 .0005)

               (list  1.00 .0080  2.00 .0005  3.19 .0001)

               (list  1.01 .0298  2.01 .0005)))

	(*piano-attack-duration* .04)
	(*piano-release-duration* .2)
	(*db-drop-per-second* -10.0))

    (define (get-piano-partials freq)
      (let ((pitch (inexact->exact (round (* 12 (/ (log (/ freq 32.703)) (log 2)))))))
	(list-ref piano-spectra pitch)))

    (define (make-piano-ampfun dur)
      (let* ((releaseAmp (db->linear (* *db-drop-per-second* dur)))
	     (attackTime (/ (* *piano-attack-duration* 100) dur)))
	(list 0 0 (/ attackTime 4) 1.0 attackTime 1.0 100 releaseAmp)))
    
    ;; This thing sounds pretty good down low, below middle c or so.  
    ;; Unfortunately, there are some tens of partials down there and we're using 
    ;; exponential envelopes.  You're going to wait for a long long time just to 
    ;; hear a single low note.  The high notes sound pretty rotten--they just don't
    ;; sparkle;  I have a feeling that this is due to the low amplitude of the 
    ;; original data, and the lack of mechanical noise.
    ;;
    ;; The only thing you can do to alter the sound of a piano note is to set the 
    ;; pfreq parameter.  Pfreq is used to look up the partials.  By default, it's 
    ;; set to the requested frequency.  Setting it to a neighboring freq is useful 
    ;; when you're repeating notes.  Note that there's no nyquist detection; 
    ;; a high freq with a low pfreq, will give you fold over (hmmm...maybe 
    ;; I can get those high notes to sparkle after all).

    (let* ((partials (normalize-partials (get-piano-partials pfreq)))
	   (beg (inexact->exact (floor (* begin-time (mus-srate)))))
	   (newdur (+ duration *piano-attack-duration* *piano-release-duration*))
	   (end (+ beg (inexact->exact (floor (* newdur (mus-srate))))))
	   (env1dur (- newdur *piano-release-duration*))
	   (env1samples (inexact->exact (floor (* env1dur (mus-srate)))))
	   (siz (inexact->exact (floor (/ (vct-length partials) 2))))
	   (oscils (make-vector siz))
	   (alist (make-vct siz))
	   (locs (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	   (ampfun1 (make-piano-ampfun env1dur))
	   (ampenv1 (make-env :envelope ampfun1
			      :scaler  amplitude
			      :duration env1dur
			      :base 10000.0))
	   (releaseamp (list-ref ampfun1 (1- (length ampfun1))))
	   (ampenv2 (make-env :envelope '(0 1 100 0)
			      :scaler (* amplitude releaseamp)
			      :duration env1dur
			      :base 1.0))
	   (sktr 0))
      
      (do ((i 0 (+ i 2))
	   (j 0 (1+ j)))
	  ((= i (vct-length partials)))
	(vct-set! alist j (vct-ref partials (+ i 1)))
	(vector-set! oscils j (make-oscil :frequency (* (vct-ref partials i) frequency))))
      (ws-interrupt?)
    (<rt-play> begin-time duration
	       (lambda ()
		 (set! sktr (1+ sktr))
		 (let ((sum 0.0))
		   (do ((k 0 (1+ k)))
		       ((= k siz))
		     (set! sum (+ sum (* (vct-ref alist k)
					 (oscil (vector-ref oscils k))))))
		   (locsig locs (* sum
				   (if (> sktr env1samples) 
				       (env ampenv2) 
				       (env ampenv1))))))))))


#!
(lbj-piano 0 9 525 0.8)
(rte-info)
!#


(definstrument (resflt start dur driver 
	       ranfreq noiamp noifun cosamp cosfreq1 cosfreq0 cosnum
	       ampcosfun freqcosfun 
	       frq1 r1 g1-1 frq2 r2 g2-1 frq3 r3 g3-1
	       :key (degree 0.0)
		    (distance 1.0)
		    (reverb-amount 0.005))
  ;; driver=0 -- use sum of cosines to drive the filter,
  ;; driver=1 -- use white noise
  ;; if noise used, ranfreq=frequency of random number generator,
  ;;                noiamp=amplitude thereof,
  ;;                noifun=amplitude envelope on white noise
  ;; if sum-of-cosines (i.e. a band-limited pulse train),
  ;;                cosamp=amplitude of pulse train,
  ;;                cosfreq1=top frequency (given freqcosfun) (i.e. pulse frequency)
  ;;                cosfreq0=bottom frequency,
  ;;                cosnum=number of cosines in the pulse,
  ;;                ampcosfun=amplitude envelope on pulse train
  ;;                freqcosfun=frequency envelope on pulse train
  ;; There are then 3 resonators, centered at frq1, frq2, frq3,
  ;; with pole-radius r1, r2, and r3 respectively, and
  ;; with gains of g1, g2, and g3.
  (c-display cosfreq0 cosnum (= driver 1) noifun ampcosfun)
  (let* ((beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (f1 (make-two-pole :radius r1 :frequency frq1))
	 (f2 (make-two-pole :radius r2 :frequency frq2))
	 (f3 (make-two-pole :radius r3 :frequency frq3))
	 (g1 g1-1) (g2 g2-1) (g3 g3-1)
	 (with-noise (= driver 1))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* *rt-num-channels*))
	 (frqf (make-env :envelope (if (not freqcosfun) '(0 0 1 1) freqcosfun)  :duration dur
			 :scaler (hz->radians (- cosfreq1 cosfreq0))))
	 (ampf (if with-noise
		   (make-env :envelope noifun :scaler noiamp :duration dur)
		   (make-env :envelope ampcosfun :scaler cosamp :duration dur)))
	 (rn (make-rand :frequency ranfreq))
	 (cn (make-sum-of-cosines :frequency cosfreq0 :cosines (max 1 cosnum))))
    (ws-interrupt?)
    (<rt-play> start dur
     (lambda ()
	 (let ((input1 (if with-noise
			   (* (env ampf) (rand rn))
			   (* (env ampf) (sum-of-cosines cn (env frqf))))))
	   (locsig loc (+ (two-pole f1 (* input1 g1))
			  (two-pole f2 (* input1 g2))
			  (two-pole f3 (* input1 g3)))))))))

#!
(resflt 0 1.0 0 0 0 #f .1 200 230 10 '(0 0 50 1 100 0) '(0 0 100 1) 500 .995 .1 1000 .995 .1 2000 .995 .1)
(resflt 0 1.0 1 10000 .01 '(0 0 50 1 100 0) 0 0 0 0 #f #f 500 .995 .1 1000 .995 .1 2000 .995 .1)
!#

#!
BUG IN COMPILER

(definstrument (scratch start file src-ratio turnaroundlist)
  (let* ((f (make-readin file)) ;;(make-file->sample file))
         (beg (inexact->exact (round (* (mus-srate) start))))
	 (turntable (list->vct turnaroundlist))
	 (turn-i 1)
	 (turns (length turnaroundlist))
	 (cur-sample (inexact->exact (floor (* (mus-srate) (vct-ref turntable 0)))))
         (turn-sample (inexact->exact (floor (* (mus-srate) (vct-ref turntable turn-i)))))
	 (turning 0)
	 (last-val 0.0)
	 (last-val2 0.0)
         (rd (make-src :srate src-ratio))
	 (forwards (> src-ratio 0.0)))
    (if (and forwards (< turn-sample cur-sample))
	(set! (mus-increment rd) (- src-ratio)))
    (ws-interrupt?)
    (<rt-play> start
     (lambda ()
       (declare (<float> last-val))
	 (let ((val (src rd 0.0
			 (lambda (dir)
			   (set! (mus-location f) cur-sample)
			   (let ((inval (readin f)))
			     (set! cur-sample (+ cur-sample dir))
			     inval)))))
	   (if (= turning 0)
	       (if (and forwards (>= cur-sample turn-sample)) ;; we past turn point going forwards
		   (set! turning 1)
		   (if (and (not forwards) (<= cur-sample turn-sample)) ;; we past turn point going backwards
		       (set! turning -1)))
	       ;; wait for an inflection...
	       (if (or (and (<= last-val2 last-val) (>= last-val val))
		       (and (>= last-val2 last-val) (<= last-val val)))
		   (begin
		     (set! turn-i (1+ turn-i))
		     (if (< turn-i turns)
			 (begin
			   (set! turn-sample (inexact->exact (floor (* (mus-srate) (vct-ref turntable turn-i)))))
			   (set! forwards (not forwards))
			   (set! (mus-increment rd) (- (mus-increment rd)))))
		     (set! turning 0))))
	   (set! last-val2 last-val)
	   (set! last-val val)
	   (out val)
	   (if (>= turn-i turns)
	       (remove-me)))))))
!#

#!
(define filename "/gammelhd/home/kjetil/flute2.wav")
(define filename "/hom/kjetism/Blub_mono16.wav")
(scratch 0.0 filename 1.5 '(0.0 .5 .25 1.0))
!#

(define-rt (file->sample reader pos)
  (set! (mus-location reader) pos)
  (readin reader))


;;; spectral modeling (SMS)

(definstrument (pins beg dur file amp-1
		     :key (transposition 1.0) ; this can be used to transpose the sound
			  (time-scaler 1.0)    ; this can make things happen faster (< 1.0)/slower (> 1.0) in the output
			  (fftsize 256)        ; should be a power of 2
			  ;; at 22050 srate, this is ok for sounds above 300Hz or so, below that you need 512 or 1024,
			  ;; at 44100, probably best to double these sizes -- it takes some searching sometimes.
			  (highest-bin 128)    ; how high in fft data should we search for peaks
			  (max-peaks 16)       ; how many spectral peaks to track at the maximum
			  attack)	       ; whether to use original attack via time domain splice
  ;; do the sliding fft shuffle, translate to polar coordinates, find spectral peaks,
  ;;   match with current, do some interesting transformation, resynthesize using oscils
  ;;   All the envelopes are created on the fly.  max-peaks is how many of these peaks
  ;;   we are willing to track at any given time.
  (let* ((max-peaks-1 max-peaks)
	 (fftsize-1 fftsize)
	 (highest-bin-1 highest-bin)
	 (amp amp-1)
	 (start (inexact->exact (floor (* beg (mus-srate)))))
	 (end (+ start (inexact->exact (floor (* dur (mus-srate))))))
	 (fil (make-readin file))
	 (file-duration (mus-sound-duration file))
	 (fdr (make-vct fftsize-1))
	 (fdi (make-vct fftsize-1))
	 (window (make-fft-window blackman2-window fftsize-1))
	 (fftamps (make-vct fftsize-1))
	 (max-oscils (* 2 max-peaks-1))
	 (current-peak-freqs (make-vct max-oscils))
	 (last-peak-freqs (make-vct max-oscils))
	 (current-peak-amps (make-vct max-oscils))
	 (last-peak-amps (make-vct max-oscils))
	 (peak-amps (make-vct max-peaks-1))
	 (peak-freqs (make-vct max-peaks-1))
	 (resynth-oscils (make-vector max-oscils))
	 (amps (make-vct max-oscils))	;run-time generated amplitude and frequency envelopes
	 (rates (make-vct max-oscils))
	 (freqs (make-vct max-oscils))
	 (sweeps (make-vct max-oscils))
	 (lowest-magnitude .001)
	 (hop (inexact->exact (floor (/ fftsize-1 4))))
	 (outhop (inexact->exact (floor (* time-scaler hop))))
	 (ifreq (/ 1.0 outhop))
	 (ihifreq (hz->radians ifreq))
	 (fftscale (/ 1.0 (* fftsize-1 .42323))) ;integrate Blackman-Harris window = .42323*window width and shift by fftsize-1
	 (fft-mag (/ (mus-srate) fftsize-1))
	 (furthest-away-accepted .1)
	 (filptr 0)
	 (trigger 0)
	 (sum 0.0)
	 (log10 (log 10.0))
	 (cur-oscils max-oscils)
	 (ramped (or attack 0))
	 (splice-attack (number? attack))
	 (attack-size (or attack 1))
	 (ramp-ind 0)
	 (ramped-attack (make-vct attack-size)))
    (if (> (/ dur time-scaler) file-duration)
	(snd-print (format #f "~A is ~1,3F seconds long, but we'll need ~1,3F seconds of data for this note" 
			   file file-duration (/ dur time-scaler))))
    (do ((i 0 (1+ i)))
	((= i max-oscils))
      (vector-set! resynth-oscils i (make-oscil :frequency 0)))
    (set! trigger outhop)
    (vct-scale! window fftscale)
    (ws-interrupt?)
    (<rt-play> beg dur
     (lambda ()
	 (if splice-attack
	     (let ((ramp (/ 1.0 attack-size)))
	       ;; my experience in translating SMS, and rumor via Greg Sandell leads me to believe that
	       ;; there is in fact no way to model some attacks successfully in this manner, so this block
	       ;; simply splices the original attack on to the rest of the note.  "attack" is the number
	       ;; of samples to include bodily.
	       (out (* amp (file->sample fil filptr)))
	       (set! filptr (1+ filptr))
	       (if (> filptr attack-size)
		   (let ((mult 1.0))
		     (do ((k 0 (1+ k)))
			 ((= k attack-size))
		       (vct-set! ramped-attack k (* mult (file->sample fil (+ filptr k))))
		       (set! mult (- mult ramp)))
		     (set! splice-attack #f))))
	     (begin
	       (if (>= trigger outhop)
		   (let ((peaks 0))
		     ;; get next block of data and apply window to it
		     (set! trigger 0)
		     (do ((k 0 (1+ k)))
			 ((= k fftsize-1))
		       (vct-set! fdr k (* (vct-ref window k) (file->sample fil filptr)))
		       (set! filptr (1+ filptr)))
		     (vct-fill! fdi 0.0)
		     (set! filptr (- filptr (- fftsize-1 hop)))
		     ;; get the fft 
		     (fft fdr fdi fftsize-1 1)
		     ;; change to polar coordinates (ignoring phases)
		     (do ((k 0 (1+ k)))
			 ((= k highest-bin-1))	;no need to paw through the upper half (so (<= highest-bin-1 (floor fft-size 2)))
		       (let ((x (vct-ref fdr k))
			     (y (vct-ref fdi k)))
			 (vct-set! fftamps k (* 2 (sqrt (+ (* x x) (* y y)))))))
		     (do ((k 0 (1+ k)))
			 ((= k max-oscils))
		       (vct-set! last-peak-freqs k (vct-ref current-peak-freqs k))
		       (vct-set! last-peak-amps k (vct-ref current-peak-amps k))
		       (vct-set! current-peak-amps k 0.0))
		     (vct-fill! peak-amps 0.0)
		     (let ((ra (vct-ref fftamps 0))
			   (la 0.0)
			   (ca 0.0))
		       ;; search for current peaks following Xavier Serra's recommendations in
		       ;; "A System for Sound Analysis/Transformation/Synthesis 
		       ;;      Based on a Deterministic Plus Stochastic Decomposition"
		       (do ((k 0 (1+ k)))
			   ((= k highest-bin-1))
			 (set! la ca)
			 (set! ca ra)
			 (set! ra (vct-ref fftamps k))
			 (if (and (> ca lowest-magnitude)
				  (> ca ra)
				  (> ca la))
			     ;; found a local maximum above the current threshold (its bin number is k-1)
			     (let* ((logla (/ (log la) log10))
				    (logca (/ (log ca) log10)) 
				    (logra (/ (log ra) log10))
				    (offset (/ (* .5 (- logla logra)) (+ logla (* -2 logca) logra)))
				    (amp (expt 10.0 (- logca (* .25 (- logla logra) offset))))
				    (freq (* fft-mag (+ k offset -1))))
			       (if (= peaks max-peaks-1)
				   ;; gotta either flush this peak, or find current lowest and flush him
				   (let ((minp 0)
					 (minpeak (vct-ref peak-amps 0)))
				     (do ((j 1 (1+ j)))
					 ((= j max-peaks-1))
				       (if (< (vct-ref peak-amps j) minpeak)
					   (begin
					     (set! minp j)
					     (set! minpeak (vct-ref peak-amps j)))))
				     (if (> amp minpeak)
					 (begin
					   (vct-set! peak-freqs minp freq)
					   (vct-set! peak-amps minp amp))))
				   (begin
				     (vct-set! peak-freqs peaks freq)
				     (vct-set! peak-amps peaks amp)
				     (set! peaks (1+ peaks))))))))
		     ;; now we have the current peaks -- match them to the previous set and do something interesting with the result
		     ;; the end results are reflected in the updated values in the rates and sweeps arrays.
		     ;; search for fits between last and current, set rates/sweeps for those found
		     ;;   try to go by largest amp first 
		     (do ((k 0 (1+ k)))
			 ((= k peaks))
		       (let ((maxp 0)
			     (maxpk (vct-ref peak-amps 0)))
			 (do ((j 1 (1+ j)))
			     ((= j max-peaks-1))
			   (if (> (vct-ref peak-amps j) maxpk)
			       (begin
				 (set! maxp j)
				 (set! maxpk (vct-ref peak-amps j)))))
			 ;; now maxp points to next largest unmatched peak
			 (if (> maxpk 0.0)
			     (let* ((closestp -1)
				    (closestamp 10.0)
				    (current-freq (vct-ref peak-freqs maxp))
				    (icf (/ 1.0 current-freq)))
			       (do ((j 0 (1+ j)))
				   ((= j max-peaks-1))
				 (if (> (vct-ref last-peak-amps j) 0.0)
				     (let ((closeness (* icf (abs (- (vct-ref last-peak-freqs j) current-freq)))))
				       (if (< closeness closestamp)
					   (begin
					     (set! closestamp closeness)
					     (set! closestp j))))))
			       (if (< closestamp furthest-away-accepted)
				   (begin
				     ;; peak-amp is transferred to appropriate current-amp and zeroed,
				     (vct-set! current-peak-amps closestp (vct-ref peak-amps maxp))
				     (vct-set! peak-amps maxp 0.0)
				     (vct-set! current-peak-freqs closestp current-freq)))))))
		     (do ((k 0 (1+ k)))
			 ((= k max-peaks-1))
		       (if (> (vct-ref peak-amps k) 0.0)
			   ;; find a place for a new oscil and start it up
			   (let ((new-place -1))
			     (do ((j 0 (1+ j)))
				 ((or (not (= new-place -1))
				      (= j max-oscils)))
			       (if (and (= (vct-ref last-peak-amps j) 0.0) 
					(= (vct-ref current-peak-amps j) 0.0))
				   (set! new-place j)))
			     (vct-set! current-peak-amps new-place (vct-ref peak-amps k))
			     (vct-set! peak-amps k 0.0)
			     (vct-set! current-peak-freqs new-place (vct-ref peak-freqs k))
			     (vct-set! last-peak-freqs new-place (vct-ref peak-freqs k))
			     (set! (mus-frequency (vector-ref resynth-oscils new-place)) (* transposition (vct-ref peak-freqs k))))))
		     (set! cur-oscils 0)
		     (do ((k 0 (1+ k)))
			 ((= k max-oscils))
		       (vct-set! rates k (* ifreq (- (vct-ref current-peak-amps k) (vct-ref last-peak-amps k))))
		       (if (or (not (= (vct-ref current-peak-amps k) 0.0))
			       (not (= (vct-ref last-peak-amps k) 0.0)))
			   (set! cur-oscils k))
		       (vct-set! sweeps k (* ihifreq transposition (- (vct-ref current-peak-freqs k) (vct-ref last-peak-freqs k)))))
		     (set! cur-oscils (1+ cur-oscils))
		     ))
	       ;; run oscils, update envelopes
	       (set! trigger (1+ trigger))
	       (if (= ramped 0)
		   (set! sum 0.0)
		   (begin
		     (set! sum (vct-ref ramped-attack ramp-ind))
		     (set! ramp-ind (1+ ramp-ind))
		     (if (= ramp-ind ramped) (set! ramped 0))))
	       (do ((k 0 (1+ k)))
		   ((= k cur-oscils))
		 (if (or (not (= (vct-ref amps k) 0.0))
			 (not (= (vct-ref rates k) 0.0)))
		     (begin
		       (set! sum (+ sum (* (vct-ref amps k) (oscil (vector-ref resynth-oscils k) (vct-ref freqs k)))))
		       (vct-set! amps k (+ (vct-ref amps k) (vct-ref rates k)))
		       (vct-set! freqs k (+ (vct-ref freqs k) (vct-ref sweeps k))))))
	       (out (* amp sum) )))))))

#!
(define filename "/gammelhd/home/kjetil/flute2.wav")
(pins 0 2 filename 0.5 :time-scaler 16.2)
!#


(definstrument (zc time dur freq amp-1 length1 length2 feedback)
  (let* ((beg (inexact->exact (floor (* (mus-srate) time))))
	 (end (+ beg (inexact->exact (floor (* (mus-srate) dur)))))
	 (amp amp-1)
	 (s (make-pulse-train :frequency freq))
	 (d0 (make-comb :size length1 :max-size (1+ (max length1 length2)) :scaler feedback))
	 (zenv (make-env :envelope '(0 0 1 1) :scaler (- length2 length1) :duration dur)))
    (ws-interrupt?)
    (<rt-play> time dur
     (lambda ()
       (out (comb d0 (* amp (pulse-train s)) (env zenv)))))))

#!
(zc 0 3 100 .6 20 100 .95) (zc 3.5 3 100 .1 100 20 .95)
!#


(definstrument (zn time dur freq amp-1 length1 length2 feedforward)
  ;; notches are spaced at srate/len, feedforward sets depth thereof
  ;; so sweep of len from 20 to 100 sweeps the notches down from 1000 Hz to ca 200 Hz 
  ;; so we hear our downward glissando beneath the pulses.
  (let* ((beg (inexact->exact (floor (* (mus-srate) time))))
	 (end (+ beg (inexact->exact (floor (* (mus-srate) dur)))))
	 (amp amp-1)
	 (s (make-pulse-train :frequency freq))
	 (d0 (make-notch :size length1 :max-size (1+ (max length1 length2)) :scaler feedforward))
	 (zenv (make-env :envelope '(0 0 1 1) :scaler (- length2 length1) :duration dur)))
    (ws-interrupt?)
    (<rt-play> time dur
     (lambda ()
       (out (notch d0 (* amp (pulse-train s)) (env zenv)))))))

#!
(begin
  (zn 0 1 100 .6 20 100 .995)
  (zn 1.5 1 100 .6 100 20 .995))
(rte-info)
!#


(definstrument (za time dur freq amp-1 length1 length2 feedback feedforward)
  (let* ((beg (inexact->exact (floor (* (mus-srate) time))))
	 (end (+ beg (inexact->exact (floor (* (mus-srate) dur)))))
	 (amp amp-1)
	 (s (make-pulse-train :frequency freq))
	 (d0 (make-all-pass feedback feedforward :size length1 :max-size (1+ (max length1 length2))))
	 (zenv (make-env :envelope '(0 0 1 1) :scaler (- length2 length1) :duration dur)))
    (ws-interrupt?)
    (<rt-play> time dur
     (lambda ()
       (out (all-pass d0 (* amp (pulse-train s)) (env zenv)))))))

#!
(begin
  (za 0 1 100 .6 20 100 .95 .95)
  (za 1.5 1 100 .6 100 20 .95 .95))
!#


(definstrument (clm-expsrc beg dur input-file exp-ratio src-ratio amp-1 :key rev start-in-file)
  (let* ((st (inexact->exact (floor (* beg (mus-srate)))))
	 (stf (inexact->exact (floor (* (or start-in-file 0) (mus-sound-srate input-file)))))
	 (fdA (make-readin input-file :channel 0 :start stf))
	 (exA (make-granulate :expansion exp-ratio))
	 (two-chans (and (= (mus-sound-chans input-file) 2) (= *rt-num-channels* 2)))
	 (fdB (make-readin input-file :channel (if two-chans 1 0) :start stf))
	 (exB (make-granulate :expansion exp-ratio))
	 (srcA (make-src :srate src-ratio))
	 (srcB (make-src :srate src-ratio))
	 (revit (and *reverb* rev))
	 (rev-amp (if revit (if two-chans (* rev .5) rev) 0.0))
	 (amp amp-1)
	 (nd (+ st (inexact->exact (floor (* (mus-srate) dur))))))
    (<rt-play> beg dur
     (lambda ()
       (let ((valA               (* amp (src srcA 0.0 (lambda (dir) (granulate exA (lambda (dir) (readin fdA)))))))
	     (valB (if two-chans (* amp (src srcB 0.0 (lambda (dir) (granulate exB (lambda (dir) (readin fdB)))))) 0.0)))
	 (out 0 valA)
	 (if two-chans (out 1 valB)))))))
	 ;;;(if revit (out-any i (* rev-amp (+ valA valB)) 0 *reverb*))))))))

#!
(clm-expsrc 0 2 filename 5.5 0.5 0.9)
!#


#!
BUG IN COMPILER

(definstrument (exp-snd file beg dur amp :key (exp-amt 1.0) (ramp .4) (seglen .15) (sr 1.0) (hop .05) ampenv)
  ;; granulate with envelopes on the expansion amount, segment envelope shape,
  ;; segment length, hop length, and input file resampling rate
  (let* ((st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate))))))
	 (f0 (make-readin file 0))
	 (expenv (make-env :envelope (if (list? exp-amt) 
					 (or exp-amt (list 0 1 1 1)) 
					 (list 0 exp-amt 1 exp-amt))
			   :duration dur))
	 (lenenv (make-env :envelope (if (list? seglen) 
					 (or seglen (list 0 .15 1 .15)) 
					 (list 0 seglen 1 seglen))
			   :duration dur))
	 (max-seg-len (if seglen (if (list? seglen) (max-envelope seglen) seglen) .15))
	 (initial-seg-len (if seglen (if (list? seglen) (cadr seglen) seglen) .15))
	 (scaler-amp (if (> max-seg-len .15) (/ (* 0.6 .15) max-seg-len) 0.6))
	 (srenv  (make-env :envelope (if (list? sr) 
					 (or sr (list 0 1 1 1)) 
					 (list 0 sr 1 sr))
			   :duration dur))
	 (rampdata (if (list? ramp) 
		       (or ramp (list 0 .4 1 .4))
		       (list 0 ramp 1 ramp)))
	 (rampenv (make-env :envelope rampdata :duration dur))
	 (initial-ramp-time (if ramp (if (list? ramp) (cadr ramp) ramp) .4))
	 (hopenv (make-env :envelope (if (list? hop) 
					 (or hop (list 0 .05 1 .05)) 
					 (list 0 hop 1 hop))
			   :duration dur))
	 (max-out-hop (if hop (if (list? hop) (max-envelope hop) hop) .05))
	 (initial-out-hop (if hop (if (list? hop) (cadr hop) hop) .05))
	 (min-exp-amt (if exp-amt (if (list? exp-amt) (min-envelope exp-amt) exp-amt) 1.0))
	 (initial-exp-amt (if exp-amt (if (list? exp-amt) (cadr exp-amt) exp-amt) 1.0))
	 (max-in-hop (/ max-out-hop min-exp-amt))
	 (max-len (inexact->exact (ceiling (* (mus-srate) (+ (max max-out-hop max-in-hop) max-seg-len)))))
	 (ampe (make-env :envelope (or ampenv (list 0 0 .5 1 1 0)) :scaler amp :duration dur))
	 (exA (make-granulate :expansion initial-exp-amt
			      :max-size max-len
			      :ramp initial-ramp-time 
			      :hop initial-out-hop
			      :length initial-seg-len 
			      :scaler scaler-amp))
	 (ex-samp 0.0)
	 (next-samp 0.0)
	 (vol (env ampe))
	 (valA0 (* vol (granulate exA (lambda (dir) (readin f0)))))
	 (valA1 (* vol (granulate exA (lambda (dir) (readin f0))))))
    (ws-interrupt?)
    (if (or (<= (min-envelope rampdata) 0.0)
	    (>= (max-envelope rampdata) 0.5))
	(snd-warning (format #f "ramp argument to expsnd must always be between 0.0 and 0.5: ~A" ramp))
	(<rt-play> beg dur
	 (lambda ()
	   (declare (<float> valA0 valA1))
	     ;;(if (c-g?) (set! i (1- nd)))
	     (let* ((expa (env expenv)) ;current expansion amount
		    (segl (env lenenv)) ;current segment length
		    (resa (env srenv)) ;current resampling increment
		    (rmpl (env rampenv)) ;current ramp length (0 to .5)
		    (hp (env hopenv)) ;current hop size
		    ;; now we set the granulate generator internal state to reflect all these envelopes
		    (sl (inexact->exact (floor (* segl (mus-srate)))))
		    (rl (inexact->exact (floor (* rmpl sl)))))
	       (set! vol (env ampe))
	       (set! (mus-length exA) sl)
	       (set! (mus-ramp exA) rl)
	       (set! (mus-frequency exA) hp)
	       (set! (mus-increment exA) expa)
	       (set! next-samp (+ next-samp resa))
	       (if (> next-samp (1+ ex-samp))
		   (let ((samps (inexact->exact (floor (- next-samp ex-samp)))))
		     (do ((k 0 (1+ k)))
			 ((= k samps))
		       (set! valA0 valA1)
		       (set! valA1 (* vol (granulate exA (lambda (dir) (readin f0)))))
		       (set! ex-samp (1+ ex-samp)))))
	       (if (= next-samp ex-samp)
		   (out 0 valA0)
		   (out 0 (+ valA0 (* (- next-samp ex-samp) (- valA1 valA0)))))))))))
!#


;;; (with-sound () (exp-snd "fyow.snd" 0 3 1 '(0 1 1 3) 0.4 .15 '(0 2 1 .5) 0.05))
;;; (with-sound () (exp-snd "oboe.snd" 0 3 1 '(0 1 1 3) 0.4 .15 '(0 2 1 .5) 0.2))


#!
BUG IN COMPILER

(define-rt-vct-struct grn
  :rampval
  :rampinc
  :loc
  :segctr
  :whichseg
  :ramplen
  :steadylen
  :trigger)


(definstrument (expfil start duration hopsecs rampsecs steadysecs file1 file2)
  (let* ((fil1 (make-file->sample file1))
	 (fil2 (make-file->sample file2))
	 (hop (inexact->exact (floor (* hopsecs (mus-srate)))))
	 (ramplen (inexact->exact (floor (* rampsecs (mus-srate)))))
	 (steadylen (inexact->exact (floor (* steadysecs (mus-srate)))))
	 (grn1 (make-grn :rampval 0.0 :rampinc (/ 1.0 ramplen) :loc 0 :segctr 0 :whichseg 0 :ramplen ramplen :steadylen steadylen :trigger 0))
	 (grn2 (make-grn :rampval 0.0 :rampinc (/ 1.0 ramplen) :loc 0 :segctr 0 :whichseg 0 :ramplen ramplen :steadylen steadylen :trigger 0))
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* duration (mus-srate))))))
	 (out1 beg)
	 (out2 (+ hop beg)))
    (ws-interrupt?)
    (<rt-play> start duration
     (lambda ()
	 (let ((val 0.0))
	   (if (= i out1)
	       (let* ((grn grn1)
		      (inval (in (=> grn :loc) fil1)))
		 (set! (=> grn :loc) (1+ (=> grn :loc)))
		 (if (= (=> grn :whichseg) 0)	;ramp-up
		     (begin
		       (set! inval (* inval (=> grn :rampval)))
		       (set! (=> grn :rampval) (+ (=> grn :rampval) (=> grn :rampinc)))
		       (set! (=> grn :segctr) (1+ (=> grn :segctr)))
		       (if (= (=> grn :segctr) (=> grn :ramplen))
			   (begin
			     (set! (=> grn :segctr) 0)
			     (set! (=> grn :whichseg) (1+ (=> grn :whichseg))))))
		     (if (= (=> grn :whichseg) 1)		;steady-state
			 (begin
			   (set! (=> grn :segctr) (1+ (=> grn :segctr)))
			   (if (= (=> grn :segctr) (=> grn :steadylen))
			       (begin
				 (set! (=> grn :segctr) 0)
				 (set! (=> grn :whichseg) (1+ (=> grn :whichseg))))))
			 (begin				;ramp-down
			   (set! inval (* inval (=> grn :rampval)))
			   (set! (=> grn :segctr) (1+ (=> grn :segctr)))
			   (set! (=> grn :rampval) (- (=> grn :rampval) (=> grn :rampinc)))
			   (if (= (=> grn :segctr) (=> grn :ramplen))
			       (begin
				 (set! (=> grn :segctr) 0)
				 (set! (=> grn :trigger) 1)
				 (set! (=> grn :whichseg) 0)
				 (set! (=> grn :rampval) 0.0))))))
		 (set! val inval)
		 (set! out1 (1+ out1))
		 (if (= (=> grn :trigger) 1)
		     (begin
		       (set! (=> grn :trigger) 0)
		       (set! out1 (+ out1 hop))))))
	   (if (= i out2)
	       (let* ((grn grn2)
		      (inval (in (=> grn :loc) fil2)))
		 (set! (=> grn :loc) (1+ (=> grn :loc)))
		 (if (= (=> grn :whichseg) 0)	;ramp-up
		     (begin
		       (set! inval (* inval (=> grn :rampval)))
		       (set! (=> grn :rampval) (+ (=> grn :rampval) (=> grn :rampinc)))
		       (set! (=> grn :segctr) (1+ (=> grn :segctr)))
		       (if (= (=> grn :segctr) (=> grn :ramplen))
			   (begin
			     (set! (=> grn :segctr) 0)
			     (set! (=> grn :whichseg) (1+ (=> grn :whichseg))))))
		     (if (= (=> grn :whichseg) 1)		;steady-state
			 (begin
			   (set! (=> grn :segctr) (1+ (=> grn :segctr)))
			   (if (= (=> grn :segctr) (=> grn :steadylen))
			       (begin
				 (set! (=> grn :segctr) 0)
				 (set! (=> grn :whichseg) (1+ (=> grn :whichseg))))))
			 (begin				;ramp-down
			   (set! inval (* inval (=> grn :rampval)))
			   (set! (=> grn :segctr) (1+ (=> grn :segctr)))
			   (set! (=> grn :rampval) (- (=> grn :rampval) (=> grn :rampinc)))
			   (if (= (=> grn :segctr) (=> grn :ramplen))
			       (begin
				 (set! (=> grn :segctr) 0)
				 (set! (=> grn :trigger) 1)
				 (set! (=> grn :whichseg) 0)
				 (set! (=> grn :rampval) 0.0))))))
		 (set! val (+ val inval))
		 (set! out2 (1+ out2))
		 (if (= (=> grn :trigger) 1)
		     (begin
		       (set! (=> grn :trigger) 0)
		       (set! out2 (+ out2 hop))))))
	   (out val)))))))

#!

;;; (with-sound () (expfil 0 2 .2 .01 .1 "oboe.snd" "fyow.snd"))



#!
From: Marco Trevisani <marco@ccrma.Stanford.EDU>

This should work like a Graphic Equalizer....
Very easy to use. Just some note:

"amp" & "amp-env" apply an enveloppe to the final result of the
filtering.  

"dur" as ""standard"" in my instruments, when dur = 0 it will take the length of the
sndfile input, otherwise the duration in seconds.

"gain-freq-list" is a list of gains and frequencies to
filter --in this order gain and frequencies--. There is no limit to
the size of the list. Gain can be a number or an
envelope. Unfortunatelly in this version they cant alternate, one
should chose, all envelopes or all numbers i.e.: 
case 1 -> '( .1 440.0 .3 1500.0 .2 330.0 ...etc) or 
case 2 -> '((0 .1 1 .5) 440.0 (0 1 1 .01) 1500 (0 .3 1 .5) 330.0 ...etc) 
'( .1 440.0 (0 1 1 .01) 1500 ..etc) <<< again, this is not allowed ..

"offset-gain" This apply to all the gains if case 1. It adds or
subtracts an offset to all the gains in the list. This number can be positive or
negative. In case the result is a negative number --let's say offset =
-.4 and, like in case 1, the first gain is .1, the result would be
-.3 -- the instrument will pass a gain equal to 0.  

"filt-gain-scale" & "filt-gain-base" will apply to the elements of the
envelopes if we are in case 2, gains are envelopes.

"stats" if #t --default-- prints the number of seconds processed, if
nil doesnt print anything, which will speed up a bit the process.
!#

(definstrument (graphEq file :key (beg 0) (dur 0) (or-beg 0) (amp 1) (amp-env '(0 1 .8 1 1 0)) (amp-base 1) 
	(offset-gain 0)  
	(gain-freq-list '((0 1 1 0) 440 (0 0 1 1) 660))      
	(filt-gain-scale 1)                   
	(filt-gain-base 1)                    
	(a1 .99)
	(stats #t))                      
  (let* ((st (inexact->exact (floor (* beg (mus-srate)))))
	 (durata (if (= 0 dur) (mus-sound-duration file) dur))
	 (nd (+ st (inexact->exact (floor (* (mus-srate) durata)))))
	 (or-start (inexact->exact (round (* or-beg (mus-sound-srate file)))))
	 (RdA (make-readin :file file :start or-start))
	 (half-list (/ (length gain-freq-list) 2))
	 (ampenv (make-env :envelope amp-env :scaler amp :duration durata :base amp-base))
	 (gain-list (let ((lst '())
			  (len (length gain-freq-list)))
		      (do ((i (- len 2) (- i 2)))
			  ((< i 0))
			(set! lst (cons (list-ref gain-freq-list i) lst)))
		      lst))
	 (freq-list (let ((lst '())
			  (len (length gain-freq-list)))
		      (do ((i (- len 1) (- i 2)))
			  ((<= i 0))
			(set! lst (cons (list-ref gain-freq-list i) lst)))
		      lst))
	 (env-size (if (list? (car gain-list))
		       (make-vector (length freq-list))
		       #f))
	 (if-list-in-gain (list? (car gain-list)))
	 (frm-size (make-vector (length freq-list)))
	 (gains (make-vct (length freq-list)))
	 (samp -1))

    (do ((k 0 (1+ k)))
	((= k half-list))
      (let ((gval (list-ref gain-list k))
	    (fval (list-ref freq-list k)))
	(if (list? gval)
	  (begin
	    (vector-set! env-size k (make-env :envelope gval
					      :scaler filt-gain-scale
					      :duration durata :base filt-gain-base))
	    (vector-set! frm-size k (make-formant fval a1)))
	  (vector-set! frm-size k (make-formant fval a1))
	  (vct-set! gains k (if (< (+ offset-gain gval) 0) 
				0
				(+ offset-gain gval))))))
    (ws-interrupt?)

    (<rt-play> st (- nd st)
     (lambda ()
	 (if stats 
	     (begin
	       (set! samp (1+ samp))
	       (if (= samp (mus-srate))
		   (begin
		     ;;(snd-print (format #f "~% ~F" (/ i (mus-srate))))
		     (set! samp 0)))))
	 (let ((outval 0.0)
	       (inval (readin RdA)))
	   (do ((k 0 (1+ k)))
	       ((= k half-list))
	     (if if-list-in-gain
		 (set! (mus-xcoeff (vector-ref frm-size k) 0) (* (env (vector-ref env-size k)) (- 1.0 a1))))
	     (set! outval (+ outval (* (vct-ref gains k)
				       (formant (vector-ref frm-size k) inval)))))
	   (out (* (env ampenv) outval)))))))

#!
(define filename "/hom/kjetism/Blub_mono16.wav")
(graphEq filename)
!#



(definstrument (anoi infile start dur :key (fftsize 128) (amp-scaler 1.0) (r (* 2.0 pi)))
  ;; a kind of noise reduction -- on-going average spectrum is squelched to some extent
  ;; obviously aimed at intermittent signal in background noise
  ;; this is based on Perry Cook's Scrubber.m
  (let* ((freq-inc (inexact->exact (floor (/ fftsize 2))))
	 (fdi (make-vct fftsize))
	 (fdr (make-vct fftsize))
	 (spectr (make-vct freq-inc 1.0))
	 (scales (make-vct freq-inc 1.0))
	 (diffs (make-vct freq-inc))
	 (win (make-fft-window blackman2-window fftsize))
	 (k 0)
	 (amp 0.0)
	 (incr (/ (* amp-scaler 4) (mus-srate)))
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (file (make-readin infile))
	 (radius (- 1.0 (/ r fftsize)))
	 (bin (/ (mus-srate) fftsize))
	 (fs (make-vector freq-inc))
	 (samp 0))
    (do ((ctr 0 (1+ ctr)))
	((= ctr freq-inc))
      (vector-set! fs ctr (make-formant (* ctr bin) radius)))
    (ws-interrupt?)
    (<rt-play> start dur
     (lambda ()
	 (let ((inval (file->sample file samp)))
	   (set! samp (1+ samp))
	   (vct-set! fdr k inval)
	   (set! k (1+ k))
	   (if (< amp amp-scaler) (set! amp (+ amp incr)))
	   (if (>= k fftsize)
	       (begin
		 (set! k 0)
		 (spectrum fdr fdi win 1)
		 (do ((ctr 0 (1+ ctr)))
		     ((= ctr freq-inc))
		   (vct-set! spectr ctr (+ (* .9 (vct-ref spectr ctr)) (* .1 (vct-ref fdr ctr))))
		   (if (>= (vct-ref spectr ctr) (vct-ref fdr ctr)) 
		       (vct-set! diffs ctr 
				 (/ (vct-ref scales ctr) (- fftsize)))
		       (vct-set! diffs ctr
				 (/ (- (/ (- (vct-ref fdr ctr) (vct-ref spectr ctr)) 
					  (vct-ref fdr ctr)) 
				       (vct-ref scales ctr))
				    fftsize))))))
	   (let ((outval 0.0))
	     (do ((ctr 1 (1+ ctr)))
		 ((= ctr freq-inc))
	       (let ((cur-scale (vct-ref scales ctr)))
		 (set! outval (+ outval (* cur-scale (formant (vector-ref fs ctr) inval))))
		 (vct-set! scales ctr (+ (vct-ref scales ctr) (vct-ref diffs ctr)))))
	     (out (* amp outval))))))))


#!
(define filename "/hom/kjetism/Blub_mono16.wav")
(anoi filename 0 2)
!#



#|
Date: Fri, 25 Sep 1998 09:56:41 +0300
From: Matti Koskinen <mjkoskin@sci.fi>
To: linux-audio-dev@ginette.musique.umontreal.ca
Subject: [linux-audio-dev] Announce: alpha version of denoising
[...]
	I wrote a simple denoiser called anoi after it's parent
	clm-instrument anoi.ins.

	anoi tries to remove white noise like tape hiss from wav-
	files. Removing of noise succeeds ok, but depending of the
	original sound, some distortion can be audible.

	If someone is interested, http://www.sci.fi/~mjkoskin
	contains tarred and gzipped file.

	Now only monophonic wav-files can be denoised, but adding
	others isn't too difficult. 

-matti
mjkoskin@sci.fi
|#

#!
No frame-set! function.

(definstrument (fullmix in-file :optional beg outdur inbeg matrix srate reverb-amount)
  ;; "matrix" can be a simple amplitude or a list of lists
  ;;     each inner list represents one input channel's amps into one output channel
  ;;     each element of the list can be a number, a list (turned into an env) or an env
  (let* ((st (inexact->exact (floor (* (mus-srate) (or beg 0.0)))))
	 (dur (or outdur
		  (/ (- (mus-sound-duration in-file) (or inbeg 0.0))
		     (or (and srate (abs srate)) 1.0))))
	 (samps (inexact->exact (floor (* (mus-srate) dur))))
	 (nd (+ st samps))
	 (in-chans (mus-sound-chans in-file))
	 (inloc (inexact->exact (floor (* (or inbeg 0.0) (mus-sound-srate in-file)))))
	 (file (if (not srate) 
		   (make-file->frame in-file)
		   (let ((vect (make-vector in-chans)))
		     (do ((i 0 (1+ i)))
			 ((= i in-chans))
		       (vector-set! vect i (make-readin in-file i inloc)))
		     vect)))
	 (out-chans *rt-num-channels*)
	 (mx (if matrix
		 (make-mixer (max in-chans out-chans))
		 (make-scalar-mixer (max in-chans out-chans) 1.0)))
	 (rev-mx (if (and *reverb* reverb-amount (> reverb-amount 0.0))
		     (let ((rmx (make-mixer in-chans)))
		       (do ((i 0 (1+ i)))
			   ((= i in-chans))
			 (mixer-set! rmx i 0 reverb-amount)) ; 0->assume 1 chan reverb stream, I think
		       rmx)
		     #f))
	 (revframe (if rev-mx (make-frame 1) #f))
	 (envs #f))
    (if matrix
	(begin
	  (if (list? matrix) ; matrix is list of scalers, envelopes (lists), or env gens
	      (do ((inp 0 (1+ inp)))
		  ((= inp in-chans))
		(let ((inlist (list-ref matrix inp)))
		  (do ((outp 0 (1+ outp)))
		      ((= outp out-chans))
		    (let ((outn (list-ref inlist outp)))
		      (if outn
			  (if (number? outn)
			      (mixer-set! mx inp outp outn)
			      (if (or (env? outn)
				      (list? outn))
				  (begin
				    (if (not envs)
					(begin
					  (set! envs (make-vector in-chans))
					  (do ((i 0 (1+ i)))
					      ((= i in-chans))
					    (vector-set! envs i (make-vector out-chans #f)))))
				    (if (env? outn)
					(vector-set! (vector-ref envs inp) outp outn)
					(vector-set! (vector-ref envs inp) outp (make-env outn :duration dur))))
				  (snd-warning (format #f "unknown element in matrix: ~A" outn)))))))))
	      (do ((inp 0 (1+ inp))) ; matrix is a number in this case (a global scaler)
		  ((= inp in-chans))
		(if (< inp out-chans)
		    (mixer-set! mx inp inp matrix))))))
    (if (not srate)
	;; no src
	(begin
	  (mus-mix *output* file st samps inloc mx envs)
	  (if rev-mx (mus-mix *reverb* revframe st samps inloc rev-mx #f)))
	;; with src
	(let* ((inframe (make-frame in-chans))
	       (outframe (make-frame out-chans))
	       (srcs (make-vector in-chans #f)))
	  (do ((inp 0 (1+ inp)))
	      ((= inp in-chans))
	    (vector-set! srcs inp (make-src :srate srate)))
	  ;; can't use run here yet because 2-dim vect and #f elements
	  (if envs
	      (run 
	       (lambda ()
		 (do ((i st (1+ i)))
		     ((= i nd))
		   (do ((inp 0 (1+ inp)))
		       ((= inp in-chans))
		     (do ((outp 0 (1+ outp)))
			 ((= outp out-chans))
		       (if (and (vector-ref envs inp)
				(env? (vector-ref (vector-ref envs inp) outp)))
			   (mixer-set! mx inp outp (env (vector-ref (vector-ref envs inp) outp)))))))
		 (do ((inp 0 (1+ inp)))
		     ((= inp in-chans))
		   (frame-set! inframe inp (src (vector-ref srcs inp) 0.0 (lambda (dir) (readin (vector-ref file inp))))))
		 (frame->file *output* i (frame->frame inframe mx outframe))
		 (if rev-mx (frame->file *reverb* i (frame->frame inframe rev-mx revframe)))))
	      ;; no envs
	      (run 
	       (lambda ()
		 (do ((i st (1+ i)))
		     ((= i nd))
		   (do ((inp 0 (1+ inp)))
		       ((= inp in-chans))
		     (frame-set! inframe inp (src (vector-ref srcs inp) 0.0 (lambda (dir) (readin (vector-ref file inp))))))
		   (frame->file *output* i (frame->frame inframe mx outframe))
		   (if rev-mx (frame->file *reverb* i (frame->frame inframe rev-mx revframe)))))))))))

!#

#|
  (with-sound (:channels 2 :statistics #t)
    (fullmix "pistol.snd")
    (fullmix "oboe.snd" 1 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5)))))
|#


;;; bes-fm -- can also use bes-j0 here as in earlier versions


(rt-renamefunc bes-j1 j1 <double> (<double>))


(definstrument (bes-fm beg dur freq amp ratio index)
  "(bes-fm beg dur freq amp ratio index) produces J1(J1) imitating FM"
  (let* ((st (inexact->exact (floor (* beg (mus-srate)))))
	 (nd (+ st (inexact->exact (floor (* dur (mus-srate))))))
	 (car-ph 0.0)
	 (mod-ph 0.0)
	 (car-incr (hz->radians freq))
	 (mod-incr (* ratio car-incr))
	 (ampenv (make-env :envelope '(0 0 25 1 75 1 100 0) :scaler amp :duration dur)))
    (<rt-play> beg dur
     (lambda ()
       (out (* (env ampenv) (bes-j1 car-ph)))
       (set! car-ph (+ car-ph (+ car-incr (* index (bes-j1 mod-ph)))))
       (set! mod-ph (+ mod-ph mod-incr))))))

#!
(bes-fm 0 1 440 10.0 1.0 4.0)
!#


;;; ssb-fm
;;; this might be better named "quasi-ssb-fm" -- cancellations are not perfect

(def-clm-struct sbfm 
  (am0 #f :type clm) (am1 #f :type clm) 
  (car0 #f :type clm) (car1 #f :type clm)
  (mod0 #f :type clm) (mod1 #f :type clm))

(define (make-ssb-fm freq)
  "(make-ssb-fm freq) makes an ssb-fm generator"
  (make-sbfm :am0 (make-oscil freq 0)
	     :am1 (make-oscil freq (* 0.5 pi))
	     :car0 (make-oscil 0 0)
	     :car1 (make-oscil 0 (* 0.5 pi))
	     :mod0 (make-hilbert-transform 40)
	     :mod1 (make-delay 40)))

(define (ssb-fm gen modsig)
  "(ssb-fm gen modsig) runs an ssb-fm generator"
  (+ (* (oscil (sbfm-am0 gen)) 
	(oscil (sbfm-car0 gen) (hilbert-transform (sbfm-mod0 gen) modsig)))
     (* (oscil (sbfm-am1 gen)) 
	(oscil (sbfm-car1 gen) (delay (sbfm-mod1 gen) modsig)))))


;;; if all we want are asymmetric fm-generated spectra, we can just add 2 fm oscil pairs:

(define (make-fm2 f1 f2 f3 f4 p1 p2 p3 p4)
  "(make-fm2 f1 f2 f3 f4 p1 p2 p3 p4) makes two FM paired oscils"
  ;; (make-fm2 1000 100 1000 100  0 0  (* 0.5 pi) (* 0.5 pi))
  ;; (make-fm2 1000 100 1000 100  0 0  0 (* 0.5 pi))
  (list (make-oscil f1 p1)
	(make-oscil f2 p2)
	(make-oscil f3 p3)
	(make-oscil f4 p4)))

(define (fm2 gen index)
  "(fm2 gen index) runs an fm2 generator"
  (* .25 (+ (oscil (list-ref gen 0) (* index (oscil (list-ref gen 1))))
	    (oscil (list-ref gen 2) (* index (oscil (list-ref gen 3)))))))


;;; rms gain balance
;;; This is a translation of the rmsgain code provided by Fabio Furlanete.

(def-clm-struct rmsg 
  (c1 0.0 :type float)
  (c2 0.0 :type float)
  (q 0.0 :type float)
  (r 0.0 :type float)
  (avg 0.0 :type float)
  (avgc 0 :type int))

(define* (make-rmsgain :optional (hp 10.0))
  "(make-rmsgain :optional (hp 10.0)) makes an RMS gain generator"
  (let* ((b (- 2.0 (cos (* hp (/ (* 2.0 pi) (mus-srate))))))
	 (c2 (- b (sqrt (- (* b b) 1.0))))
	 (c1 (- 1.0 c2)))
    (make-rmsg :c1 c1 :c2 c2)))

(define (rms gen sig)
  "(rms gen sig) runs an RMS gain generator"
  (set! (rmsg-q gen) (+ (* (rmsg-c1 gen) sig sig)
			(* (rmsg-c2 gen) (rmsg-q gen))))
  (sqrt (rmsg-q gen)))


(define (gain gen sig rmsval)
  "(gain gen sig rmsval) returns the current RMS gain"
  (set! (rmsg-r gen) (+ (* (rmsg-c1 gen) sig sig)
			(* (rmsg-c2 gen) (rmsg-r gen))))
  (let ((this-gain (if (zero? (rmsg-r gen))
		       rmsval
		       (/ rmsval (sqrt (rmsg-r gen))))))
    (set! (rmsg-avg gen) (+ (rmsg-avg gen) this-gain))
    (set! (rmsg-avgc gen) (+ (rmsg-avgc gen) 1))
    (* sig this-gain)))

(define (balance gen signal compare)
  "(balance gen signal compare) scales a signal based on a RMS gain"
  (gain gen signal (rms gen compare)))

(define (gain-avg gen)
  "(gain-avg gen) is part of the RMS gain stuff"
  (/ (rmsg-avg gen) (rmsg-avgc gen)))

(define (balance-avg gen)
  "(balance-avg gen) is part of the RM gain stuff"
  (rmsg-avg gen))
