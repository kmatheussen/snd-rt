
;;
;; This is the midi synth example from the LAC2010 paper
;;
;; Changed: * reduced wet amount from 0.09 to 0.01
;;          * reduced clean from 0.5 to 0.2 
;;          * Increased sustain to 0.6
;;
;;

(define-stalin (Reverb delay-length)
  (Seq (all-pass :feedback -0.7 :feedforward 0.7 :size 1051)
       (all-pass :feedback -0.7 :feedforward 0.7 :size 337)
       (all-pass :feedback -0.7 :feedforward 0.7 :size 113)
       (Sum (comb :scaler 0.742 :size 9601)
	    (comb :scaler 0.733 :size 10007)
	    (comb :scaler 0.715 :size 10799)
	    (comb :scaler 0.697 :size 11597))
       (delay :size delay-length:-s)))
;;(define-stalin (Reverb delay-length)
;;  (Seq (* 0.2)))

(define-stalin (Stereo-pan c)
  (define gakk 9)
  (Split Identity
	 (* left)
	 (* right)
    :where left  (* sqrt2/2 (+ (cos angle) (sin angle)))
    :where right (* sqrt2/2 (- (cos angle) (sin angle)))
    :where angle (- pi/4 (* c pi/2))
    :where sqrt2/2 (/ (sqrt 2) 2)))
  
(define-stalin (Fx-ctrl clean wet Fx)
  (Sum (* clean)
       (Seq Fx
	    (* wet))))

(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on       
      (gen-sound :while (-> adsr is-running)
        (Seq (Prod (square-wave :frequency (midi->hz (midi-note)))
		   (midi-vol)
		   (-> adsr next))
	     (Stereo-pan (/ (midi-note) 127))))
      (spawn
	(wait-midi :command note-off :note (midi-note)
	  (-> adsr stop)))      
      :where adsr (make-adsr :a 20:-ms
			     :d 40:-ms
			     :s 0.2
			     :r 10:-ms))))

;;(define midi-filename "/home/kjetil/Malaguena.mid")
(define midi-filename "/gammelhd/gammelhd/home/kjetil/mus220c/sinclair.MID")

#!
 (seed 500)
 (spawn
  (range i 1 2000
    (wait (random 30):-ms)
    (define osc (make-triangle-wave :frequency (between 50 2000)))
    (define dur (between 400 2000):-ms)
    (define e (make-env '((0 0)(.5 .05)(1 0)) :dur dur))
    (sound :dur dur
      (out (* (env e) (triangle-wave osc))))))

  (spawn
    (wait 20:-s)
    (range i 1 2000
      (wait (random 30):-ms)
      (define osc (make-triangle-wave :frequency (between 50 2000)))
      (define dur (between 400 2000):-ms)
      (define e (make-env '((0 0)(.5 .05)(1 0)) :dur dur))
      (sound :dur dur
        (out (* (env e) (triangle-wave osc))))))

  (spawn
    (wait 40:-s)
    (range i 1 2000
      (wait (random 30):-ms)
      (define osc (make-triangle-wave :frequency (between 50 2000)))
      (define dur (between 400 2000):-ms)
      (define e (make-env '((0 0)(.5 .05)(1 0)) :dur dur))
      (sound :dur dur
        (out (* (env e) (triangle-wave osc))))))

!#
  
(<rt-stalin> :runtime-checks #f
 (seed 500)

 ;; triangle-wave grain cloud
 (spawn
   (wait 5:-s)
   (range i 1 500
     (wait (random 30):-ms)
     (define osc (make-triangle-wave :frequency (between 50 2000)))
     (define dur (between 400 2000):-ms)
     (define e (make-env '((0 0)(.5 .05)(1 0)) :dur dur))
     (sound :dur dur
       (out (* (env e) (triangle-wave osc))))))

 ;; Insect swarm
 (spawn
   (wait 20:-s)
   (range i 1 400
     (wait (between 0.01:-s 0.03:-s))
     (spawn
       (define dur (between 0.4 1.6))
       (define frequency (between 600 8000))
       (define amplitude (between 0.03 0.2))
       (define amp-env '(0 0 25 1 75 .7 100 0))
       (define mod-freq (between 30 80))
       (define mod-skew (between -30.0 -2.0))
       (define mod-freq-env '(0 0 40 1 95 1 100 .5))
       (define mod-index (between 250 700.0))
       (define mod-index-env '(0 1 25 .7 75 .78 100 1))
       (define fm-index (between 0.003 0.700))
       (define fm-ratio .500)
       
       (define degree 45.0)
       (define distance 1.0)
       (define reverb-amount 0.005)
       (let* ((carrier (make-oscil :frequency frequency))
              (fm1-osc (make-oscil :frequency mod-freq))
              (fm2-osc (make-oscil :frequency (* fm-ratio frequency)))
              (ampf (make-env amp-env :scaler amplitude :duration dur))
              (indf (make-env mod-index-env :scaler (hz->radians mod-index) :duration dur))
              (modfrqf (make-env mod-freq-env :scaler (hz->radians mod-skew) :duration dur))
              (fm2-amp (hz->radians (* fm-index fm-ratio frequency))))
         (sound :dur dur:-s
           (let* ((garble-in (* (env indf)
                                (oscil fm1-osc (env modfrqf))))
                  (garble-out (* fm2-amp (oscil fm2-osc garble-in))))
             (out (* (env ampf) 
                     (oscil carrier (+ garble-out garble-in))))))))))

 ;; pluck strings (jaffe-smith algorithm)
 (spawn
   (wait 40:-s)
   (range i 1 2000
     (wait (random 0.01):-s)
     (define dur (between 0.1 1.2))
     (define freq (between 50 400))
     (define amp (between 0.3 0.8))
     (define weighting (between 0.2 0.5))
     (define lossfact (between 0.9 0.99))
     
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
            (lf (if (= lossfact 0.0) 1.0 (min 1.0 lossfact)))
            (wt (if (= wt0 0.0) 0.5 (min 1.0 wt0)))
            (tab (make-vct dlen))
            ;; get initial waveform in "tab" -- here we can introduce 0's to simulate different pick
            ;; positions, and so on -- see the CMJ article for numerous extensions.  The normal case
            ;; is to load it with white noise (between -1 and 1).
            (allp (make-one-zero (* lf (- 1.0 wt)) (* lf wt)))
            (feedb (make-one-zero c 1.0)) ;or (feedb (make-one-zero 1.0 c))
            (ctr 0))
       
       (range i 0 dlen
         (vct-set! tab i (- 1.0 (random 2.0))))
       
       (sound :dur dur:-s
         (let ((val (vct-ref tab ctr)))	;current output value
           (vct-set! tab ctr (* (- 1.0 c) 
                                (one-zero feedb 
                                          (one-zero allp val))))
           (set! ctr (+ ctr 1))
           (if (>= ctr dlen) (set! ctr 0))
           (out (* amp val)))))))
 
 
 ;; Midi synthesizer (plays in the background all the time)
 (gen-sound
  (Seq (In (softsynth))
       (Par (Fx-ctrl 0.3 0.04 (Reverb 0.03))
            (Fx-ctrl 0.3 0.04 (Reverb 0.31))))))

(begin
  (system (<-> "aplaymidi --port=129 " midi-filename ""))
  (sleep 3) ;; wait for reverb and decay
  (rte-silence!)
  (tar_bench_print (get_last_rt_heap))
  (tar_end_fragmentation_analysis (get_last_rt_heap)))

#!
(tar_delete_heap (get_last_rt_heap) 1)
(get_last_rt_heap)
(eval-c ""
	(proto->public "void tar_bench_print(tar_heap_t *heap)")
	(proto->public "void tar_touch_heaps(tar_heap_t *heap)")
        (proto->public "void tar_end_fragmentation_analysis(tar_heap_t *heap)")
	)
!#



