;;
;; Code examples from the LAC2010 paper
;; "Implementing a Polyphonic MIDI Software Synthesizer using Coroutines, Realtime Garbage Collection,
;;  Closures, Auto-allocated Variables, Dynamic Scoping, and Continuation Passing Style Programming"
;;
;;
;; Public domain.



;;
;; Chapter 3:
;;
(<rt-stalin>
  (let ((phase 0.0))
    (sound
      (out (sin phase))
      (inc! phase (hz->radians 400)))))


;;
;; Chapter 4:
;;
;; Changed: * fixed paranthesis
;;          * note-num -> ,note-num
;;          * added ":runtime-checks #f" option.
;;
;; Warning: * Takes extremely long time to compile!
;;          * Didn't even compile when increasing tar-atomic-heap-size to 400 MB!
;;            (giving up)
;; 
(range note-num 0 128
  (<rt-stalin> :runtime-checks #f
   (define phase 0.0)
   (define volume 0.0)
   (sound
     (out (* volume (sin phase)))
     (inc! phase (midi->radians ,note-num)))
   (while #t
     (wait-midi :command note-on :note ,note-num
       (set! volume (midi-vol)))
     (wait-midi :command note-off :note ,note-num
       (set! volume 0.0)))))


;;
;; Chapter 5:
;;
;; Changed: * fixed paranthesis
;;          * added ":runtime-checks #f" option.
;;            (still too heavy for my CPU though)
;;
(<rt-stalin> :runtime-checks #f
  (range note-num 0 128
   (spawn
     (define phase 0.0)
     (define volume 0.0)
     (sound
       (out (* volume (sin phase)))
       (inc! phase (midi->radians note-num)))
     (while #t
       (wait-midi :command note-on :note note-num
         (set! volume (midi-vol)))
       (wait-midi :command note-off :note note-num
         (set! volume 0.0))))))


;;
;; Chapter 6:
;;
(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on
      (define osc  (make-oscil :freq (midi->hz (midi-note))))
      (define tone (sound (out (* (midi-vol) (oscil osc)))))
      (spawn
	(wait-midi :command note-off :note (midi-note)
	  (stop tone))))))

(<rt-stalin>
 (softsynth))



;;
;; Chapter 7:
;;
(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on
      (define tone
	(sound (out (* (midi-vol)
		       (oscil :freq (midi->hz (midi-note)))))))
      (spawn
        (wait-midi :command note-off :note (midi-note)
          (stop tone))))))
(<rt-stalin>
 (softsynth))



;;
;; Chapter 8:
;;
;; Changed: * reduced wet amount from 0.09 to 0.01
;;
(define-stalin (reverb input)
  (delay :size (* .013 (mus-srate))
    (+ (comb :scaler 0.742 :size  9601 allpass-composed)
       (comb :scaler 0.733 :size 10007 allpass-composed)
       (comb :scaler 0.715 :size 10799 allpass-composed)
       (comb :scaler 0.697 :size 11597 allpass-composed)
       :where allpass-composed
         (send input :through 
           (all-pass :feedback -0.7 :feedforward 0.7)
           (all-pass :feedback -0.7 :feedforward 0.7)
           (all-pass :feedback -0.7 :feedforward 0.7)
           (all-pass :feedback -0.7 :feedforward 0.7)))))

(define-stalin bus (make-bus))

(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on
      (define tone
        (sound
          (write-bus bus
                     (* (midi-vol)
                        (oscil :freq (midi->hz (midi-note)))))))
      (spawn
        (wait-midi :command note-off :note (midi-note)
          (stop tone))))))

(define-stalin (fx-ctrl input clean wet processor)
  (+ (* clean input)
     (* wet (processor input))))

(<rt-stalin>
 (spawn
   (softsynth))
 (sound
   (out (fx-ctrl (read-bus bus)
		 0.5 0.01
		 reverb))))


;;
;; Chapter 9:
;;
;; Changed: * reduced wet amount from 0.09 to 0.01
;;
(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on
      (define tone
	(sound (out (* (midi-vol)
		       (oscil :freq (midi->hz (midi-note)))))))
      (spawn
        (wait-midi :command note-off :note (midi-note)
          (stop tone))))))

(<rt-stalin>
  (sound
    (out (fx-ctrl (in (softsynth)) 
	 	  0.5 0.01
		  reverb))))


;;
;; Chapter 10:
;;
;; Changed: * reduced wet amount from 0.09 to 0.01
;;
(define-stalin (stereo-pan input c)
  (let* ((sqrt2/2 (/ (sqrt 2) 2))
	 (angle (- pi/4 (* c pi/2)))
	 (left  (* sqrt2/2 (+ (cos angle) (sin angle))))
	 (right (* sqrt2/2 (- (cos angle) (sin angle)))))
    (out 0 (* input left))
    (out 1 (* input right))))

(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on
      (define tone
	(sound
	  (stereo-pan (* (midi-vol)
			 (oscil :freq (midi->hz (midi-note))))
		      (/ (midi-note) 127.0))))
      (spawn
        (wait-midi :command note-off :note (midi-note)
	  (stop tone))))))

(<rt-stalin>
  (sound
    (in (softsynth)
	(lambda (sound-left sound-right)
	  (out 0 (fx-ctrl sound-left  0.5 0.01 reverb))
	  (out 1 (fx-ctrl sound-right 0.5 0.01 reverb))))))



;;
;; Chapter 10.3:
;;
;; Changed: * reduced wet amount from 0.09 to 0.01
;;
(define-stalin (Reverb delay-length)
  (Seq (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (all-pass :feedback -0.7 :feedforward 0.7)
       (Sum (comb :scaler 0.742 :size 9601)
	    (comb :scaler 0.733 :size 10007)
	    (comb :scaler 0.715 :size 10799)
	    (comb :scaler 0.697 :size 11597))
       (delay :size delay-length:-s)))

(define-stalin (Stereo-pan c)
  (define gakk 9)
  (Split Identity
	 (* left)
	 (* right)
    :where left  (* sqrt2/2 (+ (cos angle) (sin angle)))
    :where right (* sqrt2/2 (- (cos angle) (sin angle)))
    :where angle (- pi/4 (* c pi/2))
    :where sqrt2/2 (/ (sqrt 2) 2)))
  
(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on       
      (define tone 
	(gen-sound
	   (Seq (oscil :freq (midi->hz (midi-note)))
		(* (midi-vol))
		(Stereo-pan (/ (midi-note) 127)))))
      (spawn
	(wait-midi :command note-off :note (midi-note)
	  (stop tone))))))

(define-stalin (Fx-ctrl clean wet Fx)
  (Sum (* clean)
       (Seq Fx
	    (* wet))))

(<rt-stalin>
  (gen-sound
    (Seq (In (softsynth))
	 (Par (Fx-ctrl 0.5 0.01 (Reverb 0.13))
	      (Fx-ctrl 0.5 0.01 (Reverb 0.11))))))



;;
;; Chapter 11:
;;
(define-stalin (softsynth)
  (while #t
    (wait-midi :command note-on       
      (gen-sound :while (-> adsr is-running)
        (Seq (Prod (oscil :freq (midi->hz (midi-note)))
		   (midi-vol)
		   (-> adsr next))
	     (Stereo-pan (/ (midi-note) 127))))
      (spawn
	(wait-midi :command note-off :note (midi-note)
	  (-> adsr stop)))      
      :where adsr (make-adsr :a 20:-ms
			     :d 30:-ms
			     :s 0.2 
			     :r 70:-ms))))

(define midi-filename "/home/kjetil/Malaguena.mid")
(define midi-filename "/gammelhd/gammelhd/home/kjetil/mus220c/sinclair.MID")

(<rt-stalin>
 (gen-sound
  (Seq (In (softsynth))
       (Par (Fx-ctrl 0.5 0.01 (Reverb 0.03))
            (Fx-ctrl 0.5 0.01 (Reverb 3.31))))))

(system (<-> "aplaymidi --port=129 " midi-filename "&"))

(begin
  (tar_bench_print (get_last_rt_heap))
  (rte-silence!))




