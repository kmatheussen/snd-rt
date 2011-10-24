
;;3.1. Playing A 3 seconds long sound

(<rt-out> :dur 0 3:-s (oscil))


;;3.2. Coroutines

(<rt-run>
 (define osc (spawn
               (debug "Making a sound.")
               (block (out (oscil)))))
 (wait 5:-s)
 (debug "Exactly 5 seconds later.")
 (stop osc)
 (debug "Now there is silence."))



;;3.3. Sine-wave grain cloud

;;| Changed: Turned off runtime-checks. (using too much CPU)

(<rt-stalin> :runtime-checks #f
 (while #t
   (wait (random 30):-ms)
   (define osc (make-oscil :freq (between 50 2000)))
   (define dur (between 400 2000):-ms)
   (define e (make-env '((0 0)(.5 .05)(1 0)) :dur dur))
   (sound :dur dur
     (out (* (env e) (oscil osc))))))




;;3.4. Making an oscillator GUI using Faust

(<rt-faust>
 (= vol (hslider "volume" .5 .0 1 0.01))
 (= freq (hslider "freq" 1000 0 4000 0.1))
 (out (vgroup "Osc" (* (osci freq) vol))))



;;3.5. Making an oscillator GUI using “RT”

(<rt-out>
 (* (<slider> "Amp" 0 0.5 1)
    (oscil* (<slider> "Freq" 20 200 4000 :log #t))))



;;3.6. Sequential operation
;;The following block of code shows how to play a pure
;;tone for 5 seconds, followed by a 6 seconds long square
;;sound, only using features provided by the language and
;;not having to schedulemore than one<realtime>instance:

;;| Changed: Changed frequency of the square sound to better
;;           hear the difference.

(<rt-run> (block :dur 5:-s (out (oscil)))
          (block :dur 6:-s (out (square-wave :freq 500))))



;;Due to the regularity of the above block of code, it’s
;;tempting to write this general seq low-level Lisp macro:

;;| Changed: (car rest) and (cadr rest) were swapped.

(define-rt-macro (seq . rest)
  `(begin ,@(let loop ((rest rest))
              (if (null? rest) '()
                  (cons `(block :dur ,(cadr rest)
                           (out ,(car rest)))
                        (loop (cddr rest)))))))

;;And by using this macro, the operation can be shortened
;;into:

(<rt-run> (seq (oscil) 5:-s (square-wave :freq 500) 6:-s))


;;3.7. Parallel operation
;;By using the seq macro from the sequential example, it’s
;;just spawning a new coroutine for each sound to run the 5
;;second pure tone and the 6 second square sound in parallel,
;;still without having to schedule more than one
;;<realtime> instance:


;;| Changed: Changed frequency of the square sound to better
;;           hear the difference, plus fixed paranthesis.

(<rt-run> (spawn (seq (oscil) 5:-s))
          (spawn (seq (square-wave :freq 500) 6:-s)))

;;Like in the example with the sequential operation, it’s
;;possible to write a small Lisp par macro to make parallel
;;operations less verbose.



;;3.8. Midi synthesizer
;;This example shows how to implement a very simple polyphonic
;;midi soft synth:
(<rt-stalin>
 (while #t
   (wait-midi :command note-on
     (define osc
       (make-oscil :freq (midi-to-freq (midi-note))))
     (spawn
       (define player
         (spawn (block (out (* (midi-vol) (oscil osc))))))
       (wait-midi :command note-off :note (midi-note)
         (stop player))))))

(<rt-stalin>
 (while #t
   (wait-midi :command note-on
     (define osc
       (make-oscil :freq (midi-to-freq (midi-note))))
     (spawn
       (define player (sound (out (* (midi-vol) (oscil osc)))))
       (wait-midi :command note-off :note (midi-note)
         (stop player))))))

(<rt-stalin>
 (sound (out (random 0.5))))

(<rt-stalin>
 (make-bus)
 (wait 20:-s))


(<rt-stalin>
 (while #t
   (wait-midi :command note-on
     (pool ((note midi-note))
      (define osc
        (make-oscil :freq (midi-to-freq note)))
      (run
       (spawn
         (define player
           (spawn (block (out (* (midi-vol) (oscil osc))))))
         (wait-midi :command note-off :note (midi-note)
           (stop player))))))))

(define-stalin (midi-synth)
  (while #t
    (wait-midi :command note-on
      (define adsr (make-adsr))
      (define osc  (make-oscil :freq (midi-to-freq (midi-note))))
      (sound
        (if ((adsr 'stopped?))
            (stop)
            (out (* 0.2 ((adsr 'run)) (midi-vol) (oscil osc)))))
      (spawn
        (wait-midi :command note-off :note (midi-note)
          ((adsr 'stop)))))))

(<rt-stalin>
 (midi-synth))

;;3.9. San Dysth
;;This example shows a larger example doing sample by
;;sample processing.
;;San Dysth [5] is a standalone realtime midi soft synthesizer
;;written in Snd using the Snd-Rt language. San
;;Dysth’s synthesis technique works by using a set of rules
;;which change state for each sample.
;;The function san-dysth-dsp provides the main synthesis
;;routine for San-dysth. san-dysth-dsp returns one sample
;;per call:

(define-rt (san-dysth-dsp direction)
  (cond ((<= val −1) (set! inc-addval #t))
        ((>= val 1) (set! inc-addval #f))
        ((> addval max-add-val)
         (set! periodcounter period)
         (set! inc-addval #f))
        ((< addval (− max-add-val))
         (set! periodcounter period)
         (set! inc-addval #t))
        ((= 0 (inc! periodcounter −1))
         (set! periodcounter period)
         (set! inc-addval (not inc-addval))))
  (define drunk-change (random max-drunk-change))
  (set! addval (filter das-filter (if inc-addval
                                      drunk-change
                                      (− drunk-change))))
  (inc! val addval))

;;For a complete implementation of this routine, please
;;refer to [5] or San Dysth’s homepage.


;;; testing
(<rt-stalin>
 (define osc (make-oscil :freq 440))
 (block :dur 1:-s
   ;;(debug "gakk")
   ;;(out (random 0.9))
   (out (* 0.4 (oscil osc)))
   (* 5 2))
 (debug "hello")
 ;(out 0.5)
 ;(wait 84100)
 )


;; sound hello world
(<rt-stalin> :runtime-checks #f
 (define osc (make-oscil :freq 440.0))
 (sound (out (oscil osc))))

(<rt-stalin>
 (define osc (make-oscil :freq 440.0))
 (out (oscil osc))
 (debug "hello"))

(<rt-stalin>
 (define osc (make-oscil :freq 440.0))
 (let loop ()
   (out (* 0.1 (oscil osc)))
   (wait 1)
   (loop)))




   (debug "hello")))

 (define osc (make-oscil :freq 440.0))
 (out (oscil osc))
 (debug "hello"))



