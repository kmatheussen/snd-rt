
;; ladspa.scm
;; Use ladspa plugins with the help of a mouse. -Kjetil S. Matheussen.

;; Changes 9.8.2003 ->
;; Many changes.
;;
;; Changes 8.8.2003 -> 9.8.2003
;; -Added some workaround code for vst plugins. Previously, applying didn't work.
;; -Possible to use plugins without input audio ports.
;; -Channels were reversed. Fixed
;; -Applying to a single channel allways made the script apply to channel 0. Fixed.
;; -Try to fix dialog-proportions after opening.


;; Changes 5.8.2003 -> 8.8.2003
;; -Fixed segfaults when exiting.
;; -Improved the apply function.
;; -Use the help text from new-plugins.scm written by Dave Phillips.
;; -Fixed the sliders so that sound is updated also when changing
;;  position. Not only after releasing.
;; 

;; ladspa.scm -> ladspa2.scm:
;; -Making heavy use of the new ladspa-functions in snd 6.11.
;; -Realtime preview works perfectly. Seems to at least.
;; -Many plugins can be previewed at the same time.
;; -Previewing is automaticly looped.
;; -The plugins with more than one input or output audio port work.
;; -The default values are based on ladspa hints.
;; -Toggle ports uses toggle buttons instead of sliders.
;; -Integer ports uses integer sliders instead of float sliders.
;; -Initialisation is probably faster.
;; -Added the hour-glass progress reporter when applying.
;;
;; Problems:
;; -Theres a lot of (if (not (string=? "vst" libname)) tests. This is a workaround
;;  to make a vst plugin remember parametersettings even when changed inside a native gui.
;;  However, this can lead to problems when the number of channels for the
;;  selection is changed from what was the situation when
;;  the dialog was opened. Doing a workaround for that situation too is messy as well.
;; -The Model-E vst plugin have no less than 513 input control ports,
;;  and makes the script stop because snd is out of stack. There might
;;  be something wrong with the script. Don't know scheme too well.
;; -In some resize-positions, some widgets becomes invisible.
;; -Realtime preview wont work if number of frames sent to the
;;  dac-hook is more than 8192. (minor problem probably, and is solved
;;  by increasing "ladspa-maxbuf" to something larger than 8192).




(provide 'snd-ladspa.scm)



;; Increase this number if you can't preview sound because of large latency in the system.
;; Note, this is not the latency, just the maximum buffer size. The only bad consequence about
;; increasing this number, is the increased use of memory.

(define ladspa-maxbuf 8192)



(if (not (provided? 'snd-gui.scm))
    (load-from-path "gui.scm"))


(c-load-from-path eval-c)


;; Organize help texts.
(c-load-from-path ladspa-help)

(define ladspa-help-assoclist '())

(define (insert-ladspa-help alist)
  (if (not (null? alist))
      (begin
	(set! ladspa-help-assoclist 
	      (cons (list (string-append (car alist) (cadr alist))
			  (caddr alist) (cadddr alist))
		    ladspa-help-assoclist))
	(insert-ladspa-help (cddddr alist)))))

(insert-ladspa-help ladspa-help-texts)


(define ladspa-effects-menu (add-to-main-menu "Ladspa"))


(define num-running-ladspas 0)
(define (ladspa-add-num-instance!)
  (if (and (defined? 'c-use-rt-player?)
	   (c-use-rt-player?)
	   (-> (c-p) isplaying))
      (begin
	(-> (c-p) pause)
	(set! num-running-ladspas (1+ num-running-ladspas))
	(-> (c-p) continue))
      (set! num-running-ladspas (1+ num-running-ladspas))))
(define (ladspa-remove-num-instance!)
  (set! num-running-ladspas (max 0 (1- num-running-ladspas))))

(def-class (<ladspa> libname plugname)

  (def-var descriptor (ladspa-descriptor libname plugname))
 
  ;; <array> of ladspa plugin handles.
  (define handles #f)

  ;; These are lists of numbers.
  (def-var input-controls '())
  (def-var output-controls '())

  (define input-audios '())
  (define output-audios '())

  ;; Is #t if ladspa plugin has no audio inputs.
  (define no_audio_inputs #f)

  ;; A ladspa plugin does not need to have equal number of audio
  ;; inputs and outputs. This variable holds the mininum of the two.
  ;; Except when no_audio_inputs is #t. Then it holds the number
  ;; of audio-outputs instead.
  (define min_num_audios 0)


  ;; <array> of vcts. All handles use the same vcts.
  (define ports #f)


  (define isopened #f)


  (define* (open #:optional (num_handles 1))
    (-> ports map! (lambda (n port) (or port 
					(make-vct ladspa-maxbuf))))
    (call-with-current-continuation
     (lambda (return)
       (set! handles (<array/map> num_handles
				  (lambda (n)
				    (let ((handle (ladspa-instantiate descriptor (srate) )))
				      (if (not handle)
					  (begin
					    (c-display "Error: Could not make ladspa handle.")
					    (this->close)
					    (return #f))
					  (begin
					    (-> ports for-each (lambda (n port)
								 (ladspa-connect-port descriptor handle n port)))
					    (ladspa-activate descriptor handle)))
				      handle))))))
    #t)


  ;; Close all handles.
  (def-method (close)
    ;;(display "close called ")(display (-> handles length))(newline)
    (if handles
	(-> handles for-each (lambda (n handle)
			       (if handle
				   (begin
				     (ladspa-deactivate descriptor handle)
				     (ladspa-cleanup descriptor handle))))))
    (set! handles #f))


  (define (get-port portnum)
    (ports portnum))

  ;; Not used.
  (define (set-port! portnum vct)
    (display "ai: ")(display handle)(newline)
    (ports portnum vct)
    (ladspa-connect-port descriptor handle portnum vct)) ;; Which handle???

  (define (get-num-input-audio-ports)
    (length input-audios))
  (define (get-input-audio-port num)
    (get-port (list-ref input-audios num)))
  (define (set-input-audio-port num vct)
    (set-port! (list-ref input-audios num) vct))

  (define (get-num-output-audio-ports)
    (length output-audios))
  (define (get-output-audio-port num)
    (get-port (list-ref output-audios num)))
  (define (set-output-audio-port num vct)
    (set-port! (list-ref output-audios num) vct))


  ;; Warning, len must not be larger than ladspa-maxbuf.
  (define (run len)
    (-> handles for-each (lambda (n handle)
			   (ladspa-run descriptor handle len))))



  (def-method (set-default-input-controls)
    ;;;; There is a serious bug regarding getting default values using lrdf. Disabled for now.
    ;;(let* ((def-uri (lrdf_get_default_uri (.UniqueID descriptor)))
    ;;   (defs (and def-uri (lrdf_get_setting_values def-uri)))
    ;;   (def-count (and defs (lrdf_defaults_count defs)))
    ;;   (def-vals (and defs (-> (<array/map> def-count (lambda (n) (cons (lrdf_defaults_pid defs n) (lrdf_defaults_value defs n)))) get-list))))
    ;;(if defs (lrdf_free_setting_values defs))
      (for-each (lambda (x)
		  (let ((hint (car (x 1)))
			(lo (cadr (x 1)))
			(hi (caddr (x 1))))
		    (define (ishint dashint)
		      (= (logand hint LADSPA_HINT_DEFAULT_MASK) dashint))
		    (define (ishint_notdefault dashint)
		      (not (= (logand hint dashint ) 0)))
		    (this 'input-control-set! (x 0) 
			  (cond ;;((and def-vals (assoc (x 0) def-vals )) (cdr (assoc (x 0) def-vals)))
				((ishint LADSPA_HINT_DEFAULT_0) 0)
				((ishint LADSPA_HINT_DEFAULT_MINIMUM) lo)
				((ishint LADSPA_HINT_DEFAULT_LOW) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
								      (exp (+ (* 0.75 (log lo)) (* 0.25 (log hi))))
								      (+ (* 0.75 lo) (* 0.25 hi))))
				((ishint LADSPA_HINT_DEFAULT_1) 1)
				((ishint LADSPA_HINT_DEFAULT_MAXIMUM) hi)
				((ishint LADSPA_HINT_DEFAULT_HIGH) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
								       (exp (+ (* 0.75 (log hi)) (* 0.25 (log lo))))
								       (+ (* 0.75 hi) (* 0.25 lo))))
				((ishint LADSPA_HINT_DEFAULT_MIDDLE) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
									 (exp (+ (* 0.5 (log hi)) (* 0.5 (log lo))))
									 (+ (* 0.5 hi) (* 0.5 lo))))
				((ishint LADSPA_HINT_DEFAULT_100) 100)
				((ishint LADSPA_HINT_DEFAULT_440) 440)
				((ishint LADSPA_HINT_SAMPLE_RATE) (srate))
				(else
				 (/ (+ lo hi) 2))))))
		
		(map (lambda (x) (<array> x (list-ref (.PortRangeHints descriptor) x)))
		     input-controls)))
  ;;)
  
  
  (define (minimum-num-handles sndchannels pluginchannels)
    (+ (if (> (modulo sndchannels pluginchannels) 0) 1 0)
       (c-integer (/ sndchannels pluginchannels))))



  ;; This one always return #f because it can be used as a hook.
  (define apply-soundobject
    (let ((len #f)
	  (num_chans #f)
	  (chan #f))
      (lambda (sdobj)
	(set! len (sound-data-length sdobj))
	(set! num_chans (sound-data-chans sdobj))
	(if (> len ladspa-maxbuf)
	    (begin 
	      (display "Ladspa buffer to small. Can't use ladspa plugin in realtime.")
	      (display "You can try to reduce the dac-size to fix the problem.")
	      (newline))
	    (begin
	      (set! chan 0)
	      (-> handles for-each (lambda (handlenum handle)
				     (if (not no_audio_inputs)
					 (c-for 0 < min_num_audios 1
						(lambda (n)
						  (if (< (+ chan n) num_chans)
						      (sound-data->vct sdobj
								       (+ chan n)
								       (get-input-audio-port n))))))
				     (ladspa-run descriptor handle len)
				     (c-for 0 < min_num_audios 1
					    (lambda (n)
					      (if (< (+ chan n) num_chans)
						  (vct->sound-data (get-output-audio-port n)
								   sdobj
								   (+ chan n)))))
				     (set! chan (+ chan min_num_audios))))))
	#f)))


  (def-method (apply! #:optional parset-func)
    (define (get-startchan snd chan)
      (if (selection-member? snd chan)
	  chan
	  (get-startchan snd (+ chan 1))))
    (if (not (selection-member? (selected-sound)))
	(select-all (selected-sound)))
    (let* ((snd (selected-sound))
	   (start (selection-position snd))
	   (length (selection-frames snd))
	   (end (+ start length))
	   (startchan #f)
	   (chans #f)
	   (tempfilename #f)
	   (new-file #f)
	   (buflen (if parset-func 32 ladspa-maxbuf))
	   (vct-out (make-vct buflen))
	   (sdobj #f)
	   (isbreaked #f)
	   (readers '()))
      
      (define (apply-open)
	(if (not (string=? "vst" libname))
	    (open (minimum-num-handles chans min_num_audios))
	    #t))

      (c-for 0 < (channels snd) 1
	     (lambda (ch)
	       (if (selection-member? snd ch)
		   (if (not startchan)
		       (begin
			 (set! startchan ch)
			 (set! chans 1))
		       (set! chans (1+ (- ch startchan)))))))
      (if (not startchan)
	  (begin
	    (set! startchan 0)
	    (set! chans (chans snd))))

      (set! sdobj (make-sound-data chans buflen))

      (if (not (apply-open))
	  (begin
	    (display "Could not start plugin.")
	    (newline))
	  (begin
	    
	    ;; Set up sample readers for each channel.
	    (c-for 0 < chans 1
		   (lambda (ch)
		     (set! readers (cons (make-sample-reader start
							     snd
							     (+ startchan ch))
					 readers))))
	    (set! readers (reverse readers))


	    ;; Create a temporary file.
	    (set! tempfilename (snd-tempnam))
	    (set! new-file (mus-sound-open-output tempfilename (srate snd) chans #f #f)) ; Bill S 2-Dec-06

	    ;; Start the hour-glass
	    (start-progress-report)


	    ;; Do the ladspa.
	    (c-for 0 < (+ buflen length) buflen
		   (lambda (n)
		     (if (< n length)
			 (let ((len (min (- length n)
					 buflen)))
			   
			   ;;(display "N: ")(display n)
			   ;;(display " len: ")(display len)
			   ;;(display " length: ")(display length)(newline)

			   (if (or isbreaked (c-g?))
			       (if (not isbreaked) (set! isbreaked #t))
			       (begin
			       
				 ;; Update hour-glass
				 (progress-report (/ n length) snd)
				 
				 ;; The length of sdobj must be the length of the data. Can only happen at last iteration.
				 (if (< len buflen)
				     (begin
				       (set! sdobj (make-sound-data chans len))
				       (set! vct-out (make-vct len))))

				 ;; Reading data into soundobject from soundfile.
				 (if (not no_audio_inputs)
				     (c-for-each (lambda (ch reader)
						   (vct-map! vct-out
							     (lambda () (next-sample reader)))
						   (vct->sound-data vct-out sdobj ch))
						 readers))

				 ;; Automation
				 (if parset-func
				     (parset-func snd (+ start n)))

				 ;; Process soundobject
				 (apply-soundobject sdobj)
				 
				 ;; Writing data from soundobject into temporary file.
				 (mus-sound-write new-file
						  0
						  (1- len)
						  chans
						  sdobj)))))))
	   

	    ;; Close temporary file.
	    (mus-sound-close-output new-file (* chans (* (mus-bytes-per-sample mus-out-format) length)))


	    ;; Let snd know about the new file.
	    (if (defined? 'c-report-and-clear)
		(c-report-and-clear "Please wait, inserting new data."))

	    (if (not isbreaked)
		(c-for 0 < chans 1
		       (lambda (ch)
			 (set-samples start length tempfilename snd (+ ch startchan) #f (string-append "ladspa: " libname "/" plugname) ch))))
	    

	    ;; Close hour-glass
	    (finish-progress-report)


	    ;; Close plugin.
	    (if (not (string=? "vst" libname))
		(this->close))))))


      


  (def-method (add-dac-hook!)
    (let* ((num_channels (channels (selected-sound)))
	   (num_ins (length input-audios))
	   (num_outs (length output-audios)))
      (define (init-dac-hook-stuff)
	(if (not (open (minimum-num-handles num_channels min_num_audios)))
	    #f
	    (begin
	      (ladspa-add-num-instance!)
	      (add-hook! dac-hook apply-soundobject)
	      #t)))
      (if (= 0 min_num_audios)
	  (begin
	    (c-display "Ladspa plugin has no output audio ports.")
	    #f)
	  (if (not (string=? "vst" libname))
	      (init-dac-hook-stuff)
	      (if (not handles)
		  (init-dac-hook-stuff)
		  (begin
		    (ladspa-add-num-instance!)
		    (add-hook! dac-hook apply-soundobject)
		    #t))))))


  (def-method (remove-dac-hook!)
    (ladspa-remove-num-instance!)
    (remove-hook! dac-hook apply-soundobject)
    (if (not (string=? "vst" libname))    
	(this->close)))

  (def-method (input-control-set! num val)
    (vct-set! (ports num) 0 val))

  (def-method (get-input-control num)
    (vct-ref (ports num) 0))


  (def-method (get-hint portnum)
    (car (list-ref (.PortRangeHints descriptor) portnum)))

  (define (ishint portnum dashint)
    (not (= (logand (this->get-hint portnum) dashint) 0)))

  (def-method (get-lo portnum)
    (* (if (ishint portnum LADSPA_HINT_SAMPLE_RATE)
	   (srate)
	   1)
       (if (not (ishint portnum LADSPA_HINT_BOUNDED_BELOW))
	   0                                   ;The value Ardour use.
	   (cadr (list-ref (.PortRangeHints descriptor) portnum)))))

  (def-method (get-hi portnum)
    (* (if (ishint portnum LADSPA_HINT_SAMPLE_RATE)
	   (srate)
	   1)
       (if (not (ishint portnum LADSPA_HINT_BOUNDED_ABOVE))
	   (if (not (ishint portnum  LADSPA_HINT_TOGGLED))
	       4                                   ;The value Ardour use.
	       1)
	   (caddr (list-ref (.PortRangeHints descriptor) portnum)))))


  ;; Constructor:
  (if (not descriptor)
      (set! this #f)
      (begin
	(c-for-each (lambda (n x)
		      (if (> (logand x LADSPA_PORT_CONTROL) 0)
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! input-controls (append input-controls (list n)))
			      (set! output-controls (append output-controls (list n))))
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! input-audios (append input-audios (list n)))
			      (set! output-audios (append output-audios (list n))))))
		    (.PortDescriptors descriptor))
	(if (= (length input-audios) 0)
	    (begin
	      (set! min_num_audios (length output-audios))
	      (set! no_audio_inputs #t))
	    (set! min_num_audios (min (length input-audios) (length output-audios))))
	(set! ports (apply <array> (map (lambda (x) (if (> (logand x LADSPA_PORT_CONTROL) 0)
							(make-vct 1)
							#f))
					(.PortDescriptors descriptor))))
	(this->set-default-input-controls))))



(define (install-ladspa-menues)  

  (define das-ladspa-list #f)

  (define (get-ladspa-list)
    (if (not das-ladspa-list)
	(set! das-ladspa-list (sort (map (lambda (listpart) (list (ladspa-descriptor (car listpart) (cadr listpart))
								  (car listpart)
								  (cadr listpart)))
					 (list-ladspa))
				    (lambda (x y)
				      (string-ci<? (.Name (car x))
						   (.Name (car y)))))))
    das-ladspa-list)
  
  (define (get-ladspa-with-id id)
    (if (null? (get-ladspa-list))
	#f
	(if (= (.UniqueID (caar das-ladspa-list)) id)
	    (let ((ret (car das-ladspa-list)))
	      (set! das-ladspa-list (cdr das-ladspa-list))
	      ret)
	    (let loop ((part das-ladspa-list))
	      (if (or (null? part)
		      (null? (cdr part)))
		  #f
		  (if (= (.UniqueID (caadr part)) id)
		      (let ((ret (cadr part)))
			(set-cdr! part (cddr part))
			ret)
		      (loop (cdr part))))))))


  (define ladspa-add-effect-menuitem
    (let* ((num-effects-per-submenu 12)
	   (ladspa-effect-num num-effects-per-submenu)
	   (uncat-submenu #f)
	   (curr-submenu #f))
      (lambda* (name proc #:optional menu)
	       (if menu
		   (menu-add menu name proc)
		   (begin
		     (if (not uncat-submenu)
			 (set! uncat-submenu 
			       (if (provided? 'snd-lrdf)
				   (menu-sub-add ladspa-effects-menu "Uncategorised")
				   ladspa-effects-menu)))
		     (if (= num-effects-per-submenu ladspa-effect-num)
			 (begin
			   (set! curr-submenu (menu-sub-add uncat-submenu (string-append (substring name 0 (min (string-length name) 20)) " ... ")))
			   (set! ladspa-effect-num -1)))
		     (menu-add curr-submenu name proc)
		     (set! ladspa-effect-num (+ 1 ladspa-effect-num)))))))
  
    
  (define (make-ladspadialog descriptor libraryname effectname)
    (define ladspa #f)
    (define dialog #f)
    (define toggles #f)

    (let ((name (.Name descriptor))
	  (author (.Maker descriptor))
	  (lisense (.Copyright descriptor))
	  (isplaying #f)
	  (onoffbutton #f)
	  (islooping #f)
	  (isplayingselection #f)
	  (nodelines (make-hash-table 4))
	  (play-hooks (make-hash-table 4))
	  (node-graphs (make-hash-table 4))
	  (open-portnums '()))
      
      (define (ShowDialog)
	(MakeDialogIfNotMade)
	(-> dialog show)
	(-> onoffbutton set #t)
	(enableplugin)
	(for-each (lambda (portnum)
		    (automation-onoff #t portnum
				      (-> ladspa get-lo portnum)
				      (-> ladspa get-hi portnum)
				      (list-ref (.PortNames descriptor) portnum)))
		  open-portnums)
	)
      
      (define (Help)
	(let ((dashelp (assoc (string-append libraryname effectname) ladspa-help-assoclist)))
	  (help-dialog author
		       (string-append (if dashelp
					  (caddr dashelp)
					  lisense)
				      (string #\newline #\newline)
				      "Processing can be stopped by pressing C-g"))))
      
      (define (Apply)
	(stop-playing)
	(disableplugin)
	(-> onoffbutton set #f)
	(if (null? (hash-fold acons '() nodelines))
	    (-> ladspa apply!)
	    (-> ladspa apply!
		(lambda (snd pos)
		  (hash-fold (lambda (portnum nodeline s)
			       (let ((lo (-> ladspa get-lo portnum))
				     (hi (-> ladspa get-hi portnum)))
				 (-> ladspa input-control-set!
				     portnum
				     (c-scale (-> nodeline get-val (c-scale pos 0 (frames snd 0) 0 1))
					      0 1
					      hi lo))))
			     '() nodelines)))))
      
      (define (Close)
	(set! open-portnums (hash-fold (lambda (portnum nodeline s) (nodeline-off portnum) (cons portnum s))
				       '() nodelines))
	(-> onoffbutton set #f)
	(-> dialog hide)
	(disableplugin)
	(if (string=? "vst" libraryname)
	    (-> ladspa close)))
      
      (define (Preview button onoroff)
	(if onoroff
	    (begin
	      (if (not (selection-member? (selected-sound)))
		  (select-all (selected-sound)))
	      (if (defined? 'c-play-selection)
		  (c-play-selection)
		  (letrec ((das-play (lambda ()
				       (play-selection #f
						       (lambda (x)
							 (if (= x 0)
							     (das-play)
							     (-> button set #f)
							     ))))))
		    (das-play))))
	    (stop-playing)))

      (define (Reset)
	(-> ladspa set-default-input-controls)
	(for-each (lambda (slider portnum)
		    (-> slider set! (-> ladspa get-input-control portnum)))
		  (-> dialog sliders)
		  (remove (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
			  (-> ladspa input-controls)))
	(for-each (lambda (toggle portnum)
		    (-> toggle set (> (-> ladspa get-input-control portnum) 0)))
		  toggles
		  (filter-org (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
			      (-> ladspa input-controls))))

      (define (enableplugin)
	(if (not (-> ladspa add-dac-hook!))
	    (begin
	      (display "Unable to use ladspa plugin.")
	      (newline))))
      
      (define (disableplugin)
	(-> ladspa remove-dac-hook!))
      
      (define (onoff onoroff)
	(if onoroff
	    (enableplugin)
	    (disableplugin)))
      
      (define (nodeline-off portnum)
	(remove-hook! play-hook (hash-ref play-hooks portnum))
	(hash-set! node-graphs portnum (-> (hash-ref nodelines portnum) get-graph))
	(hash-remove! play-hooks portnum)
	(-> (hash-ref nodelines portnum) paint)
	(-> (hash-ref nodelines portnum) delete!)
	(hash-remove! nodelines portnum))
      
      (define (automation-onoff onoroff portnum lo hi name)
	(if onoroff
	    (let* ((nodeline (<editor-nodeline> (selected-sound) 0
						(c-scale (-> ladspa get-input-control portnum) hi lo 0 1)
						(lambda (val)
						  (format #f "~1,3f(~A)" (c-scale val 0 1 hi lo) name))
						#f
						(list-ref (list cursor-context selection-context) (random 2))))
		   (das-play-hook (let ((lastcursor (- (dac-size))))
				    (lambda (samples)
				      (let* ((snd (selected-sound))
					     (ch 0)
					     (newcursor (cursor snd ch)))
					(if (or (= newcursor (- lastcursor (dac-size)) )
						(= newcursor lastcursor))
					    (set! newcursor (min (frames snd ch) (+ (dac-size) lastcursor))))
					(set! lastcursor newcursor)
					(-> ladspa input-control-set!
					    portnum
					    (c-scale (-> nodeline get-val (c-scale newcursor 0 (frames snd ch) 0 1))
						     0 1
						     hi lo)))))))
	      (if (hash-ref node-graphs portnum)
		  (-> nodeline set-graph!
			(hash-ref node-graphs portnum)))
	      (hash-set! nodelines portnum nodeline)
	      (add-hook! play-hook das-play-hook)
	      (hash-set! play-hooks portnum das-play-hook))
	    (nodeline-off portnum)))
      
      (define (slider-moved val portnum lo hi name)
	(if (not (hash-ref nodelines portnum))
	      (-> ladspa input-control-set! portnum val)))
      
      (define (ishint dashint dashint2)
	(not (= (logand dashint dashint2 ) 0)))

      (define (MakeDialogIfNotMade)
	(if (not dialog)
	    (begin
	      ;; Make sure the plugin is disabled before quitting.
	      (add-hook! exit-hook (lambda args
				     (disableplugin)
				     (if (string=? "vst" libraryname)
					 (-> ladspa close))
				     #f))
	      
	      
	      ;; Make dialog
	      (set! dialog 
		    (<dialog> name Close
			      "Close" Close
			      "Apply" Apply
			      "Preview" Preview
			      "Reset" Reset
			      (if (assoc (string-append libraryname effectname) ladspa-help-assoclist)
				  "Help"
				  "Not much help")
			      Help))
	      
	      ;; Add sliders.
	      (if (not (null? (-> ladspa input-controls)))
		  (dialog 'add-sliders
			  (map (lambda (portnum)
				 (let* ((lo (-> ladspa get-lo portnum))
					(init (-> ladspa get-input-control portnum))
					(hi (-> ladspa get-hi portnum))
					(hint (-> ladspa get-hint portnum))
					(scale (if (ishint hint LADSPA_HINT_INTEGER)
						   1
						   (if use-gtk 1000.0 100.0)))
					(name (list-ref (.PortNames descriptor) portnum)))
				   (list name
					 lo
					 init
					 hi
					 (lambda (val) (slider-moved val portnum lo hi name))
					 scale
					 (lambda (onoff) (automation-onoff onoff portnum lo hi name)))))
			       (remove (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
				       (-> ladspa input-controls)))
			  ))
	      
	      
	      ;; Add toggle buttons.
	      (set! toggles (map (lambda (portnum)
				   (let* ((hint (-> ladspa get-hint portnum))
					  (hi (-> ladspa get-hi portnum))
					  (lo (-> ladspa get-lo portnum))
					  (portname (list-ref (.PortNames descriptor) portnum))
					  (ison (> (-> ladspa get-input-control portnum) 0)))
				     (<checkbutton> dialog
						    portname
						    (lambda (on)
						      (-> ladspa input-control-set! portnum (if on hi lo)))
						    ison)))
				 (filter-org (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
					     (-> ladspa input-controls))))
	      
	      
	      ;; Add on/off button.
	      (set! onoffbutton (<checkbutton> dialog
					       "On/off"
					       onoff
					       #t)))))
      
      
      (set! ladspa (<ladspa> libraryname effectname))
      (if (not ladspa)
	  (begin
	    (c-display "Could not load plugin" libraryname effectname)
	    #f)
	  (ShowDialog))))
  
  (define* (make-ladspadialog-menuitem  descriptor libraryname effectname #:optional menu)
    (ladspa-add-effect-menuitem (.Name descriptor) (lambda ()
						     ;;ShowDialog
						     (make-ladspadialog descriptor libraryname effectname))
				menu))

  ;; Following function made while looking at the source for jack-rack written by Bob Ham.

  (define (menu-descend-base menu uri base)
    (let ((uris (lrdf_get_subclasses uri)))
      (if uris
	  (let ((n 0))
	    (while (< n (lrdf_uris_count uris))
		   (let* ((item (lrdf_uris_get_item uris n))
			  (label (lrdf_get_label item))
			  (newmenu (menu-sub-add menu label)))
		     (menu-descend newmenu item (string-append base "/" label)))
		   (set! n (1+ n)))
	    (lrdf_free_uris uris))))
    (let ((uris (lrdf_get_instances uri)))
      (if uris
	  (let ((n 0))
	    (while (< n (lrdf_uris_count uris))
		   (let* ((item (lrdf_uris_get_item uris n))
			  (das-ladspa (get-ladspa-with-id (lrdf_get_uid item))))
		     (if das-ladspa
			 (apply make-ladspadialog-menuitem (append das-ladspa (list menu)))))
		   (set! n (1+ n)))
	    (lrdf_free_uris uris))))
    )

  (define (menu-descend menu uri base)
    (menu-descend-base menu uri base))

  (if (provided? 'snd-lrdf)
      (menu-descend ladspa-effects-menu (string-append (LADSPA-BASE) "Plugin") ""))

  (for-each (lambda (x)
	      (apply make-ladspadialog-menuitem x))
	    (get-ladspa-list))
  )







;; Remove the following line to take away lrdf support.
(provide 'snd-lrdf)


(define lrdf-is-inited #f)

(define ladspa-not-initialized #t)

(if (and (provided? 'snd-lrdf)
	 (not lrdf-is-inited))
    (begin
      (eval-c "-llrdf"
	      
	      "#include <lrdf.h>"
	      
	      (proto->public
	       "void lrdf_init();"
	       "void lrdf_cleanup();"
	       ;;"int lrdf_read_files(const char *uri[]);"
	       "int lrdf_read_file(const char *uri);"
	       ;;"void lrdf_add_triple(const char *source, const char *subject, const char* predicate, const char *object, enum lrdf_objtype literal);"
	       "char* lrdf_add_preset(const char *source, const char *label, unsigned long id,                      lrdf_defaults *vals);"
	       "void lrdf_remove_matches(lrdf_statement *pattern);"
	       "void lrdf_remove_uri_matches(const char *uri);"
	       "void lrdf_rebuild_caches();"
	       "int lrdf_export_by_source(const char *src, const char *file);"
	       "lrdf_uris *lrdf_match_multi(lrdf_statement *patterns);"
	       "lrdf_statement *lrdf_matches(lrdf_statement *pattern);"
	       "lrdf_statement *lrdf_one_match(lrdf_statement *pattern);"
	       "int lrdf_exists_match(lrdf_statement *pattern);"
	       "lrdf_uris *lrdf_get_all_superclasses(const char *uri);"
	       "lrdf_uris *lrdf_get_subclasses(const char *uri);"
	       "lrdf_uris *lrdf_get_all_subclasses(const char *uri);"
	       "lrdf_uris *lrdf_get_instances(const char *uri);"
	       "lrdf_uris *lrdf_get_all_instances(const char *uri);"
	       "lrdf_statement *lrdf_all_statements();"
	       "void lrdf_free_uris(lrdf_uris *u);"
	       "void lrdf_free_statements(lrdf_statement *s);"
	       "char *lrdf_get_setting_metadata(const char *uri, const char *element);"
	       "char *lrdf_get_default_uri(unsigned long id);"
	       "lrdf_uris *lrdf_get_setting_uris(unsigned long id);"
	       "unsigned long lrdf_get_uid(const char *uri);"
	       "lrdf_defaults *lrdf_get_setting_values(const char *uri);"
	       "lrdf_defaults *lrdf_get_scale_values(unsigned long id, unsigned long port);"
	       "void lrdf_free_setting_values(lrdf_defaults *def);"
	       "char *lrdf_get_label(const char *uri);")
	      
	      (public
	       (<int> lrdf_defaults_count (lambda ((<lrdf_defaults*> defs))
					    (return defs->count)))
	       
	       (<int> lrdf_defaults_pid (lambda ((<lrdf_defaults*> defs)
						 (<int> n))
					  (return defs->items[n].pid)))
	       
	       (<float> lrdf_defaults_value (lambda ((<lrdf_defaults*> defs)
						     (<int> n))
					      (return defs->items[n].value)))
	       
	       (<int> lrdf_uris_count (lambda ((<lrdf_uris*> uris))
					(return uris->count)))
	       
	       (<char*> lrdf_uris_get_item (lambda ((<lrdf_uris*> uris)
						    (<int> n))
					     (return uris->items[n])))
	       
	       (<char*> LADSPA-BASE (lambda ()
				      (return LADSPA_BASE)))))
      
      (lrdf_init)

      (for-each (lambda (path)
		  (catch #t
			 (lambda ()
			   (let* ((dir (opendir path))
				  (entry (readdir dir)))
			     (while (not (eof-object? entry))
				    (if (and (not (string=? "." entry))
					     (not (string=? ".." entry)))
					(lrdf_read_file (string-append "file://" path "/" entry)))
				    (set! entry (readdir dir)))
			     (closedir dir)))
			 (lambda (key . args)
			   #f)))
		(string-split	(if (getenv "LADSPA_RDF_PATH")
				    (getenv "LADSPA_RDF_PATH")
				    "/usr/local/share/ladspa/rdf:/usr/share/ladspa/rdf")
				#\:))
      (set! lrdf-is-inited #t)))


(install-ladspa-menues)
(set! ladspa-not-initialized #f)

