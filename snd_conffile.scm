;; My config file for snd.
;; -Kjetil S. Matheussen.

;; Should work for gtk and perhaps motif.
(define srfi-loaded #f)
(define common-list-loaded #f)

(define (atleast-version a b c)
  (let ((version (map string->number (string-split (version) #\.))))
    (or (> (car version) a)
	(and (= (car version) a)
	     (or (> (cadr version) b)
		 (and (= b (cadr version))
		      (>= (caddr version) c))))))) 
 
(define (atleast1.8.0?)
  (atleast-version 1 8 0))

(define (atleast1.6.4?)	 	
  (atleast-version 1 6 4))

(if (not (atleast1.6.4?))
    (begin
      (display "Warning, snd_conffile.scm has not been tested with earlier versions of Guile than 1.6.4.")(newline)
      (display "In case of problems, please upgrade Guile to the latest version, and recompile Snd.")(newline)))

(if (not (atleast1.8.0?))
    (display "Warning. snd_conffile.scm should be run using Guile at least v1.8. This might not work.\n"))



(use-modules (ice-9 debug))
(use-modules (ice-9 rdelim))

(provide 'snd-snd_conffile.scm)


(set! (show-backtrace) #t)
(debug-enable 'debug)
(read-enable 'positions)
(if #f
    (begin
      (read-enable 'positions)
      (debug-enable 'debug)
      (debug-enable 'backtrace)
      (debug-set! frames 28)
      (debug-set! depth 250)))

;(use-modules (ice-9 readline))
;(activate-readline)


(define c-initializing
  (let ((num 33))
    (lambda ()
      (gtk_window_set_title (GTK_WINDOW (cadr (main-widgets))) (string-append "Initializing: " (number->string num)))
      (while (= 1 (gtk_events_pending))
	     (gtk_main_iteration))
      (set! num (1- num)))))

(c-initializing)
(define c-old-load-from-path load-from-path)
(define (load-from-path filename)
  (c-old-load-from-path filename)
  (c-initializing))

			 
(define (c-for init pred least add proc)
  (do ((n init (+ n add)))
      ((not (pred n least)))
    (proc n)))

(set! (window-height) 600)
(while (= 1 (gtk_events_pending))
       (gtk_main_iteration))
(set! (window-width) 800)
(while (= 1 (gtk_events_pending))
       (gtk_main_iteration))

(set! (show-listener) #t)
(while (= 1 (gtk_events_pending))
       (gtk_main_iteration))


(set! (window-width) 800)
(while (= 1 (gtk_events_pending))
       (gtk_main_iteration))

;(gtk_paned_set_position (GTK_PANED (list-ref (main-widgets) 3)) 100)
;(while (= 1 (gtk_events_pending))
;       (gtk_main_iteration))


#!

(set! (show-listener) #f)
(gtk_main_iteration_do #f)
!#


;;##############################################################
;; c-define is used instead of define when theres a variable the
;; user might want to define before loading this file.
;;##############################################################

(define-macro (c-define name val)
  (if (not (defined? name))
      `(define ,name ,val)))



;;##############################################################
;; Let everything that is printed to the listener go to stdout
;; as well. (Error printing from hooks and timed callbacks)
;;##############################################################

(add-hook! print-hook
  (lambda (msg)
    (display msg)
    #f))
  

;(add-hook! snd-error-hook
;  (lambda (msg) 
;    (display "snd-error!")(newline)
;    (display msg)
;    #f))

;;##############################################################
;; Various settings.
;; (ladspa-dir), %load-path, (temp-dir) and (save-dir) are
;; variables you might want to set in your .snd file.
;;##############################################################

(if (defined? 'notam-settings)
    ;; This is the settings used at Notam/Oslo. May not suite your setup.
    (begin
      (set! (ladspa-dir) "/usr/lib/ladspa")
      (set! %load-path (cons "/hom/kjetism/snd-run" %load-path))
      (set! (temp-dir) "/lyd/local/tmp")
      (set! (save-dir) "/lyd/local/tmp")))

(if (defined? 'kjetil-settings)
    ;; This is for my personal computers settings. May not suite your setup.
    (begin
      (set! %load-path (cons "/home/kjetil/snd-run" %load-path))
      (primitive-eval '(define *eval-c-CFLAGS* "-march=athlon-xp"))
      ))
    





;;##############################################################
;; Load various files
;;##############################################################


;;(if (not (provided? 'snd-snd-hobbit.scm))
;;    (load-from-path "snd-hobbit.scm"))



(if (not (provided? 'snd-gui.scm))
    (load-from-path "gui.scm"))


(if (not use-gtk)
    (c-display "Motif is currently not much supported in snd_conffile.scm."
	       "You should compile up Snd using the --with-gtk and --with-static-xm configure options."))




(c-load-from-path rgb)


(define-macro (c-show-times time force)
  `(call-non-cons c-show-times-das ,time ,force))




;;(load-from-path "bird.scm")




;;##############################################################
;; Set various variables
;;##############################################################

;;(set! *snd-remember-paths* #t)

(set! (just-sounds) #t)

(c-define c-default-output-srate 44100)
(set! (default-output-srate) c-default-output-srate)

(if (defined? 'c-default-output-type)
    (c-display "Warning: c-default-output-type replaced with c-default-output-header-type"))
(c-define c-default-output-header-type mus-riff)
(set! (default-output-header-type) c-default-output-header-type)

(c-define c-default-output-chans 2)
(set! (default-output-chans) c-default-output-chans)

(if (defined? 'c-default-output-format)
    (c-display "Warning: c-default-output-format replaced with c-default-output-data-format"))
(c-define c-default-output-data-format mus-lfloat)
(set! (default-output-data-format) c-default-output-data-format)

(c-define c-show-backtrace #t)
(set! (show-backtrace) c-show-backtrace)

(c-define c-show-indices #t)
(set! (show-indices) c-show-indices)

(c-define c-auto-resize #f)
(set! (auto-resize) c-auto-resize)

; Regions are created when needed when using the ctrl+y and ctrl+w keybindings.
(set! (selection-creates-region) #f)

;; Less than 64 seems to be very unreliable.
(c-define *c-minimum-dac-size* 2048)
(if (< (dac-size) *c-minimum-dac-size*)
    (set! (dac-size) *c-minimum-dac-size*))


;; Set graph-style to filled.
(set! (graph-style) graph-filled)

;; This is the value for the loop-button.
(c-define *c-islooping* #t)

(c-define *c-backgroundcolor* (make-color 0.8 0.8 0.78))
(c-define *c-selectioncolor* white)

(c-define *c-restore-previous-session* #t)
(c-define *c-use-envelope* #f)
(c-define *c-cursor-color* blue)

(c-define *c-zoomfactor* 1.2)

;; Percent of the window to move the cursor forward/backward using which keys.
(c-define *c-cursormovetime-rightleft* 2)
(c-define *c-cursormovetime-updown* 10)
(c-define *c-cursormovetime-pageupdown* 50)



(c-define *c-show-sonogram-cursor* #t)
(set! (show-sonogram-cursor) *c-show-sonogram-cursor*)

#!
;; Workaround for transform-hook bug
(add-hook! before-transform-hook
	   (lambda (snd chn)
	     (if (show-sonogram-cursor)
		 (set! (show-sonogram-cursor) #f))))

(add-hook! after-transform-hook
	   (lambda (snd chn scaler)
	     (c-display "scaler" scaler)
	     (if (not (eq? (show-sonogram-cursor) *c-show-sonogram-cursor*))
		 (set! (show-sonogram-cursor) *c-show-sonogram-cursor*))))
!#


(c-define *c-show-transform-peaks* #t)
(set! (show-transform-peaks) *c-show-transform-peaks*)

(c-define *c-transform-graph-type* graph-as-sonogram)
(set! (transform-graph-type) *c-transform-graph-type*)

(c-define *c-fft-log-magnitude* #t)
(set! (fft-log-magnitude) *c-fft-log-magnitude*)

(c-define *c-color-inverted* #f)
(set! (color-inverted) *c-color-inverted*)

(c-define *c-color-scale* 0.5)
(set! (color-scale) *c-color-scale*)

(c-define *c-color-cutoff* 0.0)
(set! (color-cutoff) *c-color-cutoff*)

(c-define *c-colormap* 4)
(set! (colormap) *c-colormap*)

(c-define *c-transform-size* 1024)
(set! (transform-size) *c-transform-size*)



;; Hopefully this will change.
(define c-setting-cursor-color-cause-redraw #t)

(define c-is-stop-hooks-here #f)
;;(defined? 'stop-playing-hook))


(define *c-is-starting-up* #t)


;; We need our own after-open-hook, because its too complicated to ensure
;; that the c-snd-putgetdata hook is run first.
(define *c-after-open-hook* (<hook>))


;;##############################################################
;; Documentation for this configuration file.
;;##############################################################



(add-to-menu 4 "--------------------------------" (lambda () #t) 0)
(add-to-menu 4 "But! How do I..."
	     (lambda ()
	       (help-dialog  "How do I..."
			     (apply string-append (map (lambda (s) (string-append s (string #\newline)))
						       '(
							 "Play:"
							 "     <Space>"
							 ""
							 "Play selection:"
							 "     <Space>"
							 ""
							 "Stop playing:"
							 "     <Space>"
							 ""
							 "Play from cursor:"
							 "     p"
							 ""
							 "Pause playing:"
							 "     p"
							 ""
							 "Play starting from mousepointer position:"
							 "     <Mouse Scroll Up/Down>"
							 ""
							 "Cut:"
							 "      <Ctrl>+w"
							 ""
							 "Paste:"
							 "      <Ctrl>+y"
							 ""
							 "Mix-Paste:"
							 "      <Ctrl>+x+y"
							 ""
							 "Zoom in:"
							 "      +"
							 ""
							 "Zoom out:"
							 "      -"
							 ""
							 "Show all:"
							 "      a"
							 ""
							 "Show selection:"
							 "      s"
							 ""
							 "Show/hide controls:"
							 "      c"
							 ""
							 "Show/hide listener:"
							 "      l"
							 ""
							 "Undo:"
							 "      <Ctrl>+_"
							 ""
							 "Make mark"
							 "      m"
							 ""
							 "Move cursor 2 percent forward"
							 "      <Arrow right>"
							 ""
							 "Move cursor 2 percent backward"
							 "      <Arrow left>"
							 ""
							 "Move cursor 10 percent forward"
							 "      <Arrow up>"
							 ""
							 "Move cursor 10 percent backward"
							 "      <Arrow down>"
							 ""
							 "Move cursor 50 percent forward"
							 "      <Page Up>"
							 ""
							 "Move cursor 50 percent backward"
							 "      <Page Down>"
							 ""
							 "Set cursor at beginning:"
							 "      <"
							 ""
							 "Set cursor at end:"
							 "      >"
							 ""
							 "Change selection without destroying the old one:"
							 "      <Ctrl> + <Mouse>"
							 ""
							 "Move selected area:"
							 "      <Shift> + <Mouse>"
							 ""
							 "Remove Node"
							 "      <Right Mouse click>  OR  <Shift> + <Right Mouse>"
							 ""
							 "Set Node at Middle position:"
							 "      <Shift> + <Mouse>"
							 ""
							 "Finetune Node position:"
							 "      <Ctrl> + <Mouse>"
							 ""
							 "Move node only in x or y directioy"
							 "      <Right Mouse button>"
							 ""
							 "Apply Envelope:"
							 "      v"
							 ))))
	       )
	     0)





;;##############################################################
;; Various more or less general functions
;;##############################################################


(define (c-get-nameform snd)
  (if use-gtk
      (GTK_BOX (list-ref (sound-widgets snd) 10))
      (find-child (list-ref (sound-widgets snd) 2) "snd-name-form")))

(define (c-for-each-nameform-button snd name func)
  (for-each-child (c-get-nameform snd)
		  (lambda (w)
		    (if use-gtk
			(if (and (GTK_IS_BUTTON w)
				 (string=? name (gtk_button_get_label (GTK_BUTTON w))))
			    (func (GTK_BUTTON w)))
			(if (string=? (XtName w) name)
			    (func w))))))

(define (c-get-nameform-button snd name)
  (call-with-current-continuation
   (lambda (return)
     (c-for-each-nameform-button snd name return)
     #f)))


(define (c-redraw)
  (set! (cursor-color) (cursor-color)))

;; Selection-position and selection-frames doesn't allways work properly.
(define (c-selection-position)
  (selection-position (c-selected-sound)))
(define (c-selection-frames)
  (selection-frames (c-selected-sound)))

(define* (c-set-selection-position! snd ch val #:optional (dassync (c-sync? snd)))
  (let ((mustredraw #f))
    (c-for 0 < (chans snd) 1
	   (lambda (n)
	     (if (or dassync
		     (= n ch))
		 (set! (selection-position snd n) val)
		 (if (selection-member? snd n)
		     (begin
		       (set! mustredraw #t)
		       (set! (selection-member? snd n) #f))))))
    (if mustredraw	
	(c-redraw))))


(define* (c-set-selection-frames! snd ch val #:optional (dassync (c-sync? snd)))
  (c-for 0 < (chans snd) 1
	 (lambda (n)
	   (if (or dassync
		   (= n ch))
	       (set! (selection-frames snd n) val)))))


(define* (c-set-selection! snd ch start end #:optional (dassync (c-sync? snd)))
  (c-set-selection-position! snd ch start dassync)
  (c-set-selection-frames! snd ch (- end start -1) dassync))

;; Like (selection?) but only for the selected sound.
(define (c-selection?)
  (selection-member? (c-selected-sound)))



;; Returns true if file is allready loaded. Necesarry to unscrew up open-hook handling.
(define (c-isloaded? filename)
  (member filename (map (lambda (snd) (file-name snd))
			(sounds))))


(define (c-set-cursor-pos pos)
  (c-for-each-channel2 (c-selected-sound)
		       (lambda (ch)
			 (set! (cursor (c-selected-sound) ch) (c-integer pos)))))


(define* (c-use-rt-player? #:optional (snd (c-selected-sound)))
  (and (defined? '*rt-use-rt-player*)
       *rt-use-rt-player*
       (<= num-running-ladspas 0)
       (not (expand-control? snd))
       (not (filter-control? snd))
       (not (contrast-control? snd))
       (not (reverb-control? snd))))


(define (c-playing)
  (or (playing)
      (and (c-use-rt-player?)
	   (rt-snd-is-playing?))))

;; Like (set-cursor-pos pos), but legalize pos and
;; calls c-show-times as well. Also works when playing.
(define (c-set-cursor-pos2 pos)
  (let ((isplaying (c-playing))
	(legalpos (min (frames) (max 0 pos))))
    (if isplaying
	(-> (c-p) set-cursor legalpos)
	(begin
	  (c-show-times legalpos #f)
	  (c-set-cursor-pos legalpos)))))


(define c-snd-putgetdata (make-hash-table 32))

(define (c-put snd name data)
  (hash-set! (hash-ref c-snd-putgetdata snd) name data))


(define* (c-get snd name #:optional default)
  (hash-ref (hash-ref c-snd-putgetdata snd) name default))



;; This following command had to be moved nearly to the end of this file, before opening previous files:
;(add-hook! after-open-hook
;	   (lambda (snd)
;	     (hash-set! c-snd-putgetdata snd (make-hash-table 16))
;	     #f))






;;##############################################################
;; Customize the cursor look
;;##############################################################

(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! start-playing-hook 
	       (lambda (snd) 
		 (set! (cursor-color) yellow)
		 #f)))

(set! (cursor-color) *c-cursor-color*)

(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! stop-playing-hook
	       (lambda (snd) 
		 (set! (cursor-color) blue)
		 ;;(gc)
		 #f)))

;; Set different color on the cursor when playing selection.
(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! start-playing-selection-hook
	       (lambda ()
		 ;;(display "Start playing selection")(newline)
		 (set! (cursor-color) green)
		 #f)))


(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! stop-playing-selection-hook
	       (lambda ()
		 ;;(display "Stop playing selection")(newline)
		 (set! (cursor-color) blue)
		 #f)))



;; Let the cursor move when playing. (copied from the manual)
(add-hook! after-open-hook 
	   (lambda (snd)
	     (c-for-each-channel2 snd
				  (lambda (i)
				    (set! (cursor snd i) 0)))
	     (c-set-sound-cursor snd cursor-line)
	     #f))

(set! (cursor-follows-play) #t)












;;##############################################################
;; Various mouse handling
;;##############################################################


;; Removes default mouse-click-hook handling. The possibility to paste with
;; mouse-button 2 is really annoying when using a wheel mouse.
(add-hook! mouse-click-hook (lambda (snd chn button state x y axis)
			      #t))


;; Moves the playing position when clicking in the editor if clicking button 1 or 4.
;; Moves cursor and stops playing if clicking buttton 5. (wheel down)
;; Moves cursor and starts playing if clicking button 4. (wheel up)

(add-hook! mouse-click-hook
	   (lambda (snd chn button state x y axis)
	     (if (and (not (= state 260))
		      (= axis time-graph))
		 (let ((samp (max 0 (c-integer (* (srate snd) (position->x x snd chn)))))
		       (dasspeed (speed-control))
		       (play (c-p snd)))
		   (cond ((= button 4)
			  (if (< dasspeed 0)
			      (set! (speed-control) (* -1 dasspeed)))
			  (-> play play2 samp))
			 ((= button 5)
			  (if (> dasspeed 0)
			      (set! (speed-control) (* -1 dasspeed)))
			  (-> play play2 samp))
			 (else
			  (c-show-times samp #t)
			  (if (-> play isplaying)
			      (-> play stop samp)
			      (set! (cursor snd) samp))))))
	     #f))
	

#!
(set! (speed-control) -1)
(play (- (frames) 1000) #f #f #f 1000 #f #f)
!#


;;##############################################################
;; Playing
;;##############################################################


(def-class (<play> snd)
  (define isplaying #f)
  (define startplaypos 0)
  (define playtype 'song)

  (define (get-selection-start)
    (if (c-use-rt-player?)
	(if (selection-member? snd)
	    (selection-position snd)
	    0)
	(if (selection-member? snd)
	    (if (< (speed-control snd) 0)
		(+ (selection-position snd) (selection-frames snd))
		(selection-position snd))
	    (if (< (speed-control snd) 0)	 	
		(1- (frames snd))	 	
		0))))

  (define (get-selection-end)
    (if (c-use-rt-player?)
	(if (selection-member? snd)
	    (+ (selection-position snd) (selection-frames snd))
	    (frames snd))
	(if (selection-member? snd)
	    (if (< (speed-control snd) 0)
		(selection-position snd)
		(+ (selection-position snd) (selection-frames snd)))
	    (if (< (speed-control snd) 0)	 	
		0	 	
		(1- (frames snd))))))

  (define (my-stop-playing)
    (if (c-use-rt-player?)
	(rt-snd-stop-playing))
    (stop-playing))
    
  (def-method (play pos)
    (define (das-play)
      (play 0 #f #f #f #f #f das-callback))
    (define (das-callback x)
      (if (and (= x 0) *c-islooping*)
	  (das-play)
	  (set! isplaying #f)))
    (set! isplaying #t)
    (set! startplaypos (if pos (c-integer pos) (cursor snd)))
    (set! playtype 'song)
    
    (if (c-use-rt-player?)
	(rt-snd-play snd 0 #f startplaypos)
	(play startplaypos #f #f #f #f #f das-callback)))

  ;; Stops if allready playing
  (def-method (play2 pos)
    (if isplaying
	(begin
	  (set! (cursor-follows-play) #f)
	  (my-stop-playing)
	  (set! (cursor-follows-play) #t)))
    (this->play pos))

  (def-method (play-selection #:optional (pos (get-selection-start)))
    (define (das-play)
      (play (get-selection-start) #f #f #f (get-selection-end) #f das-callback))
    (define (das-callback x)
      (if (and (= x 0) *c-islooping*)
	  (das-play)
	  (set! isplaying #f)))
    (set! isplaying #t)
    (set! startplaypos (c-integer pos))
    (set! playtype 'selection)
    (if (c-use-rt-player?)
	(rt-snd-play snd (get-selection-start) (get-selection-end) (if (>= (speed-control snd) 0)
								       pos
								       (1- (get-selection-end))))
	(play pos #f #f #f (get-selection-end) #f das-callback)))
  
  (def-method (stop #:optional pos)
    (set! (cursor-follows-play) #f)
    (my-stop-playing)
    (set! (cursor) (if pos (c-integer pos) startplaypos))
    (c-show-times (cursor) #t)
    (set! (cursor-follows-play) #t))

  (def-method (pause)
    (set! (cursor-follows-play) #f)
    (let ((pos (cursor snd)))
      (my-stop-playing)
      (set! (cursor snd) pos)
      (set! (cursor-follows-play) #t)))

  (def-method (continue #:optional (pos (cursor snd)))
    (if (eq? playtype 'song)
	(this->play pos)
	(this->play-selection pos)))


  (def-method (isplaying)
    (c-playing))

  (def-method (set-cursor pos)
    (set! (cursor-follows-play) #f)
    (my-stop-playing)
    (this->continue pos)
    (set! (cursor-follows-play) #t))

  (def-method (dosomepause thunk)
    (if (this->isplaying)
	(begin
	  (this->pause)
	  (thunk)
	  (this->continue))
	(thunk)))	  
 
  (def-method (selection-is-changed)	 	
    (if (this->isplaying)	 	
	(begin	 	
	  (set! (cursor-follows-play) #f)	 	
	  (my-stop-playing)	 	
	  (this->continue (get-selection-start))	 	
	  (set! (cursor-follows-play) #t))))	 	
 
  )



(add-hook! after-open-hook
	   (lambda (snd)
	     (c-put snd 'play (<play> snd))))

(define* (c-p #:optional (snd (c-selected-sound)))
  (c-get snd 'play))

(define* (c-play #:optional samp)
  (-> (c-p) play samp))
(define (c-stop)
  (-> (c-p) stop))
(define (c-pause)
  (-> (c-p) pause))
(define (c-continue)
  (-> (c-p) continue))
(define (c-play-selection)
  (-> (c-p) play-selection))



;; Can't play many minutes before memory is used up: (and allocating memory stops the world as well)
(if (defined? 'c-stop-gc-when-playing)
    (begin
      (add-hook! stop-dac-hook
		 (lambda ()
		   (c-gc-on)))
      (add-hook! stop-playing-hook
		 (lambda (snd)
		   (c-gc-on)))
      (add-hook! stop-playing-selection-hook
		 (lambda ()
		   (c-gc-on)))
      (add-hook! start-playing-hook
		 (lambda (snd)
		   (c-gc-off #f)))
      (add-hook! start-playing-selection-hook
		 (lambda ()
		   (c-gc-off #f)))))


#!
Doesnt work any more.
(add-hook! stop-playing-hook
	   (lambda (snd)
	     (c-display "stop-playing-hook")
	     (set! (cursor-follows-play) #f)
	     (c-set-cursor-pos (cursor))
	     (c-show-times (cursor) #t)
	     (set! (cursor-follows-play) #t)))
!#


;; Replace the old space binding with one that starts playing from the current cursor position,
;; all channels. And stops playing if allready playing.
(bind-key (char->integer #\ ) 0 
	  (lambda ()
	    (if (-> (c-p) isplaying)
		(-> (c-p) stop)
		(if (c-selection?)
		    (-> (c-p) play-selection)
		    (-> (c-p) play #f)))))

	     
#!
(-> (c-p) stop)
(-> (c-p) play #f)
(-> (c-p) isplaying)
(in 50
    (lambda ()
      (asdfasdf)))
!#

;; Makes the P key a pause button. If playing, stops playing, but doesn't reset the cursor pos.
;; If not playing, starts playing from cursor.
(bind-key (char->integer #\p) 0 
	  (lambda x
	    (if (-> (c-p) isplaying)
		(-> (c-p) pause)
		(-> (c-p) continue))))




;;##############################################################
;; View
;;##############################################################

;; Let the "a" key be a "show-all" key.
(bind-key (char->integer #\a) 0
	  (lambda ()
	    (set! (x-bounds) (list 0.0 (/ (frames) (srate))))))


;; Let the "s" key be a "show-selection" key.
(bind-key (char->integer #\s) 0
	  (lambda ()
	    (set! (x-bounds) (list (/ (c-selection-position) (srate))
				   (/ (+ (c-selection-position) (c-selection-frames)) (srate))))))


(define (c-set-x-bounds! x1 x2)
  (let* ((all (/ (frames) (srate)))
	 (n-x1 (max 0 x1))
	 (n-x2 (min all (+ (- n-x1 x1) x2))))
    (if (> x2 all)
	(set! n-x1 (max 0 (- n-x1 (- x2 all)))))
    (if (and (or (>= (* (abs (- n-x1 (car (x-bounds)))) (srate)) 1)
		 (>= (* (abs (- n-x2 (cadr (x-bounds)))) (srate)) 1))
	     (>= (* (srate) (- n-x2 n-x1)) (if (< (- (frames) (cursor)) 10) 10 1)))
	(set! (x-bounds) (list n-x1 n-x2)))))

(define (c-zoom zoomfactor)
  (let* ((cursor (/ (cursor) (srate)))
	 (x1 (car (x-bounds)))
	 (x2 (cadr (x-bounds)))
	 (length (- x2 x1))
	 (n-length (* zoomfactor length))
	 (n-x2 (/ (+ (* 2 cursor) n-length) 2))
	 (n-x1 (- n-x2 n-length)))
    (c-set-x-bounds! n-x1 n-x2)))

;; -: Zoom out
(bind-key (char->integer #\-) 0
	  (lambda x
	    (c-zoom *c-zoomfactor*)))

;; +: Zoom in
(bind-key (char->integer #\+) 0
	  (lambda ()
	    (c-zoom (/ 1 *c-zoomfactor*))))



;; Shows the full sound after opening.
(add-hook! after-open-hook
	   (lambda (n)
	     (set! (x-bounds) (list 0.0 (/ (+ (frames) 1) (srate))))
	     #f))

;(add-hook! initial-graph-hook
;	   (lambda (snd chn dur)
;	     (list 0.0 dur)))






;;##############################################################
;; Cursor position
;;##############################################################

(let ((m (lambda (func percent)
	   (lambda ()
	     (c-set-cursor-pos2 (func (cursor) (* (srate) 
						  (- (cadr (x-bounds)) (car (x-bounds)))
						  0.01
						  percent)))))))
  (bind-key #xFF56 0
	    (m - *c-cursormovetime-pageupdown*))
  
  (bind-key #xFF55 0
	    (m + *c-cursormovetime-pageupdown*))
 
  (bind-key #xFF52 0
	    (m + *c-cursormovetime-updown*))

  (bind-key #xFF54 0
	    (m - *c-cursormovetime-updown*))

  (bind-key #xFF51 0
	    (m - *c-cursormovetime-rightleft*))

  (bind-key #xFF53 0
	    (m + *c-cursormovetime-rightleft*))

  (bind-key #xFF57 0
	    (lambda () (c-set-cursor-pos2 0)))

  (bind-key (char->integer #\<) 0
	    (lambda () (c-set-cursor-pos2 0))))

#!
Does not work.
(bind-key (char->integer #\>) 0
	  (lambda ()
	    (c-set-cursor-pos2 (frames))))
!#







;;##############################################################
;; Various key-bindings
;;##############################################################

;; Make snd quit when pressing C-x-c, just like emacs.
(bind-key (char->integer #\c) 4
	  (lambda ()
	    (exit))
	  #t)


(bind-key (char->integer #\y) 4
	  (lambda ()
	    (mix-selection (cursor)))
	  #t)


;; Let "c" turn on/off controls
(bind-key (char->integer #\c) 0
	  (lambda ()
	    (set! (show-controls)
		  (if (show-controls)
		      #f
		      #t))
	    (focus-widget (c-editor-widget (c-selected-sound)))))

;; Let "l" turn on/off listener
(bind-key (char->integer #\l) 0
	  (lambda ()
	    (set! (show-listener)
		  (if (show-listener #f)
		      #f
		      #t))
	    (focus-widget (c-editor-widget (c-selected-sound)))))

#!
(add-hook! key-press-hook 
	   (lambda (snd chn key state)
	     (c-display "key: " key)))
!#




;; Key-bindings to have fun.

(bind-key (char->integer #\z) 0
	  (lambda ()
	    (set! (speed-control) (* -1 (speed-control)))))


(bind-key (char->integer #\q) 0
	  (lambda ()
	    (set! (speed-control) (* 0.75 (speed-control)))))
(bind-key (char->integer #\w) 0
          (lambda ()
            (set! (speed-control) (* (if (< (speed-control) 0)
					 (if (= 1 (random 10)) 1 -1)
					 (if (= 1 (random 10)) -1 1))
				     (/ (random 1000) 500)))))
(bind-key (char->integer #\e) 0
	  (lambda ()
	    (set! (speed-control) (* 1.25 (speed-control)))))






;;##############################################################
;; Set some colors
;;##############################################################

;; Bacground
(set! (selected-graph-color) *c-backgroundcolor*)
(set! (graph-color) *c-backgroundcolor*)

;; Selection
(set! (selection-color) *c-selectioncolor*)




;;##############################################################
;; Minibuffer-stuff
;;##############################################################

(define c-is-prompting-in-minibuffer #f)
(define prompt-in-minibuffer-old prompt-in-minibuffer)
(define report-in-minibuffer-old report-in-minibuffer)

(define (prompt-in-minibuffer question func . rest)
  (set! c-is-prompting-in-minibuffer #t)
  (apply prompt-in-minibuffer-old (append (list question
						(lambda (response)
						  (let ((ret (func response)))
						    (set! c-is-prompting-in-minibuffer #f)
						    ret)))
					  rest)))
(define (report-in-minibuffer . args)
  (if (not c-is-prompting-in-minibuffer)
      (apply report-in-minibuffer-old args)))



;;##############################################################
;; Marks
;;##############################################################

; Let m make a named mark, and sync it if the current sound is synced.
(bind-key (char->integer #\m) 0
	  (lambda ()
	    (define (my-add-mark sample snd ch syncnum name)
	      (if (> ch -1)
		  (let ((newmark (add-mark sample snd ch)))
		    (set! (mark-sync newmark) syncnum)
		    (set! (mark-name newmark) name)
		    (my-add-mark sample snd (1- ch) syncnum name))))
	    (define my-mark-sync-max
	      (let ((max-mark-sync 1000))
		(lambda ()
		  (set! max-mark-sync (max (1+ (mark-sync-max)) (1+ max-mark-sync)))
		  max-mark-sync)))
	    (report-in-minibuffer "")
	    (prompt-in-minibuffer "mark: "
				  (lambda (ret)
				    (if (> (sync) 0)
					(my-add-mark (cursor) (c-selected-sound) (1- (channels)) (my-mark-sync-max) ret)
					(set! (mark-name (add-mark (cursor))) ret)))
				  (c-selected-sound)
				  #t)))
					      

(c-load-from-path marks)

;; The following mark-related lines are copied from marks.scm. For some reason the functions was commented away.

(define (eval-header sndf)
  (and (string? (comment sndf))
       (catch #t
	      (lambda ()
		(eval-string (comment sndf)))
	      (lambda args #f))))


(define (marks->string sndf)
  (let ((str (format #f "(if (not (defined? 'mark-property)) (load \"marks.scm\"))~%(let ((m #f))~%"))
	(chan 0))
    (for-each
     (lambda (chan-marks)
       (for-each 
	(lambda (m)
	  (set! str 
		(string-append str 
			       (format #f
				       "  (set! m (add-mark ~A #f ~D))~%" 
				       (mark-sample m)
				       chan)))
	  (if (and (string? (mark-name m))
		   (> (string-length (mark-name m)) 0))
	      (set! str 
		    (string-append str 
				   (format #f
					   "  (set! (mark-name m) ~S)~%"
					   (mark-name m)))))
	  (if (not (null? (mark-properties m)))
	      (set! str
		    (string-append str 
				   (format #f
					   "  (set! (mark-properties m) '~A)~%"
					   (mark-properties m))))))
	  chan-marks)
       (set! chan (1+ chan)))
     (marks sndf))
    (string-append str (format #f "  m)~%"))))

(add-hook! output-comment-hook (lambda (str) (marks->string (c-selected-sound)) #f))
(add-hook! after-open-hook (lambda (snd) (eval-header snd) #f))






;;##############################################################
;; Replace mouse-selection handling. (probably broken for motif)
;;##############################################################


(let ((about-to-move #f)
      (selection-starting-point #f)
      (dassync #f))

  (define (mouse-press-callback snd ch x y button state)
    (if (c-leftbutton? button)
	(let ((samp (c-integer (* (srate snd) (position->x x snd ch))))
	      (selmember (selection-member? snd ch)))
	  (set! dassync (> (position->y y snd 0) 0))
	  (if (< samp 0) (set! samp 0))
	  (if (and (c-ctrl? state) selmember)
	      (let ((selpos (selection-position snd ch))
		    (selframes (selection-frames snd ch)))
		(if (< samp (+ (/ selframes 2) selpos))
		    (set! selection-starting-point (+ selpos selframes))
		    (set! selection-starting-point selpos))
		(mouse-motion-callback snd ch x y button state))
	      (if (and (c-shift? state) selmember)
		  (let ((selpos (selection-position snd ch))
			(selframes (selection-frames snd ch)))
		    (color-samples-allchans (channels) green selpos selframes)
		    (set! about-to-move (<array> selpos selframes)))
		  (set! selection-starting-point samp)))
	  'allways-stop!)))

  (define (mouse-motion-callback snd ch x y button state)
    (-> (c-p snd) selection-is-changed)
    (let ((samp (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
      (if about-to-move
	  (c-set-selection! snd ch samp (+ samp (about-to-move 1)) dassync)
	  (begin
	    (if (not (selection-member? snd ch))
		(if (< x selection-starting-point)
		    (c-set-selection! snd ch x selection-starting-point dassync)
		    (c-set-selection! snd ch selection-starting-point x dassync))
		(let* ((selstart (selection-position snd ch))
		       (selframes (selection-frames snd ch))
		       (newselstart (if (< samp selection-starting-point)
					samp
					selection-starting-point))
		       (newselframes (if (< samp selection-starting-point)
					 (- selection-starting-point samp)
					 (1+ (- samp selection-starting-point)))))
		  (if (and (or (not (= selstart newselstart))
			       (not (= selframes newselframes)))
			   (not (= samp selection-starting-point)))
		      (begin
			(c-put snd 'region-generation (1+ (c-get snd 'region-generation 0)))
			(c-set-selection! snd ch newselstart (+ newselstart newselframes) dassync)))))
	    (c-show-times (cursor) #t)))))
    
  (define (mouse-release-callback snd ch x y button state)
    (define (nofunc . x)
      #t)
    (let ((samp (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
      (if about-to-move
	  (begin
	    (add-hook! graph-hook nofunc)
	    (c-set-selection! snd ch (about-to-move 0) (+ (about-to-move 0) (about-to-move 1)) dassync)
	    (c-cut)
	    (c-set-cursor-pos samp)
	    (remove-hook! graph-hook nofunc)
	    (c-paste)
	    (c-set-selection! snd ch samp (+ samp (about-to-move 1)) dassync)
	    (set! about-to-move #f))
	  (begin
	    (if selection-starting-point
		(if (not (= samp selection-starting-point))
		    (begin
		      (if (< samp selection-starting-point)
			  (c-set-selection! snd ch samp selection-starting-point dassync)
			  (c-set-selection! snd ch selection-starting-point samp dassync))
		      (c-gc-on)
		      (gc))))
	    (c-show-times (cursor) #t)
	    (set! selection-starting-point #f))))
    (-> (c-p snd) selection-is-changed)
    (c-gc-on))


  (<mouse-cycle> mouse-press-callback mouse-motion-callback mouse-release-callback)

  )








;;##############################################################
;; Cut and Paste
;;##############################################################


;; Replace the C-y and C-w keybindings. These are equal, but writes
;; "Please wait" to the window bar, colorize the inserted region and
;; automaticly creates regions when needed if (selection-creates-region) is false.

(define c-iscolorized #f)

(define (color-samples-allchans chans color start end)
  (if (> chans 0)
      (begin
	(if (eq? color 'black)
	    (uncolor-samples (c-selected-sound) (- chans 1))
	    (color-samples color start end (c-selected-sound) (- chans 1)))
	(color-samples-allchans (- chans 1) color start end))))



(define (c-clear-report-hook snd ch)
  (remove-hook! after-graph-hook c-clear-report-hook)
  (c-report " ")
  #f)


;; Prints a message to the title-bar, and takes it away when the graph-hook is called.
(define (c-report-and-clear message)
  (c-report message)
  (add-hook! after-graph-hook c-clear-report-hook))



#!
;;This one could have been very nice, but the graph-hook is called very often,
;;and then the window bar changes title too often too, which is disturbing.
(add-hook! graph-hook
  (lambda (snd ch y0 y1)
    (c-report-and-clear "Please wait, updating time graph...")
    #f))
!#


(define (c-make-region)
  (if (not (selection-creates-region))
      (if (> (c-get (c-selected-sound) 'region-generation 0) 0)
	  (begin
	    (c-put (c-selected-sound) 'region-generation 0)
	    (c-report-and-clear "Please wait, making region...")
	    (make-region)))))

(bind-key (char->integer #\r) 0
	  c-make-region)


(define (c-paste)
  (if (c-selection?)
      (c-make-region))
  (if (> (length (regions)) 0)
      (begin
	(c-report "Please wait, inserting region...")
	(color-samples-allchans (channels) 'black 0 0)
	(let ((length (region-frames))
	      (curspos (cursor)))
	  (if (> length 5000000)
	      (begin
		(set! (with-background-processes) #f) ;; To prevent using horrible long time, and the little picture not to update properly.
		(insert-region curspos)
		(set! (with-background-processes) #t))
	      (insert-region curspos)) ;; The little picture might not update properly. But turning off background-processes slows down pasting of small regions a lot.
	  (if (> (sync) 0)
	      (color-samples-allchans (channels) green curspos (region-frames))
	      (color-samples green curspos (region-frames)))
	  (set! c-iscolorized #t)
	  (c-report-and-clear "Please wait, updating time graph...")
	  (update-time-graph)))))

(bind-key (char->integer #\y) 4
	  c-paste)


(define (c-cut)
  (if (c-selection?)
      (let ((curspos (c-selection-position)))
	(color-samples-allchans (channels) 'black 0 0)
	(c-set-cursor-pos curspos)
	(c-make-region)
	(c-report-and-clear "Please wait, deleting selection...")
	(delete-selection)
	(set! c-iscolorized #f))))

(bind-key (char->integer #\w) 4
	  c-cut)








;;##############################################################
;; Progress reporting (graphic)
;;##############################################################

(define c-last-report-value 0.0)

(define (progress-report pct . various) ;name current-channel channels snd)
  (c-get-bounds (c-selected-sound) 0
		(lambda (minx miny maxx maxy)
		  (let ((width (- maxx minx)))
		    (if (> (abs (- pct c-last-report-value)) (/ 1 width))
			(let ((old-color (foreground-color))
			      (height 10)
			      (new-report-value (/ (floor (* 100 pct)) 100)))
			  (if (and (defined? 'rational?)
				   (rational? new-report-value))
			      (set! new-report-value (exact->inexact new-report-value)))
			  (set! (foreground-color) blue)
			  (fill-rectangle minx miny (* pct width) height)
			  (set! (foreground-color) old-color)
			  (report-in-minibuffer (string-append (number->string new-report-value)
							       (if (member new-report-value '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
								   "0"
								   "")))
			  (set! c-last-report-value pct)))))))

(define  (start-progress-report . snd)
  (set! c-last-report-value 0.0)
  (report-in-minibuffer "0.00"))



(define (finish-progress-report . snd)
  (report-in-minibuffer ""))






;;##############################################################
;; Show times
;;##############################################################

;; Show cursor and selection position in minutes, seconds and 1/10th seconds.
;; This function is often called when playing, so I have tried as much as possible to avoid triggering a garbage collection.
;; (without very much (if any) success by the way).
;; The garbage collector can't just be turned off while playing either. I have tried many times,
;; but theres a huge chance everything goes bananas because of a swapping hell that is eventually
;; going to happen. Its a scary amount of memory that is allocated by guile just doing small things.

(define c-show-times-das

  (let (
	 
	(c-dodasprint
	 (let* ((lastpainted (list-tabulate 3 (lambda (n) (list-tabulate 4 (lambda (n) #f)))))
		(fontwidth 9)	
		(fontheight 12)
		(stringlen (* fontwidth 7))

		(sound-widget #f)
		(width #f)
		(x #f)
		(dim #f)

		(old-color #f)
		(height #f)
		(y #f))
	   
	   (lambda-non-cons (string level color framepos force)
	     (set! sound-widget (c-editor-widget (c-selected-sound)))
	     (set! width (- (car (widget-size sound-widget)) fontwidth))
	     (set! x (min (- width stringlen) (x->position (/ framepos (srate)))))
	     (set! dim (list-ref lastpainted level))
	     
	     (if (or force
		     (not (car dim))
		     (not (= x (car dim))))
		 (begin
		   (set! old-color (foreground-color))
		   (set! y (+ (list-ref (axis-info (c-selected-sound) 0) 11)
			      2
			      (* fontheight level)))
		   (if (car dim)
		       (begin
			 (set! (foreground-color) *c-backgroundcolor*)
			 ;(c-draw-string sound-widget
			;		(list-ref (snd-gcs) 1)
			;		(car dim) (+ fontheight (cadr dim))
			;		(cadddr dim))
			 (fill-rectangle (car dim) (cadr dim) (caddr dim) (+ 2 fontheight))
			 ))
		   (c-draw-string sound-widget
				  color
				  x (+ fontheight y)
				  string) 

		   (set-car! dim x)
		   (set-car! (cdr dim) y)
		   (set-car! (cddr dim) stringlen)
		   (set-car! (cdddr dim) string)
		   (set! (foreground-color) old-color))))))
	
	(get-time-string
	 (let* ((ret-string "        ")
		(intdiv (lambda-non-cons (a b)
			  (c-integer (/ a b))))
		(int->char (lambda-non-cons (i)
			     (integer->char (+ (char->integer #\0) i))))
		
		(time #f)
		(minutes #f)
		(minutes100 #f)
		(minutes10 #f)
		(minutes1 #f)
		(seconds #f)
		(seconds10 #f)
		(seconds1 #f)
		(seconds-1 #f)

		(p (lambda-non-cons (n)
		     (if (< minutes 10)
			 (max 0 (- n 2))
			 (if (< minutes 100)
			     (max 0 (- n 1))
			     n)))))
	   
	   (lambda-non-cons (dastime)
	     (set! time (/ (floor (* 10 (/ dastime (srate (c-selected-sound))))) 10))
	     (set! minutes (call-non-cons intdiv time 60))
	     (set! minutes100 (call-non-cons intdiv minutes 100))
	     (set! minutes10 (call-non-cons intdiv (- minutes (* minutes100 100)) 10))
	     (set! minutes1 (- minutes (* minutes100 100) (* minutes10 10)))
	     (set! seconds (/ (floor (* 10 (- time (* minutes 60)))) 10))
	     (set! seconds10 (call-non-cons intdiv seconds 10))
	     (set! seconds1 (c-integer (floor (- seconds (* seconds10 10)))))
	     (set! seconds-1 (c-integer (floor (* 10 (- seconds (* 10 seconds10) seconds1)))))

	     (if (< minutes 100)
		 (string-set! ret-string 7 #\ )
		 (string-set! ret-string 0 (call-non-cons int->char minutes100)))
	     (if (< minutes 10)
		 (string-set! ret-string 6 #\ )
		 (string-set! ret-string (call-non-cons p 1) (call-non-cons int->char minutes10)))
	     
	     (string-set! ret-string (call-non-cons p 2) (call-non-cons int->char minutes1))
	     (string-set! ret-string (call-non-cons p 3) #\:)
	     (string-set! ret-string (call-non-cons p 4) (call-non-cons int->char seconds10))
	     (string-set! ret-string (call-non-cons p 5) (call-non-cons int->char seconds1))
	     (string-set! ret-string (call-non-cons p 6) #\.)
	     (string-set! ret-string (call-non-cons p 7) (call-non-cons int->char seconds-1))
	     
	     ret-string)))
	
	
	(last-time-showed 0)
	(largestring "                                        ")

	(largetimechange #f)
	(stereocombined #f)
	(wanttoupdate #f))
    

    (lambda-non-cons (dastime force)
      (set! largetimechange (>= (abs (- dastime last-time-showed))
				(/ (srate (c-selected-sound)) 5)))
      (set! stereocombined (and (= (channels) 2) 
				(= (channel-style (c-selected-sound)) channels-combined)))
      (set! wanttoupdate (or force largetimechange))
      (if wanttoupdate
	  (set! last-time-showed dastime))

      (if stereocombined
	  (begin
	    (call-non-cons c-dodasprint (call-non-cons get-time-string dastime) 0 (list-ref (snd-gcs) 9) dastime wanttoupdate)
	    (if (and force (c-selection?))
		(begin
		  (call-non-cons c-dodasprint (call-non-cons get-time-string (c-selection-position)) 1 (list-ref (snd-gcs) 3) (c-selection-position) #t)
		  (call-non-cons c-dodasprint (call-non-cons get-time-string (+ (c-selection-position) (c-selection-frames))) 2 (list-ref (snd-gcs) 3) (+ (c-selection-position) (c-selection-frames)) #t))))
	  (if wanttoupdate
	      (report-in-minibuffer (if (c-selection?)
					(begin
					  (substring-move! (call-non-cons get-time-string dastime) 0 7 largestring 0)
					  (substring-move! (call-non-cons get-time-string (c-selection-position)) 0 7 largestring 9)
					  (substring-move! (call-non-cons get-time-string (+ (c-selection-position) (c-selection-frames))) 0 7 largestring 18)
					  largestring)
					(call-non-cons get-time-string dastime))))))))






;; Show the time in the minibuffer when playing
(let ((samplecount 0))
  (add-hook! play-hook
	     (lambda (samples)
	       (set! samplecount (+ samplecount (* 40 samples)))
	       (if (> samplecount (srate (c-selected-sound)))
		   (begin
		     (set! samplecount 0)
		     (c-show-times (cursor) #f)))
	       #f)))


;; ..And when updating the graphics.
(add-hook! after-graph-hook
	   (lambda (snd ch)
	     (if (= 0 ch)
		 (c-show-times (cursor snd) #t))
	     #f))







;;##############################################################
;; The Sync and Loop buttons
;;##############################################################

;; Doing several things after opening a file.
;; -Sync and Unite newly loaded files by default.
;;  The sync value should be unique for each sound.
;; -Replace the sync button with a new one that uses get-unique-sync-num
;;  as the sync-number. Simply using (+ 1 snd) doesn't work when a sound is closed
;;  in certain situations.
;; -Remove the play button. Its useless now with the way p, space and the mouse scroll-button is configured.
;; -Added a loop button where the play button was.

(define c-get-unique-sync-num
  (let ((unique-sync-num 0))
    (lambda ()
      (set! unique-sync-num (+ unique-sync-num 1))
      unique-sync-num)))

#!
(display-widget-tree (list-ref (sound-widgets (c-selected-sound)) 2))
(find-child (list-ref (sound-widgets (c-selected-sound)) 2) "snd-name-form")
!#

(add-hook! close-hook
	   (lambda (snd)
	     (c-for-each-nameform-button snd "loop" checkbutton-remove)
	     (c-for-each-nameform-button snd "sync" checkbutton-remove)
	     #f))

#!
(checkbutton-remove (c-get-nameform-button (c-selected-sound) "sync"))
!#

(define *loop-on-off-hook* (<hook>))


(let ((not-now #f))
  (add-hook! after-open-hook
	     (lambda (snd)

	       (checkbutton-remove (c-get-nameform-button snd "unite"))

	       (let ((oldplay (c-get-nameform-button snd "play")))
		 (<checkbutton> (c-get-nameform snd)
				"loop"
				(lambda (on)
				  (if (not not-now) ;; checkbutton-set or focus-widget triggers a button-get-active callback, which again
				      ;;;;;;;;;;;;;;;; cause checkbutton-set or focus-widget to be called for all other sounds, which again... etc.: and stack runs out.
				      (begin
					(set! not-now #t)
					(set! *c-islooping* on)
					(-> *loop-on-off-hook* run on)
					(for-each (lambda (s)
						    (if (not (= s snd)) (c-for-each-nameform-button s "loop"
												    (lambda (b) 
												      (checkbutton-set b *c-islooping*)))))
						  (sounds))
					(focus-widget (c-editor-widget snd))
					(set! not-now #f))))
				*c-islooping*
				(if use-gtk '() (list XmNx (car (widget-position oldplay)))))
		 (checkbutton-remove oldplay))

	       (if (> (channels snd) 1)
		   (set! (channel-style snd) channels-combined))
	       (set! (sync snd) (c-get-unique-sync-num))
	       
	       (let ((oldsync (c-get-nameform-button snd "sync")))
		 (<checkbutton> (c-get-nameform snd)
				"sync"
				(lambda (on)
				  (if on
				      (set! (sync snd) (c-get-unique-sync-num))
				      (set! (sync snd) 0))
				  
				  (focus-widget (c-editor-widget snd)))
				#t
				(if use-gtk '() (list XmNx (car (widget-position oldsync)))))
		 (checkbutton-remove oldsync))
	       
	       #f)))






;;##############################################################
;; Gain node-line
;;##############################################################

(define c-apply-envelope
  (let ((isenving #f))
    (lambda* (snd #:optional (upperval 2) (lowerval 0))
	     (if (not isenving)
		 (let* ((snd (c-selected-sound))
			(nodelines (c-get snd 'nodelines))
			(doit (lambda (ch)
				(let* ((nodeline (nodelines ch))
				       (sel (selection-member? snd ch))
				       (start (if sel (selection-position snd ch) 0))
				       (start2 (c-scale start 0 (frames snd ch) 0 1))
				       (length (if sel (selection-frames snd ch) (frames snd ch))))
				  (env-channel (apply append (map (lambda (xy) (list (- (car xy) start2) (c-scale (cadr xy) 0 1 upperval lowerval)))
								  (-> nodeline get-graph
								      start2
								      (c-scale (+ start length) 0 (frames snd ch) 0 1))))
					       start length snd ch)))))
		   (set! isenving #t)
		   (c-for-each-channel snd doit)
		   (set! isenving #f))))))


(define (c-make-envelope snd)
  (c-put snd 'nodelines
	 (<array/map> (channels snd)
		      (lambda (ch)
			(<editor-nodeline> snd ch 0.5
					   (lambda (val)
					     (format #f "~1,3f" (c-scale val 0 1 2 0)))
					   (lambda (this)
					     (if (c-sync? snd)
						 (c-for-each-channel2 snd
								      (lambda (newch)
									(if (not (= ch newch))
									    (-> ((c-get snd 'nodelines) newch) set-graph!
										(-> this get-graph))))))))))))
  
(define (c-envfunc1)
  (lambda ()
    (c-apply-envelope (c-selected-sound))))
(define (c-envfunc-hook snd)
  (c-display "c-envfunc3 c-snd" snd)
  (c-make-envelope snd)
  #f)

(define *c-envelope-enabled* #f)

(define (c-enable-envelope)
  (if (not *c-envelope-enabled*)
      (begin
	(set! *c-envelope-enabled* #t)
	(for-each (lambda (snd)
		    (c-make-envelope snd))
		  (sounds))
	(bind-key (char->integer #\v) 0 c-envfunc1)
	(-> *c-after-open-hook* add! c-envfunc-hook))))

(define (c-disable-envelope)
  (if *c-envelope-enabled*
      (begin
	(set! *c-envelope-enabled* #f)
	(bind-key (char->integer #\v) 0 (lambda x x))
	(for-each (lambda (snd)
		    (let ((envbutton (c-get snd 'envbutton)))
		      (if envbutton
			  (-> envbutton remove)))
		    (let ((nodelines (c-get snd 'nodelines)))
		      (if nodelines
			  (begin
			    (-> nodelines for-each
				(lambda (n nodeline)
				  (-> nodeline delete!)))))))
		  (sounds))
	(c-redraw)
	(-> *c-after-open-hook* remove! c-envfunc-hook))))

(add-hook! after-open-hook
	   (lambda (snd)
	     (c-put snd 'envbutton (<button> (c-get-nameform snd)
					     "Env"
					     (lambda ()
					       (if (not *c-envelope-enabled*)
						   (c-enable-envelope)
						   (c-apply-envelope snd)))))
	     (gtk_widget_set_name (-> (c-get snd 'envbutton) button) "doit_button")))

(add-hook! close-hook
	   (lambda (snd)
	     (-> (c-get snd 'envbutton) remove)))

(if *c-use-envelope*
    (c-enable-envelope))


;;(add-hook! after-open-hook
;;	   (lambda (snd)
	     
#!
(c-disable-envelope)
(c-enable-envelope)
!#



;;##################################################################
;; Focus
;;##################################################################

#!
;; The brutal method:
(define (fokus-right)
  (let ((snd (c-selected-sound)))
    (if snd
	(focus-widget (c-editor-widget snd))))
  (in 100 fokus-right))

(fokus-right)
!#

(add-hook! mouse-enter-graph-hook
  (lambda (snd chn)
    (if (sound? snd) (focus-widget (car (channel-widgets snd chn))))))

(add-hook! mouse-leave-text-hook
	   (lambda (w)
	     (in 100
		 (lambda ()
		   (let ((snd (c-selected-sound)))
		     (if snd
			 (focus-widget (c-editor-widget snd))))))))

(add-hook! mouse-enter-listener-hook
	   (lambda (widget)
	     (focus-widget widget)))


(define *c-control-hook* (<hook>))
(define *c-controls-on-off-hook* (<hook>))

;; All checkbuttons in the control-panel and the channel widgets.
(add-hook! after-open-hook
	   (lambda (snd)
	     (let ((ws '()))
	       (define (fixit w)
		 (if (not (member w ws))
		     (begin
		       (set! ws (cons w ws))
		       (for-each-child w
				       (lambda (w)
					 (if (and (GTK_IS_BUTTON w)
						  (string? (gtk_button_get_label (GTK_BUTTON w)))
						  (string=? "Apply" (gtk_button_get_label (GTK_BUTTON w))))
					     (begin
					       (c-remove-handler w "clicked")
					       (g_signal_connect w "clicked"
								   (lambda (w c)
								     (if (selection-member? snd)
									 (apply-controls snd 2)
									 (apply-controls))
								     (focus-widget (c-editor-widget snd))
								     #t)))
					     (if (GTK_IS_CHECK_BUTTON w)						 
						 (g_signal_connect w "button_release_event"
								     (lambda (w e i)
								       (-> (c-p snd) dosomepause (lambda ()
												   (gtk_button_released (GTK_BUTTON w))
												   (-> *c-controls-on-off-hook* run)
												   (focus-widget (c-editor-widget snd))))))))

					 (if (and (GTK_IS_SCROLLBAR w)
						  (GTK_IS_RANGE w))
					     (let ((adj (gtk_range_get_adjustment (GTK_RANGE w))))
					       (g_signal_connect adj "value_changed"
								   (lambda (w d)
								     (-> *c-control-hook* run)))))
					 
					 (if (GTK_IS_CONTAINER w)
					     (fixit w)))))))
	       (c-for-each-channel2 snd
				    (lambda (ch)
				      (for-each (lambda (w)
						  (if (GTK_IS_CONTAINER w)
						      (fixit w)))
						(channel-widgets snd ch))))
	       (fixit (caddr (sound-widgets snd))))))


	   
;;##############################################################
;; dac-size slider in the control-panel
;;##############################################################

(add-to-menu 3 "Set dac size"
	     (lambda ()
	       (letrec ((d (<dialog> "Set dac size" #f
				     "Close" (lambda () (-> d hide))))
			(snd (c-selected-sound))
			(must-wait-more #f)
			(iswaiting #f))
		 (define (waitfunc)
		   (in 50
		       (lambda ()
			 (if must-wait-more
			     (begin
			       (set! must-wait-more #f)
			       (waitfunc))
			     (begin
			       (-> (c-p snd) continue)
			       (set! iswaiting #f))))))
		 (<slider> d "dac-size" 1 (dac-size) 8192
			   (lambda (val)
			     (let ((isplaying (c-playing)))
			       (if isplaying
				   (-> (c-p snd) pause))
			       (set! (dac-size) (c-integer val))
			       (focus-widget (c-editor-widget snd))
			       (if isplaying
				   (if iswaiting
				       (set! must-wait-more #t)
				       (begin
					 (set! iswaiting #t)
					 (waitfunc))))))
			   1)
		 (-> d show)
		 (gtk_window_resize (GTK_WINDOW (-> d dialog)) (window-width) (cadr (widget-size (-> d dialog)))))
	       ))

#!
(set! (verbose-cursor) #t)
(set! (with-tracking-cursor) #f)
(cursor-update-interval)
(set! (tracking-cursor-style 0 0) 1)
!#




;;##############################################################
;; Buffer-menu
;;##############################################################

;; Makes the buffer-menu a bit more pleasent to use. This is code
;; copied from examp.scm and modified.

(c-load-from-path examp)


;;;;;;;;;;;;;;;;
;;(set! last-height 500)
;;(set! last-width 700)

(define c-sounds-in-menu '())
(define (c-remove-sounds-in-menu filename)
  (set! c-sounds-in-menu (remove (lambda (name) (string=? name filename))
				 c-sounds-in-menu)))

;; For gtk, the -notebook switch doesn't seem to work very well.
;; For motif, the -notebook switch doesn't seem to work very well either.

(if (or (not use-gtk)
	(not (string=? "GtkNotebook" (gtk_widget_get_name (list-ref (main-widgets) 5)))))

    (let ((buffer-menu (add-to-main-menu "Buffers")))

      (define* (c-switch-to-buf filename #:optional from-select-sound)
	(let* ((width (car (widget-size (car (sound-widgets (car current-buffer))))))
	       (height (cadr (widget-size (car (sound-widgets (car current-buffer)))))))
	  
	  ;;(c-display "saving:" (c-selected-sound))
	  (if (selection-member? (c-selected-sound))
	      (begin
		(c-put (c-selected-sound) 'selection-position (c-selection-position))
		(c-put (c-selected-sound) 'selection-frames (c-selection-frames))))
	  
	  (call-with-current-continuation
	   (lambda (give-up)
	     (if (or (not (string? filename))
		     (= (string-length filename) 0))
		 (let ((temp current-buffer))
		   (if last-buffer
		       (set! current-buffer last-buffer)
		       (let ((index (new-sound)))
			 (set! current-buffer (list index 0))))
		   (set! last-buffer temp))
		 (let ((index (find-sound filename)))
		   (if index
		       (begin
			 (set! last-buffer current-buffer)
			 (set! current-buffer (list index 0)))
		       (give-up (report-in-minibuffer (format #f "can't find ~A" response))))))
	     (close-all-buffers)
	     (report-in-minibuffer "")
	     (open-current-buffer width height)
	     (set! (selection-member? #t) #f)
	     (let ((selpos (c-get (c-selected-sound) 'selection-position)))
	       ;;(c-display (c-selected-sound) selpos)
	       (if selpos
		   (begin
		     (c-set-selection-position! (c-selected-sound) 0 selpos #t)
		     (c-set-selection-frames! (c-selected-sound) 0 (c-get (c-selected-sound) 'selection-frames) #t))))))))

      
      ;;(set! (widget-size (car (sound-widgets (car current-buffer)))) (list 1000 1000) )
      (define position 0)
      ;;(define num 0)
      (define (c-open-buffer filename)
	(if (c-isloaded? filename)
	    (begin
	      (c-switch-to-buf filename)
	      #t)
	    (letrec ((was-starting-up? *c-is-starting-up*)
		     (is-in-menu? (member filename c-sounds-in-menu))
		     (add-buffer-menu (lambda (position addstar?)
					(let ((gen-filename filename ));; (let ((ret (string-append (number->string num) " " filename))) (set! num (1+ num)) ret)))
					  (add-to-menu buffer-menu 
						       gen-filename
						       (lambda ()
							 (if was-starting-up?
							     (begin
							       ;;(c-display "here")
							       (c-remove-sounds-in-menu filename)
							       (remove-from-menu buffer-menu gen-filename)
							       (open-sound filename))
							     (begin
							       ;;(c-display "not here")
							       (remove-from-menu buffer-menu gen-filename)
							       (add-buffer-menu position #t)
							       (c-switch-to-buf filename))))
						       position)))))
	      (if *c-is-starting-up*
		  (set! c-sounds-in-menu (cons filename c-sounds-in-menu)))
	      (add-buffer-menu position #f)
	      ;;(c-display "position" position)
	      (if (not was-starting-up?)
		  (set! position (1+ position)))
	      *c-is-starting-up*)))

      (define (c-close-buffer snd)
	"(close-buffer snd) removes the menu item associated with snd (use with close-hook)"
	(let ((filename (file-name snd)))
	  ;;(remove-from-menu buffer-menu (string-append "* " filename))
	  (remove-from-menu buffer-menu filename)
	  (remove-from-menu buffer-menu filename)
	  (c-remove-sounds-in-menu filename))
	  (if (and (= 1 (length (sounds)))
		   (> (length c-sounds-in-menu) 1))
	      (let ((filename (find (lambda (name)
				      (not (member name (map file-name (sounds)))))
				    c-sounds-in-menu)))
		(if filename
		    (begin
		      ;;(c-display "opening " filename)
		      (open-sound filename)))))
	#f)

      (add-hook! select-sound-hook (lambda (snd)
				     (if current-buffer
					 (let* ((width (car (widget-size (car (sound-widgets (car current-buffer))))))
						(height (cadr (widget-size (car (sound-widgets (car current-buffer)))))))
					   (set! current-buffer (list snd 0))
					   (let ((sound-pane (car (sound-widgets (car current-buffer)))))
					     (if sound-pane
						 (begin
						   (close-all-buffers)
						   (show-widget sound-pane))))))))

      (add-hook! open-hook c-open-buffer)
      (add-hook! close-hook c-close-buffer)
      ;;(bind-key (char->integer #\b) 0 (lambda (x) (switch-to-buf)))
      (add-hook! close-hook xb-close)
      (add-hook! after-open-hook
		 (lambda (snd)
		   (set! (selection-member? #t) #f)))
      (add-hook! after-open-hook xb-open)))





;;##############################################################
;; Various
;;##############################################################

;;"various generally useful Snd extensions". See source.
(c-load-from-path extensions)


;; When exiting.
(check-for-unsaved-edits #t)


(c-load-from-path edit-menu)


;;Show the little picture of the whole sound in the upper right corner.
(c-load-from-path draw)
(make-current-window-display)


;; The background-process slows things down when the little picture is active and loading large files.
;; Better turn off the background-process when loading.
(add-hook! open-hook (lambda (filename) (if (c-isloaded? filename)
					    #t
					    (begin
					      (c-report (string-append "Please wait, making waveform-data for \"" filename "\"."))
					      (set! (with-background-processes) #f)
					      #f))))
(add-hook! after-open-hook (lambda (filename)
			     (set! (with-background-processes) #t)
			     (c-report " ")
			     #f))



;; Adds a lot of things when pressing the right mouse button. Very nice.
(if use-gtk
    (c-load-from-path gtk-popup)
    (c-load-from-path popup))


(if use-gtk
    (c-load-from-path gtk-effects)
    (c-load-from-path new-effects))


(if (provided? 'snd-ladspa)
    (c-load-from-path ladspa))



;; Stores the peak information for sounds in the ~/peaks/ directory. Seems to work correctly. I have tried
;; to fool it in many ways, but it seems to be very intelligent. Extremely nice.
;; The point is to decrease the loading time.
; First make sure the peaks directory is present
(system (string-append "mkdir " (getenv "HOME") "/peaks >/dev/null 2>/dev/null"))
(c-load-from-path peak-env)
(add-hook! after-open-hook save-peak-env-info)
;;(add-hook! after-save-hook save-peak-env-info)
;;(add-hook! after-save-as-hook save-peak-env-info)

(if use-gtk
    (c-load-from-path snd-gtk)
    (c-load-from-path snd-motif))


;; Shows diskspace for the partition the opened sound was placed on.
;(add-hook! after-open-hook show-disk-space)

;; Add extra control for the controls dialog.
(make-hidden-controls-dialog)

;; Add rename option to the file menu.
(if (not use-gtk)
    (add-rename-option))



;; Dave Phillips fft-menu.
(if (not (provided? 'snd-fft-menu.scm))
    (load-from-path "fft-menu.scm"))

;; Dave Phillips panic-menu
(if (not (provided? 'snd-panic.scm))
    (load-from-path "panic.scm"))

;; Dave Phillips special-menu
;;(if (not (provided? 'snd-special-menu.scm))
;;    (load-from-path "special-menu.scm"))


;; (load-from-path "plugins-menu.scm")


;; All of Dave Phillips nice things.
;; Uncomment to try. (Most of it is already present)
;;(if (not (defined? 'change-selection-popup-color))
;;  (load-from-path "misc.scm"))


(add-hook! after-save-as-hook
   (lambda (snd filename from-dialog)
     (if from-dialog
         (open-sound filename))))



;;##############################################################
;; Add/Remove things to the edit-menu
;;##############################################################

;;; -------- Insert/append file
;;; Code copied from dlp/special-menu

(add-to-menu edit-menu #f #f)

;;; this is now built-in (27-Jul-05)
#!
(add-to-menu edit-menu "Insert file"
  (lambda ()
    (select-file
      (lambda (filename)
	(let ((chans (mus-sound-chans filename)))
	  (if (= 1 chans)
	      (insert-sound filename (cursor) 0 (c-selected-sound) (selected-channel))
	      (insert-sound filename))))
      "Insert file" "." "*" "File will be inserted at cursor location.")))
!#

(define (append-sound filename)
  ;; appends sound file
  (insert-sound filename (frames)))

(add-to-menu edit-menu "Append file"
  (lambda ()
    (select-file
     (lambda (filename)
       (append-sound filename)))))

(add-to-menu edit-menu #f #f)

(for-each (lambda (name) (remove-from-menu edit-menu name))
	  (list "Delete Selection"
		"Insert Selection"
		"Play Selection"
		"Mix Selection"))
		 


;;##############################################################
;; Load rt-player. Takes some time.
;;##############################################################


(eval-c (string-append "-I" snd-header-files-path)
	"#include <_sndlib.h>"
	(proto->public "int mus_audio_api(void)")
	(variables->public
	 (<int> MUS_JACK_API MUS_ALSA_API MUS_OSS_API)))

(if (= (mus_audio_api) (MUS_JACK_API))
    (load-from-path "rt-player.scm"))



;;(c-display "GGGGGGGGGG1")


;;##############################################################
;; Store and restore list of used sound-files
;;##############################################################

(define (c-open-sounds-filename)
  (string-append (getenv "HOME") "/.snd_soundfilelist"))


;; Save all filenames when exiting.
(define (c-save-all-filenames filename)
  (let ((fd (open-file filename "w")))
    (if (c-selected-sound)
	(for-each (lambda (filename)
		    (write-line filename fd))
		  (reverse (delete-duplicates (append (map (lambda (snd) (file-name snd))
							   (cons (c-selected-sound) (sounds)))
						      c-sounds-in-menu)))))
    (close fd)))


(add-hook! exit-hook (lambda args
		       (c-save-all-filenames (c-open-sounds-filename))
		       #f))
				   

(add-hook! after-open-hook (lambda args
			     (if (not *c-is-starting-up*)
				 (c-save-all-filenames (c-open-sounds-filename)))
			     #f))
			

(add-hook! after-open-hook
	   (lambda (snd)
	     (hash-set! c-snd-putgetdata snd (make-hash-table 16))
	     (-> *c-after-open-hook* run snd)
	     #f))


;; Load GTK mnemonics code from Maxim Krikun.
(load-from-path "kmenu.scm")



;;##############################################################
;; Load files from previous session.
 ;;##############################################################
 
(if *c-restore-previous-session*
    (let ((filename #f))
      (system (string-append "touch " (c-open-sounds-filename) " >/dev/null 2>/dev/null"))
      (for-each-line-in-file (c-open-sounds-filename)
			     (lambda (line)
			       (if (not (access? line F_OK))
				   (c-display "File \"" line "\" not found.")
				   (catch #t
					  (lambda ()
					    (if filename
						(open-sound filename))
					    (set! filename line))
					  (lambda (key . args)
					    (c-display "File \"" line  "\" not found.")
					    #f)))))
      (if filename
	  (begin
	    (set! *c-is-starting-up* #f)
	    (open-sound filename)
	    (set! *c-is-starting-up* #t)))
      (in (if use-gtk 0 2000)
	  (lambda ()
	    (set! (window-width) 800)
	    (set! (window-height) 600)))))






;;##############################################################
;; Try to make the window-size stay the same.
;; Seems to work for me at least.
;;##############################################################

(let ((width 800)
      (height 600)
      (num-retries 0)
      (isretrying #f))
		   

  (define (dotheretry)
    (if (> num-retries 0)
	(begin
	  (if (not (= (window-width) width))
	      (set! (window-width) width))
	  (if (not (= (window-height) height))
	      (set! (window-height) height))
	  (set! num-retries (1- num-retries))
	  (in 2 dotheretry))
	(set! isretrying #f)))

  (add-hook! open-hook
	     (lambda (filename)
	       (set! height (window-height))
	       (set! width (window-width))
	       (set! num-retries 250)))
  
  (add-hook! close-hook
	     (lambda (snd)
	       (set! height (window-height))
	       (set! width (window-width))
	       (set! num-retries 250)))
  
  (add-hook! graph-hook
	     (lambda (snd chn y0 y1)
	       (if (not isretrying)
		   (begin
		     (set! isretrying #t)
		     (dotheretry))))))
  

(set! load-from-path c-old-load-from-path)


(set! *c-is-starting-up* #f)



;(define dhg
;  (lambda (snd)
;    (if (show-controls)
;	(gtk_paned_set_position (GTK_PANED (car (sound-widgets snd))) (c-integer (max 100 (min (- (window-height) 400) (* (window-height) 0.5))))))
;    (if (show-listener)
;	(gtk_paned_set_position (GTK_PANED (list-ref (main-widgets) 3)) (c-integer (* (window-height) 0.75))))))
;
;(add-hook! graph-hook (lambda x (dhg (car x))))


(if #f
    (begin
      (c-display "ai")
      (while (= 1 (gtk_events_pending))
	     (gtk_main_iteration))
      
      (if (= (mus_audio_api) (JACK_API))
	  (load-from-path "rt-player.scm"))))


(c-display)
(c-display "#:snd_conffile.scm loaded.")
(c-display (if (not (provided? 'snd-rt-player.scm))
	       "SND is currently not using jack, so the RT-Player is not initialized."
	       "The RT-Player is currently configured only to run when not using\n LADSPA or the Expand, Contrast, Reverb or Filter control."))



(set! (show-listener) #f)

;;(gtk_window_set_title (GTK_WINDOW (cadr (main-widgets))) "Snd")

(while (= 1 (gtk_events_pending))
       (gtk_main_iteration))


(let ((selected-sound (c-selected-sound)))
  (if selected-sound
      (focus-widget (c-editor-widget selected-sound))))



