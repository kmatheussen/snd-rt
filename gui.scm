
;;; Functions to help making various gui-things more convenient and without
;;; worrying about whether we use gtk or motif. (see ladspa.scm and snd_conffile.scm for examples of use)
;;; -Kjetil S. Matheussen.

(if (provided? 'snd-gui.scm)
    (begin
      (display "Very warning: gui.scm has already been loaded. (This is not good.)")
      (newline)))


(provide 'snd-gui.scm)

(use-modules (ice-9 optargs)
	     (ice-9 format)
	     (srfi srfi-1)
	     (srfi srfi-13))


;;(use-modules (oop goops))


(define-macro (c-load-from-path filename)
  `(if (not (provided? (symbol-append 'snd- ',filename '.scm)))
       (load-from-path (symbol->string (symbol-append ',filename '.scm)))))




;;##############################################################
;; Load files
;;##############################################################

(c-load-from-path eval-c)




;;##############################################################
;; Various functions
;;##############################################################

(define use-gtk (if (provided? 'snd-gtk)
		    #t
		    #f))


(if (not use-gtk)
    (begin
      (display "Warning, gui.scm might not work very well with Motif anymore.")(newline)
      (display "You should compile up Snd using the --with-gtk and --with-static-xm configure options.")(newline)))


(if use-gtk
    (if (not (provided? 'xg))
	(let ((hxm (dlopen "xg.so")))
	  (if (string? hxm)
	      (snd-error (format #f "gui.scm needs the xg module: ~A" hxm))
	      (dlinit hxm "Init_libxg"))))
    (if (not (provided? 'xm))
	(let ((hxm (dlopen "xm.so")))
	  (if (string? hxm)
	      (snd-error (format #f "gui.scm needs the xm module: ~A" hxm))
	      (dlinit hxm "Init_libxm")))))
    






(define (c-integer somekindofnumberorsomething)
;;    somekindofnumberorsomething)
  (inexact->exact (floor somekindofnumberorsomething)))

(define (c-integer2 somekindofnumberorsomething)
  (inexact->exact (floor somekindofnumberorsomething)))

(define (c-editor-widget snd)
  (list-ref (channel-widgets snd 0) 0))


(define (c-selected-sound)
  (cond ((selected-sound) (selected-sound))
	((not (null? (sounds)))
	 (set! (selected-sound) (car (sounds)))
	 (selected-sound))
	(else
	 #f)))


;; Copied from gtk-popup.scm
(define* (c-g_signal_connect obj name func #:optional data)
  (g_signal_connect_closure_by_id (GPOINTER obj)
				  (g_signal_lookup name (G_OBJECT_TYPE (GTK_OBJECT obj)))
				  0
				  (g_cclosure_new func data #f)
				  #f))




(define-c <float> (c-scale ((<float> x)
			    (<float> x1)
			    (<float> x2)
			    (<float> y1)
			    (<float> y2))
			   (return (+ y1
				      (/ (* (- x x1)
					    (- y2 y1))
					 (- x2 x1))))))

(define (c-sync? snd)
  (> (sync snd) 0))



(define (for-each-line-in-file filename func)
  (let* ((fd (open-file filename "r"))
	 (line (read-line fd)))
    (while (not (eof-object? line))
	   (func line)
	   (set! line (read-line fd)))))

(define (c-for-each-channel2 snd func)
  (c-for 0 < (chans snd) 1 func))

(define (c-for-each-channel snd func)
  (if (c-sync? snd)
      (let ((oh-yes #f))
	(for-each (lambda (gakk)
		    (if (= (car gakk) snd)
			(begin
			  (set! oh-yes #t)
			  (func (cadr gakk)))))
		  (selection-members))
	(if (not oh-yes)
	    (c-for-each-channel2 snd func)))
      (func (selected-channel snd))))

(define (c-get-channel snd y)
  (define (get-channel ch)
    (if (>= ch (chans snd))
	#f
	(let ((axinfo (axis-info snd ch)))
	  (if (and (>= y (list-ref axinfo 13))
		   (< y (list-ref axinfo 11)))
	      ch
	      (get-channel (1+ ch))))))
  (get-channel 0))


(define (c-get-bounds snd ch func)
  (let ((axinfo (axis-info snd ch)))
    (func (list-ref axinfo 10)
	  (list-ref axinfo 13)
	  (list-ref axinfo 12)
	  (list-ref axinfo 11))))

(define (c-get-mouse-info2 snd x y mustcall func)
  (let ((ch (if (number? mustcall) mustcall (c-get-channel snd y))))
    (if (and mustcall
	     (not ch))
	(set! ch 0))
    (if ch
	(let* ((axinfo (axis-info snd ch)))
	  (if (or mustcall
		  (and (>= x (list-ref axinfo 10))
		       (< x (list-ref axinfo 12))))
	      (func ch
		    (c-scale (* (srate snd) (position->x x snd ch)) (list-ref axinfo 0) (list-ref axinfo 1) 0 1)
		    (c-scale (position->y y snd ch) 1 -1 0 1)))))))


(define (c-get-mouse-info snd x y mustcall func)
  (let ((axinfo (axis-info snd 0))
	(ch 0)
	(xmax 0)
	(ymax 0)
	(newx 0)
	(newy 0))
    (define (set-channel!)
      (if (>= ch (chans snd))
	  #f
	  (begin
	    (set! axinfo (axis-info snd ch))
	    (if (and (>= y (list-ref axinfo 13))
		     (< y (list-ref axinfo 11)))
		#t
		(begin
		  (set! ch (1+ ch))
		  (set-channel!))))))
    (let ((legalchannel (set-channel!)))
      (if (or mustcall
	      (and legalchannel
		   (>= x (list-ref axinfo 10))
		   (< x (list-ref axinfo 12))))
	  (begin
	    (set! ch (min (- (chans snd) 1) ch))
	    (set! axinfo (axis-info snd ch))
	    (set! xmax (- (list-ref axinfo 12) (list-ref axinfo 10)))
	    (set! ymax (- (list-ref axinfo 11) (list-ref axinfo 13)))
	    (set! newx (min xmax (max 0 (- x (list-ref axinfo 10)))))
	    (set! newy (min ymax (max 0 (- y (list-ref axinfo 13)))))
	    (func ch x newy))))))

;; Taken from new-effects.scm
(define yellow-pixel
  (let ((pix #f))
    (lambda ()
      (if (not pix)
	  (let* ((shell (cadr (main-widgets)))
		 (dpy (XtDisplay shell))
		 (scr (DefaultScreen dpy))
		 (cmap (DefaultColormap dpy scr))
		 (col (XColor)))
	       (if (= (XAllocNamedColor dpy cmap "yellow" col col) 0)
		   (snd-error "can't allocate yellow!")
		   (set! pix (.pixel col)))))
      pix)))


(define (c-report das-text)
  (let ((text (if (string=? " " das-text)
		  (string-append "snd: "
				 (apply string-append (map (lambda (snd) (string-append (short-file-name snd) ", "))
							   (reverse (cdr (sounds)))))
				 (short-file-name (car (sounds))))
		  das-text)))
    (if use-gtk
	(gtk_window_set_title (GTK_WINDOW (cadr (main-widgets))) text)
	(if (defined? 'change-window-property)
	    (change-window-property "SND_VERSION" "WM_NAME" text)
	    (set! (window-property "SND_VERSION" "WM_NAME") text)))))
  

;; Set cursor-style. Copied from the manual.
(define (c-set-sound-cursor snd shape)
  (do ((j 0 (1+ j)))
      ((= j (channels snd)) #f)
    (set! (tracking-cursor-style snd j) shape)
    (set! (cursor-style snd j) shape)))


(define (c-scale-do x x1 x2 y1 y2)
  (+ y1
     (/ (* (- x x1)
	   (- y2 y1))
	(- x2 x1))))

(define (c-scale-do2 x y1 y2)
  (+ y1 (* x (- y2 y1))))

(define (c-scale-do3 x x1 x2)
  (/ (- x x1)
     (- x2 x1)))

(define-macro (c-scale2 x x1 x2 y1 y2)
  (if (and (number? x1) (number? x2) (= 0 x1) (= 1 x2))
      `(c-scale-do2 ,x ,y1 ,y2)
      (if (and (number? y1) (number? y2) (= 0 y1) (= 1 y2))
	  `(c-scale-do3 ,x ,x1 ,x2)
	  `(c-scale-do ,x ,x1 ,x2 ,y1 ,y2))))

(define (insert! list pos element)
  (if (= 0 pos)
      (cons element list)
      (let ((f (drop list (- pos 1))))
	(set-cdr! f (cons element (cdr f)))
	list)))

(define (sublist l start end)
  (take (drop l start) (- end start)))

;;(sublist '(0 1 2 3 4 5 6 7 8 9) 2 5)

;; A desperate way to minimize consing. (Guile is definitely not a realtime friendly language...)

(c-for 0 < 50 1
       (lambda (n)
	 (define-toplevel (string->symbol (format #f "c-arg~A" n)) 0)))


(define-macro (lambda-non-cons args . body)
  (let ((lets '())
	(sets '()))
    (c-for-each (lambda (n val)
		  (set! lets (cons (list  val #f) lets))
		  (set! sets (cons (list 'set!
					 val
					 (string->symbol (format #f "c-arg~A" n)))
				   sets)))
		args)
    `(let (,@lets)
       (lambda ()
	 ,@sets
	 ,@body))))


(define-macro (call-non-cons func . args)
  (let ((sets '()))
    (c-for-each (lambda (n val)
		  (set! sets (cons (list 'set!
					 (string->symbol (format #f "c-arg~A" n))
					 val)
				   sets)))
		args)
    `(begin
       ,@(reverse sets)
       (,func))))



(define-macro (add-call-hook! funcname func)
  `(define ,funcname
     (let ((func-old ,funcname))
       (lambda args
	 (apply ,func args)
	 (apply func-old args)))))

(define-macro (add-called-hook! funcname func)
  `(define ,funcname
     (let ((func-old ,funcname))
       (lambda args
	 (apply func-old args)
	 (apply ,func args)))))



(define c-gc-isoff #f)

;; The following functions ensure that gc-off is not called more times than gc-on.
;; It takes an optional argument which is the number of milliseconds before turning
;; the garbage collector on again, no matter what. Set this arguement to #f to avoid
;; automaticly turning the garbage collector on. But that is dangerous.
;;(define* (c-gc-off #:optional (timeout 2000))
(define* (c-gc-off #:optional (timeout 2000))
  (if (not c-gc-isoff)
      (begin
	(gc-off)
	(set! c-gc-isoff #t)
	(if timeout
	    (in timeout
		(lambda ()
		  (c-gc-on)))))))


;; Ouch, might actually freeze the machine if using jack. Better turn the function off.
;; (re-mlocking all memory in audio.c without using MCL_FUTURE seems to fix the freezing problem)
;(define (c-gc-off)
;  #t)

(define (c-gc-on)
  (if c-gc-isoff
      (begin
	(gc-on)
	(set! c-gc-isoff #f))))







;;##############################################################
;; Mouse stuff. All c-code mouse handling in snd is disabled,
;; and replaced with scheme-code. Selection handling is
;; handled in snd_conffile.scm.
;;##############################################################


(define mouse-button-press-hook (<hook>))
(define mouse-doubleclick-hook (<hook>))
(define mouse-move-hook (<hook>))
(define mouse-drag2-hook (<hook>))
(define mouse-button-release-hook (<hook>))
(define mouse-scroll-hook (<hook>))
(define selection-changed2-hook (<hook>))


(if (not use-gtk)
    (c-display "c-remove-handler not implemented for motif")
    (eval-c (string-append (string #\`) "pkg-config --cflags gtk+-2.0" (string #\`))
	      "#include <gtk/gtk.h>"

	      (public
	       (<void> c-remove-handler (lambda ((<gpointer> g)
						 (<char-*> handlername))
					  (g_signal_handler_disconnect g (g_signal_handler_find g
												G_SIGNAL_MATCH_ID
												(g_signal_lookup handlername (G_OBJECT_TYPE g))
												0 0 0 0))))
	       
	       (<void> c-draw-string (lambda ((<GtkWidget-*> widget) (<GdkGC-*> gc) (<int> x) (<int> y) (<char-*> text))
				       (let* ((font <GdkFont*> (gtk_style_get_font widget->style)))
					 (gdk_draw_string widget->window font gc x y text)))))))




;; Remove all gtk mousehandlers for a widget.
(define (c-remove-mousehandlers w)
  (c-remove-handler w "motion_notify_event")
  (c-remove-handler w "button_press_event")
  (c-remove-handler w "button_release_event")
  (c-remove-handler w "scroll_event"))


(add-hook! after-open-hook
	   (lambda (snd)
	     (let ((w (c-editor-widget snd)))
	       (if (not use-gtk)
		   (begin
		     (XtAddEventHandler w ButtonPressMask #f 
					(lambda (w c e f)
					  (-> mouse-button-press-hook run 
					      snd (.x e) (.y e) (.state e))))
		     (XtAddEventHandler w ButtonMotionMask #f 
					(lambda (w c e f)
					  (-> mouse-drag2-hook run 
					      snd (.x e) (.y e) (.state e))))
		     (XtAddEventHandler w ButtonReleaseMask #f 
					(lambda (w c e f)
					  (-> mouse-button-release-hook run 
					      snd (.x e) (.y e) 1 (.state e)))))
		   (let ((ispressed #f)
			 (ismoved #f))
		     
		     (c-remove-mousehandlers w)

		     (c-g_signal_connect w "button_press_event"
					 (lambda (w e i)
					   (focus-widget w)
					   (set! ispressed #t)
					   (set! ismoved #f)
					   (let ((did-doubleclick? 'nope)
						 (did-singleclick? 'nope))
					     (if (= (.type (GDK_EVENT_BUTTON e)) GDK_2BUTTON_PRESS)
						 (set! did-doubleclick? (-> mouse-doubleclick-hook run
									    snd 
									    (.x (GDK_EVENT_BUTTON e))
									    (.y (GDK_EVENT_BUTTON e))
									    (.state (GDK_EVENT_BUTTON e)))))
					     (if (not (eq? 'stop! did-doubleclick?))
						 (set! did-singleclick? (-> mouse-button-press-hook run
									    snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e))
									    (.button (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))))
					     
					     (if (and (not (eq? 'stop! did-doubleclick?))
						      (not (eq? 'stop! did-singleclick?))
						      (= (.button (GDK_EVENT_BUTTON e)) 3))
						 (run-hook gtk-popup-hook w e i snd 0)))))
		     (c-g_signal_connect w "motion_notify_event"
					 (lambda (w e i)
					   (set! ismoved #t)
					   ;;(c-display snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e)) (.button (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))
					   (let ((args (if (.is_hint (GDK_EVENT_MOTION e))
							   (let ((s (cdr (gdk_window_get_pointer (.window e)))))
							     (list snd (car s) (cadr s) (.button (GDK_EVENT_BUTTON e)) (caddr s)))
							   (list snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e))
								 (.button (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e))))))
					     (if (and (not (eq? 'stop! (apply (<- mouse-move-hook run) args)))
						      ispressed)
						 (apply (<- mouse-drag2-hook run) args)))))
		     (c-g_signal_connect w "button_release_event"
					 (lambda (w e i)
					   (set! ispressed #f)
					   (-> mouse-button-release-hook run
					       snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e)) (.button (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))))
		     (c-g_signal_connect w "scroll_event"
					 (lambda (w e i)
					   (set! ispressed #f)
					   (-> mouse-scroll-hook run
					       snd (.direction (GDK_EVENT_SCROLL e)) (.x (GDK_EVENT_SCROLL e)) (.y (GDK_EVENT_SCROLL e)) (.state (GDK_EVENT_SCROLL e)))
					   ))
		     )))))


(define (c-rightbutton? button)
  (= 3 button))

(define (c-leftbutton? button)
  (= 1 button))

(define (c-ctrl? stat)
  (= (logand stat 4) 4))

(define (c-shift? stat)
  (= (logand stat 1) 1))

(define (c-altGr? stat)
  (= (logand stat 8192) 8192))

;; Common way to treat mouse
(def-class (<mouse-cycle> clickfunc movefunc releasefunc #:key (scaled #f) (add-type 'add!))

  (define (press-hook snd pix-x pix-y button stat)
    (let ((isdragged #f)
	  (mousefunc (if scaled c-get-mouse-info2 c-get-mouse-info)))
      (mousefunc snd pix-x pix-y #t
		 (lambda (ch x y)
		   (let ((res (clickfunc snd ch x y button stat)))
		     (if (or (eq? 'stop! res)
			     (eq? 'allways-stop! res))
			 (begin
			   (-> mouse-move-hook only!
			       (lambda (snd pix-x pix-y button stat)
				 (set! isdragged #t)
				 (mousefunc snd pix-x pix-y ch
					    (lambda (dasch x y)
					      (movefunc snd ch x y button stat)))))
			   (-> mouse-button-release-hook only!
			       (lambda (snd pix-x pix-y button stat)
				 (-> mouse-move-hook not-only!)
				 (-> mouse-button-release-hook not-only!)
				 (mousefunc snd pix-x pix-y ch
					    (lambda (dasch x y)
					      (if (and (eq? 'allways-stop! res) (not isdragged))
						  (begin
						    ;; Small hack needed to get the mouse-click-hook to run.
						    (focus-widget (c-editor-widget snd))
						    (select-channel ch)
						    (run-hook mouse-click-hook
							      snd ch button stat pix-x pix-y time-graph))
						  (if (= 7 (car (procedure-property releasefunc 'arity)))
						      (releasefunc snd ch x y button stat isdragged)
						      (releasefunc snd ch x y button stat)))))))
			   'stop!)))))))

  (def-method (delete!)
    (-> mouse-button-press-hook remove! press-hook))

  (mouse-button-press-hook add-type press-hook)

  )



;; Allways select current channel
(-> mouse-button-press-hook add-system!
    (lambda (snd x y button stat)
      (let ((ch (c-get-channel snd y)))
	(if ch 
	    (select-channel ch)))))


;; Run the mouse-click-hook
(let ((isdragged #f))
  (-> mouse-button-press-hook add!
      (lambda (snd x y button stat)
	(set! isdragged #f)))
  (-> mouse-drag2-hook add!
      (lambda (snd x y button stat)
	(set! isdragged #t)))
  (-> mouse-scroll-hook add!
      (lambda (snd direction orgx y stat)
	(c-get-mouse-info snd orgx y #t
			  (lambda (ch x y)
			    (focus-widget (c-editor-widget snd))
			    (run-hook mouse-click-hook
				      snd ch (+ direction 4) stat orgx y time-graph)))))
  (-> mouse-button-release-hook add!
      (lambda (snd orgx y button stat)
	(if (not isdragged)
	    (c-get-mouse-info snd orgx y #t
			      (lambda (ch x y)
				(focus-widget (c-editor-widget snd))
				(select-channel ch)
				(run-hook mouse-click-hook
					  snd ch button stat orgx y time-graph)))))))


;;  Moving marks with the mouse
(let ((currmark #f))
  (<mouse-cycle> (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch)))))
			 (pointrange (- (* (srate snd) (position->x 15 snd ch))
					(* (srate snd) (position->x 0 snd ch)))))
		     (if (and (> y 7)
			      (< y 15))
			 (call-with-current-continuation
			  (lambda (return)
			    (for-each
			     (lambda (mark)
			       (if (< (abs (- (mark-sample mark) pointpos)) pointrange)
				   (begin
				     (set! currmark mark)
				     (return 'stop!))))
			     (list-ref (list-ref (marks) snd) ch)))))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     (if (> (mark-sync currmark) 0)
			 (move-syncd-marks (mark-sync currmark)
					   (- pointpos (mark-sample currmark)))
			 (set! (mark-sample currmark) pointpos))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     (if (> (mark-sync currmark) 0)
			 (move-syncd-marks (mark-sync currmark)
					   (- pointpos (mark-sample currmark)))
			 (set! (mark-sample currmark) pointpos))))

		 #:add-type 'add-system!
		 ))




;;  Moving mixes with the mouse
(let ((currmixes #f)
      (offset 0))

  (<mouse-cycle> (lambda (snd ch x y button stat)
		 (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch)))))
		       (pointrange (- (* (srate snd) (position->x (mix-tag-width) snd ch))
				      (* (srate snd) (position->x 0 snd ch)))))
		   (call-with-current-continuation
		    (lambda (return)
		      (c-for-each
		       (lambda (n mix)
			 (if (< (abs (- (mix-position mix) pointpos)) pointrange)
			     (let ((ypos (if (> (mix-tag-y mix) 0) 
					     (mix-tag-y mix)
					     (- (* (mix-tag-height) n) 5))))
			       (if (and (>= y ypos)
					(<= y (+ ypos (+ (mix-tag-height) 3))))
				   (begin
				     (set! offset (- (mix-position mix) pointpos))
				     (set! currmixes '())
				     (if (c-sync? snd)
					 (for-each (lambda (dasmix)
						     (if (= (mix-position mix) (mix-position dasmix))
							 (set! currmixes (cons dasmix currmixes))))
						   (apply append (mixes snd)))
					 (set! currmixes (list mix)))
				     (return 'stop!))))))
		       (mixes snd ch))))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     ;;(set! (mix-position currmix) (+ offset pointpos))
		     (draw-line x 0 x (list-ref (axis-info snd ch) 11))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     (for-each (lambda (mix)
				 (set! (mix-position mix) (+ offset pointpos)))
			       currmixes)
		     (set! currmixes #f)))
		 
		 #:add-type 'add-system!
		 ))




;; To avoid irritating non-smoothness, we turn off the garbage collector for 2 seconds. (Does not seem to be necessary for guile 1.7 and newer)
(if (not (c-atleast1.7?))
    (-> mouse-button-press-hook add-system!
	(lambda (snd orgx orgy button stat)
	  (c-gc-off))))







;;##############################################################
;; Double buffer
;;##############################################################

(def-class (<doublebuffer> parent)
  
  (def-var pixmap #f)
  (def-var drawable #f)

  (define width 0)
  (define height 0)

  (define (make-pixmap!)
    (set! width (car (widget-size parent)))
    (set! height (cadr (widget-size parent)))
    (set! pixmap (gdk_pixmap_new (.window parent) width height -1))
    (set! drawable (GDK_DRAWABLE pixmap)))

  (def-method (pixmap->parent)
    (gdk_draw_drawable (GDK_DRAWABLE (.window parent))
		       (.black_gc (.style parent))
		       drawable
		       0 0
		       0 0
		       width height))

  (def-method (parent->pixmap)
    (gdk_draw_drawable drawable
		       (.black_gc (.style parent))
		       (GDK_DRAWABLE (.window parent))
		       0 0
		       0 0
		       width height))

  (make-pixmap!)
  (this->parent->pixmap)

  (c-g_signal_connect parent "expose_event" 
		      (lambda (w e d) 
			(c-display "ai2")
			(this->parent->pixmap)))

)



;;##############################################################
;; Paint (not usable yet)
;;##############################################################

(def-class (<paint> parent width height)

  (define pixmap #f)
  (define colors '())
  (define gc #f)
  (define font #f)

  (define xmin width)
  (define ymin height)
  (define xmax 0)
  (define ymax 0)

  (define (update-minmax x1 y1 x2 y2)
    (set! xmin (min xmin x1 x2))
    (set! ymin (min ymin y1 y2))
    (set! xmax (max xmax x1 x2))
    (set! ymax (max ymax y1 y2)))

  (define (reset-minmax)
    (set! xmin width)
    (set! ymin height)
    (set! xmax 0)
    (set! ymax 0))

  (def-method (line color x1 y1 x2 y2)
    (update-minmax x1 y1 x2 y2)
    (gdk_draw_line pixmap color x1 y1 (- x2 x1 -1) (- y2 y1 -1)))

  (define (update-do x1 y1 x2 y2)
    (gdk_draw_pixmap (.window parent)
		     gc
		     pixmap
		     x1 y1
		     x1 y1
		     (- x2 x1 -1) (- y2 y1 -1)))

  (def-method (update)
    (if (and (>= xmax xmin)
	     (>= ymax ymin))
	(update-do xmin ymix xmax ymax))
    (reset-minmax))

  (def-method (update-all)
    (reset-minmax)
    (update-do 0 0 width height))

  (set! pixmap (gdk_pixmap_new (.window parent)
			       width
			       height
			       -1))
  
  (gtk_signal_connect (GTK_OBJECT parent) "expose_event"
		      (lambda (w e)
			(this->update-all))
		      #f)

  )




;;##############################################################
;; Pool (this class is pointless, right?)
;; The point is to avoid garbage collection, but just
;; calling a function does consing of its arguments, or?
;;##############################################################

(def-class (<pool>)
  (define all 0)
  (define pool '())
  (define (get)
    (set! all (+ all 8))
    (if (or #t (null? pool))
	(cons #f #f)
	(let ((ret pool))
	  (set! pool (cdr pool))
	  ret)))
  (define (put cell)
    (if #t
	(c-free cell)
	(begin
	  (set-cdr! cell pool)
	  (set! pool cell))))
  (def-method (cons a b)
    (let ((cell (get)))
      (set-car! cell a)
      (set-cdr! cell b)
      cell))
  (def-method (list . rest)
    (if (null? rest)
	rest
	(this->cons (car rest) (apply this->list (cdr rest)))))
  (def-method (list-copy daslist)
    (apply this->list daslist))
  (def-method (map func daslist)
    (let* ((ret '(0))
	   (tail ret))
      (for-each (lambda (el)
		  (let ((new (this->cons (func el) '())))
		    (set-cdr! tail new)
		    (set! tail new)))
		daslist)
      (cdr ret)))
  (def-method (insert! list pos element)
    (if (= 0 pos)
	(this->cons element list)
	(let ((f (drop list (- pos 1))))
	  (set-cdr! f (this->cons element (cdr f)))
	  list)))
  (define (returnalot alot)
    (if (and (not (null? alot)) (pair? alot))
	(let ((temp #f))
	  (for-each (lambda (a) (returnalot a)) alot)
	  (while (not (null? alot))
		 (set! temp alot)
		 (set! alot (cdr alot))
		 (put temp)))))
  
  (def-method (returnalot alot)
    (c-display "pool-length:" (length pool))
    ;(c-display "inserting: " alot)
    (returnalot alot)
    (c-display "pool-length:" (length pool))
    ;(newline)
    )

)


;;##############################################################
;; Nodeline
;;##############################################################


(def-class (<nodeline> dasnodes linefunc textfunc changefunc)

  (define nodes (map list-copy dasnodes))

  (define lines '())
  (define boxes '())

  (define minx 0)
  (define maxx 1)
  (define miny 0)
  (define maxy 0)
  (define proportion 1)

  (def-var boxsize 0.04)

  (define (gfx-> x)
    (c-scale x 0 1 minx maxx))
  (define (gfy-> x)
    (c-scale x 0 1 miny maxy))

  (define (<-gfx x)
    (c-scale x minx maxx 0 1))
  (define (<-gfy x)
    (c-scale x miny maxy 0 1))

  (define (for-each-node func)
    (let ((prev #f)
	  (i 0))
      (for-each (lambda (n)
		  (if prev
		      (func i
			    (car prev) (cadr prev)
			    (car n) (cadr n)))
		  (set! i (1+ i))
		  (set! prev n))
		nodes)))

  (define (for-each-box func)
    (for-each (lambda (n)
		(apply func n))
	      boxes))

  (define (for-each-line func)
    (for-each (lambda (n)
		(apply func n))
	      lines))

  (define (get-last-line)
    (car lines))


  (define (nodes-partly minx maxx func)
    (for-each-node (lambda (i x1 y1 x2 y2)
		     (if (and (< x1 maxx) (> x2 minx))
			 (let ((nx1 x1)
			       (nx2 x2)
			       (ny1 y1)
			       (ny2 y2))
			   (if (< nx1 minx)
			       (begin
				 (set! nx1 minx)
				 (set! ny1 (c-scale nx1 x1 x2 y1 y2))))
			   (if (< ny1 miny)
			       (begin
				 (set! ny1 miny)
				 (set! nx1 (c-scale ny1 y1 y2 x1 x2))))
			   (if (> nx2 maxx)
			       (begin
				 (set! nx2 maxx)
				 (set! ny2 (c-scale nx2 x1 x2 y1 y2))))
			   (if (> ny2 maxy)
			       (begin
				 (set! ny2 maxy)
				 (set! nx2 (c-scale ny2 y1 y2 x1 x2))))
			   (func i nx1 ny1 nx2 ny2))))))



  (def-method (get-graph #:optional start end)
    (if (not start)
	nodes
	(let ((ret '()))	
	  (nodes-partly start end
			(lambda (i x1 y1 x2 y2)
			  (if (null? ret)
			      (set! ret (list (list x2 y2) (list x1 y1)))
			      (set! ret (cons (list x2 y2) ret)))))
	  (reverse! ret))))


  (def-method (get-val x)
    (let ((first nodes)
	  (next #f))
      (while (< (caadr first) x)
	     (set! first (cdr first)))
      (set! next (cadr first))
      (set! first (car first))
      (c-scale x (car first) (car next) (cadr first) (cadr next))))
      

  (def-method (set-graph! graph)
    (if (not (= (length graph) (length nodes)))
	(begin
	  (changefunc this)
	  (set! nodes (map list-copy graph))
	  (make-lines-and-boxes)
	  (this->paint))
	(let ((start-not-set #t)
	      (start 0)
	      (end 0))
	  (c-for-each (lambda (n a b)
			(let ((eq (equal? a b)))
			  (if start-not-set
			      (begin
				(set! end n)
				(if (not eq)
				    (begin
				      (set! start n)
				      (set! start-not-set #f))))
			      (if (not eq)
				  (set! end n)))))
		      graph nodes)
	  (paint-some start (1+ end))
	  (set! nodes (map list-copy graph))
	  (make-lines-and-boxes)
	  (paint-some start (1+ end)))))

				  
				  
  (define (get-node x y)
    (call-with-current-continuation
     (lambda (return)
       (for-each-box (lambda (i x1 y1 x2 y2)
		       (if (and (>= x x1)
				(< x x2)
				(>= y y1)
				(< y y2))
			   (return i))))
       #f)))


  (define (perhaps-make-node x y)
    (define (square x)
      (* x x))
    (define (distance2 x1 y1 x2 y2)
      (+ (square (- x1 x2)) (square (- y1 y2))))
    (call-with-current-continuation
     (lambda (return)
       (for-each-node (lambda (i x1 y1 x2 y2)
			(if (and (>= x x1)
				 (<= x x2))
			    (let* ((gx (<-gfx x))
				   (gx1 (<-gfx x1))
				   (gx2 (<-gfx x2))
				   (a2 (distance2 gx1 y1 gx y))
				   (b2 (distance2 gx y gx2 y2))
				   (c2 (distance2 gx1 y1 gx2 y2))
				   (F2 (/ (- (* 4 a2 b2)
					     (square (- (+ a2 b2) c2)))
					  16))
				   (hc2 (* 4 (/ F2 c2))))
			      (return (if (< hc2 (square boxsize))
					  (let ((node (list x y)))
					    (changefunc this)
					    (set! nodes (insert! nodes i node))
					    (make-lines-and-boxes)
					    (this->paint)
					    node)
					  #f)))))))))


  (define (make-lines-and-boxes)
    (set! lines '())
    (set! boxes '())
    (nodes-partly minx maxx
		  (lambda (i x1 y1 x2 y2)
		    (set! lines (cons (list i
					    (<-gfx x1)
					    (<-gfy y1)
					    (<-gfx x2)
					    (<-gfy y2))
				      lines))))
    (for-each-node (lambda (i x1 y1 x2 y2)
		     (let ((makebox (lambda (i x y)
				      (if (and (<= x maxx) (>= x minx))
					  (let ((nx (<-gfx x))
						(ny (<-gfy y))
						(ax (* boxsize proportion)))
					    (set! boxes (cons (list i
								    (- nx ax)
								    (- ny boxsize)
								    (+ nx ax)
								    (+ ny boxsize))
							      boxes)))))))
		       (if (= i 1)
			   (makebox 0 x1 y1))
		       (makebox i x2 y2)))))
  


  (define (paint-some start end)
    (for-each-box (lambda (n x1 y1 x2 y2)
		    (if (and (>= n start)
			     (<= n end))
			(begin
			  (linefunc x1 y1 x2 y1)
			  (linefunc x2 y1 x2 y2)
			  (linefunc x2 y2 x1 y2)
			  (linefunc x1 y2 x1 y1)))))
    (for-each-line (lambda (n x1 y1 x2 y2)
		     (if (and (>= n start)
			      (<= n end))
			 (begin
			   (linefunc x1 y1 x2 y2)
			   (textfunc y1 (+ x1 (/ boxsize 2)) (- y1 boxsize))
			   (if (= n (car (get-last-line)))
			       (textfunc y2 (+ x2 (/ boxsize 2)) (- y2 boxsize))))))))
  
  
  (def-method (paint)
    (paint-some 0 (length nodes)))


  ;; The dasm(in|ax)(x|y) variables defines the range of the whole graph that is showed
  ;; in the current display.

  (def-method (set-bounds! dasminx dasmaxx dasminy dasmaxy dasproportion)
    (set! minx dasminx)
    (set! maxx dasmaxx)
    (set! miny dasminy)
    (set! maxy dasmaxy)
    (set! proportion dasproportion)
    (make-lines-and-boxes))



  ;; The x values for the mousefunctions are between 0 and 1 in the current display.

  (define pressednode #f)
  (define prevnode #f)
  (define nextnode #f)
  (define pressednodenum 0)
  (define x_press 0)
  (define y_press 0)
  (define x_press_offset 0)
  (define y_press_offset 0)
  (define (maixy x)
    (max 0 (min 1 x)))
  (define direction #f)


  (define (delete-node nodenum)
    (changefunc this)
    (set! nodes (delete! (list-ref nodes nodenum) nodes eq?))
    (set! pressednode #f)
    (set! prevnode #f)
    (set! nextnode #f)
    (make-lines-and-boxes)
    (this->paint)
    'stop!)
  
  (def-method (mouse-clicked x y button stat)
    (if (and pressednode
	     (c-rightbutton? button)
	     (> pressednodenum 0)
	     (< pressednodenum (1- (length nodes))))
	(delete-node pressednodenum)))

  (def-method (mouse-press x y button stat)
    (if (and (c-shift? stat)
	     (c-rightbutton? button))
	(let ((nodenum (get-node (maixy x) (maixy y))))
	  (if (and nodenum
		   (> nodenum 0)
		   (< nodenum (1- (length nodes))))
	      (delete-node nodenum)))
	(let ((func (lambda ()
		      (let ((nodenum (get-node (maixy x) (maixy y))))
			(if nodenum
			    (begin
			      (set! pressednodenum nodenum)
			      (if (> nodenum 0)
				  (set! prevnode (list-ref nodes (1- nodenum))))
			      (set! pressednode (list-ref nodes nodenum))
			      (if (< nodenum (1- (length nodes)))
				  (set! nextnode (list-ref nodes (1+ nodenum))))
			      (set! x_press x)
			      (set! y_press y)
			      (set! x_press_offset (- (<-gfx (car pressednode)) x))
			      (set! y_press_offset (- (cadr pressednode) y))
			      (set! direction (if (c-rightbutton? button) 'not-set #f))
			      'stop!)
			    #f)))))
	  (if (not (func))
	      (if (perhaps-make-node (gfx-> x) y)
		  (func))
	      'stop!))))

  (def-method (mouse-move x_org y_org button stat)
    (if pressednode
	(let ((y (+ y_press_offset (if (c-ctrl? stat) (+ y_press (/ (- y_org y_press) 12)) y_org)))
	      (x (+ x_press_offset (if (c-ctrl? stat) (+ x_press (/ (- x_org x_press) 12)) x_org)))
	      (minx2 (if prevnode (if nextnode (car prevnode) 1) 0))
	      (maxx2 (if nextnode (if prevnode (car nextnode) 0) 1)))
	  (if direction
	      (begin
		(if (eq? direction 'not-set)
		    (set! direction (if (> (abs (- x_org x_press)) (abs (- y_org y_press)))
					'x
					'y)))
		(if (eq? direction 'y)
		    (set! x (+ x_press_offset x_press))
		    (set! y (+ y_press_offset y_press)))))
	  (paint-some  pressednodenum (1+ pressednodenum))
	  (set-car! pressednode (max minx2 (min maxx2 (gfx-> x))))
	  (set-car! (cdr pressednode) (if (c-shift? stat) 0.5 (maixy y)))
	  (make-lines-and-boxes)
	  (paint-some pressednodenum (1+ pressednodenum))
	  (c-gc-on) ;; To avoid crashing the machine, actually.
	  'stop!)))

  (def-method (mouse-release x y button stat)
    (if pressednode
	(begin
	  (this->mouse-move x y button stat)
	  (set! prevnode #f)
	  (set! pressednode #f)
	  (set! nextnode #f)
	  'stop!)))


  )
		     
		  


(def-class (<editor-nodeline> snd ch orgval #:optional string-func moused-func (context mark-context))
  
  (Super (<nodeline> (begin
		       (if (or (< orgval 0) (> orgval 1))
			   (begin
			     (c-display "Warning! gui.scm/<editor-nodeline>: orgval=" orgval)
			     (set! orgval (max 0 (min orgval 1)))))
		       (list (list 0 orgval) (list 1 orgval)))
		     (lambda (x1 y1 x2 y2)
		       (c-get-bounds snd ch
				     (lambda (minx miny maxx maxy)
				       (draw-line (c-scale x1 0 1 minx maxx)
						  (c-scale y1 0 1 miny maxy)
						  (c-scale x2 0 1 minx maxx)
						  (c-scale y2 0 1 miny maxy)
						  snd
						  ch
						  context))))
		     (lambda (val x y)
		       (if string-func
			   (c-get-bounds snd ch
					 (lambda (minx miny maxx maxy)
					   (c-draw-string (c-editor-widget snd)
							  (list-ref (snd-gcs) 3)
							  (min (- maxx 20) (c-scale x 0 1 minx maxx))
							  (max 20 (c-scale y 0 1 miny maxy))
							  (string-func val))))))
		     (lambda (this)
		       (-> this paint))))

  (define active #t)

  (def-method (set-inactive)
    (-> this paint)
    (set! visible #f))

  (def-method (set-active)
    (-> this paint)
    (set! visible #t))

  (def-method (is-active?)
    active)

  (define (das-after-graph-hook dassnd dasch)
    (if (and active (= snd dassnd) (= ch dasch))
	(let ((length (/ (frames snd ch) (srate snd)))
	      (minx (car (x-bounds snd ch)))
	      (maxx (cadr (x-bounds snd ch)))
	      (miny (car (y-bounds snd ch)))
	      (maxy (cadr (y-bounds snd ch))))
	  (-> this set-bounds!
	      (c-scale minx 0 length 0 1)
	      (c-scale maxx 0 length 0 1)
	      (c-scale miny -1 1 0 1)
	      (c-scale maxy -1 1 0 1)
	      (/ (/ (window-height) (chans snd)) (window-width)))
	  (-> this paint)))
    #f)


  (define mouse-cycle (<mouse-cycle> (lambda (dassnd dasch x y button stat)
				       (if (and active (= snd dassnd) (= ch dasch))
					   (if (eq? 'stop! (-> this mouse-press x y button stat))
					       (begin
						 (if moused-func (moused-func this))
						 'stop!))))
				     (lambda (snd ch x y button stat)
				       (-> this mouse-move x y button stat)
				       (if moused-func (moused-func this)))
				     (lambda (snd ch x y button stat isdragged)
				       (if isdragged
					   (-> this mouse-release x y button stat)
					   (-> this mouse-clicked x y button stat))
				       (if moused-func (moused-func this)))
				     
				     #:scaled #t))

  (define (das-close-hook dassnd)
    (if (= dassnd snd)
	(this->delete!)))


  (def-method (delete!)
    (remove-hook! after-graph-hook das-after-graph-hook)
    (-> mouse-cycle delete!)
    (remove-hook! close-hook das-close-hook))


  (add-hook! after-graph-hook das-after-graph-hook)
  (add-hook! close-hook das-close-hook)

  (das-after-graph-hook snd ch)

  )


#!
(c-for 0 < 0.9 0.1
       (lambda (n)
	 (<editor-nodeline> (c-selected-sound) 0 n)))
!#




;;##############################################################
;; Menues
;;##############################################################

#!
(define m 
!#

(define* (menu-sub-add menu menu-label #:optional callback)
  (define (submenu menu-item)
    (gtk_menu_item_get_submenu (GTK_MENU_ITEM menu-item)))

  (let ((dasmenu (submenu (if (integer? menu) (main-menu menu) menu))))
    (if use-gtk
	(let ((menuitem (gtk_menu_item_new_with_label menu-label))
	      (submenu (gtk_menu_new)))
	  (gtk_menu_shell_append (GTK_MENU_SHELL dasmenu) menuitem)
	  ;;(gtk_menu_shell_append (GTK_MENU_SHELL (gtk_menu_item_get_submenu (GTK_MENU_ITEM dasmenu))) menuitem)
	  (gtk_widget_show menuitem)
	  (gtk_menu_item_set_submenu (GTK_MENU_ITEM menuitem) submenu)
	  (if callback
	      (g_signal_connect_closure_by_id 
	       (GPOINTER menuitem)
	       (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT menuitem))) 0
	       (g_cclosure_new (lambda (w d) 
				 (callback))
			       #f #f)
	       #f))
	  menuitem)
;;	  submenu)
	(let* ((submenu (XmCreatePulldownMenu dasmenu menu-label
					      (list XmNbackground (basic-color))))
	       (menuitem (XtCreateManagedWidget menu-label
						xmCascadeButtonWidgetClass dasmenu
						(list XmNsubMenuId submenu
						      XmNbackground (basic-color)))))
	  (if callback
	      (XtAddCallback menuitem XmNcascadingCallback (lambda (w c i) (callback))))
	  submenu))))



(define* (menu-add top-menu menu-label callback #:optional position)
  (define (submenu menu-item)
    (gtk_menu_item_get_submenu (GTK_MENU_ITEM menu-item)))
  (if (integer? top-menu)
      (if position
	  (add-to-menu top-menu menu-label callback position)
	  (add-to-menu top-menu menu-label callback))
      (if use-gtk
	  (let ((child (gtk_menu_item_new_with_label menu-label)))
	    (gtk_menu_shell_append (GTK_MENU_SHELL (submenu top-menu)) child)
	    (gtk_widget_show child)
	    ;(set-procedure-property! das 'arity '(2 0 #f))
	    ;;(set-procedure-properties! das '((arity 2 0 #f)))
	    ;;(c-display "prop: " (procedure-properties das))
	    (g_signal_connect_closure_by_id 
	     (GPOINTER child)
	     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
	     (g_cclosure_new (lambda (w d)
			       (callback))
					;das
					; (callback))
			     #f #f)
	     #f)
	    child)
	  (let ((child (XtCreateManagedWidget menu-label xmPushButtonWidgetClass top-menu
					      (list XmNbackground (basic-color)))))
	    (XtAddCallback child XmNactivateCallback
			   (lambda (w c i)
			     (callback)))
	    child))))


(define (menu-set-label menu label)
  (if use-gtk
      (gtk_label_set_text (GTK_LABEL (gtk_bin_get_child (GTK_BIN menu))) label)
      (let ((str (XmStringCreateLocalized label)))
	(XtSetValues menu (list XmNlabelString str))
	(XmStringFree str))))



#!
(define (submenu_func menu-item)
  (gtk_menu_item_get_submenu (GTK_MENU_ITEM menu-item)))

;; step-by-step how to make menues and submenues with gtk: (not exactly trivial...)
(define m (add-to-main-menu "testing"))
(begin (main-menu m))
(define s (menu-sub-add m "aiai"))
(define menuitem (gtk_menu_item_new_with_label "menuitem"))
(begin menuitem)
(define submenu (gtk_menu_new))
(gtk_menu_shell_append (GTK_MENU_SHELL (submenu_func (main-menu m))) menuitem)
(gtk_widget_show menuitem)
(gtk_menu_item_set_submenu (GTK_MENU_ITEM menuitem) submenu)
(begin submenu)

(define menuitem2 (gtk_menu_item_new_with_label "menuitem2"))
(gtk_menu_shell_append (GTK_MENU_SHELL (submenu_func menuitem)) menuitem2)
(gtk_widget_show menuitem2)


;; Menu-test
(let ((test-menu (add-to-main-menu "testing")))
  (define (adding menu n)
    (if (> n 0)
	(let ((submenu (menu-sub-add menu (format #f "Submenu ~D" n))))
	  (menu-add menu "MenuItem" (lambda () (display n)))
	  (adding submenu (- n 1)))))
  (adding test-menu 10))
!#






;;##############################################################
;; Checkbuttons
;;##############################################################

(def-class (<checkbutton> parent name callback #:optional onoff (extraopts '()))

  (def-var button #f)

  (def-method (set to)
    (if use-gtk
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) to)
	(XtSetValues button (list XmNset to))))

  (def-method (remove)
    (if use-gtk
	(hide-widget (GTK_WIDGET button))
	;;(gtk_widget_destroy (GTK_WIDGET button))
	(XtUnmanageChild button)))
    
  (if use-gtk
      (let ((dasparent (if (isdialog? parent) (-> parent getbox2) (GTK_BOX parent))))
	(set! button (if name (gtk_check_button_new_with_label name) (gtk_check_button_new)))
	(gtk_box_pack_end (GTK_BOX dasparent) button #f #f 0)
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) onoff)
	(gtk_widget_show button)
	(g_signal_connect_closure_by_id 
	 (GPOINTER button)
	 (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button))) 0
	 (g_cclosure_new (lambda (w d) 
			   (callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w))))
			 #f #f)
	 #f))
      (let* ((dasparent (if (isdialog? parent) (-> parent getbox2) parent)))
	(set! button (XtCreateManagedWidget (if name name "") xmToggleButtonWidgetClass dasparent
					    (append (list XmNbackground       (basic-color)
							  ;;XmNlabelString      name
							  XmNset              onoff
							  XmNselectColor      (yellow-pixel))
						    extraopts)))
	(XtAddCallback button XmNvalueChangedCallback (lambda (w c i) (callback (.set i)))))))



(define (checkbutton-get button)
  (if use-gtk
      (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON button))
      (c-display "checkbutton-get not implemented for motif.")))

(define (checkbutton-set button to)
  (if use-gtk
      (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) to)
      (XtSetValues button (list XmNset to))))

(define (checkbutton-remove button)
  (if use-gtk
      (hide-widget (GTK_WIDGET button))
      ;;(gtk_widget_destroy (GTK_WIDGET button))
      (XtUnmanageChild button)))





;;##############################################################
;; Buttons
;;##############################################################

(def-class (<button> parent name callback)

  (def-var button #f)

  (def-method (remove)
    (if use-gtk
	(hide-widget (GTK_WIDGET button))
	;;(gtk_widget_destroy (GTK_WIDGET button))
	(XtUnmanageChild button)))

  (if use-gtk
      (let ((dasparent (if (isdialog? parent)
			   (GTK_BOX (.action_area (GTK_DIALOG (-> parent dialog))))
			   (GTK_BOX parent))))
	(set! button (gtk_button_new_with_label name))
	(gtk_box_pack_start dasparent button #t #t 20)
	(g_signal_connect_closure_by_id (GPOINTER button)
					(g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button)))
					0 (g_cclosure_new (lambda (w data) 
							    (callback))
							  #f #f) #f)
	(gtk_widget_show button))
      (begin
	(set! button (XtCreateManagedWidget name xmPushButtonWidgetClass parent
					     (list XmNbackground (basic-color)
						   XmNarmColor   (pushed-button-color))))
	(XtAddCallback button XmNactivateCallback (lambda (w c i)
							    (callback))))))





;;##############################################################
;; Togglebuttons
;;##############################################################

(def-class (<togglebutton> parent name callback #:optional onoff (extraopts '()))

  (def-var button #f)

  (def-method (set to)
    (if use-gtk
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) to)
	(XtSetValues button (list XmNset to))))

  (def-method (remove)
    (if use-gtk
	(hide-widget (GTK_WIDGET button))
	;;(gtk_widget_destroy (GTK_WIDGET button))
	(XtUnmanageChild button)))
    
  (if use-gtk
      (let ((dasparent (if (isdialog? parent) (-> parent getbox2) (GTK_BOX parent))))
	(set! button (gtk_toggle_button_new_with_label name))
	(gtk_box_pack_end (GTK_BOX dasparent) button #f #f 0)
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) onoff)
	(gtk_widget_show button)
	(g_signal_connect_closure_by_id 
	 (GPOINTER button)
	 (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button))) 0
	 (g_cclosure_new (lambda (w d) 
			   (if (= 1 (car (procedure-property callback 'arity)))
			       (callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w)))
			       (callback this (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w)))))
			 #f #f)
	 #f))
      (let* ((dasparent (if (isdialog? parent) (-> parent getbox2) parent)))
	(set! button (XtCreateManagedWidget (if name name "") xmToggleButtonWidgetClass dasparent
					    (append (list XmNbackground       (basic-color)
							  ;;XmNlabelString      name
							  XmNset              onoff
							  XmNselectColor      (yellow-pixel))
						    extraopts)))
	(XtAddCallback button XmNvalueChangedCallback (lambda (w c i) (callback (.set i)))))))



;;##############################################################
;; Sliders
;;##############################################################

#!
(scale-log->linear 0 0.5 2)
(scale-linear->log 0 0 log-scale-ticks)

(scale-log->linear 0 1.2 2)
(scale-linear->log 0 1 2)
(scale-log-label 0 1 2)
!#

(def-class (<slider> parent
			 title
			 low initial high
			 func
			 scaler
			 #:optional autofunc use-log)

  (define hbox #f)
  (define label #f)
  (define scale #f)
  (define slider #f)

  (def-method (delete!)
    (gtk_widget_destroy (GTK_WIDGET scale))
    (gtk_widget_destroy (GTK_WIDGET label))
    (gtk_widget_destroy (GTK_WIDGET hbox)))

  (def-method (set! val)
    (gtk_adjustment_set_value (GTK_ADJUSTMENT slider) val))

  (when use-log
    (set! low (1+ low))
    (set! high (1+ high)))

  (if use-gtk
      (let* ((vbox (if (isdialog? parent) (-> parent getbox1) parent))
	     (adj (if use-log 
		      (gtk_adjustment_new (scale-log->linear low (1+ initial) high) 0 log-scale-ticks 1 10 1)
		      (gtk_adjustment_new initial low high 0.0 0.0 0.0))))
	(define (get-two-dec-string n)
	  (fix-defines
	   (define whole (c-integer n))
	   (define rest (- n whole))
	   (define dec100 (c-integer (* rest 100)))
	   (<-> (number->string whole) (if (< dec100 10) ".0" ".") (number->string dec100))))
	
	(define (get-slider-text n)
	  (if use-log
	      (<-> title " "
		   (get-two-dec-string n))
	      (format #f "~A" title)))

	(set! label (gtk_label_new (get-slider-text initial)))

	(set! hbox (gtk_hbox_new #f 0))

	(set! scale (gtk_hscale_new (GTK_ADJUSTMENT adj)))

	(if autofunc
	    (<checkbutton> hbox #f autofunc))

	(gtk_box_pack_start (GTK_BOX vbox) hbox #f #f 2)
	(gtk_widget_show hbox)
	(gtk_box_pack_start (GTK_BOX hbox) label #f #f 6)
	(gtk_widget_show label)
	(gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE scale)) GTK_UPDATE_CONTINUOUS)
	(gtk_scale_set_digits (GTK_SCALE scale)
			      (if use-log
				  0
				  (if (>= scaler 1000) 3 (if (>= scaler 100) 2 (if (>= scaler 10) 1 0)))))
	(gtk_scale_set_draw_value (GTK_SCALE scale) (not use-log))
	(gtk_widget_show scale)
	
	(gtk_box_pack_start (GTK_BOX hbox) scale #t #t 0)
	(if use-log
	    (g_signal_connect_closure_by_id (GPOINTER adj)
					    (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj))) 0
					    (g_cclosure_new (lambda (w d) 
							      (define val (1- (scale-linear->log low (gtk_adjustment_get_value (GTK_ADJUSTMENT adj)) high)))
							      (func val)
							      (change-label label (get-slider-text val))
							      )
							    #f #f)
					    #f)
	    (g_signal_connect_closure_by_id (GPOINTER adj)
					    (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj))) 0
					    (g_cclosure_new (lambda (w d) (func (gtk_adjustment_get_value (GTK_ADJUSTMENT adj)))) #f #f)
					    #f))
	(set! slider adj))
      (let* ((mainform (if (isdialog? parent) (-> parent getbox1) parent))
	     (dastitle (XmStringCreate title XmFONTLIST_DEFAULT_TAG))
	     (new-slider (XtCreateManagedWidget title xmScaleWidgetClass mainform
						(list XmNorientation   XmHORIZONTAL
						      XmNshowValue     #t
						      XmNminimum       (c-integer (* low scaler))
						      XmNmaximum       (c-integer (* high scaler))
						      XmNvalue         (c-integer (* initial scaler))
						      XmNdecimalPoints (if (= scaler 1000) 3 (if (= scaler 100) 2 (if (= scaler 10) 1 0)))
						      XmNtitleString   dastitle
						      XmNleftAttachment XmATTACH_FORM
						      XmNrightAttachment XmATTACH_FORM
						      XmNbackground    (basic-color)
						      ))))
	
	(XmStringFree dastitle)
	(XtAddCallback new-slider XmNvalueChangedCallback (lambda (w c info) (func (/ (.value info) scaler))))
	(XtAddCallback new-slider XmNdragCallback (lambda (w c info) (func (/ (.value info) scaler))))
	(set! slider new-slider))))







;;##############################################################
;; Dialogs
;;##############################################################

(define (isdialog? dialog)
  (and (object? dialog)
       (instance? dialog <dialog>)))

(def-class (<dialog> label deletefunc . buttons)

  (define box1 #f)
  (define box2 #f)

  (define wassoc (list (list 'Close "quit_button" (quit-button-color))
		       (list 'Help "help_button" (help-button-color))
		       (list 'Apply "doit_button" (doit-button-color))
		       (list 'Dismiss "quit_button" (quit-button-color))
		       (list 'Ok "doit_button" (doit-button-color))))


  (def-var dialog #f)
  (def-var sliders #f)

  (def-method (getbox2)
    (if (not box2)
	(let ((hbox #f))
	  (if use-gtk
	      (begin
		(set! hbox (gtk_hbox_new #f 0))
		(gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG dialog))) hbox #f #f 4)
		(gtk_widget_show hbox))
	      (let* ((mainform box1)
		     (sep (XtCreateManagedWidget "sep" xmSeparatorWidgetClass mainform
						 (list XmNorientation      XmHORIZONTAL
						       XmNseparatorType    XmSHADOW_ETCHED_OUT
						       XmNbackground       (basic-color))))
		     (rc (XtCreateManagedWidget "rc"  xmRowColumnWidgetClass mainform
						(list XmNorientation      XmHORIZONTAL
						      XmNbackground       (basic-color)
						      XmNradioBehavior    #f
						      XmNradioAlwaysOne   #t
						      XmNbottomAttachment XmATTACH_FORM
						      XmNleftAttachment   XmATTACH_FORM
						      XmNrightAttachment  XmATTACH_FORM
						      XmNentryClass       xmToggleButtonWidgetClass
						      XmNisHomogeneous    #t))))
		(set! hbox rc)))
	  (setbox2! hbox)))
    box2)

  (define (setbox2! dashbox)
    (set! box2 dashbox))

  (def-method (getbox1)
    (if (not box1)
	(let ((vbox #f))
	  (if use-gtk
	      (begin
		(set! vbox (gtk_vbox_new #f 2))
		(gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG dialog))) vbox #f #f 4)
		(gtk_widget_show vbox))
	      (set! vbox (XtCreateManagedWidget "formd" xmRowColumnWidgetClass dialog
						(list XmNleftAttachment      XmATTACH_FORM
						      XmNrightAttachment     XmATTACH_FORM
						      XmNtopAttachment       XmATTACH_FORM
						      XmNbottomAttachment    XmATTACH_WIDGET
						      XmNbottomWidget        (XmMessageBoxGetChild dialog XmDIALOG_SEPARATOR)
						      XmNbackground          (highlight-color)
						      XmNorientation         XmVERTICAL))))
	  (setbox1! vbox)))
    box1)

  (define (setbox1! dasvbox)
    (set! box1 dasvbox))

  (def-method (hide)
    (if use-gtk
	(gtk_widget_hide dialog)
	(XtUnmanageChild dialog))
    (if (c-selected-sound)
	(focus-widget (c-editor-widget (c-selected-sound)))))

  (def-method (show)
    (if use-gtk
	(begin
	  (gtk_widget_show dialog)
	  (gdk_window_raise (.window dialog)))
	(if (not (XtIsManaged dialog))
	    (XtManageChild dialog)
	    (raise-dialog dialog))))


  ;; Replacement for add-sliders in new-effects.scm/gtk-effects.scm
  (def-method (add-sliders dassliders)
    (set! sliders (map
			 (lambda (slider-data)
			   (apply <slider> (cons (this->getbox1) slider-data)))
			 dassliders))
    
    (if (not use-gtk)
	(let ((num_inputs (+ 1 (length sliders))))
	  (set! (widget-size dialog) (list (min 800 (max 400 (* num_inputs 20)))
						   (min 800 (max 120 (* num_inputs 70)))))))
    
    sliders)


  (let ((names '())
	(funcs '())
	(wnames '())
	(new-dialog #f))

    (if use-gtk
	(begin
	  (set! new-dialog (gtk_dialog_new))
	  (gtk_window_set_title (GTK_WINDOW new-dialog) label)
	  (gtk_container_set_border_width (GTK_CONTAINER new-dialog) 10)
	  (gtk_window_set_default_size (GTK_WINDOW new-dialog) -1 -1)
	  (gtk_window_set_resizable (GTK_WINDOW new-dialog) #t)
	  (gtk_widget_realize new-dialog)
	  
	  (g_signal_connect_closure_by_id (GPOINTER new-dialog)
					  (g_signal_lookup "delete_event" (G_OBJECT_TYPE (GTK_OBJECT new-dialog)))
					  0 (g_cclosure_new (lambda (w ev data)
							      (if deletefunc (deletefunc))
							      (gtk_widget_hide new-dialog)
							      (if (c-selected-sound)
								  (focus-widget (c-editor-widget (c-selected-sound)))))
							    #f #f) #f))
	(let ((titlestr (XmStringCreate label XmFONTLIST_DEFAULT_TAG)))
	  (set! new-dialog
		(XmCreateTemplateDialog (cadr (main-widgets)) label
					(list XmNautoUnmanage        #f
					      XmNdialogTitle         titlestr
					;XmNresizePolicy        XmRESIZE_GROW
					      XmNnoResize            #f
					      XmNbackground          (basic-color)
					      XmNtransient           #f)))
	  (XtAddCallback new-dialog XmNcancelCallback (lambda (w c i)
							(if deletefunc (deletefunc))
							(XtUnmanageChild new-dialog)
							(if (c-selected-sound)
							    (focus-widget (c-editor-widget (c-selected-sound))))))
	  (XmStringFree titlestr)))
    
    
    (for-each
     (lambda (e)
       (if (procedure? e)
	   (set! funcs (cons e funcs))
	   (begin
	     (set! names (cons e names))
	     (set! wnames (cons (if (assoc (string->symbol e) wassoc) (assoc (string->symbol e) wassoc) (list #f "noname")) wnames)))))
     buttons)
    
    (for-each
     (lambda (name func wname)
       (let ((button ((if (> (car (procedure-property func 'arity)) 0)
			  <togglebutton>
			  <button>)
		      (if use-gtk
			  (.action_area (GTK_DIALOG new-dialog))
			  new-dialog)
		      name 
		      func)))
	 (if use-gtk
	     (gtk_widget_set_name (-> button button) (cadr wname))
	     (if (car wname)
		 (XtVaSetValues
		  (-> button button)
		  (list XmNarmColor   (pushed-button-color)
			XmNbackground (caddr wname)))))))
     
     (reverse names) (reverse funcs) (reverse wnames))
    
    ;; build rest in (.vbox (GTK_DIALOG new-dialog))
    (set! dialog new-dialog)))
    






;;##############################################################
;; GUI test
;;##############################################################

#!
(define d (<dialog> "gakk"  #f
		    "Close" (lambda () (-> d hide))
		    "Apply" (lambda () (display "apply"))
		    "Play" (lambda (onoroff) (c-display "play" onoroff))
		    "Help" (lambda () (display "help"))))
  
(<slider> d "slider1" 0 1 2 (lambda (val) (display val)(newline)) 100)
(<slider> d "slider2" 0 0.2 1 (lambda (val) (display val)(newline)) 1000)
(<slider> d "slider3" 0 1 20 (lambda (val) (display val)(newline)) 1)
(<checkbutton> d "checkbutton1" (lambda (a) (display a)(newline)))
(<checkbutton> d "checkbutton2" (lambda (a) (display a)(newline)))
(<checkbutton> d "checkbutton3" (lambda (a) (display a)(newline)))
(<togglebutton> d "togglebutton1" (lambda (a) (display a)(newline)))
(-> d show)
(-> d hide)
!#






    



