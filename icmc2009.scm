(define midi-filename "/home/kjetil/Malaguena.mid")
(define midi-filename "/gammelhd/gammelhd/home/kjetil/mus220c/sinclair.MID")
;;(define midi-filename "/hom/kjetism/icmc2009/Malaguena.mid")

(begin
  (rte-silence!)
  (eval-c ""
	  (proto->public "void tar_bench_print(tar_heap_t *heap)")
	  (proto->public "void tar_touch_heaps(tar_heap_t *heap)")
	  )
  (<rt-stalin> :runtime-checks #f
    (while #t
      (wait-midi :command note-on
	(define phase 0.0)
	(define phase-inc
	  (hz->radians (midi-to-freq (midi-note))))
	(define player (sound
			 (out (* (midi-vol) (sin phase)))
			 (inc! phase phase-inc)))
	(spawn
	  (wait-midi :command note-off :note (midi-note)
	    (stop player))))))
  (if #f
      (system (<-> "aplaymidi --port=129 &"))
      (begin
	(system (<-> "aplaymidi --port=129 " midi-filename))
	(tar_bench_print (get_last_rt_heap))
	(rte-silence!)))
  )


(system (<-> "aplaymidi --port=129 " midi-filename "&"))
(get_last_rt_heap)

