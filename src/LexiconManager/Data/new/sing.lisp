;;;;
;;;; W::sing
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
	 (W::sing
	  (wordfeats (W::morph (:forms (-vb) :past W::sang :pastpart w::sung)))
	  (SENSES
	   ((LF-PARENT ont::nonverbal-expression)
	    (meta-data :origin bolt :entry-date 20120516 :comments top500)
	    (example "sing the song")
	    (TEMPL agent-neutral-templ)
	    )
	   
	   ((LF-PARENT ont::nonverbal-expression)
	    (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
	    (example "he sang")
	    (TEMPL AGENT-TEMPL)
	    )
	   )
	  )
	 ))

