;;;;
;;;; W::tumorigenesis
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
	  (W::tumorigenesis
	  (SENSES
	   ((LF-PARENT ONT::BIOLOGICAL-PROCESS)
	    (meta-data :origin BOB :entry-date 20150107 :change-date nil :comments nil :wn nil)
; this template works well but sometimes BIOLOGICAL-PROCESS becomes preferred over, e.g., PHOSPHORYLATION, in "phosphorylation of x"	    
;	    (TEMPL reln-cause-affected-templ (XP1 (% W::PP (W::PTYPE (? ptp w::by))))
;		   (XP2 (% W::PP (W::PTYPE (? ptp2 w::of)))))
	    )
	   #|
	   ((LF-PARENT ONT::BIOLOGICAL-PROCESS)
	    (meta-data :origin BOB :entry-date 20150107 :change-date nil :comments nil :wn nil)
	    (TEMPL reln-cause-affected-templ (XP1 (% W::NP))
		   (XP2 (% W::PP (W::PTYPE (? ptp2 w::of)))))
	    )
	   |#
	   )
	  )
))

#|  ; this doesn't work because when other words, e.g., "gene-expression", is mapped to BIOLOGICAL-PROCESS, it is taken as the verb, with an unknown nominalization, rather than the nominalization itself
(define-words :pos W::v 
 :words (
   (W::tumorigenesis-verb-doesnotexist
     (wordfeats (W::morph (:forms (-vb) :nom W::tumorigenesis)))
     (SENSES
      ((LF-PARENT ONT::BIOLOGICAL-PROCESS)  
       (TEMPL agent-affected-xp-templ)
       )
      #|
      ((LF-PARENT ONT::BIOLOGICAL-PROCESS)  
        (TEMPL affected-templ)
	)
      |#

      ))))
|#

