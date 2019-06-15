;;;;
;;;; W::scream
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::scream ; like talk
    (wordfeats (W::morph (:forms (-vb) :nom w::scream)))
   (SENSES
    ;;;; ?
    (;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::manner-say)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3" "sound_emission-43.2") :wn ("scream%2:32:01" "scream%2:32:08" "scream%2:39:00"))
     )
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (example "he screamed at her [about it]")
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp W::at)))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3" "sound_emission-43.2") :wn ("scream%2:32:01" "scream%2:32:08" "scream%2:39:00"))
     )
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (example "he screamed about it [to/with her]")
     (TEMPL AGENT-FORMAL-AGENT1-2-XP1-PP-3-XP2-PP-WITH-OPTIONAL-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3" "sound_emission-43.2") :wn ("scream%2:32:01" "scream%2:32:08" "scream%2:39:00"))
     )
     ;;;; he screamed all day
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (TEMPL AGENT-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3" "sound_emission-43.2") :wn ("scream%2:32:01" "scream%2:32:08" "scream%2:39:00"))
     )
    )
   )
))

