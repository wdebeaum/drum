;;;;
;;;; W::scream
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
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
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL 
	    (xp1 (% w::pp (w::ptype (? ptp W::at)))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3" "sound_emission-43.2") :wn ("scream%2:32:01" "scream%2:32:08" "scream%2:39:00"))
     )
    (;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::manner-say)
     (example "he screamed about it [to/with her]")
     (TEMPL AGENT-ABOUT-THEME-ADDRESSEE-OPTIONAL-TEMPL)
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

