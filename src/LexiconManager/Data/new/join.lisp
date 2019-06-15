;;;;
;;;; W::join
;;;;

(define-words :pos W::v 
 :words (
  (W::join
   (SENSES
    ((EXAMPLE "green join team bravo")
     (LF-PARENT ONT::JOINING)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin coordops :entry-date 20070707 :change-date nil :comments nil)
     )
    ((EXAMPLE "they joined together")
     (LF-PARENT ONT::JOINING)
     (TEMPL AGENT-NP-PLURAL-TEMPL) 
     (meta-data :origin general :entry-date 20110128 :change-date nil :comments jantzen)
     )

    ((EXAMPLE "We joined the two parts")
     (LF-PARENT ONT::JOINING)
     (TEMPL agent-affected-PLURAL-TEMPL) 
     (preference .98)
     (meta-data :origin general :entry-date 20110128 :change-date nil :comments jantzen)
     )

    ((EXAMPLE "join this cell with the next cell")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% W::PP (W::ptype (? ptype w::to W::with)))))
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil)
     )

    ((LF-PARENT ONT::enroll)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "join the newsgroup")
     (meta-data :origin calo-ontology :entry-date 20060119 :change-date nil :comments caloy3)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )

    )
   )
))

