;;;;
;;;; W::relate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::relate
   (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("relate%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::extended-say)
     (TEMPL agent-affected-iobj-theme-templ) ; like say
     (PREFERENCE 0.96)
     )
#|
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (EXAMPLE "relate this to that")
     (LF-PARENT ONT::ASSOCIATE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected2-TEMPL (xp (% W::pp (W::ptype W::to))))
     (PREFERENCE .98)
     )
|#
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (EXAMPLE "It relates this to that")
     (LF-PARENT ONT::RELATE)
     (TEMPL neutral-neutral-neutral-xp-templ (xp (% W::pp (W::ptype W::to))))
     )
    )
   )
))

