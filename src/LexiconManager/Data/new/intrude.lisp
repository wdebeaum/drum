;;;;
;;;; W::intrude
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::intrude
   (wordfeats (W::morph (:forms (-vb) :nom w::intrusion)))
   (SENSES
    (;(LF-PARENT ONT::hindering)
     (LF-PARENT ONT::transgress)
     (meta-data :origin cardiac :entry-date 20090403 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     ;(TEMPL agent-affected-xp-templ)
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% W::pp (W::ptype (? xxx W::on)))))     
     )
    )
   )
))

