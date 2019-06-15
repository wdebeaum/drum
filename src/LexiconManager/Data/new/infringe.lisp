;;;;
;;;; W::infringe
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::infringe
   (wordfeats (W::morph (:forms (-vb) :nom W::infringement)))
   (SENSES
    (;(LF-PARENT ONT::HINDERING)
     (LF-PARENT ONT::transgress)
     (Example "a plan infringes on another")
     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-expt)
     ;(TEMPL agent-affected-xp-templ (xp (% W::PP (W::ptype W::on))))
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% W::pp (W::ptype (? xxx W::on)))))     
     )    
     ))
))

