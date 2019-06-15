;;;;
;;;; W::attack
;;;;

(define-words :pos W::v 
 :words (
  (W::attack
   (wordfeats (W::morph (:forms (-vb) :nom W::attack :nomsubjpreps (w::of w::by) :nomobjpreps (w::on))))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::criticize)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::ATTACK)
     (example "the army attacked the city" "the virus attacked his immune system")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::ATTACK)
     (example "the army attacked" )
     (TEMPL agent-templ) 
     )
    )
   )))

