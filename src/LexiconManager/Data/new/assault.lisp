;;;;
;;;; W::assault
;;;;

(define-words :pos W::v 
 :words (
  (W::assault
     (wordfeats (W::morph (:forms (-vb) :nom W::assault :nomsubjpreps (w::of w::by) :nomobjpreps (w::on))))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::criticize)
     (TEMPL AGENT-AGENT1-NP-TEMPL) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::ATTACK)
     (example "the smell assualted his senses")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    )
   )
))

