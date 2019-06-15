;;;;
;;;; W::dissociate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::dissociate
   (wordfeats (W::morph (:forms (-vb) :nom W::dissociation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("separate-23.1-2"))
     (LF-PARENT ONT::separation)
     (TEMPL AGENT-AFFECTED-XP-PP-TEMPL (xp (% w::pp (w::ptype (? t w::from)))))
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("separate-23.1-2"))
     (LF-PARENT ONT::separation)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

