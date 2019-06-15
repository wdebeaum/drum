;;;;
;;;; W::omit
;;;;

(define-words :pos W::v 
 :words (
  (W::omit
   (wordfeats (W::morph (:forms (-vb) :nom w::omission)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("remove-10.1") :wn ("omit%2:31:01"))
     (LF-PARENT ONT::omit)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::from)))) ; like eliminate
     )
    )
   )
))

