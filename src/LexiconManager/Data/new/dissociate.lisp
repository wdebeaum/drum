;;;;
;;;; W::dissociate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::dissociate
   (wordfeats (W::morph (:forms (-vb) :nom W::dissociation)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("separate-23.1-2"))
     (LF-PARENT ONT::separation)
     (TEMPL agent-affected-as-comp-templ (xp (% w::pp (w::ptype (? t w::from)))))
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("separate-23.1-2"))
     (LF-PARENT ONT::separation)
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

