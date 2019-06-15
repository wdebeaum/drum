;;;;
;;;; W::sped
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::sped
   (wordfeats (W::morph (:forms NIL)) (W::vform (? vf W::past)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2"))
     (LF-PARENT ONT::move-rapidly)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

