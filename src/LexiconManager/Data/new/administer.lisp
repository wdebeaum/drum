;;;;
;;;; W::administer
;;;;

(define-words :pos W::V
 :words (
  (W::administer
   (wordfeats (W::morph (:forms (-vb) :past W::administered :ing W::administering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("contribute-13.2-1-1"))
     (LF-PARENT ONT::assign)
     (example "administer the medication to the patient")
     ;(TEMPL agent-affected-goal-optional-templ) ; like surrender
     (TEMPL AGENT-AFFECTED-AFFECTEDR-XP-optional-TEMPL)
    )
    )
   )
))

