;;;;
;;;; W::deposit
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  (W::deposit
    (wordfeats (W::morph (:forms (-vb) :past W::deposited :ing w::depositing :nom w::deposit)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("put-9.1"))
     (LF-PARENT ONT::put)
     (TEMPL agent-affected-xp-templ) ; like insert,position
     (PREFERENCE 0.96)
     )
   
   ))))

