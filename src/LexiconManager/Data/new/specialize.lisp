;;;;
;;;; W::specialize
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::specialize
    (wordfeats (W::morph (:forms (-vb) :nom w::specialization)))
   (SENSES
    ((LF-PARENT ONT::device-adjust)
     (example "you can specialize your computer")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo :entry-date 20041122 :change-date 20090504 :comments caloy2)
     )
   ))
))

