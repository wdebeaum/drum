;;;;
;;;; W::update
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::update
   (wordfeats (W::morph (:forms (-vb) :nom w::update)))
   (SENSES
    ((LF-PARENT ONT::device-adjust)
     (example "you can update your system")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo :entry-date 20041122 :change-date 20090504 :comments caloy2)
     )
    ((meta-data :origin calo :entry-date 20040915 :change-date nil :comments caloy2)
     (LF-PARENT ONT::revise)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ))
))

