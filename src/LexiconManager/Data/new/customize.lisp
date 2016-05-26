;;;;
;;;; W::customize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::customize
    (wordfeats (W::morph (:forms (-vb) :nom w::customization)))
   (SENSES
    ((LF-PARENT ONT::device-adjust)
     (example "you can customize your computer")
     (TEMPL agent-affected-xp-templ)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo :entry-date 20041122 :change-date 20090504 :comments caloy2)
     )
   ))
))

