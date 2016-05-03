;;;;
;;;; W::transfect
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (bio)
 :words (
 (W::transfect
    (wordfeats (W::morph (:forms (-vb) :nom w::transfection)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments bio)
     (LF-PARENT ont::transformation)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-instrument-optional-templ)
     )
    ))))
