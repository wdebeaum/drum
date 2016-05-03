;;;;
;;;; W::labelled
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ;; capture alternate spelling
  (W::labelled
    (wordfeats (W::morph (:forms NIL)) (W::vform W::pastpart))
   (SENSES
    ((EXAMPLE "the link labelled advanced search")
     (meta-data :origin plow :entry-date 20050329 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::naming)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-name-templ)
     )
    )
   )
))

