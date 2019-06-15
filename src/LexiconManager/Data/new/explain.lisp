;;;;
;;;; W::explain
;;;;

(define-words :pos W::v 
 :words (
  (W::explain
    (wordfeats (W::morph (:forms (-vb) :nom w::explanation)))
   (SENSES
    ((LF-PARENT ONT::explain)
     (example "he explained the book")
     (meta-data :origin calo :entry-date 20041103 :change-date 20090506 :comments caloy2)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-NEUTRAL-XP-TEMPL (xp (% W::NP (W::sort (? !s W::wh-desc)))))
     )
    ((LF-PARENT ONT::explain)
     (example "He explained that the cat caught the mouse.")
     (meta-data :origin calo :entry-date 20041103 :change-date 20090506 :comments caloy2)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-CP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    )
   )
))

