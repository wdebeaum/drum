;;;;
;;;; w::able
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (w::able
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("able%5:00:00:competent:00" "able%3:00:00" "able%5:00:00:capable:00"))
     (EXAMPLE "He is able to do this")
     (LF-PARENT ONT::able)
     (SEM (F::GRADABILITY F::+))
     (TEMPL CENTRAL-ADJ-optional-XP-TEMPL (XP (% W::cp (W::ctype W::s-to))))
     )
    )
   )
))

