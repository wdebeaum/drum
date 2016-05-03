;;;;
;;;; W::DISTANT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::DISTANT
    ;; can't auto-generate adv with theme templ
  ;; (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("distant%3:00:01"))
     (EXAMPLE "They are equidistant [between the lines]")
     (LF-PARENT ONT::REMOTE)
     (example "the distant horizon")
     (TEMPL ADJ-THEME-TEMPL)
     (SEM (f::orientation ont::more) (f::intensity ont::hi))
     )
    )
   )
))

