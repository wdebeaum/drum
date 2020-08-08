;;;;
;;;; W::LOCAL
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::LOCAL
   (wordfeats (w::comp-op w::less))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn nil)
     (EXAMPLE "This problem is local [to the area]")
     (LF-PARENT ONT::local-VAL)
     (TEMPL ADJ-CO-THEME-TEMPL)
     (SEM (f::orientation F::neg) (f::intensity ont::med))
     )
    )
   )
))

