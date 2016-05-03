;;;;
;;;; W::LOCAL
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::LOCAL
   (wordfeats (w::comp-op w::less))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("local%3:00:01"))
     (EXAMPLE "This problem is local [to the area]")
     (LF-PARENT ONT::DISTANCE-VAL)
     (TEMPL ADJ-CO-THEME-TEMPL)
     (SEM (f::orientation ont::less) (f::intensity ont::med))
     )
    )
   )
))

