;;;;
;;;; W::vomit
;;;;

(define-words :pos W::v 
 :words (
  (W::vomit
     (wordfeats (W::morph (:forms (-vb) :past W::vomited :ing W::vomiting)))
   (SENSES
    ((EXAMPLE "The patient vomited")
     (LF-PARENT ONT::vomit)
     (TEMPL affected-TEMPL)
     )
    ((EXAMPLE "The patient vomited green matter")
     (LF-PARENT ONT::vomit)
     (TEMPL affected-affected-TEMPL)
     )
    )
   )
))

