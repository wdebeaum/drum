;;;;
;;;; W::dash
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::dash
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("dash%2:38:00"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like stroll,walk
     )
    )
   )
))

(define-words :pos W::value :boost-word t
 :words (
  (w::dash
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   (PREFERENCE 0.92)
    )
   )
)
))

