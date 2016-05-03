;;;;
;;;; W::stretch
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::stretch
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("stretch%1:28:00"))
     (LF-PARENT ONT::time-interval)
     (example "they waited a long stretch")
     (TEMPL other-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::stretch
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::body-movement)
     (lf-parent ont::stretch) ;; 20120523 GUM change new parent
     (TEMPL affected-templ)
     (preference .98)
     )
    ((meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::body-movement)
     (lf-parent ont::stretch) ;; 20120523 GUM change new parent
     (example "he stretched his legs")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

