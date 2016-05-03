;;;;
;;;; W::nominate
;;;;

(define-words :pos W::v
 :words (
  (W::nominate
   (SENSES
    (;(LF-PARENT ONT::suggest)
     ;(lf-parent ont::nominate-refer) ;; 20120524 GUM change new parent
     (lf-parent ont::nominate) ;; [BOB]
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-neutral-xp-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date 20090513 :comments nil :vn ("appoint-29.1") :wn ("nominate%2:32:00" "nominate%2:41:02" "nominate%2:41:01" "nominate%2:41:00"))
     )
    )
   )
))

