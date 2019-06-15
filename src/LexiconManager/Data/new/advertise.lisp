;;;;
;;;; W::advertise
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::advertise
   (SENSES
    ((EXAMPLE "advertise the computer")
     (meta-data :origin calo :entry-date 20040907 :change-date 20090506 :comments caloy2)
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::praise) ;; 20120523 GUM change new parent
     (TEMPL AGENT-FORMAL-XP-TEMPL)
     )
    )
   )
))

