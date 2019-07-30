;;;;
;;;; W::CAMPAIGN
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CAMPAIGN
   (SENSES
    ((LF-PARENT ONT::action) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("campaign%1:04:02")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CAMPAIGN
   (SENSES
    ((LF-PARENT ONT::commitment) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE 20050817 :comments lf-restructuring)
     )
    ))
))

(define-words :pos W::v 
 :words (
  (W::CAMPAIGN
   (SENSES 
    
    ((lf-parent ont::fighting)
       (example "he campaigned the proposal")
       (templ agent-neutral-xp-templ)
    )
    ((lf-parent ont::fighting)
       (example "he campaigned (with/for/against them)")
       (templ agent-templ)
    )
    ((lf-parent ont::fighting)
       (example "he campaigned to establish the area")
       (TEMPL AGENT-theme-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
    )

   )
  )     
        )
)

