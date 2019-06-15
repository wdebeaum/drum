;;;;
;;;; w::ready
;;;; 

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::ready
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("preparing-26.3-1"))
     (LF-PARENT ONT::PREPARE) ;; GUM change new parent 20121027
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ;; GUM change new template 20121027
     ; like prepare
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::READY
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("ready%3:00:00"))
     (EXAMPLE "it's ready")
     (lf-parent ont::available)
     (TEMPL CENTRAL-ADJ-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("ready%3:00:00"))
     (EXAMPLE "that's ready for him")
     (lf-parent ont::available)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("ready%3:00:00"))
     (EXAMPLE "he's ready to go")
     (lf-parent ont::available)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

