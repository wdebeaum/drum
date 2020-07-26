;;;;
;;;; W::mind
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::mind
   (SENSES
    ;;;; I don't mind about the weather
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090511 :comments nil :vn ("marvel-31.3-2"))
     (LF-PARENT ONT::care)
     (TEMPL experiencer-formal-xp-templ (xp (% W::PP (W::ptype (? p W::about)))))
     )
    ;;;; I don't mind the weather
    ((LF-PARENT ONT::care)
     (TEMPL experiencer-neutral-templ)
     )
    ;;;; I don't mind
    ((LF-PARENT ONT::care)
     (TEMPL experiencer-templ)
     )
    )
   )
))

