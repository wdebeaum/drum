;;;;
;;;; W::incorporate
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::incorporate
   (SENSES
    ((EXAMPLE "incorporate parts of the library into other programs")
     (LF-PARENT ONT::add-include)
     (TEMPL AGENT-AFFECTED-RESULT-TO-OBJCONTROL-TEMPL (xp (% W::PP (W::ptype (? ptp w::into W::with)))))
     (meta-data :origin task-learning :entry-date 20050826 :change-date 20090908 :comments nil)
     )
    ;; need this template to go through the adjectival passive rule
    ((EXAMPLE "the incorporated objects")
     (LF-PARENT ONT::add-include)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

