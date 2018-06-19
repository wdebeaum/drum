;;;;
;;;; W::CAUSE
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::CAUSE
   (wordfeats (W::morph (:forms (-vb) :nom w::cause)))
   (SENSES
    ((EXAMPLE "Aspirin causes headaches")
     ;(lf-parent ont::cause-produce-reproduce) ;;  20120524 GUM change new parent
     (lf-parent ont::cause-effect) ;;  20120524 GUM change new parent
     (TEMPL agent-affected-xp-templ)
     )
    ;; LF-GUM spreadsheet instructions say delete this sense, but then surface form has no representation.
    ;; changing parent instead to ont::cause-effect
    ((EXAMPLE "A caused B to hire C")
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     (lf-parent ont::cause-effect) ;;  20121028 GUM change new parent
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
;    ))
  (W::AGENT
   (SENSES
    ((LF-PARENT ONT::REASON-informal)
     (LF-FORM W::because)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )
))

