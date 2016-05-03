;;;;
;;;; W::accommodate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::accommodate
    (wordfeats (W::morph (:forms (-vb) :nom w::accommodation)))
   (SENSES
    ((EXAMPLE "this room accommodates six")
     ;;(LF-PARENT ONT::ACCOMMODATE)
     (lf-parent ont::accommodate-allow) ;; 20120524 GUM change new parent
     (TEMPL neutral-neutral-xp-templ)
     (meta-data :origin gloss-training :entry-date 20100223 :change-date nil :comments nil)
     )
    ))
))

