;;;;
;;;; W::last
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::last
   (SENSES
    ((LF-PARENT ONT::take-time)
     (TEMPL Neutral-DURATION-TEMPL)
     (example "the meeting lasted five hours")
     )
    ((LF-PARENT ONT::take-time)
     (TEMPL neutral-TEMPL)
     (example "land is the only thing that lasts")
     (SEM (F::Aspect F::stage-level))
     )
    #||((LF-PARENT ONT::take-time)    ;; should be productive
     (example "the truck lasted him five years")
     (TEMPL THEME-affected-duration-TEMPL)
     )||#
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LAST
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("last%5:00:00:closing:00" "last%3:00:00"))
     (LF-PARENT ONT::SEQUENCE-VAL)
     (example "it was the last day they met")
     ;;(preference .95)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
;   )
  (W::LAST
   (SENSES
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL PRED-S-POST-TEMPL)
     (example "he did it last")
     (PREFERENCE 0.96) ;; prefer ordinal
     )
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :tags (:base500)
 :words (
  (W::LAST
   (SENSES
    ((LF (W::LAST))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a))
     (preference .96) ;prefer compound last name for plot
     )
    )
   )
))

