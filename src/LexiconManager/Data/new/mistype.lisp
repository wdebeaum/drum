;;;;
;;;; W::mistype
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::mistype
    (SENSES
    ((EXAMPLE "I mistyped the address")
     ;;(LF-PARENT ONT::type)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin task-learning :entry-date 20050912 :change-date nil :comments nil)
     )
    )
   )
))

