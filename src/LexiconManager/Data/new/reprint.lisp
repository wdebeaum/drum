;;;;
;;;; W::reprint
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::reprint
    (SENSES
    ((EXAMPLE "reprint the document")
     ;;(LF-PARENT ONT::print)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin quicken :entry-date 20071129 :change-date nil :comments nil)
     )
    )
   )
))

