;;;;
;;;; W::print
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::print
   (SENSES
    ((LF-PARENT ONT::text-representation)
     (example "in print" "print server")
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :wn ("print%1:10:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::print
    (SENSES
    ((EXAMPLE "print the document")
     ;;(LF-PARENT ONT::print)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2 :wn-sense (4))
     )
    (;;(LF-PARENT ONT::print)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "I want to be able to print and fax")
     (TEMPL AGENT-TEMPL)
     (meta-data :origin calo :entry-date 20041122 :change-date nil :comments caloy2  :wn-sense (4))
     (PREFERENCE 0.98) ;; prefer transitive sense
     )
    )
   )
))

