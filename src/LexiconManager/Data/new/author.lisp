;;;;
;;;; W::author
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::author
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :wn ("author%1:18:00"))
     (LF-PARENT ONT::author)
     (templ other-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::author
   (SENSES
    ((EXAMPLE "he authored the letter")
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::write)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (PREFERENCE 0.98) ;; prefer the noun sense
     )
    )
   )
))

