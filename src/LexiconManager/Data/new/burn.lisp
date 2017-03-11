;;;;
;;;; W::burn
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::burn
    (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("burn%2:43:01"))
     (LF-PARENT ONT::burn)
     (TEMPL agent-affected-xp-templ) ; like ferment
     )
    ((meta-data :origin step :entry-date 20081027 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("burn%2:43:01"))
     (LF-PARENT ONT::burn)
     (example "the book burned")
     (TEMPL affected-templ) ; like ferment
     )
     ((meta-data :origin calo :entry-date 20040504 :change-date nil :comments html-purchasing-corpus)
      (EXAMPLE "burn a cd")
      ;;(LF-PARENT ONT::write)
      (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
      (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
      (preference .97)
      )
     )
    )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   ((W::burn w::out)
    (SENSES
     ((meta-data :origin beetle :entry-date 20050216 :change-date nil :comments mockup-1)
      (EXAMPLE "a lightbub burned out")
      (lf-parent ont::burn-out-light-up-change) ;; GUM change new parent 20121030
      (TEMPL affected-TEMPL)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      )
     )
    )
))

