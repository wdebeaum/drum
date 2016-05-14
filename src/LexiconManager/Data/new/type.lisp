;;;;
;;;; W::type
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::type
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((EXAMPLE "What type of networking options are available?")
     (meta-data :origin calo :entry-date 20040504 :change-date nil :wn ("type%1:09:00") :comments calo-y1variants)
     (LF-PARENT ONT::KIND)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::type
    (SENSES
    ((EXAMPLE "type the letter")
     ;;(LF-PARENT ONT::type)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin calo :entry-date 20050318 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
((W::type (w::in))
    (SENSES
    ((EXAMPLE "type in the title")
     (LF-PARENT ONT::put) ;; as in entering text
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin plow :entry-date 20050927 :change-date nil :comments naive-subjects)
     )
    )
   )
))

