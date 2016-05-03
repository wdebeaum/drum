;;;;
;;;; W::NAME
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::NAME
   (wordfeats (W::MORPH (:forms (-S-3P))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("name%1:10:00"))
     (EXAMPLE "My name is Chester")
     (LF-PARENT ONT::NAME)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::NAME
   (SENSES
    ((LF-PARENT ONT::naming)
     (meta-data :origin lou :entry-date 20040716 :change-date 20090501 :comments lou-demo :vn ("dub-29.3-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "name this site zuchinni")
     (TEMPL agent-neutral-name-templ)
     )
    ;; need this definition to allow v-passive-by rule to apply; comp arg must be -
    ((LF-PARENT ONT::naming)
     (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090501 :comments caloy3 :vn ("dub-29.3-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "this was named by him")
     (TEMPL agent-neutral-xp-templ)
     )
    ((meta-data :origin ptb :entry-date 20100429 :change-date nil :comments nil :vn ("dub-29.3-1"))
     (LF-PARENT ONT::naming)
     (example "the company named her as president")
     (TEMPL agent-neutral-as-theme-templ) ; like interpret,classify
     )
    )
   )
))

