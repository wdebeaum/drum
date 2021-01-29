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
     (TEMPL GEN-PART-OF-RELN-TEMPL (xp (% W::PP (W::ptype (? p W::of w::for)))))
     
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::NAME
   (SENSES
    ((LF-PARENT ONT::naming)
     (meta-data :origin lou :entry-date 20040716 :change-date 20090501 :comments lou-demo :vn ("dub-29.3-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "name this site zuchinni")
     ;(TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-NP-TEMPL)
     (TEMPL AGENT-NEUTRAL-NEUTRAL1-XP-TEMPL (XP (% W::NP)))
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
     ;(TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-PP-TEMPL) ; like interpret,classify
     (TEMPL AGENT-NEUTRAL-NEUTRAL1-2-XP-3-XP2-TEMPL (xp2 (% W::PP (w::ptype w::as)))) ; like interpret,classify
     )
    )
   )
))

