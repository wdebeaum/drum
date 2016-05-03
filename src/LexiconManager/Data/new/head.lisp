;;;;
;;;; W::head
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::head
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("head%1:18:00") :comments nil)
     (LF-PARENT ONT::professional)
     (templ other-reln-templ)
     (example "head of marketing")
     )    
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil)
     (LF-PARENT ONT::object-dependent-location)
     (example "the head of the line")
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::n
 :tags (:base500)
 :words (
;; physical systems, digestive, reproductive,. ...
;; those are adjectives
;; external
  (W::HEAD
  (senses((LF-PARENT ONT::external-body-part)
    (TEMPL body-part-reln-templ)
    )
   )
)
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::HEAD
   (SENSES
    ;;;; The truck/I head to Avon
    ((LF-PARENT ONT::MOVE-toward)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AFFECTED-TEMPL)
     )
    )
   )
))

