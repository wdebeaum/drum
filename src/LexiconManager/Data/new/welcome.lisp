;;;;
;;;; w::welcome
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (w::welcome
     (wordfeats (W::morph (:forms (-vb) :nom w::welcome)))
   (senses
    ((LF-PARENT ONT::welcome)
     (TEMPL AGENT-AGENT1-NP-TEMPL)
     (EXAMPLE "he welcomed the guests")
     (meta-data :origin calo-ontology :entry-date 20060125 :change-date 20090508 :comments meeting-understanding)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::WELCOME
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20061106 :wn ("welcome%3:00:00") :comments html-purchasing-corpus :comlex (ADJ-TO-INF))
     (example "a welcome change")
     (lf-parent ont::pleasing-val)
     (TEMPL central-adj-templ)
     (SEM (f::gradability +) (f::orientation F::pos) (f::intensity ont::med))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::welcome W::back)
   (SENSES
    ((LF (W::HELLO))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_GREET))
     )
    )
   )
))

