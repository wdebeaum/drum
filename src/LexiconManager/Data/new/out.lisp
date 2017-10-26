;;;;
;;;; w::out
;;;; 

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::OUT
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20081028 :comments nil)
     ;; MD: this is marked as predicative-only because you cannot say "the out bulb", or at least not easily, and it was creating major ambiguities in multi-sentence cases with "burn out"
     (LF-PARENT ONT::NOT-IN-WORKING-ORDER-val)
     (example "the bridge is out")
     (templ predicative-only-adj-templ)
     )))

   ((w::out w::of)
    (senses
    ((LF-PARENT ONT::inadequate)
     (preference .98)
     (TEMPL  predicative-adj-req-xp-templ (xp (% w::np )))
     (example "out of sugar" "out of breath" "out of time")
     ))
    )))

(define-words :pos W::ADV
 :words (
  ((W::OUT W::OF)
   (SENSES
    ((LF-PARENT ONT::OUTSIDE)
     (example "it is out of the bag") 
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
   
    ((LF-PARENT ONT::original-material)
     (example "make it out of stone")
     ;;(preference .98)  let semantic restrictions discourage this when appropriate
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date nil :comments caloy3)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::OUT
   (SENSES
    ((LF-PARENT ONT::direction)
     (TEMPL PARTICLE-TEMPL)
     ) 

    ((LF-PARENT ONT::COMPLETELY)
     (TEMPL PARTICLE-MANNER-TEMPL)
     (example "I'm zonked out." "He sorted out every scrap of manuscript, every map, and the native letters.")
     ) 

    )
   )
))

(define-words
    :pos w::adv
 :tags (:base500)
 :words (
  (w::out
  (senses((lf-parent ont::location-distance-modifier)
	    (templ adv-operator-templ)
	    (example "he is over there" "he is out in the sunshine" "he is out in the street"))
	   )
)
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::OUT
   (SENSES
    ((LF (W::OUT))
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((w::out w::of w::breath)
   (senses
   ((meta-data :origin chf :entry-date 20071227 :change-date nil :comments nil)
   (lf-parent ont::breathless-val)
   (templ less-adj-templ)
   (example "he is out of breath")
   )))
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::out w::of w::whack)
   (SENSES
    ((LF-PARENT ONT::defective-val)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::out w::of w::kilter)
   (SENSES
    ((LF-PARENT ONT::defective-val)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     )
    )
   )
))

(define-words :pos W::adj
 :words (
  ((w::out w::of w::kilter)
  (senses
   ((LF-PARENT ONT::strange)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :wn ("strange%3:00:00") :comments nil)
    )
   )
)
))

(define-words :pos W::adj
 :words (
  ((w::out w::of w::whack)
  (senses
   ((LF-PARENT ONT::strange)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::less))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :wn ("strange%3:00:00") :comments nil)
    )
   )
)
))
