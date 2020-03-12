;;;;
;;;; W::C
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::C w::D w::R w::W)
   (SENSES
    ((meta-data :origin allison :entry-date 20040922 :change-date nil :comments caloy2)
     (LF-PARENT ont::internal-computer-storage)
     (example "it has a cdrw dvd drive")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    ((W::C w::D)
   (SENSES
    ((meta-data :origin allison :entry-date 20040922 :change-date nil :comments caloy2)
     (LF-PARENT ont::data-storage-medium)
     (example "I want to be able to burn cds")
     (PREFERENCE 0.96) ; prefer compound if it's cd rom drive
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::C w::D W::ROM)
   (SENSES
    ((meta-data :origin calo :entry-date 20040622 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::internal-computer-storage)
     (example "I need a cd rom")
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::c w::m)
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    (;(LF-PARENT ONT::length-unit)
     (LF-PARENT ONT::cm)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     (meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    ((W::c w::c)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     (example "what is the cc address")
     (LF-PARENT ONT::copy)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
    ((W::c w::c)
    (SENSES
     ((lf-parent ont::sendcopy)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-NP-TEMPL (xp (% W::PP (W::ptype w::on))))
     (example "cc him on that")
     (meta-data :origin calo-ontology :entry-date 20060425 :change-date nil :comments iris)
     )
     )
    )
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::c w::e)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  ((w::c. w::e.)
  (senses
   ((LF-PARENT ONT::era)
    (TEMPL value-templ)
    )
   )
)
))

(define-words :pos W::value :boost-word t
 :words (
  (W::C
  (senses((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)  
    )
   )
)
))

