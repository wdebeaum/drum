;;;;
;;;; W::P
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ;; need this for meeting transcriptions -- detokenized from p_c
   ((W::P w::C)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::p W::cs))))
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060127 :change-date nil :comments meeting-understanding)
     (LF-PARENT ONT::computer-type)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((W::p w::d w::a)
    (wordfeats (W::morph (:forms (-S-3P) :plur (W::p w::d W::as))))
   (SENSES
    ( (meta-data :origin calo :entry-date 20040204 :change-date nil :comments calo-y1script)
     (LF-PARENT ONT::computer-type)
     )
    )
   )
))

(define-words :pos W::ADJ  :templ central-adj-templ
 :words (
  (W::p
   (SENSES
    ((LF-PARENT ONT::phosphorilated))
    ))
  ))

(define-words :pos W::value :boost-word t
 :words (
  (W::P
  (senses
   ((LF-PARENT ONT::letter-symbol)
    (TEMPL value-templ)   
    (PREFERENCE 0.96)
    )
   )
  )
))

(define-words :pos W::n :templ PRED-NO-FORM-TEMPL
 :words (
  ((W::p W::m)
   (SENSES
    ((LF-PARENT ONT::time-object)
     (LF-FORM W::PM)
     (SEM (F::time-function F::day-period))
     )
    )
   )
))

(define-words :pos W::n :templ PRED-NO-FORM-TEMPL
 :words (
  ((W::p W::punc-period W::m W::punc-period)
   (SENSES
    ((LF-PARENT ONT::time-object)
     (LF-FORM W::PM)
     (SEM (F::time-function F::day-period))
     )
    )
   )
))

