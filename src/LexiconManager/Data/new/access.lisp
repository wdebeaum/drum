;;;;
;;;; W::ACCESS
;;;;

;(define-words :pos W::n :templ COUNT-PRED-TEMPL
; :words (
;   (W::ACCESS
;   (SENSES
;    ((LF-PARENT ONT::NON-MEASURE-ORDERED-DOMAIN) (TEMPL MASS-PRED-TEMPL)
;     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
;		:COMMENTS HTML-PURCHASING-CORPUS))))
;))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::access
   (wordfeats (W::morph (:forms (-vb) :nom w::access :nomobjpreps (w::of w::to))))
   (SENSES
    ((EXAMPLE "He couldn't access the data")
     (meta-data :origin calo :entry-date 20040915 :change-date nil :comments caloy2)
     (LF-PARENT ONT::acquire)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

