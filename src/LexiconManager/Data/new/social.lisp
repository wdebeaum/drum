;;;;
;;;; w::social
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::social w::security w::number)
    (SENSES
     ((meta-data :origin plot :entry-date 20080505 :change-date nil :comment nil)
      (LF-PARENT ONT::ssn)
      (templ other-reln-templ)
      (example "you need to know your social security number")
      )
     )
   )
))

  (DEFINE-WORDS :POS W::NAME :WORDS
 (
;                 :COMMENTS "temporary entry"))))
    ((w::social w::security)
      (senses
      ((LF-PARENT ONT::federal-organization) (TEMPL name-TEMPL)
       (META-DATA :ORIGIN plot :ENTRY-DATE 20080505 :CHANGE-DATE NIL
		 :COMMENTS nil))))
))

