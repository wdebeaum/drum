;;;;
;;;; W::AGE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
    (W::AGE
   (SENSES
    ((LF-PARENT ONT::age-scale)
     (TEMPL OTHER-RELN-TEMPL)
     (ExAMPLE "Assess the age of the building" "How old is she?")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("age%1:07:00")
		:COMMENTS HTML-PURCHASING-CORPUS))
))))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::age
  (wordfeats (W::morph (:forms (-vb) :nom w::aging))) 
   (SENSES
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "The death of his child aged him tremendously")
     (LF-PARENT ONT::age)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::age-scale))
     )
    ((meta-data :origin cause-result-relations :entry-date 20180803 :change-date nil :comments nil)
     (EXAMPLE "The wine aged beautifully")
     (LF-PARENT ONT::age)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended) (F::scale ont::age-scale))
     (TEMPL affected-unaccusative-templ)
     )
))))

