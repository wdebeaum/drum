;;;;
;;;; W::NET
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::NET
   (SENSES
    ((LF-PARENT ONT::computer-network) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("net%1:06:01")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ agent-neutral-xp-templ
 :words (
  (W::net
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090430 :change-date nil :comments nil)
     (LF-PARENT ONT::earning)
     (EXAMPLE "I netted $1 million on the sale")
     )
    )
   )
))

