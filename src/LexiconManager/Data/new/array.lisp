;;;;
;;;; W::ARRAY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ARRAY
   (SENSES
    ((LF-PARENT ONT::collection) (TEMPL other-reln-templ)
     (EXAMPLE "array of ducks")
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))
    ((LF-PARENT ONT::data-structure) (TEMPL other-reln-templ)
     (EXAMPLE "array of integers")
     (META-DATA :ORIGIN domain-reorganization :ENTRY-DATE 20170821 :CHANGE-DATE NIL))
))
))

