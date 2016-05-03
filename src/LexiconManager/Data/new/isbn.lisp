;;;;
;;;; W::isbn
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::isbn
   (SENSES
    ((LF-PARENT ONT::id-number) (TEMPL other-reln-templ)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040113 :CHANGE-DATE NIL
      :COMMENTS calo-y2))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::isbn w::number)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::isbn W::numbers))))
   (SENSES
    ((LF-PARENT ONT::id-number) (TEMPL other-reln-templ)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040113 :CHANGE-DATE NIL
		:COMMENTS calo-y2))))
))

