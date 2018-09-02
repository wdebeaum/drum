;;;;
;;;; W::CAPABILITY
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::CAPABILITY
   (SENSES
    ((LF-PARENT ONT::able-scale)
     (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040908 :CHANGE-DATE NIL :COMMENTS caloy2)
     (templ other-reln-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )))
   ))

