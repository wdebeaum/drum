;;;;
;;;; W::BIOS
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ;; basic input output system -- firmware
  (W::BIOS
   (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::computer-firmware) (TEMPL COUNT-PRED-TEMPL)
     (example "you can turn it on in the bios")
     (META-DATA :ORIGIN allison :ENTRY-DATE 20040922 :CHANGE-DATE NIL
		:COMMENTS caloy2))))
))

