;;;;
;;;; W::daisy
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::daisy w::chain)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::daisy W::chains))))
   (SENSES
    ((LF-PARENT ONT::COMPUTER-network) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN caloy2 :ENTRY-DATE 20050522 :CHANGE-DATE NIL
		:COMMENTS meeting-understanding))))
))

