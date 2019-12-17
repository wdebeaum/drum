;;;;
;;;; W::ANGLE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ANGLE
   (SENSES
    (;(LF-PARENT ONT::shape)
     (LF-PARENT ONT::shape-object) ; put in here for now
     (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("angle%1:25:00")
		:COMMENTS HTML-PURCHASING-CORPUS))
    
    ;; MD FIXME -- potential duplicate
    ((meta-data :origin lam :entry-date 20050706 :change-date nil :wn ("angle%1:25:00") :comments benoit-train )
     (LF-PARENT ONT::Location)
     (TEMPL PART-OF-RELN-TEMPL)
     )
    )
   )
))

