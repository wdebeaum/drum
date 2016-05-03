;;;;
;;;; W::ID
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ID
   (SENSES
      ;; physical form of id
     ((LF-PARENT ONT::symbolic-representation) (TEMPL other-reln-templ)
      (META-DATA :ORIGIN PLOT :ENTRY-DATE 20080505 :CHANGE-DATE NIL
      :COMMENTS NIL))
     ;; abstract
    ((LF-PARENT ONT::identification) (TEMPL other-reln-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE 20050113 :wn ("id%1:10:00")
      :COMMENTS HTML-PURCHASING-CORPUS))
    ))
))

