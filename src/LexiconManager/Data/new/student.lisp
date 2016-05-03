;;;;
;;;; W::STUDENT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::STUDENT
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus :wn ("student%1:18:00" "student%1:18:01"))
     (LF-PARENT ONT::scholar)
     )
    ;; MD FIXME - potential duplicate
    ((LF-PARENT ONT::professional) 
     (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("student%1:18:01" "student%1:18:00")
					     :COMMENTS HTML-PURCHASING-CORPUS))
    )
   )
))

