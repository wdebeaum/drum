;;;;
;;;; W::CHIP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CHIP
   (SENSES
    ((LF-PARENT ONT::computer-part) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("chip%1:06:00")
		:COMMENTS HTML-PURCHASING-CORPUS))
    ;; MD FIXME potential duplicate
    ((LF-PARENT ONT::computer-part)
     (TEMPL GEN-PART-OF-RELN-TEMPL)
     (meta-data :origin calo :entry-date 20041206 :change-date nil :wn ("chip%1:06:00") :comments caloy2)
     (example "one module in one chip")
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (W::CHIP
  (senses
	   ((LF-PARENT ONT::CRACKERS)
	    (TEMPL count-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos W::V 
 :words (
  (W::chip
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("chip%2:35:00" "chip%2:35:01" "chip%2:35:03"))
     (LF-PARENT ont::break-object)
     (TEMPL agent-affected-xp-templ) ; like break
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("break-45.1") :wn ("chip%2:35:00" "chip%2:35:01" "chip%2:35:03"))
     (LF-PARENT ont::break-object)
     (TEMPL affected-templ) ; like crash,tear,break
     )
    
   )
)))

