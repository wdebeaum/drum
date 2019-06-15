;;;;
;;;; W::MONITOR
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MONITOR
   (SENSES
    ((LF-PARENT ONT::computer-monitor) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("monitor%1:06:02")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::monitor
    (wordfeats (W::morph (:forms (-vb) :past W::monitored :ing W::monitoring)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("investigate-35.4"))
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL AGENT-FORMAL-XP-TEMPL) ; like explore,investigate,examine,test,survey,inspect
     )
    ((meta-data :origin coordops :entry-date 20070706 :change-date nil :comments nil :vn ("investigate-35.4"))
     (LF-PARENT ONT::physical-scrutiny)
     (example "green start monitoring")
     (TEMPL agent-templ) 
     )
    )
   )
))

