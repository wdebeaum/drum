;;;;
;;;; W::host
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::host
   (SENSES
    ((LF-PARENT ONT::computer-network)
     (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN task-learning :ENTRY-DATE 20050825 :CHANGE-DATE NIL :wn ("host%1:06:00") :COMMENTS nil)
     (EXAMPLE "enter the name of your SMTP host")
     )
    ((LF-PARENT ONT::person)
     (example "he is the host")
     (meta-data :origin plow :entry-date 20060712 :change-date nil :wn ("host%1:18:02") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 (W::host
   (SENSES
    ((LF-PARENT ONT::managing-resources)
     (example "host an event")
     (meta-data :origin plow :entry-date 20060712 :change-date nil :comments nil)
     )
    )
   )
))

