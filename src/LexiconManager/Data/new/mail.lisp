;;;;
;;;; W::MAIL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::MAIL
   (SENSES
    ((LF-PARENT ONT::mail) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::mail
   (SENSES
    ((LF-PARENT ONT::SEND)
     (example "mail the letter to him" "mail him the letter")
     (TEMPL AGENT-AFFECTED-TEMPL)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050930 :change-date nil :comments nil :vn ("send-11.1-1"))
     )
    )
   )
))

