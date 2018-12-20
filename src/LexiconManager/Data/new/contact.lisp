;;;;
;;;; W::contact
;;;;
#||  ;; use wordnet senses
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::contact
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090508 :wn ("contact%1:10:01") :comments Break-Contact)
     (example "he cut off contact with the other team")
     (LF-PARENT ONT::communication)
     )
    )
   )
))||#

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::contact
   (senses
   ((meta-data :origin calo :entry-date 20031212 :change-date 20090508 :comments calo-y1script)
    (LF-PARENT ONT::establish-communication)
    (example "I'll contact the lab manager")
    (TEMPL AGENT-ADDRESSEE-TEMPL)
    )
    ;; NOW COMPOSITIONAL
   #|((meta-data :origin calo :entry-date 20031212 :change-date 20090508 :comments calo-y1script)
    (LF-PARENT ONT::establish-communication)
    (example "I'll contact the lab manager about it")
    (TEMPL AGENT-ADDRESSEE-Associated-information-TEMPL)
    )|#
   )
   )
))

