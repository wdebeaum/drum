;;;;
;;;; W::ABUSE
;;;;

;(define-words :pos W::n :templ COUNT-PRED-TEMPL
; :words (
;    (W::ABUSE
;   (SENSES
;    ((LF-PARENT ONT::NON-MEASURE-ORDERED-DOMAIN) (TEMPL COUNT-PRED-TEMPL)
;     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
;      :COMMENTS HTML-PURCHASING-CORPUS))))
;))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::abuse
   (wordfeats (W::morph (:forms (-vb) :nom w::abuse)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("abuse%2:30:00" "abuse%2:32:00" "abuse%2:41:00"))
     (LF-PARENT ont::abuse)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::abuse)
     (example "he abused his influence")
     (TEMPL AGENT-FORMAL-XP-TEMPL) 
     )
    )
   )
))

