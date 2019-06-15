;;;;
;;;; W::POSITION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::POSITION
   (SENSES
    ((LF-PARENT ONT::location) (TEMPL other-reln-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n
 :words (
  (w::position
  (senses
   ((LF-PARENT ONT::body-property)
    (meta-data :origin cardiac :entry-date 20080422 :change-date nil :comments nil)
    (TEMPL count-PRED-TEMPL)
    )
   )
)
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::POSITION
   (SENSES
    ((LF-PARENT ONT::MOLECULAR-SITE) 
     (META-DATA :ORIGIN BOB :ENTRY-DATE 20150107 :CHANGE-DATE NIL :wn ("position%1:10:01" "position%1:15:00")
		))))
))


(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::position
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090507 :comments nil :vn ("put-9.1") :wn ("position%2:35:00"))
     ;(LF-PARENT ONT::arranging)
     (LF-PARENT ONT::put)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like arrange
     ;(PREFERENCE 0.96)
     )
    )
   )
))

