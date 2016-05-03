;;;;
;;;; W::CAUTION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CAUTION
   (SENSES
    ((LF-PARENT ONT::non-measure-ordered-domain) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::caution
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::WARN)
     (TEMPL agent-addressee-theme-optional-templ) ; like warn
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("advise-37.9-1"))
     (LF-PARENT ONT::WARN)
     (TEMPL agent-addressee-theme-objcontrol-req-templ (xp (% w::cp (w::ctype w::s-to)))) ; like advise,instruct
     )
    )
   )
))

