;;;;
;;;; W::BRAND
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::BRAND
   (SENSES
    ((LF-PARENT ONT::name) (TEMPL gen-part-of-reln-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("brand%1:10:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::brand
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("dub-29.3-1"))
     (LF-PARENT ONT::naming)
     ;(TEMPL AGENT-NEUTRAL-FORMAL-2-XP-3-XP2-NP-OPTIONAL-TEMPL) ; like name
     (TEMPL AGENT-NEUTRAL-NEUTRAL1-XP-TEMPL (XP (% W::NP)))
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("dub-29.3-1"))
     (LF-PARENT ONT::naming)
     (TEMPL agent-neutral-xp-templ) ; like label
     )
    )
   )
))

