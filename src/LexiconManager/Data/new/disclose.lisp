;;;;
;;;; W::disclose
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::disclose
   (wordfeats (W::morph (:forms (-vb) :nom W::disclosure)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("say-37.7"))
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::tell)
     (TEMPL agent-theme-xp-templ) ; like respond,reply
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo-ontology :entry-date 20060315 :change-date 20090506 :comments nil)
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::tell)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-that)))) ; like report
     )
    ((meta-data :origin calo-ontology :entry-date 20060315 :change-date 20090506 :comments nil)
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::encodes-message)
     (example "the report disclosed that they left")
     (TEMPL neutral-theme-xp-templ (xp (% w::cp (w::ctype w::s-that)))) ; like report
     )
    )
   )
))

