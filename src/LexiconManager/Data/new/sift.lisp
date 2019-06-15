;;;;
;;;; W::sift
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::sift
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20061005 :comments nil :vn ("search-35.2") :wn ("sift%2:35:02" "sift%2:38:00"))
     (LF-PARENT ONT::physical-scrutiny)
     (TEMPL AGENT-FORMAL-XP-TEMPL) ; like check,search
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("sift%2:35:00"))
     (LF-PARENT ONT::separation)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype (? t w::from))))) ; like detach
     )
    )
   )
))

