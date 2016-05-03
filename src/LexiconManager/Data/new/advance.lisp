;;;;
;;;; W::ADVANCE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::ADVANCE
   (SENSES
    ((LF-PARENT ONT::improve) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("advance%1:11:01")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
(W::advance
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("escape-51.1-2"))
     (LF-PARENT ONT::move-forward)
     (TEMPL affected-templ) ; like go,fall
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("future_having-13.3") :wn ("advance%2:40:00"))
     (LF-PARENT ONT::lend)
     (example "advance him the money")
     (TEMPL agent-affected-recipient-alternation-templ) ; like grant,offer
     (PREFERENCE 0.96)
     )
    #||((example "He used an advanced technique")   ;; SUBSUMED BY CAUSE-AFFECTED
     (sem (f::aspect f::dynamic))
     (meta-data :origin plow :entry-date 20050316 :change-date nil :comments nil)
     (LF-PARENT ONT::improve)
     )||#
    ((example "the improvements advanced the field")
     (sem (f::aspect f::dynamic))
     (templ agent-affected-xp-templ)
     (meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
     (LF-PARENT ONT::improve)
     )
    )
   )
))

