;;;;
;;;; W::excuse
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::excuse
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("excuse%2:32:01" "excuse%2:32:02"))
     (LF-PARENT ONT::forgive)
     (TEMPL agent-addressee-theme-templ (xp (% w::pp (w::ptype w::for)))) ; like thank
     (PREFERENCE 0.96)
     )
    ;;;; swier -- excuse me
    ((LF-PARENT ONT::PARDON)
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::excuse W::me)
   (SENSES
    ((LF (W::EXCUSE-ME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_NOLO-COMPRENDEZ))
     )
    )
   )
))

