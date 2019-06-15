;;;;
;;;; W::pardon
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::pardon
   (wordfeats (W::morph (:forms (-vb) :ing W::pardoning :past W::pardoned)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33") :wn ("pardon%2:32:00" "pardon%2:32:01"))
     (LF-PARENT ont::pardon)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-A-TEMPL (xp (% w::pp (w::ptype (? pt w::for w::of)))))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::pardon W::me)
   (SENSES
    ((LF (W::EXCUSE-ME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_NOLO-COMPRENDEZ))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  (W::pardon
   (SENSES
    ((LF (W::EXCUSE-ME))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_NOLO-COMPRENDEZ))
     )
    )
   )
))

