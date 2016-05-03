;;;;
;;;; W::SURE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (W::SURE
    (wordfeats (W::morph (:FORMS (-LY))))
    (SENSES
     ((LF-PARENT ONT::CERTAIN)
      (example "the plan is sure to succeed")
      (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-to))))
      (meta-data :origin calo :entry-date 20040921 :change-date 20090731 :wn ("sure%3:00:00") :comments caloy2)
      )
     ((LF-PARENT ONT::CERTAIN)
      (example "he is sure of the plan")
      (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::pp (W::ptype W::of))))
      )
     ((LF-PARENT ONT::CERTAIN)
      (example "he is sure that he will win")
      (syntax (w::allow-deleted-comp -))
      (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-finite))))
      (meta-data :origin calo :entry-date 20040921 :change-date 20090731 :wn ("sure%3:00:00") :comments caloy2)
      )
     ((LF-PARENT ONT::CERTAIN)
      (example "he is sure what to do")
      (syntax (w::allow-deleted-comp -))
      (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::NP (w::sort w::wh-desc))))
      (meta-data :origin beeetle :entry-date 20081106 :change-date 20090731 :wn ("sure%3:00:00") :comments pilot4)
      )     
     )
    )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
;   )
  (W::SURE
   (SENSES
    ((LF (W::SURE))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_CONFIRM))
     )
    )
   )
))

