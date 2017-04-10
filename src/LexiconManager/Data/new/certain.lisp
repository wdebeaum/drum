;;;;
;;;; W::CERTAIN
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::CERTAIN
   (SENSES
    ((lf-parent ont::specific-val)
     (example "a certain number of results")
     )
    ((LF-PARENT ONT::CERTAIN)
     (example "the plan is certain to succeed")
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-to))))
     (meta-data :origin calo :entry-date 20040921 :change-date 20090731 :wn ("certain%3:00:03") :comments caloy2)
     )
    ((LF-PARENT ONT::CERTAIN)
     (syntax (w::allow-deleted-comp -))
     (example "he is certain that he will win")
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-finite))))
     (meta-data :origin calo :entry-date 20040921 :change-date 20090731 :wn ("certain%3:00:02") :comments caloy2)
     )
    ((LF-PARENT ONT::CERTAIN)
     (example "he is certain what to do")
     (syntax (w::allow-deleted-comp -))
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::NP (w::sort w::wh-desc))))
     (meta-data :origin beeetle :entry-date 20081106 :change-date 20090731 :wn ("sure%3:00:00") :comments pilot4)
     (preference 0.98)
     )     
    )
   )
))

