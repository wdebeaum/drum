;;;;
;;;; W::UNSURE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (W::UNSURE
    (SENSES
     ((LF-PARENT ONT::UNCERTAIN)
      (example "he is unsure that the answer is correct")
      (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::cp (W::ctype W::s-finite))))
      (meta-data :origin lam :entry-date 20050421 :change-date 20090731 :wn ("unsure%3:00:00") :comments lam-initial)
      )
     ((LF-PARENT ONT::UNCERTAIN)
      (example "he is unsure of the answer")
      (syntax (w::allow-deleted-comp -))
      (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::PP (W::ptype W::of))))
      (meta-data :origin lam :entry-date 20050421 :change-date 20090731 :wn ("unsure%3:00:00") :comments lam-initial)
      ;; preference lowered just slightly so that the first version with reduced complement always wins
      (preference 0.99)
      )     
     ((LF-PARENT ONT::UNCERTAIN)
      (example "he is unsure what to do")
      (syntax (w::allow-deleted-comp -))
      (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::NP (w::sort w::wh-desc))))
      (meta-data :origin beeetle :entry-date 20081106 :change-date 20090731 :wn ("sure%3:00:00") :comments pilot4)
      )     
     )
    )
))

