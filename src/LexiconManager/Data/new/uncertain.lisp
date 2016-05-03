;;;;
;;;; W::UNCERTAIN
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::UNCERTAIN
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090731 :wn ("uncertain%3:00:02") :comments html-purchasing-corpus)
     (LF-PARENT ONT::UNRELIABLE)
     )
    ((LF-PARENT ONT::UNCERTAIN)
     (example "he is uncertain what to do")
     (syntax (w::allow-deleted-comp -))
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::NP (w::sort w::wh-desc))))
     (meta-data :origin beeetle :entry-date 20081106 :change-date 20090731 :wn ("sure%3:00:00") :comments pilot4)
     (preference 0.98)
     )     
    )
   )
))

