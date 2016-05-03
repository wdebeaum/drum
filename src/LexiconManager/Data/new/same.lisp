;;;;
;;;; W::SAME
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::SAME
   (wordfeats (W::ALLOW-POST-N1-SUBCAT +))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("same%3:00:02"))
     (EXAMPLE "They are same [as John's]" "the same book as John's")
     (LF-PARENT ONT::SAME)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE W::AS))))
     )    
    ))   
))

