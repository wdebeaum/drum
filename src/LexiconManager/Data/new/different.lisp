;;;;
;;;; W::DIFFERENT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::DIFFERENT
   (wordfeats (w::allow-post-n1-subcat +))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments beetle2-pilots :wn ("different%3:00:00"))
     (EXAMPLE "They are different/ they are different from each other / a different state than the terminal 1")
     (LF-PARENT ONT::DIFFERENT)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? ptp W::FROM w::than)))))
     )
    )
   )
))

