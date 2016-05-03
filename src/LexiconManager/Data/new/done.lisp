;;;;
;;;; W::DONE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; later - derive from verb
  (W::DONE
   (SENSES
    ((LF-PARENT ONT::FINISHED)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::PP (W::ptype W::with))))
     (example "we are done with buying books")
     (meta-data :origin plow :entry-date 20060516 :change-date 20090731 :wn ("done%5:00:00:finished:00") :comments pqs)
     )
    ((LF-PARENT ONT::FINISHED)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::vp (W::vform W::ing))))
     (example "we are done buying books")
     (meta-data :origin plow :entry-date 20060516 :change-date 20090731 :wn ("done%5:00:00:finished:00") :comments pqs)
     )
    )
   )
))

