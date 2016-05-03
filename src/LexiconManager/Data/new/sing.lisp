;;;;
;;;; W::sing
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
(W::sing
   (wordfeats (W::morph (:forms (-vb) :past W::sang :pastpart w::sung)))
   (SENSES
    ((LF-PARENT ONT::intentionally-act)
     (meta-data :origin bolt :entry-date 20120516 :comments top500)
     (example "sing him the song")
     (TEMPL agent-affected-iobj-theme-templ)
     )
    ((LF-PARENT ONT::intentionally-act)
     (meta-data :origin bolt :entry-date 20120516 :comments top500)
     (example "he sang the song")
     (TEMPL AGENT-THEME-XP-TEMPL)
     )
    ((LF-PARENT ONT::intentionally-act)
     (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
     (example "he sang")
     (TEMPL AGENT-TEMPL)
     )
    )
   )
))

