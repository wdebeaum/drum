;;;;
;;;; W::bloat
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  ((W::bloat (w::up))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (examples "his stomach bloated up")
     (templ affected-unaccusative-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (examples "fluid bloats his stomach up")
     (preference .97) ;; ?? prefer intransitive
     (templ agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::bloat
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (examples "his stomach bloated")
     (templ affected-unaccusative-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::swell)
     (SYNTAX (w::resultative +))
     (examples "fluid bloats his stomach" "rain bloats the river")
     (preference .95) ;; prefer intransitive     (preference .95) ;; prefer intransitive
     (templ agent-affected-xp-templ)
     )
    )
   )
))

