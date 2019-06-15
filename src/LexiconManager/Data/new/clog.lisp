;;;;
;;;; W::clog
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::clog
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::clog)
     (SYNTAX (w::resultative +))
     (examples "his arteries clogged")
     (templ affected-unaccusative-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::clog)
     (SYNTAX (w::resultative +))
     (preference .95) ;; prefer intransitive
     (examples "rich food clogs his arteries")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::clog (w::up))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::clog)
     (SYNTAX (w::resultative +))
     (examples "his arteries clogged up")
     (templ affected-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090129 :change-date 20090504 :comments LM-vocab :vn ("entity_specific_cos-45.5"))
     (LF-PARENT ONT::clog)
     (SYNTAX (w::resultative +))
     (preference .95) ;; prefer intransitive
     (examples "rich food clogs up his arteries")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

