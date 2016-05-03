;;;;
;;;; w::teach
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (w::teach
   (wordfeats (W::morph (:forms (-vb) :past w::taught)))
   (senses
    ((LF-PARENT ONT::teach-train)
     (example "teach him web page reading")
     (TEMPL agent-affected-iobj-theme-templ)
     (meta-data :origin calo :entry-date 20041025 :change-date nil :comments caloy2)
     )
    ((LF-PARENT ONT::teach-train)
     (example "teach him about web page reading")
     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL)
     (meta-data :origin calo :entry-date 20041025 :change-date nil :comments caloy2)
     (preference .98)
     )
;    ((EXAMPLE "Teach him what he needs to know")
;     (LF-PARENT ONT::transfer-information)
;     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL  (xp (% W::NP (W::sort W::wh-desc))))
;     (meta-data :origin calo :entry-date 20041025 :change-date nil :comments caloy2)
;     )
    ((EXAMPLE "Teach him to do it")
     (LF-PARENT ONT::teach-train)
     (TEMPL AGENT-ADDRESSEE-THEME-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin calo :entry-date 20041025 :change-date nil :comments caloy2)
     (preference .98) 
     )
     ((EXAMPLE "he teaches every thursday")
     (LF-PARENT ONT::teach-train)
     (TEMPL AGENT-TEMPL)
     (meta-data :origin trips-general :entry-date 20090406 :change-date nil :comments nil)
     (preference .97) ;; disprefer intransitive
     )
    )
   )
))

