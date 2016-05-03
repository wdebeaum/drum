;;;;
;;;; W::sound
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
   (W::sound
    (wordfeats (W::morph (:forms (-vb) :nom w::sound)))
   (SENSES
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     (LF-PARENT ONT::make-sound)
     (example "the alarm sounded")
     (TEMPL affected-templ)
     )
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     (LF-PARENT ONT::make-sound)
     (example "he sounded the alarm")
     (TEMPL agent-affected-xp-templ)
     )
    ((LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (example "the wind sounds fierce")
     (TEMPL neutral-theme-complex-subjcontrol-templ)
     )
    )
   )
))

