;;;;
;;;; W::sound
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::sound
   (SENSES
    ((LF-PARENT ONT::audio)
     )
    )
   )
   ))

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
   (W::sound
    ;(wordfeats (W::morph (:forms (-vb) :nom w::sound)))
   (SENSES
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     (LF-PARENT ONT::make-sound)
     (example "the alarm sounded")
     (TEMPL agent-templ) ;; agent emits a sound
     )
;    ((meta-data :origin cause-result-relations :entry-date 20180411 :change-date nil)
;     (LF-PARENT ONT::make-sound)
;     (example "the gun sounds/goes 'bang'")
;     (TEMPL agent-neutral-templ) ;; agent emits a sound 
;     )
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     ;(LF-PARENT ONT::play)
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

