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

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
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
    ;((meta-data :origin cause-result-relations :entry-date 20180411 :change-date nil)
     ;(LF-PARENT ONT::make-sound)
     ;(example "the gun sounds/goes 'bang'")
     ;(TEMPL agent-neutral-templ) ;; agent emits a sound 
;     )
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     ;(LF-PARENT ONT::play)
     (LF-PARENT ONT::make-sound)
     (example "he sounded the alarm")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    (;(LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (LF-PARENT ONT::SEEM)
     (example "the wind sounds fierce")
     ;(TEMPL NEUTRAL-FORMAL-SUBJCONTROL-TEMPL)
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::sound W::like)
   (SENSES
    (;(LF-PARENT ONT::POSSIBLY-true)
     (LF-PARENT ONT::SEEM)
     ;(SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     ;(TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::s (W::stype W::decl))))
     (TEMPL EXPLETIVE-FORMAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::s (W::stype W::decl))))
     (EXAMPLE "It sounds like something bad happened.")
     )

     ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("stimulus_subject-30.4") :wn ("smell%2:39:00" "smell%2:39:02"))
     ;(LF-PARENT ONT::appears-to-have-property)
     (LF-PARENT ONT::SEEM)
     (example "it sounds like a good plan")
     ;(TEMPL NEUTRAL-FORMAL-SUBJCONTROL-TEMPL) ; like look
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::NP)))
     (SYNTAX (w::exclude-passive +))
     ;(PREFERENCE 0.96)
     )
    
    )
   )
))
