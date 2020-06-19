;;;;
;;;; W::feel
;;;;

(define-words :pos W::v 
 :words (
  ((W::feel W::like)
    (wordfeats (W::morph (:forms (-vb-pres-past-only) :past W::felt)))
   (SENSES
    ;;;; It feels like we will have to go
    ((LF-PARENT ONT::possibly-true)
     (meta-data :origin cardiac :entry-date 20090416 :change-date nil :comments speechtests)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::s (W::stype W::decl))))
     )
    ))
))

(define-words :pos W::v
  :words (
  (W::feel
   (wordfeats (W::morph  (:forms (-vb) :past W::felt)))
   (SENSES
     ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("consider-29.9-2"))
      (LF-PARENT ONT::believe)
      (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype w::s-finite)))) ; like believe,think
     (PREFERENCE .99)
      )
     ))))

(define-words :pos W::v
  :words (
  (W::feel
   (wordfeats (W::morph  (:forms (-vb) :past W::felt)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("see-30.1") :wn ("feel%2:29:00" "feel%2:31:00" "feel%2:35:00" "feel%2:39:00"))
     (LF-PARENT ONT::perception)
     (example "I can feel the sun" "he feels pain in his leg")
     (TEMPL experiencer-neutral-xp-templ) ; like smell,taste
     (PREFERENCE 0.96)
     )
    ((meta-data :origin trips :entry-date 20090331 :change-date nil :comments missing-sense)
     (LF-PARENT ONT::perception)
     ;;(SEM (F::Aspect F::Stage-level) (F::Time-span F::extended))
     (example "he felt him move")
     (TEMPL EXPERIENCER-NEUTRAL-FORMAL-CP-OBJCONTROL-A-TEMPL (xp (% W::VP (W::vform W::base))))
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("stimulus_subject-30.4") :wn ("feel%2:39:09" "feel%2:42:00"))
     (LF-PARENT ONT::appears-to-have-property)
     (example "it feels good")
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::s (W::stype W::decl))))
     )
    ;;;; I feel poorly
    ;;;; I feel like a coke
    ;;;; I feel like doing the samba
    ;;;; I feel that I'm going downhill
    ;;;; I feel with my hands <<---- NOT THIS ONE!
    ;;;; swift 01/12/01 added the aspect feature F_Stage-Level to allow progressive 'feeling poorly'
    ;;;; swift 02/12/02 changed subject to experiencer
    ((LF-PARENT ONT::experiencer-emotion)
     (SEM (F::Aspect F::Stage-Level))
     (TEMPL EXPERIENCER-FORMAL-PRED-SUBJCONTROL-TEMPL)
     )
    )
   )
))

