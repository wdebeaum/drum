;;;;
;;;; w::look
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
((w::look (w::over))
 (senses
  ((EXAMPLE "look over the choices")
   (LF-PARENT ONT::scrutiny)
   (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
   (TEMPL agent-neutral-xp-templ)
   (meta-data :origin calo :entry-date 20050323 :change-date nil :comments caloy2)
   )
  )
 )
))


(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
((w::look (w::up))
 (senses
  ((EXAMPLE "look up the number in the book")
   (LF-PARENT ONT::look-up)
   (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
   (TEMPL agent-neutral-xp-templ)
   (meta-data :origin calo :entry-date 20050323 :change-date nil :comments caloy2)
   )
  ((EXAMPLE "look up whether this is true")
   (LF-PARENT ONT::look-up)
   (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
   (TEMPL AGENT-FORMAL-XP-CP-TEMPL)
   (meta-data :origin calo :entry-date 20050323 :change-date nil :comments caloy2)
   )
  )
 )
))


(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::look
   (wordfeats (W::morph (:forms (-vb) :nom W::look)))
   (SENSES
    ((LF-PARENT ONT::APPEARS-TO-HAVE-PROPERTY)
     (example "he looks happy (to me)")
     (TEMPL NEUTRAL-FORMAL-SUBJCONTROL-TEMPL)
     )
    ;; but shouldn't this really be an agent??
    
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("peer-30.3") :wn ("look%2:39:00" "look%2:39:02"))
     (LF-PARENT ONT::physical-scrutiny)
     (SEM (F::Time-span F::extended))
     ;(TEMPL agent-location-TEMPL)
     (TEMPL agent-TEMPL)
     (example "look behind the shed")
     )
    ((EXAMPLE "look at something else")
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-XP-TEMPL  (xp (% W::pp (W::ptype W::at))))
     (meta-data :origin calo :entry-date 20041026 :change-date nil :comments caloy2)
     )
    ((EXAMPLE "look into the options")
     (LF-PARENT ONT::scrutiny)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-neutral-XP-TEMPL  (xp (% W::pp (W::ptype W::into))))
     (meta-data :origin calo :entry-date 20050323 :change-date nil :comments caloy2)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::look W::like)
   (SENSES
    ;;;; It looks like we will have to go
    ((LF-PARENT ONT::POSSIBLY-true)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL EXPLETIVE-NEUTRAL-1-XP1-2-XP2-TEMPL (xp1 (% W::NP (W::lex W::it))) (xp2 (% W::s (W::stype W::decl))))
     )
    ((meta-data :origin calo :entry-date 20040421 :change-date nil :comments caloy1v4)
     (LF-PARENT ONT::resemble)
     (example "they look like dogs")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )
    )
   )
))

