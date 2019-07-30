;;;;
;;;; W::bring
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::bring (w::in))
   (wordfeats (W::morph (:forms (-vb) :past W::brought)))
   (SENSES
    ((LF-PARENT ont::add-include)
     (meta-data :origin mobius :entry-date 200808026 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "bring in the experts")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::bring
   (wordfeats (W::morph (:forms (-vb) :past W::brought)))
   (SENSES
    ;; note that here the goal is not a subcat but a spatial adverbial modifier
    ((LF-PARENT ONT::transport)
     (example "bring the cargo to avon")
     (meta-data :origin vn-analysis :entry-date unknown :change-date 20090529 :comments change-lf)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
   
    ((LF-PARENT ONT::adjust)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-RESULT-XP-NP-TEMPL (xp (% W::PP (W::ptype W::to))))
     (example "airing the curtain brings the fumes to acceptable levels")
     )
    (;(LF-PARENT ONT::cause-make)
     (LF-PARENT ONT::cause-produce-reproduce)
     (meta-data :origin step :entry-date 20080630 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     ;(TEMPL agent-effect-xp-templ (xp (% W::PP (W::ptype W::about))))
     (TEMPL AGENT-AFFECTEDR-XP-TEMPL (xp (% W::PP (W::ptype W::about))))
     ;(TEMPL agent-affected-create-templ (xp (% W::PP (W::ptype W::about))))
     (example "his consultations brought about his expertise")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::bring (W::up))
   (wordfeats (W::morph (:forms (-vb) :past W::brought)))
   (SENSES
    ((LF-PARENT ONT::visual-display)
     (example "bring up the map of Pacifica")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::bring (w::to))
    (wordfeats (W::morph (:forms (-vb) :past W::brought :ing W::bringing)))
   (SENSES
    ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the doctor brought him to")
      )
     ((LF-PARENT ONT::reviving)
     (meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "the elixir brought him to")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
     )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-NP-TEMPL
 :words (
  ((W::bring W::on)
   (wordfeats (W::morph (:forms (-vb) :past W::brought)))
   (SENSES
    ((example "bring on the movie")
     (LF-PARENT ONT::cause-produce-reproduce)
     (meta-data :wn ("bring_on%2:39:00"))
     )
    )
   )
))
