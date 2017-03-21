;;;;
;;;; w::put
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
((w::put (w::in))
   (wordfeats (W::morph (:forms (-vb) :past W::put)))
 (senses
  (;(lf-parent ont::set-up-device)
   ;; jr 20120719 caet jfa wants change to ont::install for tea dialog y2 "put the tea bag in"
   (lf-parent ont::install)
   (example "put in the phone lines")
   (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
   (TEMPL agent-affected-xp-templ)
   (preference .97) ; prefer in as adverbial
   (meta-data :origin calo-ontology :entry-date 20060124 :change-date 20090504 :comments caloy3)
   )
  )
 )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((w::put (w::together))
    (wordfeats (W::morph (:forms (-vb) :past W::put :ing W::putting)))
   (senses
    ((LF-PARENT ONT::joining)
     (meta-data :origin calo-ontology :entry-date 20060126 :change-date 20090505 :comments caloy3)
     (example "put the pieces together")
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::put
   (wordfeats (W::morph (:forms (-vb) :past W::put :ing W::putting)))
   (SENSES
    ((LF-PARENT ONT::put)
     (meta-data :origin vn-analysis :entry-date unknown :change-date 20040617 :comments change-lf)
     (example "put the oj into the tanker")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-GOAL-TEMPL)
     )
    ((LF-PARENT ONT::put)
     (meta-data :origin vn-analysis :entry-date unknown :change-date 20040617 :comments change-lf)
     (example "put down the box")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-RESULT-AFFECTED-TEMPL)
     )
    
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::put (w::back))
   (wordfeats (W::morph (:forms (-vb) :past W::put :ing W::putting)))
   (SENSES
    ((LF-PARENT ONT::RETURN)
     (meta-data :origin lam :entry-date 20050629 :change-date nil )
     (example "put back in the original form")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  ((W::put (W::out))
    (wordfeats (W::morph (:forms (-vb) :past W::put :ing W::putting)))
   (SENSES
    ((LF-PARENT ONT::STOP) 
     (example "put out the fire")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
    ((W::put (w::on))
   (wordfeats (W::morph (:forms (-vb) :past W::put)))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090401 :change-date nil :comments nil)
     (EXAMPLE "he put on a hat")
     (LF-PARENT ONT::put-on)
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

