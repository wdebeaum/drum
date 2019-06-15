;;;;
;;;; W::clear
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::clear
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("weather-57") :wn ("clear%2:43:00"))
     (LF-PARENT ont::atmospheric-event)
     (TEMPL expletive-templ) ; like rain
     (PREFERENCE 0.96)
     )
    
    ((LF-PARENT ONT::empty)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTEDR-AFFECTED-XP-PP-OF-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::of))))
     (example "clear the airfield of debris")
     )
    #|
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090529 :comments nil :vn ("clear-10.3-1"))
     (LF-PARENT ONT::cause-come-from)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-source-TEMPL (xp (% W::PP (W::ptype (? ptp W::from W::off)))))
     ;;;; swier --
     (example "clear the dust from the monitor")
     )
    |#
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  ((W::clear (W::out))
   (SENSES
    ((LF-PARENT ONT::empty)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "clear out the airfield")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::clear
   (wordfeats (W::morph (:forms (-vb) :nom w::clearance)))
   (SENSES
    (;(LF-PARENT ONT::allow)
     (LF-PARENT ONT::approve-authorize)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "clear the plan")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::clear
    (SENSES
    ((meta-data :origin lou :entry-date 20040311 :change-date 20090915 :wn ("clear%5:00:00:unobstructed:00") :comments lou-sent-entry)
     (LF-PARENT ONT::unobstructed)
     (example "the lane is clear")
     )
    ((meta-data :origin plow :entry-date 20060712  :change-date 20090819 :wn ("clear%3:00:02" "clear%3:00:03") :comments pq)
     (example "clear skies are predicated for tomorrow")
      (LF-PARENT ONT::CLEAR-WEATHER)
     )
    )
   )
))

(define-words :pos W::adj
 :tags (:base500)
 :words (
  (w::clear
  (senses
   ((LF-PARENT ONT::clear)
    (TEMPL central-adj-templ)
    (sem (f::gradability +) (f::intensity ont::med) (f::orientation ont::more))
    (meta-data :origin cardiac :entry-date 20090129 :change-date 20090804 :comments nil)
    (SYNTAX (W::morph (:forms (-ly))))
    )
   )
)
))

