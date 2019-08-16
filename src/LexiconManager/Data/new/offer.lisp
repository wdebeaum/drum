;;;;
;;;; w::offer
;;;;
#||
(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (w::offer
   (SENSES
    ((meta-data :origin plow :entry-date 20051004 :change-date nil :wn ("offer%1:10:01") :comments nil)
     (LF-PARENT ont::offer)
     (templ other-reln-affected-templ  (xp (% W::pp (W::ptype (? pt W::for w::on)))))
     (example "make an offer")
     )
    )
   )
))
||#

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
(W::offer
   (wordfeats (W::morph (:forms (-vb) :past W::offered :ing offering :nom offer)))
   (SENSES
    ((lf-parent ont::offer)
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-TEMPL)
     (example "offer him a job")
     ;; restructured this to allow non-movable things to be given
     (meta-data :origin calo :entry-date unknown :change-date 20090513 :comments calo-y1variants)
    )
    ((LF-PARENT ONT::offer)
     (example "the bid offers incentive (to the group)")
     (TEMPL agent-affected-goal-optional-TEMPL)
     (meta-data :origin ptb :entry-date 20100604 :change-date 20090501 :comments nil)
     )

    (
     (lf-parent ont::offer)
     (example "he offered to go")
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL))
    )
   )
))

