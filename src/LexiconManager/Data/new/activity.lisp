;;;;
;;;; W::ACTIVITY
;;;;

#|
(define-words :pos W::n 
 :words (
  (W::ACTIVITY
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20031024 :wn ("activity%1:04:00"))
      (LF-PARENT ONT::ACTING)
;      (templ other-reln-cause-templ)
      (templ reln-agent-affected-templ)
     )
    )
   )
))


  (reln-agent-affected-templ
   (SYNTAX(W::sort W::other-reln)  (W::CASE (? cas W::sub W::obj)) (w::allow-deleted-comp +) (W::MASS W::COUNT))
   (ARGUMENTS
    (SUBCAT  (:parameter xp1 (:default (% W::PP (W::ptype (? w w::of w::by))))) ONT::agent optional)
    (SUBCAT2 (:parameter xp2 (:default (% W::PP (W::ptype w::on)))) ONT::affected optional)
    ))

|#


(define-words :pos W::v 
 :words (
   (W::activity-verb-doesnotexist
     (wordfeats (W::morph (:forms (-vb) :nom W::activity :nomsubjpreps (w::of w::by) :nomobjpreps (w::on))))
     (SENSES
      ((LF-PARENT ONT::ACTIVITY-EVENT)  
       (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	)
      ((LF-PARENT ONT::ACTIVITY-EVENT)  
        (TEMPL agent-templ)
	)

      ))))

