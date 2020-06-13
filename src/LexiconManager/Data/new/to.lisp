;;;;
;;;; W::to
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::to w::tell w::you w::the w::truth)
   (SENSES
    ((LF-PARENT ONT::TOPIC-SIGNAL)
     (meta-data :origin asma :entry-date 20110921 :change-date nil :comments nil)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((w::to w::this w::end)
   (SENSES
   ((LF-PARENT ont::THEREFORE))
   ))
))

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :words (
  ((w::to w::date)
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::TIME-REL-so-far)
     (example "add the best answer to date to the list of choices")
     (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::TO-DATE)))
     )
    )
   )
  ))

;; senses that are not RESULTs

(define-words :pos W::ADV
  :words (
	  (W::TO
					;(wordfeats (w::result-only +))
	   (SENSES
	    #|
	    ((LF-PARENT ONT::orients-to)
	     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	     (example "I see the building to my right")
	     (preference 0.97)
	     )
	    |#
	    ((LF-PARENT ONT::until)
	     (example "the meeting should go to five pm")
	     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
	     (PREFERENCE 0.97)
	     )
	    ;; need a sense of to that attaches to non-trajectory NPs like plane, train
	    ((LF-PARENT ONT::destination-LOC)
	     (meta-data :origin ralf :entry-date 20040803 :change-date nil :comments nil)
	     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
	     (example "the plane to rochester")
	     ;(PREFERENCE 0.97))
	     (PREFERENCE 0.98)) ; make it higher than ATTRIBUTED-TO
	    
	   
	   ((LF-PARENT ONT::PURPOSE)
	    (example "aspirin is used to treat headaches")
	    (meta-data :origin medadvisor :entry-date 20011126 :change-date nil :comments nil)
	    (TEMPL BINARY-CONSTRAINT-S-VPbase-TEMPL)
	    )

           ((LF-PARENT ONT::ATTRIBUTED-TO)
            (TEMPL binary-constraint-S-templ)
            (EXAMPLE "he seems honest to me")
	    (PREFERENCE 0.97)
           ) 

	   ))
  ))

(define-words :pos W::ADV
 :words (
  (W::TO
   (wordfeats (w::result-only +))
   (SENSES
    
    ;;  this is the goal sense, prefers events of change
    ((LF-PARENT ONT::TO-LOC)
     (example "go to the building" "the relocation to the building")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )

    ((LF-PARENT ONT::RESULTING-OBJECT)
     (example "change to a toad")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )

    ((LF-PARENT ONT::RESULTING-STATE)
     (preference .985)
     (example "change to a waking state")
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     )

    ))))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::TO
   (SENSES
    ((LF (W::TO))
     (non-hierarchy-lf t)
     (preference .98))
    )
   )
))

(define-words :pos W::INFINITIVAL-TO :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::TO
   (SENSES
    ((LF ONT::INF-TO)
     (non-hierarchy-lf t))
    )
   )
))

#|
(define-words :pos W::ADJ
 :words (
  ((W::to w::the w::left)
   (SENSES
    ((LF-PARENT ONT::left)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? p W::of)))))
     )
    )
   )
))
|#

#|
(define-words :pos W::ADJ
 :words (
  ((W::to w::the w::right)
   (SENSES
    ((LF-PARENT ONT::right)
     (TEMPL ADJ-CO-THEME-TEMPL (XP (% W::PP (W::PTYPE (? p W::of)))))
     )
    )
   )
))|#
