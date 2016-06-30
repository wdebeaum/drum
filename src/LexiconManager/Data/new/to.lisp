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

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::TO
   (SENSES
    ((LF-PARENT ONT::PURPOSE)
     (example "aspirin is used to treat headaches")
     (meta-data :origin medadvisor :entry-date 20011126 :change-date nil :comments nil)
     (TEMPL BINARY-CONSTRAINT-S-VPbase-TEMPL)
     )
    ;;  this is the goal sense, prefers events of change
    ((LF-PARENT ONT::TO-LOC)
     (example "go to the building" "the relocation to the building")
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )

    ((LF-PARENT ONT::RESULTING-OBJECT)
     (example "change to a toad")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )

    ((LF-PARENT ONT::RESULTING-STATE)
     (example "change to a waking state")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
   #|| ;; a generalized sense of to
    ((LF-PARENT ONT::TO)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (meta-data :origin navigation :entry-date 20080902 :change-date nil :comments nil)
     (example "I see the building to my right")
     (preference .98) ;; prefer to-loc if applicable
     )||#
    ((LF-PARENT ONT::orients-to)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "I see the building to my right")
     (PREFERENCE 0.9) ;; prefer vp attachment
     )
    ;; 04 2010 no longer needed?rate
;    ((LF-PARENT ONT::TO-LOC-DEGREES)
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
;     (example "pan camera to 45 degrees")
;     (meta-data :origin coordops :entry-date 20070516 :change-date nil :comments nil)
;     )
    ((LF-PARENT ONT::event-time-rel)
     (example "the meeting should go to five pm")
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     (PREFERENCE 0.97)
     )
    ;; need a sense of to that attaches to non-trajectory NPs like plane, train
    ((LF-PARENT ONT::destination-LOC)
     (meta-data :origin ralf :entry-date 20040803 :change-date nil :comments nil)
     (TEMPL BINARY-CONSTRAINT-NP-TEMPL)
     (example "the plane to rochester")
     (PREFERENCE 0.97)
     )
    )
   )
))

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

