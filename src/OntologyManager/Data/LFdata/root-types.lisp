(in-package :om)

(define-type ONT::ROOT
 )

#|
(define-type -
 :parent ONT::ROOT
 )
|#

(define-type ONT::ANY-SEM
 :parent ONT::ROOT
 )

#|
(define-type +
 :parent ONT::ANY-SEM
 )
|#

(define-type ONT::referential-sem
 :wordnet-sense-keys ("entity%1:03:00")
 :comment "The root type for all things that can be referred to: abstract meaning for THIS and IT"
 :parent ONT::ANY-SEM
 :sem ((? rst F::phys-obj F::abstr-obj F::situation F::proposition))
)


(define-type ONT::PHYS-OBJECT
 :wordnet-sense-keys ("object%1:03:00" "physical_object%1:03:00")
 :parent ONT::referential-sem
 :comment "All physical objects: things that have substance"
 :sem (F::Phys-obj (:default (F::intentional -)))
 )

(define-type ONT::situation-root
  :parent ont::referential-sem 
  :comment "root for all events, whether verbal or nominal"
  :sem (F::Situation (F::Intentional -) (F::information F::mental-construct) (F::container -))
  :arguments (;;(:optional ont::arg0)  ;; abstract role for robust processing
	      ;;(:optional ont::arg1)   ;; abstract role for robust processing
;	      (:optional ont::norole)
	      )
  )

(define-type ONT::ELLIPSIS
     :parent ONT::SITUATION-ROOT)

(define-type ont::event-of-change 
     :parent ONT::SITUATION-ROOT
     :comment "Events that involve change or force: should ahve an AGENT or AFFECTED role"
     :arguments ((:optional  ONT::agent ((? cau4 F::situation F::Abstr-obj f::phys-obj)))
		 (:optional  ONT::affected ((? cau3a F::situation F::Abstr-obj f::phys-obj)))
		 (:optional  ONT::result ((? cau2 F::situation F::Abstr-obj f::phys-obj)))
		 (:optional ONT::beneficiary ((? cau1 f::phys-obj))))
     :sem (F::Situation (F::aspect F::dynamic)))


(define-type ont::occurring
  :wordnet-sense-keys ("happen%2:30:00")
     :parent ONT::SITUATION-ROOT
     :comment "event occurrence - e.g., an explosion happened"
     :arguments ((:essential ONT::neutral (f::situation (F::aspect F::dynamic))))
     :sem (F::Situation (F::aspect F::dynamic)))

(define-type ont::event-of-action 
     :parent ONT::event-of-change
     :comment "events that involve :agent (whether intentional or not)"
     :sem (F::Situation (F::cause F::force))
     :arguments ((:essential ONT::agent ((? cau2a F::situation F::Abstr-obj f::phys-obj)))))

#||(define-type ont::event-of-agent-interaction 
     :parent ONT::event-of-action
     :comment "events that involve the interaction between more than one agent (whether intentional or not)"
     :sem (F::Situation)
     :arguments ((:essential ONT::agent1 ((? cau3 F::situation F::Abstr-obj f::phys-obj)))))
||#

(define-type ont::event-of-awareness
     :parent ONT::event-of-change
     :comment "Events involving changing or mental state or awareness"
     :sem (F::Situation) 
     :arguments ((:essential ONT::formal ((? cau4 F::situation F::Abstr-obj)))))


(define-type ont::event-of-undergoing-action
     :parent ONT::event-of-change
     :comment "A small class of events that take an affected but do not allow an AGENT"
     :sem (F::Situation)
     :arguments ((:essential ONT::affected  ((? aff F::Abstr-obj f::phys-obj f::situation)))))

(define-type ont::event-of-causation 
     :parent ONT::event-of-action
     :comment "events involving an AGENT acting on an AFFECTED"
     :sem (F::Situation)
     :arguments ((:essential ONT::affected ((? cau5 F::Abstr-obj f::phys-obj f::situation))))
     )

(define-type ont::event-of-creation
     :parent ONT::event-of-action
     :comment "Events that involve creating some new object (typically the AFFECTED-RESULT)"
     :sem (F::Situation)
     :arguments ((:optional ONT::result ((? neu F::situation F::Abstr-obj f::phys-obj)))
		 (:optional ONT::affected-result ((? neu2 F::situation F::Abstr-obj f::phys-obj))))
     )

(define-type ont::event-of-state 
     :parent ONT::SITUATION-ROOT
     :sem (F::Situation (F::aspect f::static))
     :comment "Events describing a state of affairs holding"
     :arguments ((:essential ONT::neutral)
		 (:essential ONT::formal ((? neu F::situation F::Abstr-obj)))
		 (:optional ont::norole)
		 ))

(define-type ONT::event-of-experience
   :parent ONT::event-of-state
   :comment "A stative event involving a sentient being in a mental state"
   :arguments ((:ESSENTIAL ONT::experiencer (F::Phys-obj (F::intentional +))))
   )

(define-type ONT::EVENT-TYPE
 :parent ONT::SITUATION-ROOT
 :sem (F::Situation (F::intentional -))
 )

(define-type ONT::ABSTRACT-OBJECT
 :wordnet-sense-keys ("psychological_feature%1:03:00" "abstraction%1:03:00" "abstract_entity%1:03:00")
 :parent ont::referential-sem
 :comment "abstract objects, mental constructions, with no physical realization"
 :sem (F::abstr-obj)
 )

(define-type ont::abstract-object-nontemporal
    :parent ont::abstract-object
    :comment "formal constructions with no temporal existence: e.g., facts, types, ..."
    :sem (F::abstr-obj)
   )


(define-type ont::unit
    :parent ont::abstract-object
    :comment "names of units in various scales/domains"
    :sem (F::abstr-obj)
   )

(define-type ont::mental-construction
    :parent ont::abstract-object-nontemporal
    :comment "constructions of the mind: plans, goals, beliefs, ..."
    :sem (F::abstr-obj (f::information f::mental-construct))
   )

(define-type ont::domain-property
    :parent ont::abstract-object
    :comment "these are modifiers that characterize an object/event with respect to a scale/domain (in ONT::DOMAIN)"
    :sem (F::abstr-obj )
    :arguments ((:REQUIRED ONT::FIGURE)
		(:ESSENTIAL ONT::GROUND)
		(:ESSENTIAL ONT::SCALE))
   )

(define-type ONT::Part
    :wordnet-sense-keys ("part%1:24:00" "part%1:09:00" "portion%1:24:00" "component_part%1:24:00" "component%1:24:00" "constituent%1:24:00" "part%1:17:00" "piece%1:17:00")
    :comment "Part is actually a conceptualization of things that fill the part-of role"
    :parent ont::referential-sem
    :arguments ((:OPTIONAL ONT::FIGURE )
		)
 )

(define-type ont::remaining-part
    :parent ont::part)
