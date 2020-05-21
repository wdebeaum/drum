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
 :wordnet-sense-keys ("entity%1:03:00" "one%1:09:00")
 :comment "The root type for all things that can be referred to: abstract meaning for THIS and IT"
 :parent ONT::ANY-SEM
 :sem ((? rst F::phys-obj F::abstr-obj F::situation))
)


(define-type ONT::PHYS-OBJECT
 :wordnet-sense-keys ("object%1:03:00" "physical_object%1:03:00")
 :parent ONT::referential-sem
 :comment "All physical objects: things that have substance"
 :sem (F::Phys-obj (F::tangible +))
 )

(define-type ONT::situation-root
  :parent ont::referential-sem 
  :comment "root for all events, whether verbal or nominal"
  :sem (F::Situation (F::Intentional -) ;;(F::information F::mental-construct)
		     (F::container -) (f::tangible +))
  :arguments (;;(:optional ont::arg0)  ;; abstract role for robust processing
	      ;;(:optional ont::arg1)   ;; abstract role for robust processing
;	      (:optional ont::norole)
	      )
  )

(define-type ONT::ELLIPSIS
     :parent ONT::SITUATION-ROOT)

(define-type ont::event-of-change 
     :parent ONT::SITUATION-ROOT
     :comment "Events that involve change or force: should have an AGENT or AFFECTED role"
     :arguments ((:optional  ONT::agent ((? cau4 F::situation F::Abstr-obj f::phys-obj)))
		 (:optional  ONT::affected ((? cau3a F::situation F::abstr-obj f::phys-obj) (F::tangible +))) 
		 (:optional  ONT::result (F::Abstr-obj (:default (F::tangible -) (F::type (? !t ont::position-reln)))))
		 ;;(:optional ONT::beneficiary ((? cau1 f::phys-obj))))
		 )
     :sem (F::Situation (F::aspect F::dynamic))
     )


(define-type ont::occurring
     :wordnet-sense-keys ("come%2:30:01" "come%2:42:13" "come_about%2:30:00" "fall_out%2:30:00" "go%2:42:03" "go%2:42:12" "go_on%2:30:00" "hap%2:30:00" "happen%2:30:00" "happening%1:11:00" "natural_event%1:11:00" "occur%2:30:00" "occurrence%1:11:00" "occurrent%1:11:00" "pass%2:30:00" "pass_off%2:30:00" "play%2:42:00" "set_in%2:30:00" "take_place%2:30:00")
     :parent ONT::SITUATION-ROOT
     :comment "event occurrence - e.g., an explosion happened"
     :arguments ((:essential ONT::neutral ((? xx f::situation F::time)))
		 (:optional ONT::affected (f::phys-obj)))
     :sem (F::Situation (F::aspect F::dynamic)))


(define-type ont::time-elapse
     :wordnet-sense-keys ("pass%2:38:03")
     :parent ONT::occurring
     :comment "time occurrence - e.g., time passed by, the winter went on, ..."
     :arguments ((:essential ONT::neutral (F::time)))
     :sem (F::Situation (F::aspect F::dynamic)))

(define-type ont::event-of-action 
     :parent ONT::event-of-change
     :comment "events that involve :agent (whether intentional or not)"
     :definitions ((ont::cause-effect :agent ?agent
				     :formal (ont::event-of-change)))
     :sem (F::Situation (F::cause F::force))
     :arguments ((:essential ONT::agent ((? cau2a F::situation F::Abstr-obj f::phys-obj)))
		 (:optional  ONT::affected ((? cau3a F::situation F::abstr-obj f::phys-obj) (F::tangible +)))
		 ))

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
     :arguments ((:optional  ONT::affected ((? cau3a F::situation F::abstr-obj f::phys-obj) (F::tangible +))) 
		 (:essential ONT::formal ((? cau4 F::situation F::Abstr-obj)))))


(define-type ont::event-of-undergoing-action
     :parent ONT::event-of-change
     :comment "A small class of events that take an affected but do not allow an AGENT construction (though might be caused as in he died from the plague"
     :sem (F::Situation)
     :arguments ((:essential ONT::affected  ((? aff F::abstr-obj f::phys-obj f::situation) (F::tangible +)))
		 (:optional ont::agent )))

(define-type ont::event-of-causation 
     :parent ONT::event-of-action
     :comment "events involving an AGENT acting on an AFFECTED"
     :definitions ((ont::cause-effect :agent ?agent
				 :formal (ont::event-of-change :affected ?affected)))
     :sem (F::Situation)
     :arguments ((:essential ONT::affected ((? cau5 F::abstr-obj f::phys-obj f::situation) (F::tangible +)))
		 (:essential ONT::agent ((? cau6 F::abstr-obj f::phys-obj f::situation) (F::tangible +)))
		 )
     )

(define-type ont::event-of-creation
    :parent ONT::event-of-action
    :definitions ((cause-effect :agent ?agent
				:formal (ont::become :affected ?affected
						     :formal (ONT::EXISTS :neutral ?affected))))
     :comment "Events that involve creating some new object (typically the AFFECTED-RESULT)"
     :sem (F::Situation)
     :arguments ((:optional ONT::result ((? neu F::situation F::Abstr-obj f::phys-obj)))
		 (:optional ONT::affected-result ((? neu2 F::situation F::abstr-obj f::phys-obj)
						  (F::tangible +))))
     )

(define-type ont::event-of-state 
     :parent ONT::SITUATION-ROOT
     :sem (F::Situation (F::aspect f::static))
     :comment "Events describing a state of affairs holding"
     :definitions ((ont::imply (ont::DURING :figure ?ANY1 :ground ?time)
			       (ont::event-of-state :event-id ?event-id :time ?ANY1)))
     :arguments ((:essential ONT::neutral)
		 (:essential ONT::formal ((? neu F::situation F::Abstr-obj)))
		 (:optional ont::norole)
		 ))

(define-type ONT::event-of-experience
    :wordnet-sense-keys ("basic_cognitive_process%1:09:00")
   :parent ONT::event-of-state
   :comment "A stative event involving a sentient being in a mental state"
   :arguments ((:ESSENTIAL ONT::experiencer (F::Phys-obj (F::intentional +))))
   )

(define-type ONT::EVENT-TYPE
    :comment "classification of situated events based on social or other criteria, and typically realized by nominals, i.e.,
        they are not nominalization of verbal events"
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

(define-type ont::tangible-abstract-object
    :parent ont::abstract-object
    :sem (F::abstr-obj (F::tangible +))
    :comment "abstract notions that act like things. They can be created, transfered, e.g., mental objects"
    :sem (F::abstr-obj)
   )


(define-type ont::unit
    :parent ont::abstract-object
    :comment "names of units in various scales/domains"
    :sem (F::abstr-obj (F::scale ONT::DOMAIN))
    :wordnet-sense-keys ("definite_quantity%1:23:00")
   )

(define-type ont::mental-construction
    :parent ont::tangible-abstract-object
    :comment "constructions of the mind: plans, goals, beliefs, ..."
    :sem (F::abstr-obj (f::information f::mental-construct))
   )

;;; This is a catch-all for things that are relations between multiple
;;; objects: identity, distance, whatever. Will need better sorting at
;;; a future data
(define-type ONT::relation
    :comment "All types that denote relations. This is the root of terms under abstract objects that take the ONT::F specifier"
    :wordnet-sense-keys ("relation%1:03:00" "amount%2:42:03")
    :parent ONT::abstract-object
    :arguments ((:REQUIRED ONT::FIGURE)
		(:REQUIRED ONT::GROUND)
		(:optional ont::neutral)
		(:optional ont::neutral1)  ;; some relations based on verbs use this
		(:optional ont::norole)
		(:OPTIONAL ONT::COMPAR))
    )
 
 
	     
(define-type ont::domain-property
    :parent ont::RELATION
    :comment "these are modifiers that characterize an object/event with respect to a scale/domain (in ONT::DOMAIN)"
    :sem (F::abstr-obj (F::Scale ont::domain))
    :arguments ((:REQUIRED ONT::FIGURE)
		(:ESSENTIAL ONT::GROUND)
		(:ESSENTIAL ONT::SCALE ((? scale F::abstr-obj) (F::type ONT::DOMAIN)))
		(:OPTIONAL ONT::NOROLE)
		(:OPTIONAL ONT::COMPAR ((? ty f::abstr-obj f::phys-obj) (f::type (? !xx ONT::NUMBER))))
		(:OPTIONAL ONT::REFSET)
		)
   )

(define-type ONT::Part
    :wordnet-sense-keys ("part%1:24:00" "part%1:09:00" "part%1:17:00" "part%1:21:00")
    :comment "Part is actually a conceptualization of things that fill the part-of role"
    :parent ont::referential-sem
    :arguments ((:OPTIONAL ONT::FIGURE )
		)
 )

(define-type ont::remaining-part
    :parent ont::part)

(define-type ont::important-part
  :parent ont::part)
