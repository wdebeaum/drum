;;;;
;;;; w::IN
;;;; 

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::IN ;; alternate plural
   (SENSES
    ((meta-data :origin chf :entry-date 20070814 :change-date 20090520 :comments chf-dialogues :wn ("milliliter%1:23:00"))
     (LF-PARENT ONT::length-unit)
     (LF-FORM W::inch)
     (TEMPL ATTRIBUTE-UNIT-PLURAL-TEMPL)
     (example "it is 5 in high")
     (preference .90) ;;don't compete with other senses of w::in
     (syntax (w::abbrev +))
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::IN
   (SENSES
    ((meta-data :origin chf :entry-date 20070814 :change-date 20090520 :comments chf-dialogues :wn ("milliliter%1:23:00"))
     (LF-PARENT ONT::length-unit)
     (LF-FORM W::inch)
     (TEMPL ATTRIBUTE-UNIT-TEMPL)
     (example "it is 1 in high")
     (preference .90) ;;don't compete with other senses of w::in
     (syntax (w::abbrev +))
     )
    )
   )
))

(define-words :pos W::adv 
 :words (
  ((W::IN W::A W::HURRY)
   (SENSES
    ((LF-PARENT ONT::speedy)
     (templ pred-s-vp-templ)
     )
    )
   )))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::IN W::TROUBLE)
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("in_trouble%5:00:00:troubled:00"))
     (EXAMPLE "he is in trouble")
     (LF-PARENT ONT::STATUS-VAL)
     (TEMPL ATTRIBUTIVE-ONLY-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::IN W::NEED)
   (SENSES
    (;(LF-PARENT ONT::physical-reaction)
     (LF-PARENT ONT::desirous)
     (TEMPL postpositive-ADJ-experiencer-theme-TEMPL (XP (% W::PP (W::Ptype W::of))))
     (example "I am in need of entertainment")
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::IN W::PAIN)
   (SENSES
    ((LF-PARENT ONT::pained-val)
     (TEMPL postpositive-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((W::IN W::touch)
   (SENSES
    ((LF-PARENT ONT::in-touch-val)
     (TEMPL central-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::in W::place)
   (SENSES
    ((EXAMPLE "that's in place [for him]")
     (lf-parent ont::available)
     (TEMPL CENTRAL-ADJ-XP-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     (SYNTAX (W::atype W::predicative-only))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::in W::position)
   (SENSES
    ((EXAMPLE "he is in position")
     (meta-data :origin trips :entry-date 20070503 :change-date nil :comments nil :wn ("ready%3:00:00"))
     (LF-PARENT ONT::AVAILABLE)
     (TEMPL CENTRAL-ADJ-XP-TEMPL)
     (SYNTAX (W::atype W::predicative-only))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((w::in w::progress)
   (SENSES
    ((meta-data :origin windenergy :entry-date 20080521 :change-date nil :comments nil)
     (LF-PARENT ONT::event-time-rel)
     (example "the process is in progress")
     (templ predicative-only-adj-templ)
     )
    )
   )
))

(define-words :pos W::adv :templ CENTRAL-ADJ-TEMPL
 :words (
   ((w::in w::response w::to)
   (SENSES
    ((LF-PARENT ONT::REASON)
     (example "it melts in response to heat")
     (templ  binary-constraint-s-templ)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((W::in W::regard w::to)
   (SENSES
    ((LF-PARENT ONT::associated-information)
     (example "she would like to speak with you in regard to the transportation situation")
     (TEMPL binary-constraint-s-templ)
     (meta-data :origin calo-ontology :entry-date 20060412 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((w::in W::sequence)
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments nil)
     (LF-PARENT ONT::SEQUENCE-POSITION)
     (templ pred-vp-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::in W::any W::case)
   (SENSES
    ((LF-PARENT ONT::TOPIC-SIGNAL)
     (TEMPL DISC-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::in w::my w::humble w::opinion)
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
  ((W::in W::order W::that)
   (SENSES
    (;(LF-PARENT ONT::so-that)
     (LF-PARENT ONT::purpose)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date nil :comments sentential-conjunction-cleanup)     
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::in W::case)
   (SENSES
    ((LF-PARENT ONT::situated-in)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin trips :entry-date nil :change-date 20070609 :comments sentential-conjunction-cleanup)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((w::in w::spite w::of)
    (SENSES    
    ((LF-PARENT ONT::qualification)
     (TEMPL binary-constraint-s-templ)
     (Example "I did it despite the bad weather")
     (meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
     )
    ))
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::in W::fact)
   (SENSES
    ((LF-PARENT ONT::DEGREE-OF-BELIEF)
     (LF-FORM W::IN-FACT)
     (TEMPL PRED-S-VP-TEMPL)
     )
    ((LF-PARENT ONT::modifier)
     (LF-FORM W::IN-FACT)
     (TEMPL PRED-VP-PRE-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((w::in w::order W::TO)
   (SENSES
    ((LF-PARENT ONT::PURPOSE)
     (meta-data :origin calo :entry-date 20040923 :change-date nil :comments caloy2)
     (example "I did it in order to win")
     (TEMPL BINARY-CONSTRAINT-S-VPbase-TEMPL)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::in W::addition W::to)
   (SENSES
    ((LF-PARENT ONT::ADDITIVE)
     (TEMPL binary-constraint-S-templ)
     )
    ((LF-PARENT ONT::ADDITIVE)
     (TEMPL binary-constraint-s-ing-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::in W::place W::of)
   (SENSES
    ((LF-PARENT ONT::CHOICE-OPTION)
     (LF-FORM W::INSTEAD-OF)
     (TEMPL binary-constraint-S-templ)
     )
    ((LF-PARENT ONT::CHOICE-OPTION)
     (LF-FORM W::INSTEAD-OF)
     (TEMPL binary-constraint-s-ing-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ pred-s-post-templ
 :tags (:base500)
 :words (
;; Added by myrosia for Bee corpus
	    (w::in
	     (senses
	      ((lf-parent ont::manner)
	       (meta-data :origin bee :entry-date 20040407 :change-date 20080417 :comments (test-s36 test-s37)) 
	       (example "in general" "in particular" "in short" "in series" "in parallel" "in full" "in earnest" "the room is done in green")
	       ;; a restriction of ADJP with no argument
	       (templ binary-constraint-S-templ (xp (% w::ADJP (w::var ?var) (w::sem ?sem) (w::set-modifier -))))
	       (preference 0.93)
	       )
	      ))
))

(define-words :pos W::ADV
  :tags (:base500)
  :words (
	  ((w::in W::front)
	   (SENSES
	    ((LF-PARENT ONT::front)
	     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL  (xp (% w::pp (w::ptype (? pt w::of)))))
	     (SYNTAX (W::ALLOW-DELETED-COMP +))
	     (EXAMPLE "there is a crater in front of me")
	     (meta-data :origin joust :entry-date 20091026 :change-date nil :comments nil)
	     )
	    )
	   )
   ((w::in W::part)
   (SENSES
    ((LF-PARENT ONT::partial-incomplete)
     (TEMPL pred-s-vp-templ)
     (EXAMPLE "in part, he is happy. he is, in part, Happy")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   ((w::in W::back)
   (SENSES
    ((LF-PARENT ONT::back)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL  (xp (% w::pp (w::ptype (? pt w::of)))))
      (SYNTAX (W::ALLOW-DELETED-COMP +))
     (EXAMPLE "there is a crater in back of me")
     (meta-data :origin joust :entry-date 20091026 :change-date nil :comments nil)
     )
    )
   ) 
))

(define-words :pos W::ADV
 :words (
  ((w::IN W::BETWEEN)
   (SENSES
    ((LF-PARENT ONT::between)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (SYNTAX (W::ALLOW-DELETED-COMP +))
     (meta-data :origin fruitcarts :entry-date 20060302 :change-date nil :comments nil)
     (example "the triangle is in between the bananas")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::IN W::THE W::MEANTIME)
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s10)
     (LF-PARENT ONT::event-time-rel)
     (TEMPL pred-S-TEMPL)
     (example "he left in the meantime")
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
    ((w::in w::behalf w::of)
     (senses
     ((LF-PARENT ONT::BENEFICIARY)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (example "we raised money in behalf of earthquake victims")
     (meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
      )
     )
     )
    ((w::in w::the w::presence w::of)
     (senses
      ((LF-PARENT ONT::near-reln)
       (TEMPL BINARY-CONSTRAINT-S-TEMPL)
       (example "we combine them in the presence of oxygen")
       
       )
     ))
    ((w::in w::vitro)
     (senses ((LF-PARENT ONT::POSITION-RELN)
	      (TEMPL PRED-S-or-np-TEMPL)
	      (SYNTAX (W::ALLOW-DELETED-COMP +)))
	     ))

    ((w::in w::vivo)
     (senses ((LF-PARENT ONT::POSITION-RELN)
	      (TEMPL PRED-S-or-np-TEMPL)
	      (SYNTAX (W::ALLOW-DELETED-COMP +)))
	     ))
	     
    ))

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::IN
   (SENSES
    ((LF-PARENT ONT::situated-in)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (example "in that event" "in the meeting")
     )
    #||((LF-PARENT ONT::time-deadline-rel)
     (TEMPL BINARY-CONSTRAINT-adj-postpos-TEMPL)
     (meta-data :origin cernl :entry-date 20110114 :change-date nil :comments hpi-acn-3)
     (example "the device placed in five minutes")
     (preference .98)
     )
    ;; like within
    ((LF-PARENT ONT::scale-relation)
     (meta-data :origin calo :entry-date 20040423 :change-date nil :comments calo-y1v5)
     (example "is there a computer in that price range")
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (lf-form w::within)
     (preference .92) ;; prefer direction
     )||#
    ;;;;;need specific constraints that we're lacking now
    ((LF-PARENT ONT::TIME-span-rel)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (example "he ran the race in June" "in the middle of the night")
     )
    ((LF-PARENT ONT::event-duration-modifier)  ;; retains ambiguity on whether duration or deadline, to be resolved by reasoning
     (example "he ran the race in 5 minutes" "he will run in five minutes")
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )
    #||((LF-PARENT ONT::TIME-deadline-rel)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (example "he will run in 5 minutes")
     )||#
    ;; takes a phys-obj ont::of
    ((LF-PARENT ont::in-loc) ;ONT::SPATIAL-LOC)
     (example "the dog is in the park")
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    #||;; why is this a spatial-loc?
    ((LF-PARENT ONT::spatial-LOC)
     (TEMPL BINARY-CONSTRAINT-val-STATE-NP-TEMPL)
     (meta-data :origin beetle2 :entry-date 20080506 :change-date nil :comments pilot3)
     (example "terminals are in the same state")
     (preference 0.95) ;; don't choose if other options are available
     )||#	      
    ((LF-PARENT ONT::direction)
     (TEMPL PRED-S-POST-TEMPL)
     (preference 0.98)
     )
    ;; in the air (excluded by ont::spatial-loc)
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::IN
   (SENSES
    ((LF (W::IN))
     (non-hierarchy-lf t))
    )
   )
))

