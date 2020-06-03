;;;;
;;;; w::as
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  ((w::as w::of)
   (SENSES
    ((LF-PARENT ONT::since-until)
     (example "As of January, we will start feeding the cat" "As of January, nobody has fed the cat.")
     (TEMPL binary-constraint-S-or-NP-TEMPL)
     (meta-data :origin cernl :entry-date 20110105 :change-date nil :comments hpi-acn-4)
     )
    )
   )
))


(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   ((w::as w::in)
   (SENSES
    ((meta-data :origin step :entry-date 20080623 :change-date nil :comments nil :wn ("like%3:00:00"))
     (EXAMPLE "free as in freedom")
     (LF-PARENT ONT::exemplifies)
     (templ binary-constraint-adj-templ  (xp (% w::pp (w::PTYPE w::in))))
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((w::as w::to)
   (SENSES
    ((LF-PARENT ONT::associated-information)
     (example "evidence as to the origins of man")
     (TEMPL binary-constraint-np-templ)
     (meta-data :origin step :entry-date 20080623 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::AS W::FOR)
   (SENSES
    ((LF-PARENT ONT::TOPIC)
     (LF-FORM W::TOPIC)
     (TEMPL TOPIC-TEMPL)
     )
    ((LF-PARENT ONT::AS-IF-FOR)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    ))
  ))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::AS W::IF W::FOR)
   (SENSES
    ((LF-PARENT ONT::AS-IF-FOR)
     (TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     )
    ))
  ))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((W::as W::much w::as)
   (SENSES
    ((LF-PARENT ONT::QMODIFIER)
     (LF-FORM W::max)
     (TEMPL NUMBER-OPERATOR-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
   ((W::as W::long w::as)
    (wordfeats (W::ALLOW-DELETED-COMP +))
    (SENSES
     ((LF-PARENT ONT::pos-condition)
      (TEMPL binary-constraint-s-or-np-TEMPL)
      )
     )
    )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
 ((W::as W::far W::as w::I w::can w::tell)
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
  ((W::as W::far W::as w::I w::know)
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
 :tags (:base500)
 :words (
 ;; these senses can't be distinguished well so they should be generalized
  (W::AS
   (SENSES
    ((LF-PARENT ONT::REASON)
     (TEMPL binary-constraint-s-decl-templ)
     )
    ((LF-PARENT ONT::when-while)
     (TEMPL binary-constraint-S-decl-TEMPL)
     (meta-data :origin mobius :entry-date 20080702 :change-date nil :comments nil)
     (example "as this happens the valve closes")
     )
    ((LF-PARENT ONT::manner)
     (example "use/try/select/find/ this as an example")
     (meta-data :origin plow :entry-date 20060306 :change-date nil :comments pq0384)
     (TEMPL BINARY-CONSTRAINT-S-or-NP-TEMPL)
     )

					;    ((LF-PARENT ONT::predicate)
;     (example "take the medication as prescribed")
;     (meta-data :origin chf :entry-date 20070827 :change-date nil :comments chf-dialogues)
;     (TEMPL binary-constraint-s-or-np-general-templ)
;     )
;    ((LF-PARENT ONT::predicate)
;     (example "take the medication as the doctor prescribed")
;     (meta-data :origin chf :entry-date 20070827 :change-date nil :comments chf-dialogues)
;     (TEMPL binary-constraint-s-or-np-decl-templ)
;     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::AS W::IF)
   (SENSES
    ((LF-PARENT ONT::MANNER)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date nil :comments sentential-conjunction-cleanup)     
     (TEMPL binary-constraint-s-decl-templ)
     )
    ((LF-PARENT ONT::MANNER)
     (TEMPL binary-constraint-s-pred-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::AS W::THOUGH)
   (SENSES
    ((LF-PARENT ONT::MANNER)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date nil :comments sentential-conjunction-cleanup)     
     (TEMPL binary-constraint-s-decl-templ)
     )
     ((LF-PARENT ONT::MANNER)
     (TEMPL binary-constraint-s-pred-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::AS W::LONG W::AS)
   (SENSES
    ((LF-PARENT ONT::POS-CONDITION)
     (TEMPL binary-constraint-s-decl-templ)
     (meta-data :origin beetle2 :entry-date 20070609 :change-date 20080623 :comments sentential-conjunction-cleanup)     
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::as w::well)
   ;;   (wordfeats (W::ALLOW-DELETED-COMP +))
   (SENSES
    ((LF-PARENT ONT::additive)
     (LF-FORM W::as-well)
     ;; MD - beetle fix
     ;; removed wordfeats changed binary-constraint to another templ b/c we don't have the binary possibility AS-WELL + complement
     ;;     (TEMPL binary-constraint-S-templ)
     (TEMPL PRED-S-VP-IMPLICIT-TEMPL)
     )
    ;; MD - beetle fix
    ;; added a new definition to support as-well in "is it in the closed path as well?". This is a temporary fix, because really we need to allow more adverbials after "be" in this YNQ situation.
    ;; made it a low preference so that it does not compete with the full definition above
    ((LF-PARENT ONT::additive)
     (LF-FORM w::as-well)
     (TEMPL DISC-POST-UTT-TEMPL)
     (preference 0.93)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::as w::well W::as)
   (SENSES
    ((LF-PARENT ONT::additive)
     (LF-FORM W::as-well-as)
     (example "this as well as that")
     (TEMPL binary-constraint-S-or-NP-templ)
     )
    ((LF-PARENT ONT::additive)
     (LF-FORM W::as-well-as)
     (TEMPL binary-constraint-s-ing-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::as w::opposed w::to)
   (SENSES
    ((EXAMPLE "he wants the truck as opposed to the car")
     (LF-PARENT ONT::CHOICE-OPTION)
     (TEMPL binary-constraint-S-templ)
     (meta-data :origin caloy2 :entry-date 20050509 :change-date nil :comments projector-purchasing)
     )
    ((EXAMPLE "running as opposed to walking")
     (LF-PARENT ONT::CHOICE-OPTION)
     (TEMPL binary-constraint-S-ing-templ)
     (meta-data :origin caloy2 :entry-date 20050509 :change-date nil :comments projector-purchasing)
     )
    ))
))

(define-words :pos W::ADV
 :words (
  ((W::AS W::SOON W::AS)
   (SENSES
    ((LF-PARENT ONT::immediate)
     ;(TEMPL BINARY-CONSTRAINT-S-OR-NP-TEMPL)
     (TEMPL BINARY-CONSTRAINT-S-DECL-TEMPL)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::AS W::SOON W::AS W::POSSIBLE)
   (SENSES
    ((LF-PARENT ONT::immediate)
     (TEMPL PRED-S-POST-templ)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
  ((W::AS W::QUICK W::AS W::POSSIBLE)
   (SENSES
    ((LF-PARENT ONT::immediate)
     (TEMPL PRED-S-POST-templ)
     )
    )
   )
))

(define-words :pos W::ADV
 :words (
   ((w::AS W::USUAL)
   (SENSES
;    ((meta-data :origin calo :entry-date 20040408 :change-date nil :comments calo-y1v4)
;     (LF-PARENT ONT::FREQUENCY)
;     (TEMPL PRED-S-VP-TEMPL)
;     )
    ((meta-data :origin cardiac :entry-date 20090324 :change-date nil :comments nil)
     (LF-PARENT ONT::FREQUENCY)
     (TEMPL DISC-TEMPL)
     )
    )
   )
   ((w::AS W::expected)
    (SENSES
     (;(LF-PARENT ONT::PREDICATE)
      (LF-PARENT ONT::QUALIFICATION)
      (TEMPL DISC-TEMPL)
      )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::AS
   (SENSES
    ((LF (W::AS))
     (non-hierarchy-lf t))
    )
   )
))

