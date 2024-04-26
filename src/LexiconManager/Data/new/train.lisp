;;;;
;;;; W::TRAIN
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::TRAIN
;;   (wordfeats (W::MORPH (:forms (-S-3P -load))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("train%1:06:00"))
     (LF-PARENT ONT::land-vehicle)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::TRAIN W::STATION)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::train W::stations))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("train_station%1:06:00"))
     (LF-PARENT ONT::transportation-facility)
     (LF-FORM W::TRAIN-STATION)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  (w::train
   (senses
    ((EXAMPLE "Train him to do it")
     (LF-PARENT ONT::teach-train) ;; like teach
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-OPTIONAL-TEMPL  (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin task-learning :entry-date 20050825 :change-date nil :comments nil)
     )
    ;; need a non-intentional do
    ((LF-PARENT ONT::teach-train)
     (example "train the classifier on the email")
     (SEM (F::Cause F::agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (meta-data :origin plow :entry-date 20090129 :change-date nil :comments nil)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-A-TEMPL  (xp (% W::PP (W::ptype W::on))))
     )
    )
   )
))

