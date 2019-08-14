;;;;
;;;; W::ANSWER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::ANSWER
   (SENSES
     ((meta-data :origin trips :entry-date 20060803 :change-date 20090506 :comments nil :wn ("answer%1:10:01"))
      (LF-PARENT ONT::answer)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::ANSWER
   (SENSES
    ((LF-PARENT ONT::ANSWER)
     (example "answer the question")
     )
#|
    ((LF-PARENT ONT::SAY)
     (example "if it says rain then answer yes")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% w::utt)))
     (meta-data :origin task-learning :entry-date 20051109 :change-date nil :comments nil)
     )
|#
    )
   )
))

