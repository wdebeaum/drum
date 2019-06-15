;;;;
;;;; W::LIMIT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::LIMIT
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("limit%1:28:00"))
     (LF-PARENT ONT::constraint)
     (example "to the limit of his ability")
     )
    ((LF-PARENT ONT::edge)
     (TEMPL OTHER-RELN-TEMPL)
     (EXAMPLE "the outer limits of space")
     (meta-data :origin step :entry-date 20080626 :change-date nil :wn ("boundary%1:25:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::limit
    (wordfeats (W::morph (:forms (-vb) :past W::limited :ing W::limiting :nom w::limitation)))
   (SENSES
    ((meta-data :origin chf :entry-date 20070817 :change-date nil :comments chf-proposal)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     (example "are you limiting your fluid intake")
     )
    )
   )
))

