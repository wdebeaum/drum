;;;;
;;;; W::express
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::express
    (wordfeats (W::morph (:forms (-vb) :nom w::expression)))
    (SENSES
     ((LF-PARENT ONT::say)
      (example "she expressed the idea to him")
      (meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090506 :comments Express)
      ;; this verb doesn't participate in the alternation
      (TEMPL AGENT-FORMAL-AGENT1-OPTIONAL-TEMPL)
      ;;(PREFERENCE 0.98)
      )
          
     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION) 
      (example "the gene expresses the protein")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
      )

     #|
     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION)
      (example "the gene expresses")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
      (templ affected-templ)
      )
     |#
     ))   
   )
)


(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::express
    (wordfeats (W::morph (:forms (-vb))))
    (SENSES

     ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
      (LF-PARENT ONT::send)
      (TEMPL AGENT-AFFECTED-TEMPL) ; like mail,send,forward,transmit
      (example "he expressed the package to him")
      (PREFERENCE 0.98)
      )
))))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
   (W::express
    (wordfeats (W::morph (:forms (-vb) :nom (w::gene w::expression))))
    (SENSES
     
     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION) 
      (example "the gene expresses the protein")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
      )

     #|
     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION)
      (example "the gene expresses")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
      (templ affected-templ)
      )
     |#
     ))   
   )
)

