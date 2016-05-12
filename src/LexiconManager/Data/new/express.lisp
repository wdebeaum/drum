;;;;
;;;; W::express
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::express
    (wordfeats (W::morph (:forms (-vb) :nom w::expression)))
    (SENSES

     ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("send-11.1-1"))
      (LF-PARENT ONT::send)
      (TEMPL agent-affected-recipient-alternation-templ) ; like mail,send,forward,transmit
      (example "he expressed the package to him")
      (PREFERENCE 0.98)
      )
     
     ((LF-PARENT ONT::say)
      (example "she expressed the idea to him")
      (meta-data :origin calo-ontology :entry-date 20051209 :change-date 20090506 :comments Express)
      ;; this verb doesn't participate in the alternation
      (TEMPL agent-theme-to-addressee-optional-templ)
      (PREFERENCE 0.98)
      )
          
     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION) 
      (example "the gene expresses the protein")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL agent-affected-create-templ)
      )

     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION)
      (example "the gene expresses")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
      (templ affected-templ)
      )
     
     ))   
   )
)

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::express
    (wordfeats (W::morph (:forms (-vb) :nom (w::gene w::expression))))
    (SENSES
     
     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION) 
      (example "the gene expresses the protein")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL agent-affected-create-templ)
      )

     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-EXPRESSION)
      (example "the gene expresses")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
      (templ affected-templ)
      )
     
     ))   
   )
)

