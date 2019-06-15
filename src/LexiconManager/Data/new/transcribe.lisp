;;;;
;;;; W::transcribe
;;;;

(define-words :pos W::v 
 :words (
  (W::transcribe
   (wordfeats (W::morph (:forms (-vb) :nom w::transcription)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("transcribe-25.4") :wn ("transcribe%2:32:00" "transcribe%2:32:01" "transcribe%2:32:02" "transcribe%2:36:00"))
     (LF-PARENT ONT::record)
     (templ agent-neutral-xp-templ)
 ; like tape,record
     )

     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-TRANSCRIPTION) 
      (example "the gene transcribes the mRNA")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL AGENT-AFFECTEDR-XP-TEMPL)
      )

     ((meta-data :origin BOB :entry-date 20141212 :change-date nil :comments nil)
      (LF-PARENT ONT::GENE-TRANSCRIPTION)
      (example "the gene transcribes")
      (SEM (F::Cause F::Agentive) (F::Aspect F::bounded))
      (templ affected-templ)
      )
    )
   )
))

