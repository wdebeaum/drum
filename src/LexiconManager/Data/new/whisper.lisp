;;;;
;;;; W::whisper
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::whisper
   (wordfeats (W::morph (:forms (-vb) :past W::whispered :ing W::whispering :nom w::whisper)))
   (SENSES
    ((LF-PARENT ONT::manner-say)
     (example "He whispered that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-finite)))))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("whisper%2:32:00"))
     )
    ((LF-PARENT ONT::manner-say)
     (example "he whispered go over there")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% w::utt)))
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("whisper%2:32:00"))
     )
    ((LF-PARENT  ONT::manner-say)
     (example "whisper it to him")
     (TEMPL AGENT-THEME-to-addressee-optional-TEMPL)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("whisper%2:32:00"))
     )
    (;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::manner-say)
     (example "whisper")
     (TEMPL AGENT-TEMPL)
     (preference .98)
     (meta-data :origin "verbnet-2.0" :entry-date 20060512 :change-date nil :comments nil :vn ("manner_speaking-37.3") :wn ("whisper%2:32:00"))
     )
    )
   )
))

