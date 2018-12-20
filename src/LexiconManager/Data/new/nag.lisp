;;;;
;;;; W::nag
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::nag
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090422 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::talk)
     (LF-PARENT  ONT::complain)
     (example "he nagged all day")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-optional-TEMPL)
     (preference .98)
     )
    #|(;;(LF-PARENT ONT::TALK)
     (LF-PARENT  ONT::complain)
     (example "he nagged him about it")
     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL)
      )|#
    )
   )
))

