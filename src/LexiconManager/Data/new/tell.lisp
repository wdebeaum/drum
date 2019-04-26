;;;;
;;;; W::tell
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::tell
   (wordfeats (W::morph (:forms (-vb) :past W::told)))
   (SENSES
    (;;(LF-PARENT ONT::inform)
     (lf-parent ont::tell) ;; 20120524 GUM change new parent
     (example "tell the driver the plan")
     (TEMPL AGENT-ADDRESSEE-neutral-OPTIONAL-TEMPL (xp (% w::NP))
     ))
    ((lf-parent ont::tell) 
     (example "tell the plan to the driver")
     ;(TEMPL AGENT-THEME-TO-ADDRESSEE-optional-TEMPL)
     (TEMPL AGENT-neutral-TO-ADDRESSEE-optional-TEMPL)
     )
    #||  ;;  Now handled compositionally
    (;;(LF-PARENT ONT::inform)
     (lf-parent ont::tell) ;; 20120524 GUM change new parent
     (example "tell the driver about the plan")
     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL)
    )||#
    ((EXAMPLE "Tell the driver that the trucks are ready")
     ;;(LF-PARENT ONT::inform)
     (lf-parent ont::tell) ;; 20120524 GUM change new parent
     (TEMPL AGENT-ADDRESSEE-THEME-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ;;;; Myrosia 10/23/03 made the to complement required to avoid needless ambiguities
    ((LF-PARENT ONT::command)
     (example "tell him to do it")
     (TEMPL AGENT-addressee-theme-OBJCONTROL-REQ-TEMPL)
     )
    ((LF-PARENT ONT::tell)
     (example "a zero voltage tells you that the terminals are connected")
     (meta-data :origin beetle :entry-date 20090512 :change-date 20090609 :comments beetle-pilots)
     (templ agent-affected-theme-templ)
     (preference 0.98)
     )
    )
   )
))

