;;;;
;;;; W::tell
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::tell
   (wordfeats (W::morph (:forms (-vb) :past W::told)))
   (SENSES
    (;;(LF-PARENT ONT::inform)
     (lf-parent ont::tell) ;; 20120524 GUM change new parent
     (example "tell the driver the plan")
     (TEMPL AGENT-AGENT1-NEUTRAL-2-XP1-3-XP-OPTIONAL-TEMPL (xp (% w::NP))
     ))
    ((lf-parent ont::tell) 
     (example "tell the plan to the driver")
     ;(TEMPL AGENT-THEME-TO-ADDRESSEE-optional-TEMPL)
     (TEMPL AGENT-NEUTRAL-AGENT1-OPTIONAL-TEMPL)
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
     (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((EXAMPLE "Tell the driver about the truck")
     ;;(LF-PARENT ONT::inform)
     (lf-parent ont::tell) ;; 20120524 GUM change new parent
     (TEMPL AGENT-AGENT1-NEUTRAL-2-XP1-3-XP-TEMPL (xp (% W::pp (W::ptype W::about))))
     )
    ;;;; Myrosia 10/23/03 made the to complement required to avoid needless ambiguities
    ((LF-PARENT ONT::command)
     (example "tell him to do it")
     (TEMPL AGENT-AGENT1-FORMAL-OBJCONTROL-TEMPL)
     )
    ((LF-PARENT ONT::tell)
     (example "a zero voltage tells you that the terminals are connected")
     (meta-data :origin beetle :entry-date 20090512 :change-date 20090609 :comments beetle-pilots)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-PP-WITH-TEMPL)
     (preference 0.98)
     )
    )
   )
))

