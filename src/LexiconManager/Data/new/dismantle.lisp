;;;;
;;;; W::dismantle
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::dismantle
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("disassemble-23.3"))
     (LF-PARENT ONT::unattach)
     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% w::pp (w::ptype w::from))))
 ; like unclip,unleash,unbolt,unhitch,unpin,unlatch,unglue,unstaple,unscrew,unlace,unbutton,unhook,unclamp,unclasp,sunder,unhinge,unshackle,detach,unlock,unpeg,unfasten,untie,unchain,unstitch,unbuckle,unzip     
     )
    )
   )
))

