;;;;
;;;; W::dismantle
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::dismantle
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("disassemble-23.3"))
     (LF-PARENT ONT::unattach)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from))))
 ; like unclip,unleash,unbolt,unhitch,unpin,unlatch,unglue,unstaple,unscrew,unlace,unbutton,unhook,unclamp,unclasp,sunder,unhinge,unshackle,detach,unlock,unpeg,unfasten,untie,unchain,unstitch,unbuckle,unzip     
     )
    )
   )
))

