;;;;
;;;; W::unfix
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::unfix
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("disassemble-23.3"))
     (LF-PARENT ONT::unattach)
 ; like unclip,unleash,unbolt,unhitch,unpin,unlatch,unglue,unstaple,unscrew,unlace,unbutton,unhook,unclamp,unclasp,sunder,unhinge,unshackle,detach,unlock,unpeg,unfasten,untie,unchain,unstitch,unbuckle,unzip
     (TEMPL agent-affected-theme-optional-templ (xp (% w::pp (w::ptype w::from))))	  
     )
    )
   )
))

