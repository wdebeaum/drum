;;;;
;;;; W::SYSTEM
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::SYSTEM
   (SENSES
    ;; this sense is for a physical arrangement of components
    (;(LF-PARENT ONT::INSTRUMENTATION)
     (LF-PARENT ONT::SYSTEM)
     (example "an entertainment system")
     (meta-data :origin caloy2 :entry-date 20050404 :change-date nil :wn ("system%1:06:00") :comments projector-purchasing)
     )
    ;; this is for abstractions
    ((EXAMPLE "a meal system")
     (meta-data :origin medadvisor :entry-date 20030404 :change-date nil :wn ("system%1:14:00") :comments nil)
     (LF-PARENT ONT::procedure)
      (PREFERENCE 0.98) ;; prefer ont::instrumentation sense
      )
    
    ; lots of things can be agents, e.g., computer
    #| 
    ((LF-PARENT ONT::agent)
     (example "ask the system to help you")
     (meta-data :origin integrated-learning :entry-date 20050817 :change-date nil :comments nil)
     )
    |#
    )
   )
))

