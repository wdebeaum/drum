;;;;
;;;; W::ABILITY
;;;;

(define-words :pos W::v :templ COUNT-PRED-TEMPL
 :words (
   (W::ability-verb-doesnotexist
     (wordfeats (W::morph (:forms (-vb) :nom W::ability)))
     (SENSES
      (;(LF-PARENT ONT::ABLE)  
       (LF-PARENT ONT::ABILITY-STATE)  
       (TEMPL neutral-theme-subjcontrol-templ )
       )))))

