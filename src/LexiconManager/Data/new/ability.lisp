;;;;
;;;; W::ABILITY
;;;;

(define-words :pos W::v :templ COUNT-PRED-TEMPL
 :words (
   (W::ability-verb-doesnotexist
     (wordfeats (W::morph (:forms (-vb) :nom W::ability)))
     (SENSES
      (;(LF-PARENT ONT::ABLE)  
       (LF-PARENT ONT::ABILITY-EVENT)  
       (TEMPL agent-effect-subjcontrol-templ (xp (% W::cp (W::ctype (? ct W::s-to)))))
       )))))

