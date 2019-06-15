;;;;
;;;; W::employ
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::employ
   (wordfeats (W::morph (:forms (-vb) :nom w::employment)))
   (SENSES
    #||(LF-PARENT ONT::employ)    ;; this is purpose
     (example "Who does Chiang employ (to write his papers)")
     (meta-data :origin csli-ts :entry-date 20070313 :change-date 20090508 :comments nil :wn nil)
     (templ agent-effect-objcontrol-templ)
     )||#
    ;; need this transitive separate to get passive right
     ((LF-PARENT ONT::employ)
     (example "Abrams hired this employee")
     (meta-data :origin csli-ts :entry-date 20070316 :change-date 20090508 :comments nil :wn nil)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
     ))
    )
   )

