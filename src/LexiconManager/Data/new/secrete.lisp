;;;;
;;;; W::secrete
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
    (W::secrete
     (wordfeats (W::morph (:forms (-vb) :nom w::secretion)))
   (SENSES
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     ;;(LF-PARENT ONT::emit)
     (lf-parent ont::emit-giveoff-discharge) ;; 20121022 GUM change new parent
     (example "it secreted fluid")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    )
   )
))

