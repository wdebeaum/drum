;;;;
;;;; W::secrete
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
    (W::secrete
     (wordfeats (W::morph (:forms (-vb) :nom w::secretion)))
   (SENSES
    ((meta-data :origin text-processing :entry-date 20091216 :change-date nil :comments wf-missing-mappings)
     ;;(LF-PARENT ONT::emit)
     (lf-parent ont::emit-giveoff-discharge) ;; 20121022 GUM change new parent
     (example "it secreted fluid")
     (TEMPL agent-affected-xp-templ) 
     )
    )
   )
))

