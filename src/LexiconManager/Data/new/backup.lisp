;;;;
;;;; W::backup
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::backup
    (wordfeats (W::morph (:forms (-vb) :nom W::backup)))
    (SENSES
     ((meta-data :origin calo :entry-date 20040504 :change-date nil :comments html-purchasing-corpus)
     (EXAMPLE "backup the files")
     (LF-PARENT ONT::archive)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
         )
   )
))

