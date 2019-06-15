;;;;
;;;; w::shade
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::shade
   (SENSES
    ((meta-data :origin caloy2 :entry-date 20050404 :change-date nil :wn ("shade%1:06:00") :comments projector-purchasing)
     (LF-PARENT ONT::covering)
     (example "close the window shades")
     )
    ((meta-data :origin calo :entry-date 20031229 :change-date nil :wn ("shade%1:07:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::color-scale)
     (example "available in 40 vibrant shades")
     (TEMPL OTHER-RELN-TEMPL)
     )
    ))
))


(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::shade
   (SENSES
    ((meta-data :origin cmap-testing :entry-date 20090929 :change-date nil :comments nil :wn-sense (1) :vn ("defend-85"))
     (LF-PARENT ONT::protecting)
     (SEM (F::Time-span F::extended))
     (example "the tree shaded the courtyard")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
      )
    )
   )
))

