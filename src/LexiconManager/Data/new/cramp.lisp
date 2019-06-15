;;;;
;;;; w::cramp
;;;;

(define-words :pos W::n
 :words (
  (w::cramp
  (senses
   ((meta-data :wn ("cramp%1:26:00"))
    (LF-PARENT ONT::spasm)
    (TEMPL count-pred-TEMPL)
    )
   )
)
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::cramp
   (SENSES
    ((meta-data :origin cardiac :entry-date 20081215 :change-date nil :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::objective-influence)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     (example "restrictions cramp his style")
     (preference .95)
     )
    ((meta-data :origin cardiac :entry-date 20081215 :change-date 20090511 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::cause-body-effect)
     (TEMPL affected-TEMPL)
     (example "his body cramped (with pain/convulsions)")
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::cramp w::up)
   (SENSES
     ((meta-data :origin cardiac :entry-date 20081215 :change-date 20090511 :comments nil :vn ("hurt-40.8.3-2") :wn ("injure%2:29:00"))
     (LF-PARENT ONT::cause-body-effect)
     (TEMPL affected-TEMPL)
     (example "his leg cramped up")
     )
    )
   )
))

