;;;;
;;;; w::strenuous
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
   (w::strenuous
   (SENSES
    ((meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments LM-vocab)
     (LF-PARENT ONT::task-complexity-val) ;; like difficult
     (example "Programmers are strenuous to interview")
     (templ adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments LM-vocab)
     (LF-PARENT ONT::task-complexity-val)
     (example "Programmers are stressful for managers")
     (templ adj-content-affected-XP-templ  (xp (% w::pp (w::ptype w::for))))
     )
    ((meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments LM-vocab)
     (EXAMPLE "it's stressful to see him")
     (LF-PARENT ONT::task-complexity-VAL)
     (TEMPL adj-expletive-content-xp-TEMPL (XP (% W::CP (W::CTYPE W::s-to))))
     )
    ((meta-data :origin cardiac :entry-date 20090129 :change-date nil :comments LM-vocab)
     (example "Programmers are stressful for managers to interview")
     (LF-PARENT ONT::task-complexity-VAL)
     (TEMPL adj-expletive-content-control-TEMPL)
     )
    ))
))

