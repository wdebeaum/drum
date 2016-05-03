;;;;
;;;; W::LABOR
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::LABOR
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :wn ("labor%1:04:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::working)
     (templ mass-pred-templ)     
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
   (W::labor
    ;; if we don't specify as irregular we get doubled final consonant
    (wordfeats (W::morph (:forms (-vb) :past W::labored :ing W::laboring)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("cooperate-71-3"))
     (LF-PARENT ONT::working)
     (TEMPL agent-templ) ; like work
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("cooperate-71-3"))
     (LF-PARENT ONT::working)
     (resultative +)
     (example "he labored over his speech")
     (TEMPL agent-theme-xp-templ (xp (% w::pp (w::ptype (? p w::on w::over))))) ; like work
     )
    )
   )
))

(define-words :pos W::name
 :words (
  ((w::labor w::day)
  (senses((LF-PARENT ONT::holiday)
    (TEMPL nname-templ)
    )
   )
)
))

