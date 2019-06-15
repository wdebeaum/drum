;;;;
;;;; W::ACCOUNT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::ACCOUNT
   (SENSES
    ((meta-data :origin calo :entry-date 20040205 :change-date nil :wn ("account%1:26:00") :comments calo-y1variants)
     (example "charge it to my account")
     (LF-PARENT ONT::account)
     (templ other-reln-templ)
     )
#|
    ((meta-data :origin plow :entry-date 20050928 :change-date nil :comments naive-subjects)
     (LF-PARENT ONT::chronicle)
     (example "he wrote a descriptive account of his adventures")
     )
|#
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::account
   (wordfeats (W::morph (:forms (-vb) :nom w::account)))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date 20090508 :comments s15)
     (LF-PARENT ONT::explain)
     (example "account for the problem")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::pp (W::ptype W::for))))
     )
    )
   )
))

