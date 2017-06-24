;;;;
;;;; W::complex
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::complex
   (SENSES
    ((LF-PARENT ONT::facility)
     (meta-data :origin calo-ontology :entry-date 20051214 :change-date nil :wn ("complex%1:09:00" "complex%1:06:00") :comments Office)
     (example "an office complex")
     )

    ((LF-PARENT ONT::MACROMOLECULAR-COMPLEX)
     (meta-data :origin BOB :entry-date 20141229 :change-date nil :wn ("complex%1:27:00"))
;     (templ pred-subcat-contents-templ (xp (% W::PP (W::ptype (? xx W::of W::with)))))
     ;(templ pred-subcat-contents-templ)
     (templ other-reln-templ)
     )

    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; interesting -- these examples don't really work without the "too"
  (W::COMPLEX
   (SENSES
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090821 :wn ("complex%3:00:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::difficult)
     (example "the task is too complex [for him]")
     (TEMPL adj-content-affected-optional-xp-templ)
     )
    ((meta-data :origin calo :entry-date 20031223 :change-date 20090821 :wn ("complex%3:00:00") :comments html-purchasing-corpus)
     (LF-PARENT ONT::difficult)
     (example "the task is too complex to finish")
     (TEMPL adj-expletive-content-xp-templ (XP (% W::CP (W::CTYPE W::s-to))))
     )
    )
   )
))

(define-words :pos W::v 
 :words (
   (W::complex
   (SENSES
    ((LF-PARENT ONT::ATTACH)
     (TEMPL AGENT-affected2-optional-TEMPL (xp (% W::pp (W::ptype (? xxx W::with)))))
     (example "We complexed this protein with another protein")
     )

    ((LF-PARENT ONT::ATTACH)
     (TEMPL agent-affected-as-comp-TEMPL (xp (% W::pp (W::ptype (? xxx W::with)))))
     (example "this protein complexed with another protein")
     )

    )
   )
))
