;;;;
;;;; W::present
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::present
   (SENSES
    ((LF-PARENT ONT::giving)
     (meta-data :origin plow :entry-date 20050928 :change-date nil :wn ("present%1:21:00") :comments naive-subjects)
     (example "I am looking for a gift for my mother")
     (preference 0.97) ;; prefer adjective
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  (W::present
   (SENSES
    ((LF-PARENT ONT::show)
     (example "who is going to present the poster (to them)")
     (TEMPL agent-affected-xp-templ)
     (meta-data :origin calo-ontology :entry-date 20060712 :change-date 20090506 :comments caloy3)
     )
    ((LF-PARENT ONT::appear)
     (example "The patient presented with a right thoracic scoliosis")
     (TEMPL AFFECTED-TEMPL))

    ((LF-PARENT ONT::encodes-message)
     (meta-data :origin "bee" :entry-date 20091394 :change-date nil :comments nil)
     (example "this book presents a different solution")
     (TEMPL neutral-THEME-XP-TEMPL)
     (Preference 0.97) ;; choose agentive interpretation whenever possible
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::PRESENT
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("present%3:00:01"))
     (LF-PARENT ONT::temporal-location)
     (P "calculate the present day value")
     (TEMPL attributive-only-adj-templ)
     )
    ((meta-data :origin cernl :entry-date 20110314 :change-date nil :comments ticket-243)
     (LF-PARENT ONT::available)
     (example "infiltrates that were not present on the scan")
     )
    )
   )
))

