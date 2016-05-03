;;;;
;;;; W::ROCK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
   (W::ROCK
   (SENSES
    ((LF-PARENT ONT::natural-object) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("rock%1:17:01")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :tags (:base500)
 :words (
  (W::rock
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060608 :change-date nil :comments nil :vn ("modes_of_being_with_motion-47.3") :wn ("rock%2:38:00"))
     (LF-PARENT ONT::move-back-and-forth)
     (example "the ship rocked")
     (TEMPL affected-templ)
     )
    )
   )
))

