;;;;
;;;; W::GROUP
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::GROUP
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20090520 :comments nil :wn ("group%1:03:00"))
     (LF-PARENT ONT::group-object)
     (example "a group of cars")
     (TEMPL pred-subcat-contents-templ)
     )
    ((LF-PARENT ONT::group-object)
     (TEMPL count-pred-templ)
     (syntax (agr (? ag 3s 3p)))
     (SEM (F::intentional +))
     (example "the group entered the building")
     (meta-data :origin caloy-ontology :entry-date 20060220 :change-date 20090520 :comments caloy3)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::group
   (SENSES
   ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("herd-47.5.2") :wn ("group%2:31:00" "group%2:33:00"))
     (LF-PARENT ONT::meet)
     (example "they grouped around the painting")
     (TEMPL agent-plural-templ) ; like congregate,assemble,gather
     (PREFERENCE 0.96)
     )
    ((EXAMPLE "group the red one with those orange ones")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-affected2-TEMPL (xp (% W::PP (W::ptype W::with))))
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil)
     )
    ((EXAMPLE "group them together")
     (LF-PARENT ONT::joining)
     (TEMPL AGENT-affected-PLURAL-TEMPL)
     (meta-data :origin task-learning :entry-date 20050819 :change-date nil :comments nil)
     )
    )
   )
))

