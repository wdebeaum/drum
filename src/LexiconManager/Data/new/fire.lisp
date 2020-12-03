;;;;
;;;; W::FIRE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 ((W::FIRE w::TRUCK)
   (SENSES
    ((meta-data :origin obtw :entry-date 20111016)
     ;; 20111016 added for obtw demo
     (LF-PARENT ONT::fire-truck)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::FIRE W::STATION)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::fire W::stations))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("fire_station%1:06:00"))
     (LF-PARENT ONT::public-service-facility)
     (LF-FORM W::FIRE-STATION)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::FIRE W::DEPARTMENT)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::fire W::departments))))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031217 :change-date nil :comments s7)
     (LF-PARENT ONT::public-service-facility)
     (LF-FORM W::FIRE-DEPARTMENT)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::FIRE W::WALL)
   (SENSES
    ((LF-PARENT ONT::computer-system) ;computer-software)
     (LF-FORM W::fire-wall)
     (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::fire
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date 20111005 :comments nil :wn ("fire%1:11:00"))
     ;; changed for obtw demo from ont::located-event to ont::fire
     (LF-PARENT ONT::fire)
     (TEMPL BARE-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
  (W::fire
   (SENSES
    ((LF-PARENT ONT::explode) ;; like ignite
     (example "the spark plug fires")
     (templ affected-templ)
     (meta-data :origin step :entry-date 20080922 :change-date 20090504 :comments nil)	
     )
    ((LF-PARENT ONT::explode) ;; like ignite
     (example "the spark plug fires the engine")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (meta-data :origin step :entry-date 20080922 :change-date 20090504 :comments nil)	
     )
    ((LF-PARENT ONT::terminate)
     (example "Alice fired Bob")
     (meta-data :origin "wordnet-3.0" :entry-date 20090508 :change-date nil :comments nil)
     )
    )
   )
))

