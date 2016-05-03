;;;;
;;;; W::FLASH
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::FLASH W::MEMORY)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :wn ("flash_memory%1:06:00") :comments plow-req)
     (LF-PARENT ONT::data-storage-medium)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::FLASH W::DRIVE)
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :comments plow-req)
     (LF-PARENT ONT::data-storage-medium)
     (templ count-pred-templ)
     )
    ((meta-data :origin calo-ontology :entry-date 20060711 :change-date nil :comments caloy3)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (w::flash
   (SENSES
;    ((meta-data :origin plow :entry-date 20060615 :change-date nil :wn ("flash%1:11:00") :comments plow-req)
;     (LF-PARENT ONT::event)
;     (example "a flash of light")
;     )
    ((meta-data :origin plow :entry-date 20060615 :change-date nil :wn ("flash%1:06:00") :comments plow-req)
     (LF-PARENT ONT::device-component)
     (example "does the camera have a flash")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::flash
   (wordfeats (W::morph (:forms (-vb) :nom W::flash)))
   (SENSES
    ((LF-PARENT ONT::visual-display)
     (EXAMPLE "flashes the IE icon when a subscription has changed")
     (meta-data :origin task-learning :entry-date 20050912 :change-date 20090506 :comments nil)
     )
    )
   )
))

