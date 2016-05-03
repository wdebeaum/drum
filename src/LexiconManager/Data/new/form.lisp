;;;;
;;;; W::form
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::form
   (SENSES
    ((LF-PARENT ONT::shape)
     (TEMPL OTHER-RELN-TEMPL)
     )
    ( ;; 20050325 changed from info-function-object to template-info-object
     (LF-PARENT ONT::template-info-object)
     (example "fill out the requisition form to get approval")
     (meta-data :origin calo :entry-date 20040622 :change-date 20050325 :wn ("form%1:10:01") :comments y2)
     (preference .98)
    )
   ))
))

(define-words :pos W::v :templ agent-affected-create-templ
 :tags (:base500)
 :words (
  (W::form
    (wordfeats (W::morph (:forms (-vb) :nom w::formation)))
   (SENSES
    ((LF-PARENT ONT::create) ;; GUM change new parent 20121027
     (example "form a work based on the library")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil)
     )
   #||
   (LF-PARENT ONT::create) ;; GUM change new parent 20121027
     (example "trees form a circle" "a and the battery form a closed path")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin beetle :entry-date 20090203 :change-date nil :comments nil)
     )
   ||#
    )
  )
))

