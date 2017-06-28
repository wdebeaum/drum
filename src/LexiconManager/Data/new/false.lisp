;;;;
;;;; W::FALSE
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::FALSE
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090915 :comments nil)
     (LF-PARENT ONT::fake-val)
     (example "a false friend")
     )
    ((LF-PARENT ONT::artificial)
     (example "false teeth")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090915 :comments nil)
     (LF-PARENT ONT::false-val)
     (example "He gave a false testimony")
     )
   )
  )
 )
)
