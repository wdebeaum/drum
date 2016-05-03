;;;;
;;;; w::fall
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (w::fall
   (SENSES
    ;; :nom
;    ((EXAMPLE "what is the duration of the fall")
;     (meta-data :origin step :entry-date 20080722 :change-date nil :comments nil)
;     (LF-PARENT ONT::event)
;     )
    ((meta-data :origin trips :entry-date 20090111 :change-date nil :comments nil :wn ("autumn%1:28:00"))
     (LF-PARENT ONT::autumn)
     (templ time-reln-templ)
     )
    )
   )
))

(define-words :pos W::v :templ affected-TEMPL
 :tags (:base500)
 :words (
  (W::fall
   (wordfeats (W::morph (:forms (-vb) :past W::fell :pastpart w::fallen :nom w::fall)))
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments nil :vn ("escape-51.1-2"))
     (LF-PARENT ONT::fall)
     (TEMPL affected-TEMPL)
     )
    ((meta-data :origin cardiac :entry-date 20080828 :change-date nil :comments nil)
     (lf-parent ont::become)
     (example "he fell asleep" "he fell ill") ;; overgenerates, e.g he fell awake
     (templ affected-pred-templ)
     )
    )
   )
))

