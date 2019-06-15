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
     (TEMPL AFFECTED-FORMAL-XP-PRED-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AFFECTED-result-XP-TEMPL
:words (
 (W::fall
  (wordfeats (W::morph (:forms (-vb) :past W::fell :pastpart w::fallen :ing w::falling)))
  (SENSES
   ((meta-data :wn ("fall%2:40:12"))
    (LF-PARENT ONT::incur-inherit-receive)
    ;(templ AFFECTED-result-XP-TEMPL (xp (% W::pp (W::ptype W::to))))
    (TEMPL AFFECTED1-AFFECTED-XP-TEMPL (xp (% W::pp (W::ptype W::to))))
    (example "The estate fell to the oldest daughter")
   ) 
  )
 )
))

