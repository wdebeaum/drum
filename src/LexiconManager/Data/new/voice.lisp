;;;;
;;;; W::VOICE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::VOICE
   (SENSES
    ((meta-data :origin boudreaux :entry-date 20031024)
     (LF-PARENT ONT::MEDIUM)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::voice
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090505 :comments nil :vn ("say-37.7"))
     ;;(LF-PARENT ONT::talk)
 ; like respond,reply
     (lf-parent ont::say)
     )
    )
   )
))

