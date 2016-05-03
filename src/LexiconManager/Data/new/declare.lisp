;;;;
;;;; W::declare
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::declare
   (SENSES
#|    
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("declare%2:32:00" "declare%2:32:01"))
     (LF-PARENT ONT::talk)
      (TEMPL agent-theme-to-addressee-optional-templ)  ; like say but needs different template b.c. doesn't participate in alternation
;     (TEMPL agent-affected-iobj-theme-templ) ; like say
     )
|#
    ((lf-parent ont::assert)
     (Example "He declared them ready")
     (TEMPL agent-neutral-complex-templ)
     )
    )
   )
))

