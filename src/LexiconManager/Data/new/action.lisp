;;;;
;;;; W::ACTION
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;; still here until :nom lists implemented -- right now "act" is the :nom for w::act (v)
  (W::ACTION
   (SENSES
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("action%1:04:02" "action%1:04:03" "action%1:04:01" "action%1:04:04" "action%1:04:00"))
     (LF-PARENT ONT::ACTING)
     (templ other-reln-cause-templ)
     )
    )
   )
))

