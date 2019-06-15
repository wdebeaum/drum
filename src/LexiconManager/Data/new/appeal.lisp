;;;;
;;;; W::APPEAL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::APPEAL
   (SENSES
    ((LF-PARENT ONT::attractive-scale) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("appeal%1:07:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (w::appeal
  (senses
   (;(LF-PARENT ONT::request)
    (lf-parent ont::appeal-apply-demand) ;; 20120523 GUM change new parent
    (example "Abrams appealed to Browne to hire Chiang")
    (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
    (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
    (TEMPL AGENT-AGENT1-FORMAL-2-XP1-3-OBJCONTROL-TEMPL)
    )
   ;; need this so passive rule will work
   (;(LF-PARENT ONT::request)
    (lf-parent ont::appeal-apply-demand) ;; 20120523 GUM change new parent
    (example "Abrams appealed to Browne to hire Chiang")
    (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
    (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
    (TEMPL AGENT-AGENT1-PP-TEMPL)
    )
   )
  )
))

