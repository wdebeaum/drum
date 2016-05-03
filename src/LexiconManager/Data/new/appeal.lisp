;;;;
;;;; W::APPEAL
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::APPEAL
   (SENSES
    ((LF-PARENT ONT::NON-MEASURE-ORDERED-DOMAIN) (TEMPL mass-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("appeal%1:07:00")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
 (w::appeal
  (senses
   (;(LF-PARENT ONT::request)
    (lf-parent ont::appeal-apply-demand) ;; 20120523 GUM change new parent
    (example "Abrams appealed to Browne to hire Chiang")
    (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
    (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
    (templ agent-to-addressee-theme-objcontrol-req-templ)
    )
   ;; need this so passive rule will work
   (;(LF-PARENT ONT::request)
    (lf-parent ont::appeal-apply-demand) ;; 20120523 GUM change new parent
    (example "Abrams appealed to Browne to hire Chiang")
    (meta-data :origin csli-ts :entry-date 20070313 :change-date nil :comments nil :wn nil)
    (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
    (templ agent-to-addressee-templ)
    )
   )
  )
))

