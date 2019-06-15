;;;;
;;;; W::BASE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
    (w::base
     (senses
      ((meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-4)
       (LF-PARENT ONT::object-dependent-location)
       (TEMPL GEN-PART-OF-RELN-TEMPL)
       (example "move the avocado to the base of the triangle")
       )
    ))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
;; 20120718 separating base senses for bolt :tags (:base500)
 :words (
  (W::BASE
   (SENSES
    (
     (LF-PARENT ONT::commercial-activity)
     (example "industrial base")
     )
    ((meta-data :origin trips :entry-date 20060803 :change-date nil :comments nil :wn ("base%1:06:04"))
     (LF-PARENT ONT::transportation-facility)
     )
    ((meta-data :origin caet :entry-date 20110803 :change-date nil :comments nil)
   ;  (LF-PARENT ONT::kettle-base)
     (lf-parent ont::base)
     (example "put the kettle on the base")
     )
;    ;; how is this entry different from the above?
;    ((meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
;     (LF-PARENT ONT::object-dependent-location)
;     (TEMPL GEN-PART-OF-RELN-TEMPL)
;     (example "base of the power")
;     )

    ))   
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
; :tags (:base500)
 :words (
  (w::base
    (wordfeats (W::morph (:forms (-vb) :nom w::base)))
    (SENSES
     ((LF-PARENT ONT::establish)
      (EXAMPLE "he based the comparison on price")
      (META-DATA :ORIGIN plow :ENTRY-DATE 20060620 :CHANGE-DATE NIL :COMMENTS pq)
      (TEMPL AGENT-AFFECTEDR-MANNER-2-XP-3-XP2-TEMPL (xp (% w::pp (w::ptype (? pt w::on)))))
      )
     )
    )
))

