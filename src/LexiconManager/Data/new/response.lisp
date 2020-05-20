;;;;
;;;; W::response
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::response
   (SENSES
    ((LF-PARENT ONT::outcome) ;caused-event) ; as in reaction
     (meta-data :origin lam :entry-date 20050422 :change-date 20090508 :wn ("response%1:10:01") :comments lam-initial)
     (TEMPL OTHER-RELN-TEMPL (xp (% W::pp (W::ptype (? pt w::on W::about w::for)))))
     )
    ((LF-PARENT ONT::outcome) ;caused-event)
     (templ count-subcat-that-optional-templ)
     )
    )
   )
  ))

