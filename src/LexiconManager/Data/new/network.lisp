;;;;
;;;; W::NETWORK
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::NETWORK
   (SENSES
    ((LF-PARENT ONT::computer-network) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL :wn ("network%1:06:02")
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::network
   (SENSES
     ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("mix-22.1-2-1"))
     (LF-PARENT ONT::attach)
     (example "he networked the computers")
     (TEMPL agent-affected2-optional-templ (xp (% w::pp (w::ptype (? ptype w::to w::with)))))
      ; like connect
     (PREFERENCE 0.96)
     )
    #|| ;; need this template to go through the adjectival passive rule
     ((LF-PARENT ONT::attach)
     (example "the networked computers")
     (TEMPL AGENT-THEME-xp-TEMPL)
     )||#
    ((LF-PARENT ONT::CONNECTED)
     (example "the internet networks one computer to another")
     (SEM (F::Aspect F::indiv-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )
    )
   )
))

