;;;;
;;;; W::plug
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
    (W::plug
   (SENSES
    ((LF-PARENT ONT::device) (TEMPL COUNT-PRED-TEMPL)
     (META-DATA :ORIGIN CALO :ENTRY-DATE 20040204 :CHANGE-DATE NIL
      :COMMENTS HTML-PURCHASING-CORPUS))))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::PLUG
   (SENSES
    ((meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     (LF-PARENT ONT::DEVICE)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::plug
   (SENSES
    ((LF-PARENT ONT::ATTACH)
     (example "plug the videocard in(to) the computer")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo :entry-date 20050323 :change-date nil :comments caloy2)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype (? pt w::in W::into)))))
     )
    ((LF-PARENT ONT::connected)
     (example "the video card plugs into the computer")
     (meta-data :origin calo :entry-date 20050323 :change-date nil :comments caloy2)
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% W::pp (W::ptype W::into))))
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
 ((W::plug (w::in))
   (SENSES
    ((LF-PARENT ONT::ATTACH)
     (example "plug in the computer")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin calo :entry-date 20060128 :change-date nil :comments caloy2)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )
    )
   )
))

