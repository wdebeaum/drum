;;;;
;;;; W::LIGHT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::LIGHT
   (SENSES
    ((LF-PARENT ONT::light)
     (example "we had to close the curtains to keep out the light")
     (meta-data :origin calo :entry-date 20050527 :change-date nil :wn ("light%1:19:00") :comments projector-purchasing)
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
	 ((w::light w::bulb)
	  (senses ((lf-parent ont::device)
		   (lf-form w::lightbulb)
		   (meta-data :origin bee :entry-date 20050303 :change-date nil :comments newBEEcorpus)
		   )))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
	 (w::light
	  (senses ((lf-parent ont::device-component)
		   (Example "A light is a lightbulb -- the electrical sense only")
		   (meta-data :origin bee :entry-date 20040408 :change-date nil :comments test-s)
		   )))
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :tags (:base500)
 :words (
	  (w::light
	   (wordfeats (W::morph (:forms (-vb) :past w::lit)))
	   (senses
	    (;(lf-parent ont::change-device-state)
	     (lf-parent ont::burn-out-light-up-change) ;; GUM change new parent 20121030
	     (example "light the lamp")
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )
	    (;(lf-parent ont::change-device-state)
	     (lf-parent ont::burn-out-light-up-change) ;; GUM change new parent 20121030
	     (example "In which of the following diagrams will the bulb light")
	     (templ affected-templ)
	     (meta-data :origin bee :entry-date 20070813 :change-date nil :comments beetle-pilot)
	     ;; this is slightly odd, so lower the preference
	     (preference 0.95)
	     )	    
	    ))
))

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	  ((w::light (w::up))
	   (wordfeats (W::morph (:forms (-vb) :past w::lit)))
	   (senses
	    ((lf-parent ont::burn-out-light-up-change) ;; GUM change new parent 20121030
	     ;;(lf-parent ont::change-device-state)
	     (example "The lamp lights up")
	     (templ affected-templ)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-followup)
	     )	   
	    ))
))

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
   ((w::light w::punc-minus w::headed)
   (SENSES
    (
     (LF-PARENT ONT::DIZZY-VAL)
     (example "do you feel light-headed")
     (templ central-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
   ((w::light w::headed)
   (SENSES
    (
     (LF-PARENT ONT::DIZZY-VAL)
     (example "do you feel light headed")
     (templ central-adj-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ ADJ-EXPERIENCER-TEMPL
 :words (
   (w::lightheaded
   (SENSES
    ((meta-data :origin chf :entry-date 20070904 :change-date 20090731 :comments nil :wn nil)
     (LF-PARENT ONT::DIZZY-VAL)
     (example "do you feel lightheaded")
     (templ central-adj-templ)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  (w::lightheadedness
  (senses
   ((meta-data :wn ("lightheadedness%1:26:00"))
    (LF-PARENT ONT::lightheadedness)
    (TEMPL mass-pred-TEMPL)
    (syntax (W::morph (:forms (-none))))
    )
   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::LIGHT
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("light%3:00:01"))
     (LF-PARENT ONT::LIGHTWEIGHT)
     (TEMPL LESS-ADJ-TEMPL)
     (sem (f::gradability +) (f::intensity ont::hi) (f::orientation F::neg))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("light%3:00:05"))
     (LF-PARENT ONT::LIGHT-VAL)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((LF-PARENT ont::light-in-color-val)
     (example "light green")
     (meta-data :origin adjective-reorganization :entry-date 20170317 :change-date nil :comments nil)
     )
    )
   )
))
