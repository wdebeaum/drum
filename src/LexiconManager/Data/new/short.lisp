;;;;
;;;; w::short
;;;; 

(define-words 
    :pos W::n :templ COUNT-PRED-TEMPL
 :words (
	    ((w::short w::circuit)
	     (wordfeats (W::morph (:forms (-S-3P) :plur (W::short W::circuits))))
	     (senses 
	      ((LF-parent ONT::Device) 
	       (templ count-pred-templ)
	       (meta-data :origin bee :entry-date 20040407 :change-date 20050211 :wn ("short_circuit%1:06:00") :comments (test-s portability-followup))
	       ))
	     )
))

(define-words :pos W::n
 :words (
  ((W::SHORT W::LOIN)
  (senses
	   ((LF-PARENT ONT::BEEF)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::SHORT
   (wordfeats (W::MORPH (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("short%3:00:01"))
     (LF-PARENT ONT::short-val)
     (TEMPL LESS-ADJ-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("short%3:00:02"))
     (LF-PARENT ONT::event-duration-modifier)
     (example "a short meeting")
     (TEMPL LESS-ADJ-TEMPL)
     )
     ((meta-data :origin chf :entry-date 20070810 :change-date 20090731 :comments chf-dialogues)
      (LF-PARENT ONT::inadequate)
      (example "he is short of breath" "short of time")
      (TEMPL central-adj-xp-required-TEMPL (XP (% W::PP (W::Ptype (? pt w::on W::of)))))
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((w::short w::of w::breath)
   (senses
   ((meta-data :origin chf :entry-date 20070910 :change-date nil :comments chf-dialogues)
   (lf-parent ont::breathless-val)
   (templ less-adj-templ)
   (example "he is short of breath")
   )))
))

