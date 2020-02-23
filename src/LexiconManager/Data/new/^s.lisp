;;;;
;;;; W::^S
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::^S
   (wordfeats (W::morph (:forms NIL)) 
	      (W::vform W::pres) (W::agr W::3s)
	      (w::contraction +)    
	      )
   (SENSES      ;; these are all downgraded so we prefer the possessive if possible
    ;;;; He's loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (preference .98) 
     )
    ;;;; auxiliary have in perfect construction
    ((LF-PARENT ONT::PERFECTIVE)
     (TEMPL PERFECTIVE-TEMPL)
     (LF-FORM W::be)
     (SYNTAX (W::auxname W::perf))
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (preference .98)
     )
    ;;;; it's loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (preference .98)
     )
    ;;;; she's hungry
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     (preference .98)
     )
    ;;;; It's the truck
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-A-TEMPL)
     (preference .98)
     )
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-B-TEMPL)
     (preference .98)
     )
    ;;;; .. there's a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL (xp (% w::NP (w::agr w::3s))) )
     (preference .98)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL 
 :tags (:base500)
 :words (
  (W::^S
   (wordfeats (W::morph (:forms NIL)) 
	      (W::vform W::pres) (W::agr W::3p)
	      (w::contraction +)
	      )
   (SENSES
    ;;;; a special contraction for "there's" NOTE the AGR 3p!!
    ((LF-PARENT ONT::Exists)
     (LF-FORM W::be)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL (xp (% w::NP (w::agr w::3p))))
     (PREFERENCE 0.97)
     )
    ((LF-PARENT ONT::Have-property)
     (LF-FORM W::be)
     (TEMPL EXPLETIVE-NEUTRAL-FORMAL-1-XP1-2-XP2-TEMPL (xp1 (% W::np (W::lex W::there))))
     (PREFERENCE 0.97)
     )    
    )
   )
))

#|
(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::^S
   (wordfeats (W::agr W::1p) (W::lex W::us) (W::case W::obj))
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (LF-FORM W::US)
     (PREFERENCE 0.97)
     (templ pronoun-plural-templ)
     )
    )
   )
))
|#

(define-words :pos W::^s :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::^S
   (SENSES
    ((LF ONT::^S)
     (non-hierarchy-lf t))
    )
   )
))

