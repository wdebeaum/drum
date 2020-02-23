;;;;
;;;; W::BEEN
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::BEEN
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pastpart) (W::AGR ?agr))
   (SENSES
    ;;;; I added (AGR ?agr) in order to make "There has been an accident" work
    ;;;; Myrosia should check JFA 11/02
    ;;;; I have been loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; I have been
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::PROGR) (W::changesem +))
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; The truck has been loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; she has been hungry
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL NEUTRAL-FORMAL-PRED-SUBJCONTROL-TEMPL)
     )
    ;;;; The city has been the largest ...
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-A-TEMPL)
     ;(PREFERENCE 0.96)
     )
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL NEUTRAL-NEUTRAL1-NP-EQUAL-B-TEMPL)
     ;(PREFERENCE 0.96)
     )

    (  ;; the fact is he's happy
     (LF-PARENT ont::proposition-equal)
     (LF-FORM w::BE)
     (TEMPL NEUTRAL-NEUTRAL1-CP-STHAT-EQUAL-TEMPL)
     )

    ;;;; There has been an accident.
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL EXPLETIVE-NEUTRAL-XP-TEMPL)
     )
    )
   )
))

