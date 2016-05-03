;;;;
;;;; W::HARD
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::HARD W::DRIVE)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::hard W::drives))))
   (SENSES
    ( (meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("hard_drive%1:06:00") :comments calo-y1script)
     (LF-PARENT ont::io-device)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::HARD W::DISK)
   (wordfeats (W::morph (:forms (-S-3P) :plur (W::hard W::disks))))
   (SENSES
    ( (meta-data :origin calo :entry-date 20040421 :change-date nil :wn ("hard_disk%1:06:00") :comments calo-y1script)
     (LF-PARENT ont::internal-computer-storage)
     )
    )
   )
))

(define-words :pos W::n
 :words (
  ((W::HARD W::RED W::SPRING W::WHEAT)
  (senses
	   ((LF-PARENT ONT::GRAINS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::HARD W::RED W::WINTER W::WHEAT)
  (senses
	   ((LF-PARENT ONT::GRAINS)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::HARD
   (wordfeats (W::MORPH (:FORMS (-ER))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments csli-ts :wn ("hard%3:00:06"))
     (EXAMPLE "that's hard")
     (LF-PARENT ONT::difficult)
     ;;     (TEMPL adj-theme-cause-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     (TEMPL adj-content-TEMPL)
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments csli-ts :wn ("hard%3:00:06"))
     (EXAMPLE "that's hard for him")
     (LF-PARENT ONT::difficult)
     ;;     (TEMPL adj-theme-cause-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     (TEMPL adj-content-affected-XP-TEMPL (XP (% W::PP (W::PTYPE W::FOR))))
     )
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments csli-ts :wn ("hard%3:00:06"))
     (EXAMPLE "it's hard to see him")
     (LF-PARENT ONT::difficult)
     ;;     (TEMPL adj-theme-action-TEMPL (XP (% W::CP (W::CTYPE W::s-to))))
     (TEMPL adj-expletive-content-xp-TEMPL (XP (% W::CP (W::CTYPE W::s-to))))
     )
    ((meta-data :origin prehistoric :entry-date 20060824 :change-date 20090731 :comments csli-ts :wn ("hard%3:00:06"))
     (EXAMPLE "it's hard for me to see him")
     (LF-PARENT ONT::difficult)
     ;;     (TEMPL adj-theme-action-TEMPL (XP (% W::CP (W::CTYPE W::s-to))))
     (TEMPL adj-expletive-content-control-TEMPL)
     )
    ((meta-data :origin calo :entry-date 20031223 :change-date nil :wn ("hard%3:00:01") :comments html-purchasing-corpus)
     (LF-PARENT ONT::Texture-val)
     (TEMPL LESS-ADJ-TEMPL)
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL 
 :tags (:base500)
 :words (
	 (W::hard
	  (SENSES
	   ((LF-PARENT ONT::manner)
	    (TEMPL PRED-S-POST-TEMPL)
	    (example "Browne works hard")
	    (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
	    )
	   )	  
	  )
))

