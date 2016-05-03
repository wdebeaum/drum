;;;;
;;;; W::source
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 (W::source
   (SENSES
     ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s3)
      (LF-PARENT ONT::source)
      (templ other-reln-of-for-templ)
      )
     )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
	 (w::source
	  (senses ((lf-parent ont::device-component)
		   (Example "A source is a battery -- the electrical sense only")
		   ;; Preference  is low to make a relative reading more likely unless asked for specifically
		   (preference 0.97)
		   (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s)
		   )))
))

