;;;;
;;;; W::differ
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
	      :tags (:base500)
	      :words (
		      (W::differ
		       (SENSES
			((lf-parent ont::differ)
			 (example "they differ")
			 (TEMPL NEUTRAL-NP-PLURAL-TEMPL)
			 (meta-data :origin bolt-e :entry-date 20120516 :comments top500)
			 (preference 0.98)
			 )
			((lf-parent ont::differ)
			 (example "it differs from that")
			 (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL (xp (% w::pp (w::ptype w::from))))
			 )
			))
		      ))

