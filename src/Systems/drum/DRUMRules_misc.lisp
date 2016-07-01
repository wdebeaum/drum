;;; See DRUMRules.lisp for comments

(in-package "IM")

(reset-im-rules 'drumMisc)

(mapcar #'(lambda (x) (add-im-rule x 'drumMisc))  
	'(
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; Ras targets Raf
	  
	  ; make sure AGENT/AFFECTED are empty.  Otherwise when there is AGENT/AFFECTED or when NEUTRAL/NEUTRAL1 are empty, this rule can vacuously match and wipe out existing AGENT/AFFECTED
          (((? reln ONT::EVENT) ?ev ONT::MODULATE :NEUTRAL ?n :NEUTRAL1 ?n1 :AGENT - :AFFECTED -) 
           -neutralToAgent>
           100
	   (?reln ?ev ONT::MODULATE
	    :rule -neutralToAgent
	    :AGENT ?n
	    :AFFECTED ?n1
	    :NEUTRAL -
	    :NEUTRAL1 -
	    )
          )


	  

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)
