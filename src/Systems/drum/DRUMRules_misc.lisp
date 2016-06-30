;;; See DRUMRules.lisp for comments

(in-package "IM")

(reset-im-rules 'drumMisc)

(mapcar #'(lambda (x) (add-im-rule x 'drumMisc))  
	'(
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (((? reln ONT::EVENT) ?ev ONT::MODULATE :NEUTRAL ?n :NEUTRAL1 ?n1)
           -neutralToAgent>
           100
	   (?reln ?ev ONT::MODULATE
	    :rule -neutralToAgent
	    :AGENT ?n
	    :AFFECTED ?n1
	    )
          )


	  

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)
