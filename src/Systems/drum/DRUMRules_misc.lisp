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

#|
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; temporal extractions

          (;(ONT::F ?ev ONT::HAVE-PROPERTY :NEUTRAL ?!n :FORMAL ?!f)
	   ((? reln ONT::EVENT) ?ev ?!type)
;	   (ONT::F ?!m1 (:* (? mtype1 ONT::SPEED-VAL) ?!w1) :FIGURE ?ev)
	   (ONT::F ?!m1 (? mtype1 ONT::SPEED-VAL) :FIGURE ?ev)
           -temporalV>
           100
	   (?reln ?ev ?!type
	    :rule -temporalV
;	    :TIMEMOD ?!m1
	    :TIME-V ?mtype1
	    )
          )

          (;(ONT::F ?ev ONT::HAVE-PROPERTY :NEUTRAL ?!n :FORMAL ?!f)
	   ((? reln ONT::EVENT) ?ev ?!type)
;	   (ONT::F ?!m1 (:* (? mtype1 ONT::PERSISTENCE-VAL) ?!w1) :FIGURE ?ev)
	   (ONT::F ?!m1 (? mtype1 ONT::PERSISTENCE-VAL) :FIGURE ?ev)
           -temporalP>
           100
	   (?reln ?ev ?!type
	    :rule -temporalP
;	    :TIMEMOD ?!m1
	    :TIME-P ?mtype1
	    )
          )

	  #|
          (;(ONT::F ?ev ONT::HAVE-PROPERTY :NEUTRAL ?!n :FORMAL ?!f)
	   ((? reln ONT::EVENT) ?ev ?!type)
	   (ONT::F ?!m1 (? mtype1 ONT::SPEEDY ONT::TEMPORARY) :FIGURE ?ev)
	   (ONT::F ?!m2 (? mtype2 ONT::SPEEDY ONT::TEMPORARY) :FIGURE ?ev)
           -temporalDesc2>
           100
	   (?reln ?ev ?!type
	    :rule -temporalDesc2
;	    :TIMEMOD ?!m1
;	    :TIMEMOD ?!m2
	    :TIMEMOD ?mtype1
	    :TIMEMOD ?mtype2
	    )
          )
	  |#

          (((? reln ONT::EVENT) ?ev ONT::NO-CHANGE :AFFECTED ?!aff :EXTENT ?!ext)
	   (?reln1 ?!aff ?!affType)
	   (ONT::F ?!ext ONT::EVENT-DURATION-MODIFIER :FIGURE ?ev :GROUND ?!gd)
	   (?!sp1 ?!gd ONT::TIME-INTERVAL :MODS (?!m1))
;	   (ONT::F ?!m1 (:* ?!mtype1 ?!w1) :SCALE ?!mscale1)
	   (ONT::F ?!m1 ?!mtype1 :SCALE ?!mscale1)
           -temporalDesc>
           100
	   (?reln1 ?!aff ?!affType
	    :rule -temporalDesc
	    :TIME-P ?!mtype1
	    :LEVEL2 ONT::NO-CHANGE
	    )
          )
|#	  
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)
