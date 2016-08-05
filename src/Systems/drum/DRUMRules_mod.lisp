;;; See DRUMRules.lisp for comments

(in-package "IM")

;(setq *post-extraction-rules* '(drumMod))
(reset-im-rules 'drumMod)

(mapcar #'(lambda (x) (add-im-rule x 'drumMod))  
	'(

	  #|
;;;;;;;;;;
;; for some reason this rule doesn't work when put more towards the top, right after complex1.  Maybe complex1 doesn't work either?

	  ;; e.g., "we tested whether/if XYZ"
	  (((? reln ONT::F) ?!obj 
;	    (? word W::CLAUSE-CONDITION) :CONTENT ?!cont
	    (:* ONT::CLAUSE-CONDITION ?!w) :CONTENT ?!cont
	    )
	   ; this is so that it matches something more  
;	   ((? reln1 ONT::F ONT::THE ONT::BARE) ?!cont ?!t1)
;	   (?reln1 ?!cont ?!t1)   ; after term/event substitution ?reln1 becomes EVENT
	   (EVENT ?!cont ?!t1)   ; after term/event substitution ?reln1 becomes EVENT
	   -whether>
	   100
	   (EPI ?!obj ONT::CLAUSE-CONDITION
	    :event ?!cont
;	    :drum ?code
	    :rule -whether
	    )
	   (EVENT ?!cont ?!t1
	    :rule -whether-event
	    :epi ?!obj
	    :epi-2 (:* ONT::CLAUSE-CONDITION ?!w)
	    )
	   )
|#

	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; START: rules automatically generated from template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modality-event ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Modality-event rule
          ((ONT::EVENT ?ev
            ?type :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -modality>
           100
           (ONT::modality *1 ?!modality
            :rule -modality
            :event ?ev
            )
          )


          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule60_0_EXPERIENCER_FORMAL-modality>
           60
           (ONT::modality *1 ?!modality
            :rule -rule60_0_EXPERIENCER_FORMAL-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_FORMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_FORMAL rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_FORMAL-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-FORMAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-robust-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-robust-FORMAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_FORMAL rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-robustProSELF-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-robustPro-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-FORMAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-FORMAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustProSELF-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustPro-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-FORMAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robust-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robust-FORMAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robustProSELF-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_1 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robustPro-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_1-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-FORMAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robust-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robust-FORMAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robustProSELF-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODA1_2 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robustPro-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODA1_2-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-FORMAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robust-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robust-FORMAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robustProSELF-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_1 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robustPro-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_1-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-FORMAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robust-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robust-FORMAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robustProSELF-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_FORMAL-MODN1_2 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_FORMAL))
           -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robustPro-FORMAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_FORMAL-MODN1_2-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule60_0_EXPERIENCER_NEUTRAL-modality>
           60
           (ONT::modality *1 ?!modality
            :rule -rule60_0_EXPERIENCER_NEUTRAL-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_NEUTRAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_NEUTRAL-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-NEUTRAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-robust-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-robust-NEUTRAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-robustProSELF-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-robustPro-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-NEUTRAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-NEUTRAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustProSELF-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustPro-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-NEUTRAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-NEUTRAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robustProSELF-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robustPro-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_1-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-NEUTRAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-NEUTRAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robustProSELF-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robustPro-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODA1_2-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-NEUTRAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-NEUTRAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robustProSELF-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robustPro-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_1-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-whether>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-EXPERIENCER>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-NEUTRAL>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-EXPERIENCER>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-NEUTRAL-TERM
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-none>
           60
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robustProSELF-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule60_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::ASSUME ONT::SUPPOSE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule60_0_EXPERIENCER_NEUTRAL))
           -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robustPro-NEUTRAL>
           59
           (ONT::epi ?ev ?!eventName
            :rule -rule60_0_EXPERIENCER_NEUTRAL-MODN1_2-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule50_0_EXPERIENCER_FORMAL-modality>
           50
           (ONT::modality *1 ?!modality
            :rule -rule50_0_EXPERIENCER_FORMAL-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_FORMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_FORMAL rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_FORMAL-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-robust-FORMAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_FORMAL rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robust-FORMAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-POLARITY1-gd-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robust-FORMAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_1 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_1-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robust-FORMAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODA1_2 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODA1_2-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robust-FORMAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_1 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_1-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with EXPERIENCER and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with EXPERIENCER and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-FORMAL
            :EXPERIENCER -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with known FORMAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with known EXPERIENCER but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robust-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robust-FORMAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with neither EXPERIENCER nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-none
;            :EXPERIENCER ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with known EXPERIENCER but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robustProSELF-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_FORMAL-MODN1_2 rule with known EXPERIENCER but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_FORMAL))
           -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_FORMAL-MODN1_2-robustPro-FORMAL
            :EXPERIENCER ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule50_0_EXPERIENCER_NEUTRAL-modality>
           50
           (ONT::modality *1 ?!modality
            :rule -rule50_0_EXPERIENCER_NEUTRAL-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_NEUTRAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_NEUTRAL-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-POLARITY1-gd-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_1 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_1-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODA1_2 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODA1_2-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_1 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_1-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with EXPERIENCER and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with EXPERIENCER and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-whether
            :EXPERIENCER ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with only EXPERIENCER
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-EXPERIENCER>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-NEUTRAL
            :EXPERIENCER -
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known NEUTRAL but unknown proper name argument as EXPERIENCER
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-EXPERIENCER>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-EXPERIENCER
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-EXPERIENCER-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known EXPERIENCER but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with neither EXPERIENCER nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-none
;            :EXPERIENCER ?!ag
;            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known EXPERIENCER but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robustProSELF-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_EXPERIENCER_NEUTRAL-MODN1_2 rule with known EXPERIENCER but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::KNOW ONT::EXPECTATION ONT::BELIEVE ONT::DOUBT ONT::HYPOTHESIZE ) ?!w) :EXPERIENCER ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_EXPERIENCER_NEUTRAL))
           -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_EXPERIENCER_NEUTRAL-MODN1_2-robustPro-NEUTRAL
            :EXPERIENCER ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule50_0_NEUTRAL_FORMAL-modality>
           50
           (ONT::modality *1 ?!modality
            :rule -rule50_0_NEUTRAL_FORMAL-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_FORMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_FORMAL rule with NEUTRAL and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL rule with NEUTRAL and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_FORMAL-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-FORMAL
            :NEUTRAL -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL rule with known FORMAL but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-robust-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL rule with known NEUTRAL but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-robust-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-robust-FORMAL-TERM
            ))
          ;; rule50_0_NEUTRAL_FORMAL rule with neither NEUTRAL nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-none
;            :NEUTRAL ?!ag
;            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL rule with known NEUTRAL but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-robustProSELF-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL rule with known NEUTRAL but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-robustPro-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with NEUTRAL and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with NEUTRAL and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-FORMAL
            :NEUTRAL -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with known FORMAL but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robust-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with known NEUTRAL but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robust-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robust-FORMAL-TERM
            ))
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with neither NEUTRAL nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-none
;            :NEUTRAL ?!ag
;            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with known NEUTRAL but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robustProSELF-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-POLARITY1-gd rule with known NEUTRAL but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-POLARITY1-gd-robustPro-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_FORMAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with NEUTRAL and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with NEUTRAL and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-FORMAL
            :NEUTRAL -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with known FORMAL but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-robust-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with known NEUTRAL but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-robust-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-robust-FORMAL-TERM
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with neither NEUTRAL nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-none
;            :NEUTRAL ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with known NEUTRAL but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-robustProSELF-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_1 rule with known NEUTRAL but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_1-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_1-robustPro-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_FORMAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with NEUTRAL and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with NEUTRAL and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-FORMAL
            :NEUTRAL -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with known FORMAL but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-robust-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with known NEUTRAL but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-robust-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-robust-FORMAL-TERM
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with neither NEUTRAL nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-none
;            :NEUTRAL ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with known NEUTRAL but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-robustProSELF-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODA1_2 rule with known NEUTRAL but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODA1_2-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODA1_2-robustPro-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_FORMAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with NEUTRAL and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with NEUTRAL and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-FORMAL
            :NEUTRAL -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with known FORMAL but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-robust-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with known NEUTRAL but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-robust-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-robust-FORMAL-TERM
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with neither NEUTRAL nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-none
;            :NEUTRAL ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with known NEUTRAL but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-robustProSELF-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_1 rule with known NEUTRAL but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_1-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_1-robustPro-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_FORMAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with NEUTRAL and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with NEUTRAL and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-FORMAL
            :NEUTRAL -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with known FORMAL but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-robust-NEUTRAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with known NEUTRAL but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-robust-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-robust-FORMAL-TERM
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with neither NEUTRAL nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-none
;            :NEUTRAL ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with known NEUTRAL but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-robustProSELF-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_FORMAL-MODN1_2 rule with known NEUTRAL but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::POSSIBLY-TRUE ONT::APPEARS-TO-HAVE-PROPERTY ONT::BE-INCLINED ONT::CORRELATION ONT::REFUTE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_FORMAL))
           -rule50_0_NEUTRAL_FORMAL-MODN1_2-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_FORMAL-MODN1_2-robustPro-FORMAL
            :NEUTRAL ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule50_0_AGENT_FORMAL-modality>
           50
           (ONT::modality *1 ?!modality
            :rule -rule50_0_AGENT_FORMAL-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_FORMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_FORMAL rule with AGENT and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL rule with AGENT and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_FORMAL-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-robust-FORMAL-TERM
            ))
          ;; rule50_0_AGENT_FORMAL rule with neither AGENT nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL rule with known AGENT but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL rule with known AGENT but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_FORMAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with AGENT and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with AGENT and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :FORMAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-robust-FORMAL-TERM
            ))
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with neither AGENT nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-POLARITY1-gd rule with known AGENT but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-POLARITY1-gd-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_FORMAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with AGENT and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with AGENT and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-robust-FORMAL-TERM
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with neither AGENT nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with known AGENT but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_1 rule with known AGENT but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_1-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_1-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_FORMAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with AGENT and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with AGENT and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-robust-FORMAL-TERM
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with neither AGENT nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with known AGENT but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODA1_2 rule with known AGENT but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODA1_2-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODA1_2-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_FORMAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with AGENT and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with AGENT and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-robust-FORMAL-TERM
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with neither AGENT nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with known AGENT but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_1 rule with known AGENT but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_1-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_1-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_FORMAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with AGENT and FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with AGENT and FORMAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-AGENT
            :AGENT ?!ag
            :FORMAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with only FORMAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-FORMAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-FORMAL
            :AGENT -
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with known FORMAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with known AGENT but unknown proper name argument as FORMAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-robust-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-robust-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-robust-FORMAL-TERM
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with neither AGENT nor FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :FORMAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-none
;            :AGENT ?!ag
;            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with known AGENT but a reflexive pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-robustProSELF-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-robustProSELF-FORMAL
            :AGENT ?!ag
            :FORMAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_FORMAL-MODN1_2 rule with known AGENT but a pronoun as FORMAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::CONTEST ONT::REJECT ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_FORMAL))
           -rule50_0_AGENT_FORMAL-MODN1_2-robustPro-FORMAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_FORMAL-MODN1_2-robustPro-FORMAL
            :AGENT ?!ag
            :FORMAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule50_0_AGENT_NEUTRAL-modality>
           50
           (ONT::modality *1 ?!modality
            :rule -rule50_0_AGENT_NEUTRAL-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_NEUTRAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_NEUTRAL rule with AGENT and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL rule with AGENT and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_NEUTRAL-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-AGENT
            :AGENT ?!ag
            :NEUTRAL -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-NEUTRAL
            :AGENT -
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL rule with known NEUTRAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-robust-AGENT
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL rule with known AGENT but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-robust-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_AGENT_NEUTRAL rule with neither AGENT nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-none
;            :AGENT ?!ag
;            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL rule with known AGENT but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-robustProSELF-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL rule with known AGENT but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-robustPro-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with AGENT and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with AGENT and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-AGENT
            :AGENT ?!ag
            :NEUTRAL -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-NEUTRAL
            :AGENT -
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with known NEUTRAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robust-AGENT
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with known AGENT but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robust-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with neither AGENT nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-none
;            :AGENT ?!ag
;            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with known AGENT but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robustProSELF-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-POLARITY1-gd rule with known AGENT but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-POLARITY1-gd-robustPro-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_NEUTRAL-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with AGENT and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with AGENT and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-AGENT
            :AGENT ?!ag
            :NEUTRAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-NEUTRAL
            :AGENT -
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with known NEUTRAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-robust-AGENT
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with known AGENT but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-robust-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with neither AGENT nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-none
;            :AGENT ?!ag
;            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with known AGENT but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-robustProSELF-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_1 rule with known AGENT but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_1-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_1-robustPro-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_NEUTRAL-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with AGENT and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with AGENT and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-AGENT
            :AGENT ?!ag
            :NEUTRAL -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-NEUTRAL
            :AGENT -
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with known NEUTRAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-robust-AGENT
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with known AGENT but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-robust-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with neither AGENT nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-none
;            :AGENT ?!ag
;            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with known AGENT but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-robustProSELF-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODA1_2 rule with known AGENT but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODA1_2-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODA1_2-robustPro-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_NEUTRAL-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with AGENT and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with AGENT and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-AGENT
            :AGENT ?!ag
            :NEUTRAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-NEUTRAL
            :AGENT -
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with known NEUTRAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-robust-AGENT
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with known AGENT but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-robust-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with neither AGENT nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-none
;            :AGENT ?!ag
;            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with known AGENT but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-robustProSELF-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_1 rule with known AGENT but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_1-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_1-robustPro-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_AGENT_NEUTRAL-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with AGENT and NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with AGENT and NEUTRAL (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-whether
            :AGENT ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with only AGENT
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-AGENT>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-AGENT
            :AGENT ?!ag
            :NEUTRAL -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-NEUTRAL
            :AGENT -
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with known NEUTRAL but unknown proper name argument as AGENT
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-robust-AGENT>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-robust-AGENT
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-robust-AGENT-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with known AGENT but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-robust-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-robust-NEUTRAL-TERM
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with neither AGENT nor NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT - :NEUTRAL - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-none
;            :AGENT ?!ag
;            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with known AGENT but a reflexive pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-robustProSELF-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-robustProSELF-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_AGENT_NEUTRAL-MODN1_2 rule with known AGENT but a pronoun as NEUTRAL
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CONFIRM ONT::CONSENT ONT::ACCEPT-AGREE ONT::SHOW ONT::SCRUTINY ONT::QUESTIONING ONT::BECOMING-AWARE ONT::ACTIVE-PERCEPTION ONT::COME-TO-UNDERSTAND ONT::DETERMINE ) ?!w) :AGENT ?!ag :NEUTRAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_AGENT_NEUTRAL))
           -rule50_0_AGENT_NEUTRAL-MODN1_2-robustPro-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_AGENT_NEUTRAL-MODN1_2-robustPro-NEUTRAL
            :AGENT ?!ag
            :NEUTRAL ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule50_0_NEUTRAL_NEUTRAL1-modality>
           50
           (ONT::modality *1 ?!modality
            :rule -rule50_0_NEUTRAL_NEUTRAL1-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_NEUTRAL1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with NEUTRAL and NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with NEUTRAL and NEUTRAL1 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_NEUTRAL1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-NEUTRAL1
            :NEUTRAL -
            :NEUTRAL1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-robust-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with known NEUTRAL but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-robust-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-robust-NEUTRAL1-TERM
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with neither NEUTRAL nor NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL - :NEUTRAL1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-none
;            :NEUTRAL ?!ag
;            :NEUTRAL1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with known NEUTRAL but a reflexive pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-robustProSELF-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-robustProSELF-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1 rule with known NEUTRAL but a pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-robustPro-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-robustPro-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with NEUTRAL and NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with NEUTRAL and NEUTRAL1 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-NEUTRAL1
            :NEUTRAL -
            :NEUTRAL1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robust-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with known NEUTRAL but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robust-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robust-NEUTRAL1-TERM
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with neither NEUTRAL nor NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL - :NEUTRAL1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-none
;            :NEUTRAL ?!ag
;            :NEUTRAL1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with known NEUTRAL but a reflexive pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robustProSELF-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robustProSELF-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd rule with known NEUTRAL but a pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robustPro-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-POLARITY1-gd-robustPro-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with NEUTRAL and NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with NEUTRAL and NEUTRAL1 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-NEUTRAL1
            :NEUTRAL -
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robust-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with known NEUTRAL but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robust-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robust-NEUTRAL1-TERM
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with neither NEUTRAL nor NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL - :NEUTRAL1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-none
;            :NEUTRAL ?!ag
;            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with known NEUTRAL but a reflexive pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robustProSELF-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robustProSELF-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_1 rule with known NEUTRAL but a pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robustPro-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_1-robustPro-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with NEUTRAL and NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with NEUTRAL and NEUTRAL1 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-NEUTRAL1
            :NEUTRAL -
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robust-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with known NEUTRAL but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robust-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robust-NEUTRAL1-TERM
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with neither NEUTRAL nor NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL - :NEUTRAL1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-none
;            :NEUTRAL ?!ag
;            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with known NEUTRAL but a reflexive pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robustProSELF-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robustProSELF-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODA1_2 rule with known NEUTRAL but a pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robustPro-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODA1_2-robustPro-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with NEUTRAL and NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with NEUTRAL and NEUTRAL1 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-NEUTRAL1
            :NEUTRAL -
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robust-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with known NEUTRAL but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robust-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robust-NEUTRAL1-TERM
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with neither NEUTRAL nor NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL - :NEUTRAL1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-none
;            :NEUTRAL ?!ag
;            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with known NEUTRAL but a reflexive pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robustProSELF-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robustProSELF-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_1 rule with known NEUTRAL but a pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robustPro-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_1-robustPro-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with NEUTRAL and NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with NEUTRAL and NEUTRAL1 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-whether
            :NEUTRAL ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with only NEUTRAL
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-NEUTRAL>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-NEUTRAL1
            :NEUTRAL -
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robust-NEUTRAL>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robust-NEUTRAL
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robust-NEUTRAL-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with known NEUTRAL but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robust-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robust-NEUTRAL1-TERM
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with neither NEUTRAL nor NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL - :NEUTRAL1 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-none
;            :NEUTRAL ?!ag
;            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with known NEUTRAL but a reflexive pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robustProSELF-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robustProSELF-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL_NEUTRAL1-MODN1_2 rule with known NEUTRAL but a pronoun as NEUTRAL1
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::CORRELATION ONT::REFUTE ONT::RELATE ) ?!w) :NEUTRAL ?!ag :NEUTRAL1 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL_NEUTRAL1))
           -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robustPro-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL_NEUTRAL1-MODN1_2-robustPro-NEUTRAL1
            :NEUTRAL ?!ag
            :NEUTRAL1 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :MODALITY (:* ?!modality ?!mw))
           (ONT::SPEECHACT ?!sa ?!sa1)
           -rule50_0_NEUTRAL1_NEUTRAL2-modality>
           50
           (ONT::modality *1 ?!modality
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-modality
            :epi ?ev
            )
          )


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL1_NEUTRAL2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with NEUTRAL1 and NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with NEUTRAL1 and NEUTRAL2 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-whether
            :NEUTRAL1 ?!ag
            :FORMAL ?!cont
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 -
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with only NEUTRAL2
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-NEUTRAL2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-NEUTRAL2
            :NEUTRAL1 -
            :NEUTRAL2 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with known NEUTRAL2 but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-robust-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-robust-NEUTRAL1-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL2
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-robust-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-robust-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-robust-NEUTRAL2-TERM
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with neither NEUTRAL1 nor NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 - :NEUTRAL2 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-none
;            :NEUTRAL1 ?!ag
;            :NEUTRAL2 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with known NEUTRAL1 but a reflexive pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-robustProSELF-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-robustProSELF-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!ag
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2 rule with known NEUTRAL1 but a pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-robustPro-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-robustPro-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with NEUTRAL1 and NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with NEUTRAL1 and NEUTRAL2 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-whether
            :NEUTRAL1 ?!ag
            :FORMAL ?!cont
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 -
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with only NEUTRAL2
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-NEUTRAL2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-NEUTRAL2
            :NEUTRAL1 -
            :NEUTRAL2 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with known NEUTRAL2 but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robust-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robust-NEUTRAL1-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL2
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robust-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robust-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robust-NEUTRAL2-TERM
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with neither NEUTRAL1 nor NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 - :NEUTRAL2 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-none
;            :NEUTRAL1 ?!ag
;            :NEUTRAL2 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with known NEUTRAL1 but a reflexive pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robustProSELF-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robustProSELF-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!ag
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd rule with known NEUTRAL1 but a pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq )
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!mod (? m1 ONT::POLARITY-VAL-POSITIVE ONT::POLARITY-VAL-NEGATIVE) :FIGURE ?ev)
           (ONT::EVAL (symbolmap ?m1 ?!m1_new))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robustPro-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-POLARITY1-gd-robustPro-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :POLARITY ?!m1_new
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with NEUTRAL1 and NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with NEUTRAL1 and NEUTRAL2 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-whether
            :NEUTRAL1 ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with only NEUTRAL2
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-NEUTRAL2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-NEUTRAL2
            :NEUTRAL1 -
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with known NEUTRAL2 but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robust-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robust-NEUTRAL1-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL2
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robust-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robust-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robust-NEUTRAL2-TERM
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with neither NEUTRAL1 nor NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 - :NEUTRAL2 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-none
;            :NEUTRAL1 ?!ag
;            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with known NEUTRAL1 but a reflexive pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robustProSELF-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robustProSELF-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1 rule with known NEUTRAL1 but a pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modA))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robustPro-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_1-robustPro-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with NEUTRAL1 and NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with NEUTRAL1 and NEUTRAL2 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-whether
            :NEUTRAL1 ?!ag
            :FORMAL ?!cont
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 -
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with only NEUTRAL2
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-NEUTRAL2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-NEUTRAL2
            :NEUTRAL1 -
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with known NEUTRAL2 but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robust-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robust-NEUTRAL1-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL2
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robust-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robust-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robust-NEUTRAL2-TERM
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with neither NEUTRAL1 nor NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 - :NEUTRAL2 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-none
;            :NEUTRAL1 ?!ag
;            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with known NEUTRAL1 but a reflexive pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robustProSELF-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robustProSELF-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!ag
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2 rule with known NEUTRAL1 but a pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modA)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modA (? tmp ONT::MANNER-REFL ONT::SAME ONT::DIFFERENT ONT::CARDINALITY-VAL ONT::TRAJECTORY))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robustPro-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODA1_2-robustPro-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODA ?tmp ;?!modA
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with NEUTRAL1 and NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with NEUTRAL1 and NEUTRAL2 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-whether
            :NEUTRAL1 ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with only NEUTRAL2
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-NEUTRAL2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-NEUTRAL2
            :NEUTRAL1 -
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with known NEUTRAL2 but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robust-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robust-NEUTRAL1-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL2
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robust-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robust-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robust-NEUTRAL2-TERM
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with neither NEUTRAL1 nor NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 - :NEUTRAL2 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-none
;            :NEUTRAL1 ?!ag
;            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with known NEUTRAL1 but a reflexive pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robustProSELF-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robustProSELF-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1 rule with known NEUTRAL1 but a pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MODS (?!modN))
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robustPro-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_1-robustPro-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with NEUTRAL1 and NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with NEUTRAL1 and NEUTRAL2 (whether)
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :FORMAL ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::F) ?!obj ONT::CLAUSE-CONDITION :CONTENT ?!cont)
           (ONT::EVENT ?!cont ?t3)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-whether>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-whether
            :NEUTRAL1 ?!ag
            :FORMAL ?!cont
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!cont ?t3
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-whether-event
            :COND ONT::WHETHER
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with only NEUTRAL1
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-NEUTRAL1>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 -
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with only NEUTRAL2
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-NEUTRAL2>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-NEUTRAL2
            :NEUTRAL1 -
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with known NEUTRAL2 but unknown proper name argument as NEUTRAL1
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?!reln1 ?!ag (:* (? agtype ONT::REFERENTIAL-SEM) ?!agname) :name-of ?!name)
           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robust-NEUTRAL1>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robust-NEUTRAL1
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!ag ?agtype
            :name ?!agname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robust-NEUTRAL1-TERM
            )
           (ONT::EVENT ?!obj ?t2
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-event
            :epi ?ev
            )
           )
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with known NEUTRAL1 but unknown proper name argument as NEUTRAL2
          ;; (includes a TERM extraction for the unknown argument)
          (((? reln0  ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (?!reln1 ?!obj (:* (? objtype ONT::REFERENTIAL-SEM) ?!objname) :name-of ?!name)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robust-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robust-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
            (ONT::TERM ?!obj ?objtype
            :name ?!objname
            :drum -
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robust-NEUTRAL2-TERM
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with neither NEUTRAL1 nor NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 - :NEUTRAL2 - :AFFECTED - :AFFECTED1 - :AFFECTED-RESULT - :NEUTRAL - :NEUTRAL1 - :NEUTRAL2 - :FORMAL - :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
;           (?reln1 ?!ag ?t1)
;           ((? reln2 ONT::EVENT) ?!obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-none>
           50
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-none
;            :NEUTRAL1 ?!ag
;            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with known NEUTRAL1 but a reflexive pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           ((? tself ONT::TERM) ?!obj ?!objtype :PRO (? self W::ITSELF W::THEMSELVES))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robustProSELF-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robustProSELF-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!ag
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))
          ;; rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2 rule with known NEUTRAL1 but a pronoun as NEUTRAL2
          (((? reln0 ONT::F ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev
            (:* (? type ONT::RELATE ) ?!w) :NEUTRAL1 ?!ag :NEUTRAL2 ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :MANNER ?!modN)
           (?reln1 ?!ag ?t1)
           (ONT::TERM ?!obj ?!objtype :PRO W::IT)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!modN (? tmp ONT::MANNER-UNDO ONT::NEG))
           (ONT::EVAL (symbolmap ?type ?!eventName -rule50_0_NEUTRAL1_NEUTRAL2))
           -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robustPro-NEUTRAL2>
           49
           (ONT::epi ?ev ?!eventName
            :rule -rule50_0_NEUTRAL1_NEUTRAL2-MODN1_2-robustPro-NEUTRAL2
            :NEUTRAL1 ?!ag
            :NEUTRAL2 ?!obj
            :MODN ?tmp ;?!modN
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; END: rules automatically generated from template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	  )
	)


