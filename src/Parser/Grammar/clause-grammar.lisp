;;;;
;;;; clause-grammar.lisp
;;;;

(in-package w)

(parser::addlexicalcat 'aaux)
(parser::addlexicalcat 'part)
(parser::addLexicalCat 'uttword)

;;(define-foot-feature 'gap)

(parser::augment-grammar
 '((headfeatures
    (vp vform var agr neg sem iobj dobj comp3 part cont aux tense-pro gap subj subjvar modal auxname lex headcat transform subj-map template)
    (vp- vform var agr neg sem iobj dobj comp3 part cont   tense-pro aux modal auxname lex headcat transform subj-map advbl-needed
	 passive passive-map template) 
    (s vform var neg sem subjvar dobjvar cont  lex headcat transform)
    (cp vform var neg sem subjvar dobjvar cont  transform subj-map subj lex)
    (v lex sem lf neg var agr cont aux modal auxname ellipsis tma transform headcat)
    (aux vform var agr neg sem subj iobj dobj comp3 part cont  tense-pro lex headcat transform subj-map advbl-needed
	 passive passive-map ellipsis contraction auxname) 
    ;; (pp var)
    ;; (pps var)
    (utt neg qtype sem subjvar dobjvar cont lex headcat transform)
    (pred qtype focus gap filled transform headcat lex)
    )
   
   ;;  HANDLING START AND END OF UTTERANCE
   

   ;; If utterance begins with a filled pause  (retrict to having no punc on end yet to reduce spurious ambiguity)
   ;; TEST: Um the dog barked
   ((Utt (var ?v) (lf ?lf) (focus ?foc)
      (punc ?punc) (punctype ?p)
     (started  +)
     )
   -utt-fp-start> 1 ;; utterances frequently start with silence. But only start the utterances that go through the end of the input
    (fp)
    (head (utt (focus ?foc) (started -) (var ?v)
               (lf ?lf) (punctype ?p)))
   )
   
   ;;  here's  the end of utterance marker
   ((Utt (var ?v1) (lf ?lf) (focus ?foc)
      (punc +) (punctype ?p)
     (ended +)
     )
    -utt-end-only>
    (head (utt (focus ?foc) (ended -) (var ?v1)
               (lf ?lf) (punctype ?p)))
    (punc (lex end-of-utterance)))

   ;; utts ending with a silence
   ;; TEST: The dog barked um.
   ((Utt (var ?v1) (lf ?lf) (focus ?foc)
      (punc +) (punctype ?p)
     (ended +)
     )
    -utt-fp-end> 1
    (head (utt (focus ?foc) (ended -) (var ?v1)
               (lf ?lf) (punctype ?p)))
    (fp))

   
   ;; TOP-LEVEL UTTERANCE RULES

   ;; hello calo; We did it, calo
   ;; any utterance can be addressed to someone
   ;; The vocative feature prevents multiple vocative constructs, esp one at beginning and one at end
   ((Utt (var ?v) (vocative +) (lf (% SPEECHACT (var ?sv) (class ?cl) (constraint ?constraint))))
    -vocative-utt> .95    ;; nb: lowered below NP-CONJ rule to eliminate bad interps of utterances like 'John and Mary"
    (head (utt (focus ?foc) (var ?v) (vocative -) (lf (% SPEECHACT (var ?sv) (class (? cl ont::sa_TELL ont::sa_YN-QUESTION ont::SA-REQUEST)) (constraint ?con)))
	  (punctype -)))
    (np (var ?nv) (lf (% Description (Status (? nm gname Name))))
	(sem ($ f::phys-obj (f::intentional +) (f::object-function f::occupation)))
	)
    (add-to-conjunct (val (vocative ?nv)) (old ?con) (new ?constraint))
    )

  ;; calo find me a computer
   ;; any utterance can be addressed to someone
   ((Utt (var ?v) (lf (% SPEECHACT (var ?sv) (class ?cl) (constraint ?constraint)))
	 (vocative +))
    ;; lowering this rule allows many bad interpretations in coordops
    -vocative-utt2>  .95
    (np (var ?nv) (lf (% Description (Status (? nm gname Name))))
	(sem ($ f::phys-obj (f::intentional +) (f::object-function f::occupation)))
	)
    (head (utt (punctype -) (focus ?foc) (var ?v) (vocative -) 
	       (lf (% SPEECHACT (var ?sv) 
		      (class (? cl ont::sa_TELL ont::sa_YN-QUESTION ont::SA-REQUEST)) (constraint ?con))))
     )
    (add-to-conjunct (val (vocative ?nv)) (old ?con) (new ?constraint))
    )

   ;;There are oranges at Corning   
   ((Utt (var *) (punctype decl) (lf (% SPEECHACT (var *) (class ont::sa_TELL) (constraint (& (content ?v)))))) 
    -utt-s1>
    (head (s (stype decl) (gap -) (var ?v) (wh -) (lf ?lf) (advbl-needed -))))
   
   ;;  Punctuation: for typed input. PUNCTYPE must agree 
   ;; Myrosia 19/10/06 -- punctype need not always agree in typed dialogue
   ;; rather than trying to track the speechact via punctype, we are storing it
   ;; for further analysis in the future
   
   ((Utt  (var ?v) (focus ?foc)  ;; I changed the VAR from the punc to the UTT so that the LF is printed properly (why was it the other way?
     (punc +) (punctype ?p)
     (lf (% SPEECHACT (var ?sv) (class ?cl) (constraint ?constraint)))
     )
   -utt-punctuation>
   (head (utt (focus ?foc) (ended -) (var ?v) (punc -)
	  (lf (% SPEECHACT (var ?sv) (class ?cl) (constraint ?con)))
	  ))
    (punc (punctype ?p) (lex (? lex W::PUNC-EXCLAMATION-MARK W::punc-period W::PUNC-QUESTION-MARK W::PUNC-COLON W::ELLIPSES W::PUNC-COMMA)))
    (add-to-conjunct (val (punctype ?p)) (old ?con) (new ?constraint))
   )

    #||((Utt (var *) (punctype ?stype) (lf (% SPEECHACT (var *) (class ont::sa_TELL) (constraint (& (content ?v)))))) 
    -utt-seq1>
    (head (sseq (stype ?stype) (gap -) (var ?v) (wh -) 
		(lf ?lf) (advbl-needed -))))||#
   
   ;; QUESTIONS
   
   ;; e.g., Is the engine coming from Corning?
   ;; TEST: Is the dog barking?
   ;; TEST: Is the dog chasing the cat?
   ((Utt (LF (% SPEECHACT (class ont::sa_YN-QUESTION) 
		(constraint (& (content ?s-v))) (var *)))
     (var *) (punctype ?p)
     ) 
    -utt-ynq1>
    (head (s (stype ynq) (var ?s-v)  (gap -) (wh -) (advbl-needed -))))
   
   
   ;; TAG QUESTIONS
   ;; Note that the rules need to match tense in the TMA, not just the vform
   ;; because vform is hierarchical and so "past" and "pres" match in the vform,
   ;; but not in the TMA tense
   ;; TEST: The dog doesn't bark, does it?
   ;; TEST: The dog doesn't chase the cat, does it?
   ((Utt (LF (% SPEECHACT (class ont::sa_TAG-QUESTION) 
		(constraint (& (content ?s-v))) (var *)))
     (var *) (punctype ?p)
     ) 
    -utt-ynq-tag1>
    (head (s (stype decl) (var ?s-v)  (gap -) (wh -) (advbl-needed -)
	   (vform ?vform) 
	   (lf (% prop (tma (& (negation +)(tense ?tense)
			       ))))
	   ))
    (s (stype ynq) (vform ?vform) 
     (lf (% prop (class ont::ellipsis)
	    (tma (& (negation -) (tense ?tense)))
	    ))
     ))

   ;; TEST: The dog barks, doesn't it?
   ;; TEST: The dog chases the cat, doesn't it?
   ((Utt (LF (% SPEECHACT (class ont::sa_TAG-QUESTION) 
		(constraint (& (content ?s-v))) (var *)))
     (var *) (punctype ?p)
     ) 
    -utt-ynq-tag2>
    (head (s (stype decl) (var ?s-v)  (gap -) (wh -) (advbl-needed -)
	   (vform ?vform) 
	   (lf (% prop (tma (& (negation -) (tense ?tense)))))
	   ))
    (s (stype ynq) (vform ?vform) 
     (lf (% prop (class ont::ellipsis)
	    (tma (& (negation +) (tense ?tense)))
	    ))
    ))

   
   
   ;;  "or what" tag on questions
   ;; TEST: Did the dog bark or what?
   ;; TEST: Did the dog chase the cat or what?
   ((Utt (LF ?lf)
         (var ?v) (punctype ?p))
    -utt-ynq-OR-WHAT>
    (head (utt (LF (% SPEECHACT (class ont::sa_YN-QUESTION))) (LF ?lf) (var ?v)
	       (punctype ?p)))
    (word (lex or))
    (word (lex what)))

   ;;  "or not" tag on questions
   ;; TEST: Did the dog bark or not?
   ;; TEST: Did the dog chase the cat or not?
   ((Utt (LF ?lf)
         (var ?v) (punctype ?p))
    -utt-ynq-OR-NOT>
    (head (utt (LF (% SPEECHACT (class ont::sa_YN-QUESTION))) (LF ?lf) (var ?v)
	       (punctype ?p)))
    (word (lex or))
    (word (lex not)))
   
     ;;  e.g., hello with punctuation
   ((Utt (lf (% SPEECHACT (VAR *) (CLASS ?sa) (constraint (& (content (?lf :content ?lex))))))
         (var *) (uttword +))
    -utt4b>
    (head (uttword (lf (?lf)) (lex ?lex) (sa ?sa)))
    (punc (lex punc-comma) (var ?v1)))

     
   ;;  the one compound UTT rule - allows UTTWORD+ utterance to preceed other UTTS
   ;; TEST: hello hello
   ((Utt (sa-seq +) (lf (% SA-SEQ (VAR *) (CLASS ONT::SA-SEQ) (constraint (& (acts (?v1 ?v2))))))
         (var *))
    -uttword-utt> .96
    (utt (lf ?lf1) (var ?v1) (uttword +) (sa-seq -))
    (head (utt (LF ?lf2) (var ?v2) (sa-seq -))))


   ;;  Tag sentences, allowing uttword
   ;; TEST: the dog barks doesn't it hello
   ((Utt (sa-seq +) (lf (% SA-SEQ (VAR *) (CLASS ONT::SA-SEQ) (constraint (& (acts (?v1 ?v2))))))
         (var *))
    -utt-tag> .90 ;; lowering this from .97 because it was preferred over predicate adjectives, e.g that looks good 
    (utt (lf ?lf1) (var ?v1) (sa-seq -))
    (head (utt (LF ?lf2) (var ?v2) (uttword +) (sa-seq -))))
   

 
   ;; What do you think the red X means? That the battery is damaged
   ;;
   ((Utt (var *) (punctype decl) (no-post-adv +) (lf (% SPEECHACT (var *) (class ont::sa_PRED-FRAGMENT) (constraint (& (content ?v)))))) 
    -utt-s-that> 0.9
    (head (cp (ctype s-that-overt) (gap -) (var ?v) (wh -) (lf ?lf) (advbl-needed -))))

   ;;  REQUEST and SUGGESTIONS
   
    ;; e.g., How about from Avon to Corning? How about going through Corning?
   ;; TEST: How about chasing the cat?
   ((Utt (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_REQUEST-COMMENT) (constraint ?c))) (var *)
     ;; Myrosia commented out (lex how-about) 2006/21/02
     ;; lex is a head feature and if we specify it here, it clashes with the lex coming up from the S, where it is determined
     ;; either by an adverbial or an NP head.
     ;;(LEX HOW-ABOUT) ;; aug-trips -- we need a lex feature here -- can we get it from the s
     (punctype ?p))
    -how-about-utt>
    (head (s (stype how-about) (var ?v) (lf (constraint ?c)) (advbl-needed -))
          ))

   ;; TEST: How about chasing the cat?
   ((s (stype how-about) (lf (constraint (& (content ?v)))))
    -how-about-s>
    (word (LEX (? x how what)) (var ?v1))
    (word (LEX about)) 
    (head ((? cat np vp) (gap -) (var ?v) (lf ?lf) (case (? case obj -)))))

    ;; TEST: How about horizontally?
    ;; TEST: What about from the cat?
    ((s (stype how-about) (lf (constraint (& (content ?v)))))
    -how-about-advbl>
    (word (LEX (? x how what)) (var ?v1))
    (word (LEX about)) 
    (head (advbl (arg (% *PRO* (var *) (CLASS ONT::REFERENTIAL-SEM)))
		 (gap -) (var ?v) (lf ?lf) (case (? case obj -)))))
   
   
   
   ;; e.g., What if we use the helicopter instead
   ;; TEST: What if the dog barks?
   ;; TEST: What if the dog chases the cat?
   ;; TEST: What if the dog chases the cat instead?
   ((Utt (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_REQUEST-COMMENT) (constraint (& (content ?v))))) 
         (var *)(punctype ?p))
    -what-if1> 
    (word (LEX what))
    (word (LEX if))
    (head (s (gap -) (stype decl) (var ?v) (lf ?lf) (advbl-needed -))))
      
    ;; Typical Assertions
   
   ;; main s rule. We exclude the pp-word subjects because they have special rule
    
   ;; e.g., The boxcar is at Bath.
   ;; TEST: The dog barked.
   ;; TEST: The dog chased the cat.
   ((s (stype decl) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (LF ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
		       (pp-word -) (changeagr -))
	       )

     (advbl-needed ?avn)
     )
    -s1>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex) ;; lex needed for expletives?
      (pp-word -) (changeagr -))
    (head (vp (LF ?lf) (gap ?g)
              (template (? !x  lxm::propositional-equal-templ))
	      (subjvar ?npvar)
	      (subj (% np (sem ?npsem) (var ?npvar) (lex ?lex)))
	      (var ?v) (vform fin) (agr ?a)
	      (advbl-needed ?avn)
	      (neg ?neg)
	      )
	  )
    )

   ;;  special case S1 rule for abstract objects that can be equivalent to propositions (e.g., fact, belief, etc)
   ;;    these all take a SUBCAT-MAP to ONT::FORMAL - only verb that can do this so far is ONT::BE.
   ;;    e.g., the fact is he left angry

   ((s (stype decl) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (LF ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
		       (pp-word -) (changeagr -))
		    )

     (advbl-needed ?avn)
     )
    -s1-be-prop1>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex) ;; lex needed for expletives?
     (pp-word -) (changeagr -) (subcat-map ONT::FORMAL))
    (head (vp (LF ?lf) (gap ?g) (template lxm::propositional-equal-templ)
	      (subjvar ?npvar)
	      (subj (% np (sem ?npsem) (var ?npvar) (lex ?lex)))
	      (var ?v) (vform fin) (agr ?a)
	      (advbl-needed ?avn)
	      (neg ?neg)
	      )
     )
    )

   ;;  special case S1 rule for nominalizations that have a comp-map of ONT::FORMAL 
   ;;    e.g., the belief is he left angry

   ((s (stype decl) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (LF ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
		       (pp-word -) (changeagr -))
		    )

     (advbl-needed ?avn)
     )
    -s1-be-prop2>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex) ;; lex needed for expletives?
     (pp-word -) (changeagr -) (comp3-map ONT::FORMAL))
    (head (vp (LF ?lf) (gap ?g) (template lxm::propositional-equal-templ)
	      (subjvar ?npvar)
	      (subj (% np (sem ?npsem) (var ?npvar) (lex ?lex)))
	      (var ?v) (vform fin) (agr ?a)
	      (advbl-needed ?avn)
	      (neg ?neg)
	      )
     )
    )

    ;; commands with subjects: you/someone take a picture of me
   ;; TEST: Someone bark.
   ;; TEST: You chase the cat.
   ((s (stype imp) (var ?v) (subjvar ?npvar) (gap ?g) 
     (LF ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
		       (pp-word -) (changeagr -))
	       )
     (advbl-needed ?avn)
     )
    -command-imp-subj>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
     (lex (? l someone somebody)) ;; only someone, you allowed as subjects of imperatives
     (pp-word -) (changeagr -)
     )
    (head (vp (LF ?lf) (gap ?g)
	   
	   (subj (% np (sem ?subjsem) (var ?npvar) (lex ?lex))) 
	   (var ?v) (vform base)
	   (advbl-needed ?avn)
	   )
     )
    (unify (pattern ?subjsem) (value ?npsem))
    )
   
   ;; WH Question Utterances
   ;;

   ;; e.g., When will the engine arrive at Dansville? and "Where will the engine arrive",  but not "Where will the engine arrive at"
   ;; TEST: When will the dog bark?
   ;; TEST: When will the dog chase the cat?
   ;; TEST: Where will the dog chase the cat?
   ;; TEST: What will the dog chase?
   ((utt (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_WH-QUESTION) (constraint (& (content ?v) (focus ?foc)))))
         (var *) (qtype ?type) (punctype ?p))
    -wh-question1>
    (head (s (stype whq) (wh-var ?foc) (var ?v) (qtype (? type WHEN WHERE Q HOW-X -))(advbl-needed -))))

   
   ;; e.g., The engine will arive where?     Put it where?
   ;; TEST: The dog will chase what?
   ;; TEST: What will chase the cat?
   ((utt (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_WH-QUESTION) (constraint (& (content ?v) (focus ?foc)))))
         (var *) (qtype ?type) (punctype ?p))
    -decl-wh-question1> .96
    (head (s (stype (? st decl imp)) (WH Q) (GAP -) (wh-var ?foc) (var ?v) (advbl-needed -))))
   
   
   ;; e.g., When?
   ((utt (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_WH-QUESTION) (constraint (& (content ?v) (focus ?foc)))))
          (VAR *) (qtype ?type) (punctype ?p))
    -wh-question2> .95 ;; stop using in cca's
    (head (advbl (WH Q) (wh-var ?foc) 
                 (gap -) (var ?v) (qtype (? type WHEN WHERE Q HOW-X)))))
   
    ;; "presentational" here: here are the pictures
   ;; TEST: Here is the dog.
   ((s (stype decl) (var ?v) (gap -)
       (subj ?subj) 
     (lf (% prop (var ?v) (class ?c) (constraint ?constraint)
	    (sem ?ssem) (tma ?tma)
	    (transform ?transform)
	    ))
     (preadvbl +)
     )
     -s1-here> .9
    (advbl (lex here) 
     (argument (% S (sem ($ f::situation ))))
     (argument ?a)
     (role ?advrole) 
     (var ?advv)
     (arg ?v) (lf ?lf1)
     ;;(LF (% PROP (CLASS (:* ONT::SPATIAL-LOC HERE)))) ;; this is here to exclude the ont::to-loc, which otherwise comes up 1st
     (LF (% PROP (CLASS (:* ONT::HERE HERE)))) ;; this is here to exclude the ont::to-loc, which otherwise comes up 1st
     )
    ;; use a VP which expects "there" in the sense ont::exists
    (head (vp (LF ?lf) (gap -)
              
	   (subj (% np (sem ?subjsem) (var ?npvar) (lex there))) 
	   (var ?v) (vform fin) (agr ?vpagr)
	   (advbl-needed -)
	   (lf (% Prop (transform ?transform) (sem ?ssem) (class ?c) (class ont::exists) (constraint ?con) (tma ?tma)))
	   )
     )
    (add-to-conjunct (val (MODS ?advv)) (old ?con) (new ?constraint))
    )

   ;; to the left are the pictures
   ;; TEST: To the left is the dog.
   ((s (stype decl) (var ?v) (gap -)
       (subj ?subj) 
     (lf (% prop (var ?v) (class ?c) (constraint ?constraint)
	    (sem ?ssem) (tma ?tma)
	    (transform ?transform)
	    ))
     (preadvbl +)
     )
     -s1-spatial-loc> .93
    (advbl 
     (argument (% S (sem ($ f::situation ))))
     (argument ?a)
     (role ?advrole) 
     (var ?advv)
     (arg ?subjvar) (lf ?lf1)
     (LF (% PROP (CLASS ONT::SPATIAL-LOC)))
     )
    (head (s (stype ynq) (lf ?lf) (var ?v)	     
	     (advbl-needed -)
	     (subjvar ?subjvar) (dobjvar ?dobjvar)
	     (subj ?subj)
	     (gap (% pred (sem ?predsem) (var ?advv)))
	     (lf (% Prop (class ?c) (class ont::have-property) (sem ?ssem) (constraint ?con) (transform ?transform) (tma ?tma)))
	     )
       )
    (add-to-conjunct (val (MODS ?advv)) (old ?con) (new ?constraint))
    )
   
   ;; TEST: Why did the dog bark?
   ;; TEST: Why did the dog chase the cat?
   ((s (stype whq) (wh Q) (qtype ?qtype) (var ?v) (gap -) (wh-var ?wvar)
     (lf (% prop (var ?v) (class ?c) (constraint ?constraint)
	    (sem ?sem) (tma ?tma) (transform ?transform)
	    ))
     (preadvbl +) (subj ?subj) 
     (advbl-needed -)
     )
     -wh-setting1> .98 ;; really don't want this to apply before gaps
    (advbl
     ;;(argument (% S (sem ?sem) (sem ($ f::situation (f::type f::EVENTUALITY)))))
     (Var ?advv)
     (arg ?v) (wh Q) (wh-var ?wvar) (qtype ?qtype) (lf ?lf1)
     )
    (head (s (stype ynq) (var ?v) (gap -) (subj ?subj) (tag -)
	   (lf (% Prop (sem ?sem) (class ?c) (constraint ?con) (tma ?tma) (transform ?transform)))))
    (add-to-conjunct (val (MODS ?advv)) (old ?con) (new ?constraint)))

   ;; what time did it arrive?
   ;; TEST: What time did the dog bark?
   ;; TEST: What time did the dog chase the cat?
   ((s (stype whq) (wh Q) (qtype ?qtype) (wh-var ?tv) (var ?v) (gap -)
       (lf (% prop (var ?v) (class ?c) (constraint ?constraint)
	      (sem ?sem) (tma ?tma) (transform ?transform)
	      ))
     (preadvbl +) (subj ?subj) 
     (advbl-needed -)
       )
    -wh-setting2> .96 ;; don't want this to apply before gaps
    (np (wh q) (var ?tv) (sem ($ f::time (f::time-scale f::point))))  ;; should only be time points, not intervals
    (head (s (stype ynq) (var ?v) (gap -) (subj ?subj) (tag -)
	   (lf (% Prop (sem ?sem) (class ?c) (constraint ?con) (tma ?tma) (transform ?transform)))
	   (advbl-needed -)
	   )
     )
    (add-to-conjunct (val (MODS ?tv)) (old ?con) (new ?constraint))
    )

   ;; to feed into s-that-subjunctive: he recommends that the dog bark
   ;; TEST: The dog bark
   ;; TEST: The dog chase the cat.
   ((s (stype subjunctive) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (LF ?lf) (subj (% np (sem ?npsem) (var ?npvar) ;(agr ?a)
		       (case (? case sub -)) 
		       (pp-word -) (changeagr -))
		    )
     (advbl-needed ?avn)
     )
    -s-subjunctive>  .94 ;; only use if necessary 
    (np (sem ?npsem) (var ?npvar) ;(agr ?a)
	(case (? case sub -)) (lex ?lex) ;; lex needed for expletives?
      (pp-word -) (changeagr -))
    (head (vp (LF ?lf) (gap ?g)
              
	      (subjvar ?npvar)
	      (subj (% np (sem ?subjsem) (var ?npvar) (lex ?lex))) 
	   (var ?v) (vform base) ;(agr ?a)
	   (advbl-needed ?avn)
	   (neg ?neg)
	   )
     )
    (unify (pattern ?subjsem) (value ?npsem))
    )

      ;;============================================================================
   ;;   RELATIVE CLAUSES

   ;; e.g., (the route) which went from Corning to Avon
   ;; TEST: The dog that barked.
   ;; TEST: The dog that chased the cat.
   ((CP (ctype relc) (arg ?arg) (argsem ?argsem) (var ?v) (gap -) ;; (gap ?g)
;;;     (lf (% prop (var ?v) (arg ?arg) (class ?c) 
;;;	    (constraint ?con) (sem ?sem) (transform ?transform)
;;;	    (tma ?tma)
;;;	    )
     (lf ?lf)
     ;; aug-trips
     (lex ?vlex) (headcat ?hcat)
     ;; (lex ?plex) (headcat ?vcomp)  ;; non-aug-trips settings
     (agr ?agr)
     )
    -rel1> 
    (pro (wh (? wh R)) (sem ?argsem) (lex ?plex)
     (agr ?agr) (CASE SUB) (headcat ?vcomp))
    (head (vp (subj (% np (var ?arg) (sem ?argsem) (sem ($ ?!type)) )) (agr ?agr) 
	      (gap -)  ;; my guess is there are some eliiptical glosses that motivated allowing a gap here, but I'm deleting it for now(gap ?g) 
	      (WH -)
	      (class ?c) (constraint ?con) (vform fin) (tma ?tma)
	      (sem ?sem) (lf ?lf)
	      (transform ?transform) (lex ?vlex)
	      (advbl-needed -)
	      (headcat ?hcat) ;; aug-trips
	   )))

   #|
   ;; TEST: The man whose dog barked
   ((CP (ctype rel-whose) (arg ?arg) (argsem ?argsem)
	(gap -) (lf ?lf) (wh -)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -rel-whose>
    (NP (sem ?argsem) (agr ?agr) (CASE SUB) (wh -) (var ?arg)
	(lf (% description (constraint (& (assoc-poss (% *pro* (w::status w::Pro)
							 (constraint (& (proform w::whose)))		 
							 )))))) 
     )
    (head (vp (subj (% np (var ?arg) (sem ?argsem) (sem ($ ?!type)) (lf ?nplf) )) (agr ?agr) (gap -) ;;(WH -)
	   (class ?c) (constraint ?con) (vform fin) (tma ?tma)
	   (sem ?sem) (lf ?lf)
	   (transform ?transform) (lex ?vlex)
	   (advbl-needed -)
	   (headcat ?hcat) ;; aug-trips
	   )))
   |#
   
   ;; TEST: The man whose dog barked
   ((CP (ctype rel-whose) ;(arg ?arg) (argsem ?argsem)
	(gap -) (lf ?lf) (wh R) (wh-var ?whv)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -rel-whose>
    (NP (sem ?argsem) (agr ?agr) (CASE SUB) (wh R) (wh-var ?whv) (var ?arg)
     )
    (head (vp (subj (% np (var ?arg) (sem ?argsem) (sem ($ ?!type)) (lf ?nplf) )) (agr ?agr) (gap -) ;;(WH -)
	   (class ?c) (constraint ?con) (vform fin) (tma ?tma)
	   (sem ?sem) (lf ?lf) 
	   (transform ?transform) (lex ?vlex)
	   (advbl-needed -)
	   (headcat ?hcat) ;; aug-trips
	   )))
   
   ;; e.g., (the train) that I moved
   ;; TEST: The cat that the dog chased.
   ((CP (ctype relc) (arg ?arg) (argsem ?argsem) (gap -)
     (lf ?lf)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     (agr ?agr)
     )
    -rel2> ;; .95
    (pro (wh R) (case ?case) (sem ?argsem) (agr ?agr) 
     (lex ?l) ;;(lex (? l that which who whom)) 
     (headcat ?vcomp))
    (head (s (gap (% np (var ?arg) (sem ?argsem) (case ?case) (agr ?agr))) 
	      (adj-s-prepost -)
	   (WH -) (stype decl)
	   (vform fin) (lf ?lf)
	   (preadvbl -)
	   (advbl-needed -)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   )))

   ;; TEST: The person on whom he relies
   ((CP (ctype relc) (arg ?arg) (argsem ?argsem) (gap -)
     (lf ?lf)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -rel3> ;;.95
    (prep (lex ?pt))
    (pro (wh R) (sem ?argsem) (agr ?agr) (lex ?l) (headcat ?vcomp))
    ;; Myrosia: note that a better representation would be a PP
    ;; (PP (wh R) (ptype ?pt) (sem ?argsem) (agr ?agr) (lex ?l) (headcat ?vcomp))
    ;; but wh-pro1 rule only allows (wh q) for pronouns, throwing the relative pronouns away
    ;; we may want to reconsider this in the future
    (head (s (gap (% pp (ptype ?pt) (var ?arg) (sem ?argsem) (agr ?agr))) 
	      (adj-s-prepost -)
	   (WH -) (stype decl)
	   (vform fin) (lf ?lf)
	   (preadvbl -)
	   (advbl-needed -)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   )))
   
   ;; TEST: The city in which I live
   ;; FIXME:: the argsem isn't being propagated for (WH R) adverbials. So 
   ((CP (ctype relc) (arg ?srole) (argsem ?srolesem) (gap -)
     (lf ?newlf) (agr ?newagr)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -rel4> .92
    ;; FIXME -- this is really hacked to transfer the role from the wh pronoun to the NP it will eventually modify
    ;; WH R should only come out of advbl-binary-relative
    (advbl-r (ATYPE PRE) (ARGUMENT (% S (sem ?argsem))) 
     (GAP -) (WH R)   
     (ARG ?v) (VAR ?mod)
     (subcatsem ?srolesem)
     (ARG2 ?srole)
     )
    (head (S (VAR ?v) (LF ?lf) (LF (% prop (constraint ?con))) (SEM ?argsem) (aux -)
	      (adj-s-prepost -)
	    (tma ?tma) (stype (? stp decl imp how-about))
	   (wh -) (stype decl) (vform fin) (preadvbl -) (lex ?hlex) (headcat ?hcat)
	   )
     )
    (add-to-conjunct (val (MODS ?mod)) (old ?con) (new ?newcon))
    (change-feature-values (OLD ?lf) (NEW ?newlf) (newvalues ((CONSTRAINT ?newcon))))
    )
  
   ;;  WH-term as setting in an S structure
  
   ((CP (ctype relc) (arg ?arg) (argsem ?advsem) (gap -) (WH -) (agr ?newagr)
     (lf (% prop (var ?v) (class ?c) 
	    (constraint ?constraint) (tma ?tma)
	    (sem ?sem) (transform ?transform)
;;	    (lex ?l) (headcat ?advlh)   ;; non-aug-trips settings
	    (lex ?hlex) (headcat ?hcat) ;; aug-trips
	    )))
   -relc-setting-norole> ;;0.95
   (advbl-r
	  (var ?npvar) (arg2 ?arg) (arg ?v)
	  (argument (% S (sem ?argsem))) (WH Q)
	  ;;(sem ?npsem)
	  (role ?advrole) (subcatsem ?advsem)
	  (wh-var ?foc) (qtype ?qtype) (lf ?lf1)
	  (lex ?l) (headcat ?advlh)
	  )
   (head (s (stype decl) (sem ?argsem) (var ?s-v) (lf ?lf-s) (gap -)
	     (adj-s-prepost -)
	    (lf (% prop (var ?v) (class ?c) 
		   (constraint ?cons) (tma ?tma)
		   (sem ?sem) (transform ?transform)
		   ))
	  
	  (advbl-needed -)
	  (lex ?hlex) (headcat ?hcat) ;; aug-trips
	  )
	 )     
   ;;(role-UNIFY (arg0 ?advrole) (arg1 ?advsem) (arg2 ?roles) (arg3 ?con))
   (add-to-conjunct (val (MODS ?npvar)) (old ?cons) (new ?constraint)))

   
   
   ;; e.g., (the train) I moved
   ;; TEST: The cat the dog chased.
   ((CP (ctype relc) (arg ?arg) (argsem ?argsem) (gap -) (reduced +)
     (lf ?lf) (agr ?newagr)
     (lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -rel-reduced1-that> .92
    (head (s (gap (% np (var ?arg) (sem ?argsem) (case ?case) (agr ?agr))) 
	     (adj-s-prepost -)
           (WH -) (stype decl)
           (vform fin) (lf ?lf)
           (preadvbl -)
           (advbl-needed -)
           (lex ?hlex) (headcat ?hcat) ;; aug-trips
           )))
   
   ;;  e.g., (the train) going to Avon, (the train) loaded with oranges 
   ;; TEST: The dog barking.
   ;; TEST: The dog chasing the cat.
   ((CP (ctype relc) (arg ?arg) (argsem ?argsem) (gap -) (reduced +)
	(LF ?LF) (agr ?agr)
	(lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -rel-reduced2> .97
    (head (vp (var ?v)
	   (subj (% np (lex ?lex) (var ?arg) (sem ?argsem) (sem ($ ?!type)))) 	   
	   (agr ?agr) 
           (gap -)
	   (WH -)
	   (class ?c) 
	   (constraint ?con) (vform (? vform ing passive))
	   (sem ?sem) (tma ?tma)
	   (transform ?transform)
	   (lf ?lf)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   )))

;;  e.g., (the train) not going to Avon, (the train) not loaded with oranges 
   ;; TEST: The dog not barking.
   ;; TEST: The dog not chasing the cat.
   ((CP (ctype relc) (arg ?arg) (argsem ?argsem) (gap -) (reduced +)
; this I think also works, part 1
;	(lf (% prop (var ?v) (constraint ?con) (class ?c) (tma ?newtma)))
	(lf ?newlf) (agr ?agr)
	(lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -rel-reduced2-neg> .97
    (neg (lex w::not))
    (head (vp (var ?v)
	   (subj (% np (lex ?lex) (var ?arg) (sem ?argsem) (sem ($ ?!type)))) 	   
	   (agr ?agr) 
           (gap -)
	   (WH -)
	   (class ?c) 
	   (constraint ?con) (vform (? vform ing passive))
	   (sem ?sem) (tma ?tma)
	   (transform ?transform)
	   (lf ?lf)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   ))
; this I think also works, part 2
;    (append-conjuncts (conj1 (& (NEGATION +))) (conj2 ?tma) (new ?newtma))
     (add-to-conjunct (val (NEGATION +)) (old ?tma) (new ?newtma))
     (change-feature-values (OLD ?lf) (NEW ?newlf) (newvalues ((tma ?newtma))))
    )
   
   ;; "that" and "if" sentences, used embedded, as complements.

   ;; e.g., That the train arrives.
   ;; TEST: I know that the dog barks.
   ;; TEST: I know that the dog chased the cat.
   ((CP (ctype s-that-overt) (gap -) 
     (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?newtma)))  ;; (lf ?lf) ;; (lex ?lex) (headcat ?vcomp)) ;; non-aug-trips settings
	(lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -s-that-overt>
    (word (lex (? lx that)) ;; if whether))   these are handled by CTYPE S-IF rule   JFA 3/07
	  (headcat ?vcomp))
    (head (s (stype decl) (main -) (gap -) 
	     (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?tma))) ;;(lf ?lf) 
	      (adj-s-prepost -)
	     (vform fin)
	     (preadvbl -)
	     (advbl-needed -)
	     (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   ))
     (append-conjuncts (conj1 (& (prop +))) (conj2 ?tma) (new ?newtma))
     )   

   ;; TEST: I prefer that the dog bark.
   ((CP (ctype s-that-subjunctive) (gap -)
     (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?newtma))) ;;(lf ?lf) 
     (lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -s-that-subjunctive>
    (word (lex that)
	  (headcat ?vcomp))
    (head (s (stype subjunctive) (main -) (gap -) 
	     (adj-s-prepost -)
	     (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?tma))) ;;(lf ?lf) 
	     (vform base)
	     (preadvbl -)
	     (advbl-needed -)
	     (lex ?hlex) (headcat ?hcat) ;; aug-trips
	     ))
    (append-conjuncts (conj1 (& (prop +))) (conj2 ?tma) (new ?newtma))
    )
   
   
   ;; I guess that if the train arrives, we go
    ((CP (ctype s-that-overt) (gap -) 
      (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?newtma))) ;;(lf ?lf)
;;	 (lex that) (headcat ?vcomp) ;; non-aug-trips settings
	 (lex ?hlex) (headcat ?hcat) ;; aug-trips
	 ) 
    -s-that-overt2>
    (word (lex that) (headcat ?vcomp))
     (head (s (stype decl) (main -) (gap -) 
	       (adj-s-prepost -)
	      (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?tma))) ;;(lf ?lf) 
	      (vform fin)
	      (preadvbl +)
	      (advbl-needed -)
	      (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   ))
      (append-conjuncts (conj1 (& (prop +))) (conj2 ?tma) (new ?newtma)))
   
   ;; e.g., (i know) the train arrived.
   ;; TEST: I know the dog barked.
   ;; TEST: I know the dog chased the cat.
   ((CP (ctype s-that-missing) (gap -) 
      (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?newtma))) ;;(lf ?lf)
	(lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -s-that-missing> .98
    (head (s (stype decl) (main -) (wh -) (gap -) 
	      (adj-s-prepost -)
	      (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?tma))) ;;(lf ?lf) 
	      (vform fin)
	      ;;(preadvbl -)
	      (advbl-needed -)
	      (lex ?hlex) (headcat ?hcat) ;; aug-trips
	      ))
    (append-conjuncts (conj1 (& (prop +))) (conj2 ?tma) (new ?newtma))
    )
   
   ;; 'go-and' constructions
   ((cp (ctype s-and) (var ?v)
	(lex ?lex) (headcat ?hcat) ;; aug-trips
     (subj (% (? cat NP PP) (ptype (? ptype for -)) (case (? case sub obj -)) 
	      (agr ?subjagr) (sem ?subjsem) (var ?subjvar) (transform ?subjtransform)
	;;      (lex ?lex) ;; non-aug-trips settings
	      ))
     (gap ?g)
     (LF ?lf))
     -sand>
     (word (lex and))
     (head (vp (lf ?lf) 
	    (aux -)
	    (gap ?g)
	    (subj (% NP (sem ?subjsem) ;; (lex ?lex) ;; non-aug-trips setting
		     (sem ($ ?!type)) (var ?subjvar) (agr ?subjagr) (transform ?subjtransform)))
	    (vform base)
	    (advbl-needed -)
	    (lex ?lex) (headcat ?hcat) ;; aug-trips
	    )))

   ;;aug-trips: non-aug-trips settings have lex and hcat in subj-np
   ;;   INFINITIVE SENTENCES

   ;; e.g., To know the train arrived.
   ;; aux - to exclude cases like "to can ..."
   ;; TEST: The dog wants to bark
   ((cp (ctype s-to) (var ?v) ;;(subj ?subj) 
     #||(subj (% (? cat NP PP) (ptype (? ptype for -)) (case (? case sub obj -)) (lex ?lex)
	      (agr ?subjagr) 
	      (sem ?subjsem) (var ?subjvar) (transform ?subjtransform)
	      ))||#
     (subj ?subj)
     (dobj ?dobj)
     (gap ?g)
     (LF ?lf) ;; (lex to) (headcat ?vinf)) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat)) ;; aug-trips     
     -to1>
     (infinitival-to (lex to) (headcat ?vinf))
    (head (vp (lf ?lf) 
	   (advbl-needed -)
	   (gap ?g)
	   (subj ?subj)
	   (vform base)
	   (dobj ?dobj) 
	   (lex ?hlex) (headcat ?hcat)
	   (be-there -)
	   )))
   
   ;; We need a special rule for cases like "to be taken", because it is aux be, but for passives it's fine
   ;; TEST: They appear to be broken
    ((cp (ctype s-to) (var ?v) (subj ?subj) (gap ?g)
     (LF ?lf)
;;     (lex to) (headcat ?vinf) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
     -to1-passiv>
     (infinitival-to (lex to) (headcat ?vinf))
     (head (vp (lf ?lf) 
	    (aux +)    ;; works as long as its not -
	    (gap ?g) (subj ?subj) (vform base)
	    (lex ?hlex) (headcat ?hcat) ;; aug-trips
	    (tma (% & (passive +) (progr -)))
	    (be-there -)
	    ))
     )
   
   ;; e.g., For the train to arrive
   ((cp (ctype s-to) (var ?v) (subj ?subj) (gap ?g)
     (LF ?lf)
 ;;    (lex to) (headcat ?vinf) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -for-to1>
    (word (lex for))
    (np (sem ?subjsem) (var ?npvar) (case (? case obj -)))
    (infinitival-to (lex to) (headcat ?vinf) )
    (head (vp (LF ?lf) (gap ?g)
	      (sem ?sem)
	      (subj ?subj) (subj (% np (lex ?lex) (sem ?subjsem) (sem ($ ?!type)) (var ?npvar)))
	      (vform base)
	      (lex ?hlex) (headcat ?hcat) ;; aug-trips
	      (be-there -)
	      )))

      ; TEST: Tell him to not bark
     ((cp (ctype s-to) (var ?v) (gap ?g)
     (lex ?hlex) (headcat ?hcat)
     (subj ?subj) (dobj ?dobj)
     (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?newtma)))
     )
    -s-to-neg>
    (infinitival-to (lex to) (headcat ?vinf))
    (word (lex not))
    (head (vp (class ?lf)  (var ?v)
	      (constraint ?con)
	      (advbl-needed -)
	      (gap ?g) (tma ?tma)
	      (subj ?subj)
	      (vform base)
	      (dobj ?dobj) (dobj (% ?s3  (case (? dcase obj -)) (gap -)))
	      (lex ?hlex) (headcat ?hcat) ;; aug-trips
	    ))
    (append-conjuncts (conj1 (& (vform base) (NEGATION +))) (conj2 ?tma) (new ?newtma))
    )

   ;; V from Xing, e.g. prevent, prohibit, forbid where we want the objcontrol role on the ing form
   ;; TEST: He prevented the dog from barking.
   ((cp (ctype s-from-ing) (var ?v) (ptype ?ptype)
     (subj ?subj)
     (dobj ?dobj)
     (gap ?g)
     (LF ?lf) ;; (lex to) (headcat ?vinf)) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat)) ;; aug-trips     
     -prep-ing>
     (prep (lex ?ptype) (headcat ?ving))
     (head (vp (lf ?lf)
	       (advbl-needed -)
	       (gap ?g)
	       (subj ?subj)
	       (vform ING)
	       (dobj ?dobj) 
	       (lex ?hlex) (headcat ?hcat) ;; aug-trips
	       )))
   
     ;;  PRED rules: 
     ;;  PREDs are used for BE verbs, and create a constituent that it similar
     ;;    to a lambda experssion in the logical form.
     ;;   adjectives, adverbials, indefinite descriptions or vps can form PREDs
     ;;  NB: we must put a VAR feature in the LF of pred's if it it to be picked up in the OBJECTS
     ;;     slot of the logical form  - and it can't be the same as the VAR of an NP or 
     ;;      any other major constituent
     ;; e.g., (the train) is ORANGE
     ;; TEST: The dog is short.
     ((pred (lf ?lf) (arg ?arg) (ARGUMENT ?argument) (VAR ?v) (agr ?agr) (sem ?sem)    
            (filled -) (adjectival +)
            )
      -pred-adj> 1
      (head (adjp (lf ?lf) (VAR ?v) (arg ?arg) (ARGUMENT ?argument) (argument (% ?argcat (var ?arg))) ;;(wh -) eliminated to allow "how red"
	     (set-modifier -) ;; numbers are set-modifier +, and they don't behave as normal adjps in predicates
	     (atype (? atp central predicative-only))
	     ;; MD 2008/17/07 Eliminated cases with positive post-subcat, they should only happen when an adjective is looking for an argument after an NP, not possible in the PRED situation
	     (post-subcat -) 
	     ))
      )

   ;; TEST: The dog is from the store.
     ((pred (lf ?lf) (arg ?arg) (ARGUMENT ?argument) (VAR ?v) (agr ?agr) (sem ?sem)    
       (filled -) (adjectival -) (gap ?gap)
       (wh ?wh)
       )
      -pred-advbl> 0.98 ;; don't use it if a regular advbl interpretation is possible
      ;; MD: argument should be % NP, if an adverbial does not apply to NPs, don't use it
      (head (advbl (lf ?lf) (VAR ?v) (arg ?arg) (argument ?argument) (argument (% NP (var ?arg)))
	     (wh (? wh - q)) (sort (? srt binary-constraint pp-word pred))
	     (gap ?gap)
	     ))
      )

     ;; e.g., role NPs can be predicates "We remained friends"
     
     ((pred (arg ?arg) (var ?v)  (sem ?sem)
            (lf (% prop (status F) (arg ?arg) (VAR ?v) 
		   (CLASS ?c) (CONSTRAINT (& (of ?arg)))))
            (argument ?argument)
            (filled -)
            )
      -pred4>
      (head (np (sem ?sem) (var ?v) (case (? case obj -))
          (lf (% DESCRIPTION (STATUS (? x INDEFINITE BARE indefinite-plural)) (sem ($ F::PHYS-OBJ (F::TYPE ont::role-reln)))
	         (CLASS ?c) (CONSTRAINT ?constr))))
      ))

   ;; a construction that is limited to pred of emotional state 
   ;;  e.g., he is happy/sad/amused that the dog barked
   
   ((pred (arg ?arg) (var ?v)  (sem ?sem)
            (lf (% prop  (VAR ?v) 
		   (CLASS (? c ont::emotional-val ont::evoke-emotion))
		   (CONSTRAINT ?constraint)))
            (argument ?argument)
            (filled -)
            )
     -emotional-state-reason>
     (head (pred (var ?v) (arg ?arg) (sem ?sem)
            (lf (% prop (VAR ?v) 
		   (CLASS (? c ont::emotional-val ont::evoke-emotion))
		   (CONSTRAINT ?con)))
            (argument ?argument)
            (filled -)
            ))
    (cp (ctype s-that) (var ?reason))
    (add-to-conjunct (val (REASON ?reason)) (old ?con) (new ?constraint)))
        
     ;; Add tense information (past, present, or future)
     ;; TEST: The dog barked.
     ;; TEST: The dog barks.
     ;; TEST: The dog will bark.
     ((vp (LF (% prop (class ?c) (var ?v) (constraint ?constraint) (tma ?newtma) 
	         (transform ?transf) (sem ?sem)))
          (class ?c) (var ?v) (constraint ?constraint) (tma ?newtma)  (sem ?sem) (transform ?transf)
           (vform (? vf PAST PRES FUT))
          )
      -vp-tns+> 1.0 ;; 1 because we are simply adding tense info, not really using more rules
     (head
      (vp- (class ?c)  (constraint ?constraint) (tma ?tma1) 
        (vform (? vf PAST PRES FUT)) (subjvar ?sv)
       (advbl-needed -)
       )) 
     (add-to-conjunct (val (TENSE ?vf)) (old ?tma1) (new ?newtma))
     ) 
   
   ;; untensed vp
   ;; TEST: The dog chased the cat to scare it.
   ((vp (LF (% prop (class ?c) (var ?v) (constraint ?constraint) (transform ?transf) (tma ?newtma) (sem ?sem)))
       (class ?c) (var ?v) (constraint ?constraint) (tma ?newtma) (sem ?sem) (transform ?transf) )
     -vp-tns-> 1.0
     (head
      (vp- (class ?c) (constraint ?constraint) (tma ?tma) (subj ?subj) 
       (vform (? vf BASE PASSIVE ING PASTPART))
       (advbl-needed -)
       ))
    (add-to-conjunct (val (vform ?vf)) (old ?tma) (new ?newtma))
    )
    
     ;; VP RULES 
     ;; TEST: He said the dog barked.
    #|
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?gap) 
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
                       (mod ?prefix)
		       ))
      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role> 1  ;;  rule with no indirect object
     (head (v (aux -) ;; main verbs only
	    (lf ?c)
	    (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (tma ?tma)
	    ;; (subj (? subj (% ?s1 (var ?subjvar))))
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	   ;; (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj) (dobj (% ?s3 (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))	    
	    ;; we allow a possible gap in the DOBJ NP e.g., "what did he thwart the passage of"
	    (Comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    (prefix ?prefix)
	   ))
     ?iobj     
     ?dobj
     ?part
     ?comp
     )
   |#

    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?gap) 
      (constraint ?newc)
      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role> 1  ;;  rule with no indirect object
     (head (v (aux -) ;; main verbs only
	    (lf ?c)
	    (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (tma ?tma)
	    ;; (subj (? subj (% ?s1 (var ?subjvar))))
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	   ;; (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj) (dobj (% ?s3 (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))	    
	    ;; we allow a possible gap in the DOBJ NP e.g., "what did he thwart the passage of"
	    (Comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    (restr ?prefix)
	   ))
     ?iobj     
     ?dobj
     ?part
     ?comp
     (append-conjuncts (conj1 ?prefix) (conj2 (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       ))
		       (new ?newc))
     
     )
    
   ;; CASE with a gap in the COMP3
   ;;  e.g., what did he start to cook
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?!gap) 
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
                       ))
      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-with-gap-in-comp>  ;;  rule with no indirect object
     (head (v (aux -) ;; main verbs only
	    (lf ?c)
	    (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (tma ?tma)
	    ;; (subj (? subj (% ?s1 (var ?subjvar))))
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	   ;; (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj) (dobj (% ?s3 (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))	    
	    (Comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap ?!gap)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   ))
     ?iobj     
     ?dobj
     ?part
     ?comp
     )

   ;; The indirect object as RECIPIENT construction
  
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?gap) 
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (affected-result (% *PRO* (status F)
				(CLASS (:* ONT::recipient iobj)) (VAR *) (SEM ?subjsem) (constraint (& (val ?iobjvar) (of ?v)))))
		       (?comp3-map ?compvar)
		       
                       ))
      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-recipient-iobj> .97   ;; prefer subcategorized readings if possible
     (head (v (aux -) ;; main verbs only
	    (lf ?c)
	    (sem ($ f::situation (f::type ONT::GIVING) (F::iobj F::recipient)))
	    (vform ?vf)
	    (tma ?tma)
	    ;; (subj (? subj (% ?s1 (var ?subjvar))))
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    ;;(iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	    (part (% -)) 
	    (dobj ?dobj) (dobj (% NP (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))	    
	            ;; we allow a possible gap in the DOBJ NP e.g., "what did he thwart the passage of"
	    (Comp3 (% -))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   ))
     (np (case (? icase obj -)) (var ?iobjvar) (gerund -) (sem ($ (? rcp F::Phys-obj f::abstr-obj) (F::intentional +))) (gap -))
     ?dobj
     )

   ;; The indirect object beneficiary construction
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?gap) 
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) ;;(beneficiary ?iobjvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (beneficiary (% *PRO* (status F)
				(CLASS (:* ONT::beneficiary iobj)) (VAR *) (SEM ?subjsem) (constraint (& (val ?iobjvar) (of ?v)))))
		       (?comp3-map ?compvar)
		       
                       ))
      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-beneficiary-iobj> .98
     (head (v (aux -) ;; main verbs only
	    (lf ?c)
	    (sem ($ f::situation (f::type ONT::event-of-causation)))
	    (vform ?vf)
	    (tma ?tma)
	    ;; (subj (? subj (% ?s1 (var ?subjvar))))
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    ;;(iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	    (part (% -)) 
	    (dobj ?dobj) (dobj (% NP (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap) (gerund -)))	    
	            ;; we allow a possible gap in the DOBJ NP e.g., "what did he thwart the passage of"
	    (Comp3 (% -))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   ))
     (np (case (? icase obj -)) (var ?iobjvar) (sem ($ F::Phys-obj (F::origin F::Living) (F::intentional +) (F::type ont::person))) (gap -))
     ?dobj
     )

      
   ;; rule for modal aux's with no VP complements
   ;; TEST: The dog should.
    ((vp- (subj ?subj)  (subjvar ?subjvar) (lex ?l)
      (var ?v) (class (:* ont::ellipsis ))
      (gap -) 
      (constraint (& (LSUBJ ?subjvar) (?lsubj-map ?subjvar)))
      (tma ?newtma) (ellipsis +)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-modal-nocomp> .8 ;; prefer aux's with complements, only execute if needed
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +)
	    (contraction -)
	    (lex ?l)
	    (sem ?sem)
	    (vform ?vf)
	    (tma ?tma1)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map) 
	    ))
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?newtma))
     )
   
    ;; rule for negated modal aux's with no VP complements
    ;; TEST: The dog shouldn't.
    ((vp- (subj ?subj)  (subjvar ?subjvar) (lex ?l)
      (var ?v) (class (:* ont::ellipsis ))
      (gap -) 
      (constraint (& (LSUBJ ?subjvar) (?lsubj-map ?subjvar)))
      (tma ?newtma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-neg-modal-nocomp> .8 ;; prefer aux's with complements, only execute if needed
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +) ;; SHOULD THERE BE (contraction -) there???	    
	    (lex ?l)
	    (sem ?sem)
	    (vform ?vf)
	    (tma ?tma1)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map) 
	    ))
     (neg)
     
     ;; We need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((NEGATION +)))) 
     )
   
   
    ;; for negation after contracted forms of main verb be --disallow n't
    ;; TEST: I'm not short.
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -) 
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
                       ))
      (tma (& (negation +)))
      (postadvbl -) (vform ?vf)
      )
     -vp1-no-n^t-role>
     (head (v (aux -)
	    (lf ?c)
	    ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	    ;; unless we can match the lf-form be
	    (lex (? lx be am ^m ^re ^s))
	    (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj ?iobj)  (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	    (part ?part) 
	    (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     
	    ))
     (neg (lex w::not))
     ?iobj     
     ?dobj
     ?part
     ?comp
     )

    ;; for pre-adverbials between verb and argument
    ;; TEST: He said yesterday that the dog barked.
    ;; TEST: He saw on his left a dog barking.
    ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -)  (complex +)
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       (MODS ?mod)
                       ))
      (tma -)
      (postadvbl -) (vform ?vf)
      )
     -vp1-pre-arg-adv> .96 ;; only use if needed; prefer predicate adverbial modification 
     ;; require non-null DOBJ to avoid using with intransitive verbs (maybe also need a non-null COMP rule??)
     (head (v (aux -)
	    (lf ?c)
	    (lex ?lx)
	    (sem ?argsem) (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj (% -))
	    (part ?part) 
	    (dobj ?!dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     
	    ))
     (advbl (ATYPE PRE-VP) (GAP -)
      (ARGUMENT (% S (sem ?argsem)))
      (ARG ?v) (VAR ?mod) (role ?advrole) (subcat -))      
     ?!dobj  
     ?part
     ?comp
     )
   
     ;; for pre-adverbials after main verb be
     ;; TEST: Dogs are always short.
    ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -) 
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       (MODS ?mod)
                       ))
      (tma -)
      ;;      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-be-adv-role> .98 ;; prefer predicate adverbial modification
     (head (v (aux -)
	    (lf ?c)
	    ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	    ;; unless we can match the lf-form be
	    (lex (? lx are is was were ^s))
	    (sem ?argsem) (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj) (dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     
	    ))
     (advbl (ATYPE PRE-VP) (GAP -) (LF (% PROP (class ont::frequency))) (np-advbl -)  ;; don't allow time NPs here, e.g., "three years"
      (ARGUMENT (% S (sem ?argsem)))
      (ARG ?v) (VAR ?mod) (role ?advrole) (subcat -))      
     ?dobj
     ?part
     ?comp
     )

   
   ;; for pre-adverbials after main verb be
   ;; TEST: 5 is also not a short circuit
    ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -) 
      (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		       (LIOBJ ?iobjvar) (LCOMP ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       (MODS ?mod)
                       ))
      (tma (& (negation +)))
      (postadvbl -) (vform ?vf)
      )
     -vp1-be-adv-neg-role> .97 ;; prefer predicate adverbial modification
     (head (v (aux -)
	    (lf ?c)
	    ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	    ;; unless we can match the lf-form be
	    (lex (? lx are is was were ^s))
	    (sem ?argsem) (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     
	    ))
     (advbl (ATYPE PRE-VP) (GAP -)
      (ARGUMENT (% S (sem ?argsem)))
      (ARG ?v) (VAR ?mod) (role ?advrole) (subcat -))      
     (neg (lex not))
     ?dobj
     ?part
     ?comp
     )

   
     
   ;; VP RULE WITH DOBJ GAP
   ;; TEST: who did he see
   ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar)
     (main +) (gap (% ?s3 (Var ?gapvar) (Sem ?gapsem) (AGR ?gapagr) 
		      (case ?dcase) (ptype ?ptype)
		      ))
     (var ?v) 
     (class ?c) (constraint (& (LSUBJ ?subjvar) (LOBJ ?gapvar)
			       (LIOBJ ?iobjvar) (LCOMP ?compvar)
			       (?lsubj-map ?subjvar) (?!dobj-map ?gapvar)
			       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
			       ))
     (postadvbl -)
     )
    -vp-dobj-gap-role> ;;.97
    (head (v (aux -)  (be-there -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (VFORM ?tense-pro)
             (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	     (iobj ?iobj) (iobj (% ?s2 (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	     (part ?part) 
	     (dobj ?!dobj) (dobj (% NP (case (? dcase obj -)) (var ?gapvar) (sem ?gapsem) (agr ?gapagr) (ptype ?ptype))) ;; must have a possibility of NP dobj
	     (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -))) 
	     (subj-map ?lsubj-map) (dobj-map ?!dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	      
	     ))
    ?iobj     
    ?part
    ?comp
    )
   
   ;; VP RULES where the object pp or pred has a gap
   ;; TEST: who did he talk to?
   ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
     (var ?v) (class ?c) (gap ?!gap) (be-there -)
     (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		    (LIOBJ ?iobjvar) (LCOMP ?compvar)
		    (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		    (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		    ))
     (tma ?tma)
     (postadvbl -) (vform ?vf)
     )
    -vp1-gapped-dobj-role> 0.97
    (head (v (aux -) ;; main verbs only
	   (lf ?c)
	   (sem ($ f::situation (f::type ont::situation-root)))
	   (vform ?vf)
	   (tma ?tma)
	   ;; (subj (? subj (% ?s1 (var ?subjvar))))
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	   (iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	   (part ?part) 
	   (dobj ?dobj) (dobj (% (? s3 PP PRED) (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?!gap)))
	   (Comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    (be-there -)
	   ))
    
    ?iobj     
    ?dobj
    ?part
    ?comp
    )
   
   ;; VP RULES where the object pp or pred has a gap
   ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
     (var ?v) (class ?c) (gap ?!gap) 
     (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		    (LIOBJ ?iobjvar) (LCOMP ?!compvar)
		    (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		    (?iobj-map ?iobjvar) (?comp3-map ?!compvar)
		    ))
     (tma ?tma)
     (postadvbl -) (vform ?vf)
     )
    -vp1-gapped-comp-role> 0.98
    (head (v (aux -) ;; main verbs only
	   (lf ?c)
	   (sem ($ f::situation (f::type ont::situation-root)))
	   (vform ?vf)
	   (tma ?tma)
	   ;; (subj (? subj (% ?s1 (var ?subjvar))))
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	   (iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	   (part ?part) 
	   (dobj ?dobj) (dobj (% ?s3 (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	   (Comp3 ?!comp) (comp3 (% (? s4 PP PRED) (case (? ccase obj -)) (var ?!compvar) (sem ?compsem) (gap ?!gap)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   (be-there -)
	   ))
    
    ?iobj     
    ?dobj
    ?part
    ?!comp
    )
   
   ;; VP RULE WITH COMP3 GAP  (typically a PATH from a "where" question or a prepositional phrase)
   ;; where did he come from
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar)
     (main +) (gap (% ?s4 (Var ?!gapvar) (case ?ccase) (agr ?gapagr) (sem ?compsem) (ptype ?ptype)
		      ))
     (var ?v) 
     (class ?c) (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
				 (LIOBJ ?iobjvar) (LCOMP ?!gapvar)
				 (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
				 (?iobj-map ?iobjvar) (?comp3-map ?!gapvar)
				 ))
     (postadvbl -)
     )
    -vp-comp-gap1-role> .97
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (VFORM ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	   ;;(subjvar ?subjvar)
	   (iobj ?iobj)  (iobj (% ?s2 (var ?iobjvar) (sem ?iobjsem) (gap -) (case (? icase obj -))))
	   (part ?part) 
	   (dobj ?dobj)			(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (gap -) (case (? dcase obj -))))
	   (comp3 ?!comp) (comp3 (% ?s4 (var ?!gapvar) (sem ?compsem) (agr ?gapagr) (case (? ccase obj -)) (ptype ?ptype) ))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   
	   (be-there -)
	   ))
     ?iobj     
     ?dobj
     ?part
    )

   ;; VP RULE WITH COMP3 GAP when a verb has a particle
   ;; the difference from comp-gap1-role is that part is bound to be non-empty by double-matching on (part (% word))
   ;; and then the direct object can come after the particle
   ;; the double matching is necessary to differentiate from the cases where there is a non-empty particle
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar)
     (main +) (gap (% ?s4 (Var ?!gapvar) (case ?ccase) (agr ?gapagr) (sem ?compsem) (ptype ?ptype)
		      ))
     (var ?v) 
     (class ?c) (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
				 (LIOBJ ?iobjvar) (LCOMP ?!gapvar)
				 (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
				 (?iobj-map ?iobjvar) (?comp3-map ?!gapvar)
				 ))
     (postadvbl -)
     )
    -vp-comp-gap1-part-role> .97
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (VFORM ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	   (iobj ?iobj)  (iobj (% ?s2 (var ?iobjvar) (sem ?iobjsem) (gap -) (case (? icase obj -))))
	   (part ?!part) ;;(part (% part))
	   (dobj ?dobj)	(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (gap -) (case (? dcase obj -))))
	   (comp3 ?!comp) (comp3 (% ?s4 (var ?!gapvar) (sem ?compsem) (agr ?gapagr) (case (? ccase obj -)) (ptype ?ptype)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   
	   (be-there -)
	   ))
     ?iobj     
    ?!part
    ?dobj
    )

    
   ;; VP RULE WITH A VP COMP3 with a GAP inside it
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar)
    (main +) (gap ?!gap)
    (var ?v) 
     (class ?c) (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
				 (LIOBJ ?iobjvar) (LCOMP ?!compvar)
				 (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
				 (?iobj-map ?iobjvar) (?comp3-map ?!compvar)
				 ))
     (postadvbl -)
     )
    -vp-comp-gap2-role> 0.97
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (VFORM ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	   (iobj ?iobj)  (iobj (% ?s2 (var ?iobjvar) (sem ?iobjsem) (gap -) (case (? icase obj -))))
	   (part ?!part) 
	   (dobj ?dobj)	 (dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (gap -) (case (? dcase obj -))))
	   (comp3 ?comp) (comp3 (% ?s4 (var ?!compvar) (sem ?compsem) (case (? ccase obj -)) (gap ?!gap)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   
	   (be-there -)
	   ))
     ?iobj     
     ?dobj
     ?!part
     ?comp
    (BOUND (arg ?!gap))  ;; remember, we are parsing bottom up, so this must be bound from below
    )
      
   ;; PARTICLES with no independent semantics
   ;;  TEST: load up the oranges  ;; note: example isobsolete as UP will be compositional!!!
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar) (main +)
      (class ?c) (var ?v) 
     (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		      (LCOMP ?compvar)
		      (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		      (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		      ))
     (postadvbl -)
     )
    -vp-particle-role>
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root)))  (VFORM ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -) )) ;; note double matching required
	   (iobj (% -))
	   (part ?!part) ;;(part (% part))
	   (dobj ?dobj)			(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (case (? dcase obj -)) ))
	   (comp3 ?comp)		(comp3 (% ?s4 (var ?compvar) (sem ?compsem) (case (? ccase obj -))))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   
	   ))
    ?!part
    ?dobj
    ?comp
    )


 ;;  TEST: load up the oranges  ;; note: example isobsolete as UP will be compositional!!!
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar) (main +)
      (class ?c) (var ?v) 
     (constraint (& (LSUBJ ?subjvar) (LOBJ ?dobjvar)
		      (LCOMP ?compvar)
		      (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		      (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		      (result ?adv-v)
		      ))
     (postadvbl -)
     )
    -vp-compositional-particle-role> 1
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root)))  (VFORM ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -) )) ;; note double matching required
	   (iobj (% -))
	   (part (% -)) ;;(part (% part))
	   (dobj ?dobj)			(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (case (? dcase obj -)) ))
	   (comp3 ?comp)		(comp3 (% ?s4 (var ?compvar) (sem ?compsem) (case (? ccase obj -))))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   
	   ))
    (advbl (particle +) (var ?adv-v)  (arg ?dobjvar) (ARGUMENT (% S (sem ?sem))) (GAP -))
    ?dobj
    ?comp
    )
  
  
   
   ;; PASSIVE TRANSFORMATIONS

   ;; Lexical transformation rules for passives.  
   ;; There are rules for moving the direct object and the indirect object,
   ;; each case consists of one rule with and one without the "by" PP.

   ;;  These rules aren't ideal.  They don't allow the "by" PP
   ;; (logical subject) in VPs that also have other PPs.  They also
   ;; don't allow the object of one PP from a PPS to move.

   ;; TEST: The dog was given (me)
   ((v (vform passive) (passive +)
     (subj ?!dobj) (subj-map ?dobj-map) 
     (dobj (% -)) (agent-map ?subj-map)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
;     (prefix ?prefix)
     (restr ?prefix)
     )
    -v-passive> 1.0 
    (head (v (vform pastpart) (lex (? !lx been)) ;; exclude be
	   (subj ?subj) (subj-map ?subj-map) ;; Please don't remove - this is needed for trips-tflex conversion
	   (dobj ?!dobj) (dobj-map ?dobj-map) (exclude-passive -)
	   (iobj ?iobj) (iobj-map ?iobj-map)
	   (comp3 ?comp3) (comp3-map ?comp-map)
	   (part ?part)
;	   (prefix ?prefix)
	   (restr ?prefix)
	   )))
   
   ;; TEST:  The dog is taken care of
   ((v (vform passive)  (passive +)
     (subj (% np (lex ?subjlex) (sem ?dobjsem) (var ?dobjvar) (agr ?dobjagr)))
     (subj-map ?dobj-map) 
     (dobj (% prep (lex ?pt))) (dobj-map -)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
     )
    -v-passive-pp> 1.0
    (head (v (vform pastpart) (lex (? !lx been)) (exclude-passive -);; exclude be
	   (subj (% np (lex ?subjlex) (sem ($ ?!type))))
	   (dobj (% pp (ptype ?pt) (sem ?dobjsem) (var ?dobjvar) (agr ?dobjagr)))
	   (dobj-map ?dobj-map)
	   (iobj ?iobj) (iobj-map ?iobj-map)
	   (comp3 ?comp3) (comp3-map ?comp-map)
	   (part ?part)))
    )

   ;; TEST: The dog was given me by him
   ;; e.g. these are separated from the battery by a gap
   ;; Note that comp3 is promoted to dobj
   ((v (vform passive)  (passive +)
       (subj ?!dobj) (subj-map ?dobj-map) 
     ;;       (dobj (% -)) 
     (dobj ?comp3) (dobj-map ?comp3-map)     
       (iobj ?iobj) (iobj-map ?iobj-map)
       (comp3 (% pp (ptype by) (sem ?!subj-sem))) (comp3-map ?subj-map)
       (part ?part)) 
    -v-passive-by> 1.0
    (head (v (vform pastpart) (lex (? !lx been)) (exclude-passive -);; exclude be
	     (subj (% np (lex ?subjlex) (sem ?!subj-sem) (sem ($ ?!type))))
	     (subj-map ?subj-map) (iobj-map ?iobj-map)
	     (dobj ?!dobj) (dobj-map ?dobj-map)
	     (iobj ?iobj) 	  
	     (comp3 ?comp3) (comp3-map ?comp3-map)
	     (part ?part)))
)
   ;; TEST: I was given the dog
   ;;  Passive form with indirect object
   ((v (vform passive)  (passive +)
       (subj ?!iobj) (subj-map ?iobj-map) 
       (iobj-passive +)
       (dobj ?!dobj) (dobj-map ?dobj-map)
       (iobj (% -)) 
       (comp3 ?comp3)  (comp3-map ?comp3-map)
       (part ?part)) 
    -v-passive-iobj> 1.0
    (head (v (vform pastpart) (lex (? !lx been)) ;; exclude be
	   (subj ?subj) ;; Please don't remove - this is a dummy unrestricted var needed for trips-tflex conversion
	   (dobj ?!dobj) (dobj-map ?dobj-map)
	   (iobj ?!iobj) (iobj-map ?iobj-map) 
	   (comp3 ?comp3) (comp3-map ?comp3-map)
	   (part ?part))))
   
   ;; TEST: I was given the dog by him

   ((v (vform passive)  (passive +) (lex (? !lx been)) ;; exclude be
       (subj ?!iobj) (subj-map ?iobj-map) 
       (iobj-passive +)
       (dobj ?!dobj) (dobj-map ?dobj-map)
       (iobj (% -)) (iobj-map -)
       (comp3 (% pp (ptype by) (sem ?lsubj-sem)))  (comp3-map ?subj-map)
       (part ?part)) 
    -v-passive-iobj-by>
    (head (v (vform pastpart)
	   (subj ?subj) ;; Please don't remove - this is a dummy unrestricted var needed for trips-tflex conversion
	   (dobj ?!dobj) (dobj-map ?dobj-map)
	   (iobj ?!iobj) 
	   (comp3 ?comp3) (comp3-map (% -))
	   (part ?part))))
   
   ;;;   ;;  e.g., Terminal 1 is separated by a gap from a positive terminal
   ;;; "by" comes before a subcategorized pp argument
   ((v (vform passive)  (passive +)
       (subj ?!dobj) (subj-map ?dobj-map) 
       (comp3 ?!comp3) (comp3-map ?comp-map)
       (iobj ?iobj) (iobj-map ?iobj-map)
       (dobj (% pp (ptype by) (sem ?!subj-sem))) (dobj-map ?subj-map)
       (part ?part)) 
    -v-passive-by-reversed>
    
    (head (v (vform pastpart) (lex (? !lx been)) ;; exclude be
	   (subj (% np (lex ?subjlex) (sem ?!subj-sem) (sem ($ ?!type))))
	   (subj-map ?subj-map) (iobj-map ?iobj-map)
	   (dobj ?!dobj) (dobj-map ?dobj-map)
	   (iobj ?iobj) 
	   (comp3 ?!comp3) (comp3-map ?comp-map)
	   (part ?part))
     )
    )

   ;;  PREFIXES on verbs

   #|
   ((v (vform ?vf) 
     (subj ?subj) (subj-map ?subj-map) 
     (dobj ?!dobj) (dobj-map ?dobj-map)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
;     (prefix (:MOD (% *PRO* (status F) (class ?qual)
     (prefix (% *PRO* (status F) (class ?qual)
				   (var ?adv-v) (constraint (& (of ?v)))))
     )
    -v-prefix-hyphen> 1.01 
    (ADV (prefix +)
;     (LF ?qual) (ARG ?v) (VAR ?adv-v) (WH -)
     (LF ?qual) (VAR ?adv-v) (WH -)
     (argument (% S (sem ?argsem))) 
     )
    (word (lex w::punc-minus))
    (head (v (var ?v) (vform ?vf) (lex (? !lx been)) ;; exclude be
	     (subj ?subj) (subj-map ?subj-map) ;; Please don't remove - this is needed for trips-tflex conversion
	     (dobj ?!dobj) (dobj-map ?dobj-map) (exclude-passive -)
	     (iobj ?iobj) (iobj-map ?iobj-map)
	     (comp3 ?comp3) (comp3-map ?comp-map)
	     (part ?part)
	     (passive -)))  ;; we do the prefix first, then the passive
    )
   |#
      
   ((v (vform ?vf) 
     (subj ?subj) (subj-map ?subj-map) 
     (dobj ?!dobj) (dobj-map ?dobj-map)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
;     (prefix (:MOD (% *PRO* (status F) (class ?qual)
     (restr ?newc)
     )
    -v-prefix-hyphen> 1.01 
    (ADV (prefix +)
;     (LF ?qual) (ARG ?v) (VAR ?adv-v) (WH -)
     (LF ?qual) (VAR ?adv-v) (WH -)
     (argument (% S (sem ?argsem))) 
     )
    (word (lex w::punc-minus))
    (head (v (var ?v) (vform ?vf) (lex (? !lx been)) ;; exclude be
	     (subj ?subj) (subj-map ?subj-map) ;; Please don't remove - this is needed for trips-tflex conversion
	     (dobj ?!dobj) (dobj-map ?dobj-map) (exclude-passive -)
	     (iobj ?iobj) (iobj-map ?iobj-map)
	     (comp3 ?comp3) (comp3-map ?comp-map)
	     (part ?part) (restr ?prefix)
	     (passive -)))  ;; we do the prefix first, then the passive
     (append-conjuncts (conj1 ?prefix) (conj2 (& (MOD (% *PRO* (status F) (class ?qual)
				   (var ?adv-v) (constraint (& (of ?v)))))))
		       (new ?newc))
    )

   ((v (vform ?vf) 
     (subj ?subj) (subj-map ?subj-map) 
     (dobj ?!dobj) (dobj-map ?dobj-map)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
;     (prefix (:MOD (% *PRO* (status F) (class ?qual)
     (restr ?newc)
     )
    -v-prefix> 1.01 
    (ADV (prefix +)
;     (LF ?qual) (ARG ?v) (VAR ?adv-v) (WH -)
     (LF ?qual) (VAR ?adv-v) (WH -)
     (argument (% S (sem ?argsem))) 
     )
    (head (v (var ?v) (vform ?vf) (lex (? !lx been)) ;; exclude be
	     (subj ?subj) (subj-map ?subj-map) ;; Please don't remove - this is needed for trips-tflex conversion
	     (dobj ?!dobj) (dobj-map ?dobj-map) (exclude-passive -)
	     (iobj ?iobj) (iobj-map ?iobj-map)
	     (comp3 ?comp3) (comp3-map ?comp-map)
	     (part ?part) (restr ?prefix)
	     (passive -)))  ;; we do the prefix first, then the passive
     (append-conjuncts (conj1 ?prefix) (conj2 (& (MOD (% *PRO* (status F) (class ?qual)
				   (var ?adv-v) (constraint (& (of ?v)))))))
		       (new ?newc))
    )

   
   ;; DITRANSITIVE TRANSFORMATION 
   
      ;;  This rule makes the iobj optional
      ;; TEST: Give the dog
   ((v (iobj (% -)) (subj ?subj) (dobj ?dobj) (comp3 ?comp3) (part ?part) (vform ?vform)
       (subj-map ?subjvar) (dobj-map ?dobjvar)
       (comp3-map ?compvar))
     -iobj-delete> .98 ;; make sure you don't delete real iobj
     (head (v (iobj (% np)) (subj ?subj) (dobj ?dobj) (comp3 ?comp3) (iobj-passive -) (part ?part) (vform ?vform)
              (subj-map ?subjvar) (dobj-map ?dobjvar)
	    (comp3-map ?compvar))))

   
   ;; THERE transformation - BE and exist forms can take there as a subject
   ;; for objects
   ;; TEST: there exists a dog
    ((v (vform ?vform) (be-there +)
      (subj (% NP (lex there) (agr ?agr) (SEM ($ -)))) (subj-map -)
      (subjvar -)
      (dobj ?!subj) (dobj-map ?subj-map)
      (iobj (% -)) (iobj-map -)
      (comp3 (% -)) (comp3-map -)      
      (part (% -)) 
      )
     -be-there-sense> .97
     (head (v (vform ?vform) (be-there -)
	    (agr ?agr)
	    (LF ONT::Exists) (transform ?transform)
	    (lex (? lex exist exists existed))
	    (sem ?sem)
	    (subj ?!subj) (subj-map ?subj-map)
	    (subj (% np (agr ?agr)))
	    (dobj (% -)) (dobj-map -)
	    (iobj (% -)) (iobj-map -)
	    (comp3 (% -)) (comp3-map -)
	    (part (% -))
	    )))   


    ;; for properties  e.g., there is a man in the corner
   ((v (vform ?vform) (be-there +)
      (subj (% NP (lex there) (agr ?agr) (SEM ($ -)))) (subj-map -)
     (subjvar -)
      (dobj ?!subj) (dobj-map ?subj-map)
      (iobj (% -)) (iobj-map -)
      (comp3 ?dobj) (comp3-map ?dobj-map)
      (part (% -))
      )
     -be-there-sense1> .97
     (head (v (vform ?vform) (be-there -)
	    (agr ?agr)
	    (LF ONT::Have-property) (transform ?transform)
	    (lex (? lex be was were been is ^s are))
	    (sem ?sem)
	    (subj ?!subj) (subj-map ?subj-map)
	    (subj (% np (agr ?agr)))
	    (dobj ?dobj) (dobj (% pred)) (dobj-map ?dobj-map)
	    (iobj (% -)) (iobj-map -)
	    (comp3 (% -)) (comp3-map -)
	    (part (% -))
	    )))   

   
   ;; A transformation on BE to take expletive + filled predicate    
    ((v (vform ?vform) (be-there +)
      (subj (% NP (lex it) (agr 3s) (expletive +) (SEM ($ -)))) (subj-map -)
      (dobj ?dobjnew) (dobj-map ?dobj-map)
      (iobj (% -)) (iobj-map -)
      (comp3 (% -)) (comp3-map -)
      (part (% -))
      )
     -be-expletive-sense> .97 
     (head (v (vform ?vform) (be-there -)
	    (agr 3s)
	    (LF ONT::Have-property) (transform ?transform)
	    (lex (? lex be was were been is ^s are))
	    (sem ?sem)
	    (subj ?!subj) (subj-map ?subj-map)
	    (subj (% np (agr ?agr)))
	    (dobj ?!dobj) (dobj (% pred (filled -)))
	    (dobj-map ?dobj-map)
	    (iobj (% -)) (iobj-map -)
	    (comp3 (% -)) (comp3-map -)
	    (part (% -))
	    ))
     (change-feature-values (old ?!dobj) (new ?dobjnew) (newvalues ((filled +) (argument -))))
     )
   
   ;; TEST: The dog does bark.
   ((aux
     (tma-contrib (& (MODALITY ?auxlf) (negation ?neg)))
     (sem-contrib *NOCHANGE*)
     )
    -aux-from-modal-v-nochangesem> 1.0
    (head (v (aux +) (modal +) (lex ?l)
	   (changesem -)
	   (lf ?auxlf)
	   (neg ?neg)
	   (tma ?tma1)
	   ))
    )

   ;; TEST: The dog can bark.
   ((aux
     (tma-contrib (& (MODALITY ?auxlf) (negation ?neg)))
     (sem-contrib ((f::aspect ?aspect) (f::time-span ?time)))
     )
    -aux-from-modal-v-changesem> 1.0
    (head (v (aux +) (modal +) (lex ?l)
	   (changesem +)
	   (lf ?auxlf)
	   (neg ?neg)
	   (tma ?tma1)
	   (sem ($ f::situation (f::aspect ?aspect) (f::time-span ?time)))
	   ))
    )

   ;; TEST: The cat was chased.
   ((aux
     (tma-contrib (& (?auxname +) (negation ?neg)))
     (sem-contrib *NOCHANGE*)
     )
    -aux-from-asp-v-nochangesem> 1.0
    (head (v (aux +) (modal -) (lex ?l)
	   (lf ?auxlf)
	   (tma ?tma1)
	   (auxname ?auxname)
	   (changesem -)
	   (neg ?neg)
	   ))
    )

   ;; TEST: The dog is barking.
   ((aux
     (tma-contrib (& (?auxname +) (negation ?neg)))
     (sem-contrib ((f::aspect ?aspect) (f::time-span ?time)))
     )
    -aux-from-asp-v-changesem> 1.0
    (head (v (aux +) (modal -) (lex ?l)
	   (lf ?auxlf)
	   (tma ?tma1)
	   (auxname ?auxname)
	   (changesem +)
	   (neg ?neg)
	   (sem ($ f::situation (f::aspect ?aspect) (f::time-span ?time)))
	   ))
    )

   
   
   ))


 ;;== NEW PASSIVE TREATMENT ====
(parser::augment-grammar
 '((headfeatures
    (vp- vform var agr neg sem iobj dobj comp3 part cont   tense-pro aux modal auxname lex headcat transform subj-map advbl-needed passive subjvar template)
    (v lex sem lf neg var agr cont aux modal auxname ellipsis tma transform headcat)
    (cp vform neg sem subjvar dobjvar cont  transform)
    )


   ;; I know if/whether it's real
   ((cp (ctype s-if) (var *) (gap ?g) (condition ?lf)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     (lf (% prop (var *) (class (:* ONT::CLAUSE-CONDITION ?lex))
	    (constraint (& (content ?v)))
	    ))
     (clex ?lex)
     )
    -s-ifa> 
    (word (lex (? lex if whether)))
    (head (s (stype decl) (var ?v) (main -) (gap ?g) (vform fin)
	   (advbl-needed -)
	   (lf (% prop (var ?v) (class ?c) (constraint ?con)))
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   ))
    )

   
   ;; e.g., Tell him whether to leave
   ;; TEST: If the dog barks.
   ;; TEST: Unless the dog chases the cat.
   ((cp (ctype s-if) (var *) (gap ?g) (condition ?lf)
	;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     (lf (% prop (var *) (class (:* ONT::CLAUSE-CONDITION ?lex)) 
	    (constraint (& (content ?s-v)))
	    ))
     (clex ?lex)
     )
    -s-ifa-to> .97 ;; set this below wh-desc1a, but above adv-vp-post
    (word (lex (? lex if whether)))
    (head (cp (ctype s-to) (sem ?argsem) (var ?s-v) (lf ?lf-s) (gap -)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   (lf (% Prop (transform ?transform) (sem ?argsem) (class ?c) (constraint ?con))))
     )
    )

    ; TEST: Tell him not to bark
     ((cp (ctype s-to) (var ?s-v) (gap ?g)
     (lex ?hlex) (headcat ?hcat)
     (subj ?subj) (dobj ?dobj)
     (lf (% prop (var ?s-v) (class ?c) (constraint ?con)
	    (tma ?newtma)
	    ))
     )
    -s-neg-to>
    (word (lex not))
    (head (cp (ctype s-to) (sem ?argsem) (var ?s-v) (lf ?lf-s) (gap ?g)
	   (lex ?hlex) (headcat ?hcat) ; aug-trips
	   (subj ?subj) (dobj ?dobj)
	   (lf (% Prop (transform ?transform) (sem ?argsem) (class ?c) (constraint ?con) (tma ?tma))))
     )
    (append-conjuncts (conj1 (& (NEGATION +))) (conj2 ?tma) (new ?newtma))
    )
  
   ))

;;  A few rules with exceptional head features
(parser::augment-grammar
  '((headfeatures
     (vp vform agr neg sem var subjvar dobjvar cont  lex headcat tma transform subj-map advbl-needed template)
    (s vform neg cont  lex headcat transform advbl-needed)
    (v lex sem lf neg var agr cont lex transform aux modal tma advbl-needed headcat)
    (Utt sem subjvar dobjvar cont lex headcat transform))

    ;; YNQ QUESTIONS with BE because subjvar must be set properly
    
    
   ;;  These YNQ forms occus only with the verb to be.
   ;; they are hard to capture using the general mechanisms for the BE serves as the aux and the main verb
   ;; TEST: ; is the train late?
   ;;       Is there a train?
   ((s (stype ynq) (main +) (aux -) (gap ?gap)
     (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar)))
     (sort PRED) 
     
     (var ?v) ;; propagate up explicitly because not a head feature	   
     (agr ?subjagr) ;; propagate up explicitly because not a head feature
     (sem ?sem) ;; propagate up explicitly because not a head feature
     (transform ?transform) ;; propagate up explicitly because not a head feature
      ;; propagate up explicitly because not a head feature
     
     (subjvar ?subjvar)
     (dobjvar ?dobjvar)
     
     (advbl-needed ?avn)
     (lf (% prop (var ?v) (class ?belf) 
	    (constraint (& (lsubj ?subjvar)
;			   (dobj ?dobjvar) 
			   (lobj ?dobjvar) 
			   (comp3 ?compvar)
			   (?subj-map ?subjvar) (?dobj-map ?dobjvar)
			   (?comp3-map ?compvar) 
			   ))
	    (sem ?sem) (tma ?newtma)
	    (transform ?transform)
	    )))
    -s-ynq-be>
    (Head (v (aux -)

	   (var ?v) ;; propagate up explicitly because not a head feature	   
	   (agr ?subjagr) ;; propagate up explicitly because not a head feature
	   (sem ?sem) ;; propagate up explicitly because not a head feature
	   (transform ?transform) ;; propagate up explicitly because not a head feature
	    ;; propagate up explicitly because not a head feature
	   
	   (advbl-needed ?avn)
	   (lf ?belf)
	   (subj-map ?subj-map) (dobj-map ?dobj-map) (comp3-map ?comp3-map)
	   
	   (tma ?tma) (vform ?vf) 
	   ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	   ;; unless we can match the lf-form be
	   (lex (? lx am are is was were ^s))
	   (subj ?subj) (subj (% ?s1 (var ?subjvar) (sem ?subjsem) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	   (part (% -));; (part ?part) 
	   (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) ))
	    
	   )
     
     )
    ;;?subj
    (np (var ?subjvar) (agr ?subjagr) (case (? npcase sub -)) (sem ?subjsem) (wh -) (sort (? !sort wh-desc)) (gap -) (lex ?subjlex) )  ;; lots of restrictions on this NP to eliminate sentences like "is where the people"
    ?dobj
    ?comp
    (add-to-conjunct (val (TENSE (? vf PAST PRES))) (old ?tma) (new ?newtma))
    )
   
   
   ;; TEST: ; isn't the train late?
   ;;       Isn't there a train?
     ((s (stype ynq) (main +) (aux -) (gap ?gap)
       (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar)))
       (sort PRED) 
       (advbl-needed ?avn)
       
       (var ?v) ;; propagate up explicitly because not a head feature	   
       (agr ?subjagr) ;; propagate up explicitly because not a head feature
       (sem ?sem) ;; propagate up explicitly because not a head feature
       (transform ?transform) ;; propagate up explicitly because not a head feature
        ;; propagate up explicitly because not a head feature
       
       (subjvar ?subjvar)
       (dobjvar ?dobjvar)
              
       (lf (% prop (var ?v) (class ?belf) 
	      (constraint (& (lsubj ?subjvar)
;			     (dobj ?dobjvar) 
			     (lobj ?dobjvar) 
			     (comp3 ?compvar)
			     (?subj-map ?subjvar) (?dobj-map ?dobjvar)
			     (?comp3-map ?compvar) 
			   ))
	    (sem ?sem) (tma ?newtma)
	    (transform ?transform)
	    )))
    -s-ynq-be-neg>
      (Head (v (sem ?sem) (aux -)
	   (advbl-needed ?avn)
	   (lf ?belf)
	   (subj-map ?subj-map) (dobj-map ?dobj-map) (comp3-map ?comp3-map)
	   (agr ?a) 
	   (tma ?tma) (vform ?vf) (transform ?transform)
	   ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	   ;; unless we can match the lf-form be
	   (lex (? lx am are is was were ^s))
	   (subj ?subj) (subj (% ?s1 (var ?subjvar) (sem ?subjsem) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	    (part (% -)) ;; (part ?part) 
	   (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) ))
	    
	   )
     
       )
      (neg (lex n^t))
    ;;?subj
    (np (var ?subjvar) (case (? npcase sub -)) (sem ?subjsem) (wh -) (sort (? !sort wh-desc)) (gap -) (lex ?subjlex) )  ;; lots of restrictions on this NP to eliminate sentences like "is where the people"
    ?dobj
      ?comp
      (append-conjuncts (conj1 (& (TENSE (? vf PAST PRES)) (NEGATION +))) (conj2 ?tma) (new ?newtma))
      ;;    (add-to-conjunct (val (TENSE (? vf PAST PRES FUT))) (old ?tma) (new ?newtma))
    )
      
   ((s 
     (stype ynq) (main +) (aux -) 
     
     (var ?v) ;; propagate up explicitly because not a head feature	   
     (agr ?subjagr) ;; propagate up explicitly because not a head feature
     (sem ?sem) ;; propagate up explicitly because not a head feature
     (transform ?transform) ;; propagate up explicitly because not a head feature
      ;; propagate up explicitly because not a head feature
          
     (subjvar ?subjvar) 
     (dobjvar ?dobjvar)
     
     (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar)))
     (sort PRED) 
     (lf (% prop (var ?v) (class ?belf)
	    (constraint (& (lsubj ?subjvar)
			   (dobj ?dobjvar) 
			   (?subj-map ?subjvar) (?dobj-map ?dobjvar)
			   ))
	    (sem ?sem) (tma ?tma)
	    (transform ?transform)
	    ))
     (gap (% ?!s3 (case ?dcase) (agr ?dagr) (var ?dobjvar) (sem ?dobjsem) (gap -)))
     (advbl-needed ?avn)
     )
    -s-ynq-be-gap> 0.97
    (Head (v (aux -)
	   
	   (var ?v) ;; propagate up explicitly because not a head feature	   
	   (agr ?subjagr) ;; propagate up explicitly because not a head feature
	   (sem ?sem) ;; propagate up explicitly because not a head feature
	   (transform ?transform) ;; propagate up explicitly because not a head feature
	    ;; propagate up explicitly because not a head feature	   
	   
	   (advbl-needed ?avn)
	   (lf ?belf)
	   (subj-map ?subj-map) (dobj-map ?dobj-map) (comp3-map ?comp3-map)
	   (tma ?tma) (vform ?vf) 
	   ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	   ;; unless we can match the lf-form be
	   (lex (? lx am are is was were ^s))
	   (subj ?subj) (subj (% ?s1 (var ?subjvar) (sem ?subjsem) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	   (part (% -))		 
	   (dobj ?!dobj) (dobj (% ?!s3 (case (? dcase obj -)) (agr ?dagr) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))
	   (comp3 (% -))
	   ;;	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) ))
	    
     ))
    ;;?subj
    (np (var ?subjvar) (case sub) (sem ?subjsem) (wh -) (sort (? !sort wh-desc)) (gap -) (lex ?subjlex) )  ;; lots of restrictions on this NP to eliminate sentences like "is where the people"
    ;;?!dobj
    (add-to-conjunct (val (TENSE (? vf PAST PRES FUT))) (old ?tma) (new ?newtma))
    )

   
   
   ;;  These YNQ forms occus only with the verb to be.
   ;; they are hard to capture using the general mechanisms for the BE serves as the aux and the main verb
   ;; e.g., is the train late?
   ((s (stype ynq) (main +) (aux -) (gap -)
     (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar)))
     (sort PRED) 
     
     (var ?v) ;; propagate up explicitly because not a head feature	   
     (agr ?subjagr) ;; propagate up explicitly because not a head feature
     (sem ?sem) ;; propagate up explicitly because not a head feature
     (transform ?transform) ;; propagate up explicitly because not a head feature
      ;; propagate up explicitly because not a head feature	
     (subjvar ?subjvar) (dobjvar ?dobjvar)
     
     (lf (% prop (var ?v) (class ?belf)
	    (constraint (& (lsubj ?subjvar)
			   (dobj ?dobjvar) 
			   (comp3 ?compvar)
			   (?subj-map ?subjvar) (?dobj-map ?dobjvar)
			   (?comp3-map ?compvar) 
			   (MODS ?mod)
			   ))
	    (sem ?sem) (tma ?tma)
	    (transform ?transform)
	    ))
     (advbl-needed ?avn)
     )
    -s-ynq-be-adv>
    (head (v (sem ?sem) (aux -)
	   (lf ?belf)
	   (subj-map ?subj-map) (dobj-map ?dobj-map) (comp3-map ?comp3-map)
	   
	   (tma ?tma) (vform ?vf) 
	   
	   (var ?v) ;; propagate up explicitly because not a head feature	   
	   (agr ?subjagr) ;; propagate up explicitly because not a head feature
	   (sem ?sem) ;; propagate up explicitly because not a head feature
	   (transform ?transform) ;; propagate up explicitly because not a head feature
	    ;; propagate up explicitly because not a head feature	   
   
	   ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	   ;; unless we can match the lf-form be
	   (lex (? lx are is was were ^s))
	   (subj ?subj) (subj (% ?s1 (case sub) (var ?subjvar) (sem ?subjsem) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	   (part (% -))
	   (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	   (advbl-needed ?avn)
	   )
     
     )
    (add-to-conjunct (val (TENSE (? vf PAST PRES FUT))) (old ?tma) (new ?newtma))
    ?subj
    (advbl (ATYPE PRE-VP) (GAP -)
     (ARGUMENT (% S (sem ?argsem)))
     (ARG ?v) (VAR ?mod) (role ?advrole) (subcat -))           
    ?dobj
    ?comp) 
    
    ;; CONDITIONALS
    ;;; MD Commented out 2008/16/06 because conditionals are now handled as regular adverbials.
;;;    ;; TEST: If the train arrives, then we can load oranges.
;;;   ((s (stype ?st) (gap -) (LF (% PROP (class ?op) (var *) (constraint (&  (condition ?v1) (content ?v2)))))
;;;       (var *))
;;;    -s-if-then>
;;;    (cp (ctype s-if) (gap -) (var ?v1) (condition ?op))
;;;    (word (lex then))
;;;    (head (s (gap ?gap) (stype ?st) (vform (? x fin base)) (var ?v2)
;;;	   (advbl-needed -) 	   
;;;	   )))
;;;   
;;;   ;; TEST: If the train arrives, we can load oranges.
;;;   ((s (stype ?st) (LF (% PROP (Class ?op) (var *) (constraint (& (condition ?v1) (content ?v2))))) (gap -) (var *))
;;;    -s-ifb>
;;;    (cp (ctype s-if) (var ?v1) (gap -) (condition ?op))
;;;    (head (s (gap ?gap) (stype ?st) (var ?v2) (vform (? x fin base))
;;;	   (advbl-needed -)
;;;	   )))
;;;
;;;    ;; TEST: We can load oranges if the train arrives
;;;   ((s (stype ?st) (gap -) (LF (% PROP (class ?op) (var *) (constraint (& (condition ?v1) (content ?v2)))))
;;;       (var *))
;;;    -s-ifc>
;;;    (head (s (gap ?gap) (stype ?st) (vform (? x fin base))  (var ?v2)
;;;	   (advbl-needed -)
;;;	   ))
;;;    (cp (ctype s-if) (gap -) (var ?v1) (condition ?op))
;;;    )
   
   #||  ;; DITRANSITIVE TRANSFORMATION 

    ;;  This would be better in code above, but we have to disable 
    ;;  COMP3-MAP and IOBJ-MAP as head features 
   ;; As for the passive transformations, this rule is not ideal,
   ;; because if there's already a comp3 then you can't put the iobj
   ;; there.

   ;; This rule also overgenerates: e.g.
   ;; tell him to go -> *tell to go to him
   ;; that leaves us enough time -> *?that leaves enough time to us
   ;; TEST: give that to me
   ((v (vform ?vform) (subj ?subj) (dobj ?dobj) (iobj (% -)) (subj-map ?subj-map) (dobj-map ?dobj-map)
     (comp3 (% pp (ptype to) (var ?compvar) (sem ?iosem))) (comp3-map ?map) (part ?part)) 
    -ditransitive-to>
    (head (v (vform ?vform) (subj ?subj) (dobj ?dobj) (subj-map ?subj-map) (dobj-map ?dobj-map)
	   (iobj (% np (sem ?iosem))) (comp3 (% -)) (iobj-map ?map) (part ?part)
	   (advbl-needed -)
	   )))||#
 

    ;;  REJECTIONS
    
    ;;  Note, utterances like "not avon, bath" are now treated as two speech acts
     

;;;    ;;  e.g., not avon
;;;    ((Utt (var *) (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_REJECT) (constraint (& (content ?v1))))))
;;;     -reject-np>
;;;     (word (LEX not))
;;;     (head ((? cat np) (var ?v1))))
;;;
;;;    ;;  not via bath
;;;    ((Utt (VAR **) (lf (% SPEECHACT (VAR ?nv) (CLASS ONT::SA_REJECT) (constraint (& (content ?v))))))   
;;;      -reject-advbl>
;;;     (ADV (LEX not) (VAR ?nv))
;;;     (head (advbl (WH -) (VAR ?v) (ARGUMENT (% ?x (SEM ?sem))) (ARG (% *PRO* (VAR *) (gap -) (sem ?sem))))))

;;;    ;;  e.g., not a closed path
;;;    ((Utt (var *) (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_IDENTIFY) (constraint (& (content ?v1))))))
;;;     -not-np-utt>
;;;     (word (LEX not))
;;;     (head ((? cat np) (var ?v1))))
    
    
    
       ;;  NP UTTERANCES
   ;;   e.g.,  The train
   ;; Myrosia 2005/01/25 Added a sem restriction to prevent expletives from coming up here
   ;; TEST: The dog.
   ((UTT (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_IDENTIFY) (constraint (& (content ?v))))) (var *))
    -np-utt-simple> .98
    (head (NP (WH -) (SORT (? x PRED unit-measure)) (COMPLEX -) (VAR ?v) (sem ($ ?!type)))))

    ;; complex NPs (e.g., disjunctions, conjunctions) are dispreferred over parses with disjunction more deeply attached
    ((UTT (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_IDENTIFY) (constraint (& (content ?v))))) (var *))
     -np-utt> .97
     (head (NP (WH -) (SORT (? x PRED unit-measure)) (COMPLEX +) (VAR ?v) (sem ($ ?!type)))))

    ;; TEST: Not the dog.
    ((S (STYPE elided-verb) (VAR *) (LF (% PROP (CLASS (:* ONT::ELLIPSIS))
					   (var *)
					   (constraint (& (:theme ?v)))
					   (TMA (& (NEGATION +)))
					   ))
     )
     -not-np-s> .98
     (neg)
     (head (NP (WH -) (SORT (? x PRED unit-measure)) (gerund -) (VAR ?v) (sem ?sem) (sem ($ ?!type))))
     )

    ;; TEST: not red
    ((S (STYPE elided-verb) (VAR *) (LF (% PROP (CLASS (:* ONT::ELLIPSIS))
					   (var *)
					   (constraint (& (:property ?v)))
					   (TMA (& (NEGATION +)))
					   ))
      )
     -not-adjp-s> .94
     (neg)
     (head (adjp  (VAR ?v) (ARGUMENT (% ?x (SEM ?sem))) ;; (WH -)  I eliminated this to allow the question  "how red?"
	    (set-modifier -)  ;; disallows numbers as ADJP fragments - they already have a number interpretation 
	    (ARG (% *PRO* (VAR *) (gap -) (sem ?sem) (constraint (& (CONTEXT-REL UTT-FRAG)))))))
     )
     

    
    
     ;;  here a question mark forces a y/n question interp 
    ;; TEST: OK?
    ((Utt (VAR *) (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_PROMPT-FOR) (constraint (& (content ?sa))))))
     -utt5>
     (head (uttword (sa ?sa) (lf ?lf)))
     (punc (punctype ynq)))
    
    ;; TEST: from which city?
    ((Utt (VAR *) (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_WH-QUESTION) (constraint (& (content ?v) (focus ?foc)))))) 
     -utt4>
     (head (advbl (WH Q) (VAR ?v) (wh-var ?foc) (GAP -)
                  (ARGUMENT (% ?x (SEM ?sem)))
                  (ARG (% *PRO* (VAR *) (gap -) (sem ?sem)))))
     (punc))

;;;      Still can't figure out a good way to do this
;;;    ;; E.g., reduced ynq, e.g., can we? don't they?
;;;
;;;    ((Utt (VAR *) (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_YNQ-QUESTION)
;;;			 (constraint (& (content ?v) (focus ?foc))))))
;;;     -utt-ynq-reduced>
;;;     (s (type ynq) (reduced +) (var ?v))
;;;     (punc))
    
       
    ;;   GAP questions
    ;; who/what-dog/whose-dog did you see at the store?
    ;; TEST: who did you see?
    ;; TEST: who did you see at the store?
    ;; TEST: whose dog did you see at the store?
    ((s (stype whq) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj)
      (qtype Q) (lf ?lf) (var ?v))
     -wh-q2>
     (np (var ?npvar) (sem ?npsem) (wh Q) (agr ?a) (case ?case))
     (head (s (stype ynq) (lf ?lf) (var ?v) 
	    (advbl-needed -)
	    (subj ?subj)
	    (subjvar ?subjvar) (dobjvar ?dobjvar)
	    (gap (% np (sem ?npsem) (case ?case) (var ?npvar) (agr ?a))))
      )
     )

    ;; TEST: the dog, he said.
    ;; TEST: the dog, he told me.
    ;; TEST: the dog? he asked.
    ((s (stype decl) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj)
	(lf ?lf) (var ?v))
     -np-role-fronting> .97
     (np (var ?npvar) (sem ?npsem) 
	 (agr ?a) (case ?case)
	 (lf (% description (class ?npclass) (status ?status) (constraint ?cons) (sort ?npsort)
		(transform ?transform) 
		(dobj ?!dobj)  ;; only do this with verbs that have direct objects
		)))
     (head (s (stype ?st) (main -) (lf ?lf) (var ?v) (sem ?s-sem)
	      (gap (% np (sem ?npsem) (var ?npvar) (case ?case) (agr ?a)))
	      ))
     )

    ;; TEST: he barked, he said.
    ;; TEST: he barked, he told me.
    ;; TEST: did he bark? he asked.
    ((s (stype decl) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj)
	(lf ?lf) (var ?v))
     -utt-role-fronting> .97
     (utt (var ?uttvar) (sem ?uttsem))
     (head (s (stype ?st) (main -) (lf (% prop (class ont::report-speech))) (var ?v) (sem ?sem)
	      (gap (% utt (sem ?uttsem) (var ?uttvar)))
	      ))
     )
  
    ;; GAP questions with gapped PPs
     ((s (stype whq) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (qtype Q) (lf ?lf) (var ?v))
     -wh-q-ppgap>
     (pp (var ?npvar) (sem ?npsem) (wh Q) (agr ?a) (case ?case) (ptype ?ptp))
      (head (s (stype ynq) (lf ?lf) (var ?v) 
	     (advbl-needed -)
	     (subjvar ?subjvar) (dobjvar ?dobjvar)
	     (gap (% pp (sem ?npsem) (case ?case) (var ?npvar) (agr ?a) (ptype ?ptp))))
       )
      )

    ;; MOD GAP questions with a gapped pred -- e.g. "where is it", "how far is it"
    ;; Presumably "how large should I make it" should also parse, but this is untested
    ;; TEST: where is it?
    ((s (stype whq) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj)
      (qtype Q) (lf ?lf) (var ?v))
     -wh-q-predgap>
     (pred (var ?predvar) (sem ?predsem) (wh Q) (arg ?subjvar))
     (head (s (stype ynq) (lf ?lf) (var ?v) 
	    (advbl-needed -)
	    (subjvar ?subjvar) (dobjvar ?dobjvar)
	    (subj ?subj)
	    (gap (% pred (sem ?predsem) (var ?predvar))))
       )
      )
    
    
    ))

(parser::augment-grammar
  '((headfeatures
     (s vform var neg sem dobjvar cont  lex headcat transform advbl-needed)
     (pred qtype focus arg var sem wh lf lex headcat transform)
     (vp- vform var agr neg sem iobj dobj comp3 part cont  tense-pro aux modal lex headcat transform tma subj-map advbl-needed)
     )
    

    ;; Myrosia 01/24/02 To get the dummy sentences with predicates to
    ;; walk, I introduced a new structure: a predicate with filled
    ;; argument we usually only want unfilled arguments. Note that we
    ;; assume that adjective The reason to do it this way rather than
    ;; the previous rules (commented out at the end) is that we want
    ;; to take advantage of selectional restrictions on arguments, and
    ;; it does not seem possible in the other setup
    ;; TEST: Is it good to bark?
    ((pred (filled +) (argument -) (gap -) (adjectival +)
      )
     -fill-argument-pred>
     (head (pred (filled -) (argument ?argument) (argument (% np (var ?v) (sem ($ f::situation))))
	    (arg ?v) (wh -) (adjectival +)
	    ))
     (cp (ctype (? ctp s-to s-that-overt s-if))
      (var ?v) (gap -))  ;; note: is it good to eat is NOT expletive - the it refers and fills a gap in the pred.
     )

    ;; A special rule for expletive questions - what is possible to do --
    ;; TEST: What is good to chase?
    ((s (stype whq) (main +) (aux -) (gap -) (subjvar -)
      (qtype Q)
      (subj -)
      (var ?v)
      (advbl-needed ?avn)
      (lf (% prop (var ?v) (class ?belf) 
	     (constraint (& ;; subject not included in the new constraint
			  (dobj ?dobjvar) 
			  (comp3 ?compvar)
			  (?dobj-map ?dobjvar)
			  (?comp3-map ?compvar) 
			  ))
	     (sem ?sem) (tma ?newtma)
	     (transform ?transform)
	     )))
     -s-whq-expletive>
     (np (var ?npvar) (sem ?npsem) (wh Q) (agr ?gagr) (case ?gcase))
     (Head (v (sem ?sem) (aux -)
	    (advbl-needed ?avn)
	    (lf ?belf)
	    (subj-map ?subj-map) (dobj-map ?dobj-map) (comp3-map ?comp3-map)
	    (agr ?a) 
	    (tma ?tma) (vform ?vf) (transform ?transform)
	    ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	    ;; unless we can match the lf-form be
	    (lex (? lx am are is was were ^s))
	    (subj (% ?s1 (var ?subjvar) (sem ($ -)) (lex it) (gap -))) 
	    (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj) (dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) 
				  (gap (% np (sem ?npsem) (case ?gcase) (var ?npvar) (agr ?gagr)))))
	    (comp3 ?comp3) (comp3 (% ?s4 (gap -) (case (? ccase obj -)) (var ?compvar) (sem ?compsem) ))
	     
	    )
      
      )
     ;; no subject because no expletive
     ?dobj
     ?comp3
     (add-to-conjunct (val (TENSE (? vf PAST PRES FUT))) (old ?tma) (new ?newtma))
     )

   ))

;; putting the sem in this rule violates sem feature inheritance, so I had to move this rule here 
;; ideally, we should put the sem in the lexicon, but it's not currently possible for non-hierarchy lfs
;; which is what uttwords have
(parser::augment-grammar
  '((headfeatures
    (utt cont lex headcat transform))

 ;; TEST: hello
 ;; TEST: yes
 ;; TEST: maybe
 ;; TEST: no
   ((Utt (var *) (sem ($ f::proposition)) (uttword +)
         (lf (% SPEECHACT (VAR *) (CLASS ?sa) (constraint (& (content (?lf :content ?lex))))))
         (punctype (? x decl imp)))
    -utt4a>
    (head (uttword (lf (?lf)) (lex ?lex) (var ?v) (sa ?sa))))

    )
  )

(parser::augment-grammar
 '((headfeatures
    (vp vform agr comp3 cont postadvbl  aux modal lex headcat tma transform subj-map advbl-needed template)
    (s vform var neg sem subjvar dobjvar comp3 cont  lex headcat transform subj advbl-needed)
    (utt sem subjvar dobjvar cont lex headcat transform))
   
     ;;   Basic commands
    
    ;; e.g., tell me the plan
    ;; TEST: Bark.
    ;; TEST: Chase the cat.
    ((utt (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_REQUEST) (constraint (& (content ?v))))) (var *)
          )
     -command-imp1>
     (head (s (stype imp) (WH -) (var ?v) (lf ?lf) (gap -) (advbl-needed -))))
      
    ;; TEST: Bark.
    ;; TEST: Chase the cat.
    ((s (stype imp)
	(lf (% prop (var ?v) (class ?c) (constraint ?con)
	       (sem ?sem) (tma ?newtma)
	       (transform ?transform)
	       ))
      )
     -command-imp2>
     (head (vp (gap  -) 
	    (sem ?sem)
	    (sem ($ f::situation (f::aspect (? aspc f::dynamic f::stage-level))))
	    (var ?v) (aux -) (tma ?tma)
	    (constraint ?con)
	    (subj (% np (var (% *PRO* (CLASS ONT::PERSON) (VAR *) (SEM ?subjsem) (constraint (& (proform *YOU*)))))
		     (sem ($ f::phys-obj (f::form f::solid-object) (F::spatial-abstraction F::spatial-point)
			     (F::information -) (F::trajectory -) (F::container -) (F::Group -)
			     (F::mobility F::self-moving) (F::origin F::human) (f::intentional +)))
		     (sem ?subjsem)))
	    (subjvar (% *PRO* (CLASS ONT::PERSON) (VAR *) (constraint (& (proform *YOU*)))
			(sem ?subjsem)))
	    (subj-map (? xxx ont::AGENT ont::arg0 ont::neutral))
	    ;;($ f::phys-obj (f::form f::solid-object) (F::spatial-abstraction F::spatial-point)
	    ;;			(F::information -) (F::trajectory -) (F::container -) (F::Group -)
	    ;;			(F::mobility F::self-moving) (F::origin F::human) (f::intentional +)))))
		
	    (class ?c) (lf ?lf)
	    (vform base) (postadvbl ?pa) (main ?ma)
	    (transform ?transform)
	    (advbl-needed -)
	    )
      )
     (append-conjuncts (conj1 (& (TENSE PRES))) (conj2 ?tma) (new ?newtma))
     )

    ;; e.g., Let's go  NB: NOT the same as "let us go" - which is a REQUEST for permission

    ;; TEST: Let's bark.
    ;; TEST: Let's chase the cat.
    ((s (stype imp) 
	(lf (% prop (var ?v) (class ?c) (constraint ?con)
	    (sem ?sem) (tma ?newtma)
	    (transform ?transform)
	    ))
	)
     -lets-imp> 1.0 ;; get let's to go through this rule instead of command-imp2
     (word (lex let))
     (NP (lex us))
     (head (vp (gap  -) (sem ?sem)
	       (sem ($ f::situation (f::aspect (? aspc f::dynamic f::stage-level))))
	       (var ?v) (aux -) (tma ?tma)
	       (constraint ?con)
	       (subj (% np (var (% *PRO* (STATUS PRO) (CLASS (SET-OF ONT::PERSON)) (VAR *) (SEM ?subjsem) (constraint (& (proform US)))))
			(sem ($ f::phys-obj (f::form f::object) (f::intentional +)))
			(sem ?subjsem)))
	       (subjvar (% *PRO* (VAR *) (status pro)
			   (CLASS (SET-OF ONT::PERSON)) (SEM ?subjsem) (constraint (& (proform US)))))
	       (class ?c)
	       (vform base) (postadvbl ?pa) (main ?ma)
	    (transform ?transform)
	    (advbl-needed -)
	    )
      )
     (append-conjuncts (conj1 (& (TENSE PRES))) (conj2 ?tma) (new ?newtma))
     )
     
    ;; negative commands, e.g., don't tell me the plan
    ;; TEST: Don't bark.
    ;; TEST: Don't chase the cat.
    ((s (stype imp)
	 (lf (% prop (var ?v) (class ?c) (constraint ?con)
	    (sem ?sem) (tma ?newtma)
	    (transform ?transform)
	    ))
	)
     -command-dont>
     (v (aux +) (modal +) (ellipsis -)
      (lf ont::do))
     (neg)
     (head (vp ;;(gap  -) 
	       (sem ?sem)
	       (subj-map (? xx ont::AGENT ont::arg0))
	       (sem ($ f::situation (f::aspect (? aspc f::dynamic f::stage-level))))
	       (var ?v) (aux -) (tma ?tma)
	       (constraint ?con)
	       (subj (% np (var (% *PRO* (CLASS ONT::PERSON) (VAR *) (SEM ?subjsem) (constraint (& (proform *YOU*)))))
			(sem ($ f::phys-obj (f::form f::object) (f::intentional +)))
			(sem ?subjsem)))
	       (subjvar (% *PRO* (VAR *) (CLASS ONT::PERSON) (SEM ?subjsem) (constraint (& (proform *YOU*)))))
	       (class ?c)
	       (vform base) (postadvbl ?pa) (main ?ma)
	       (transform ?transform)
	       (advbl-needed -)
	       ))
     (change-feature-values (old ?tma) (new ?newtma) (newvalues ((TENSE PRES) (MODALITY ont::do) (NEGATION +)))) 
     )
	
  ;;  default rule for non-action VPs (useful when subject is deleted)
    ;;   e.g., have to go to elmira
    ;; TEST: Have to bark.
    ;; TEST: Have to chase the cat.
     ((Utt (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_TELL) (constraint (& (content ?v))))) (var **)
           (punctype (? x imp decl)))
      
      -vp-utt-inform> .96
      (head (vp (gap -) (sem ?sem) (sem ($ f::situation (f::cause (? cs F::Stimulating F::Phenomenal F::Mental -))))
	     (var ?v)
	     (constraint ?constraint) (class ?class)
	     (vform (? vform pres past fut)) (agr (? agr 1s 1p))
	     (transform ?transform)
	     (advbl-needed -)
	     (subj (% np (var (% *PRO* (CLASS ONT::REFERENTIAL-SEM) (VAR *) (constraint (& (proform SUBJ)))))
			(sem ($ f::phys-obj))
			))
	     (subjvar (% *PRO* (CLASS ONT::REFERENTIAL-SEM) (VAR *) (constraint (& (proform SUBJ)))
			 ))
	     )))

    ;; TEST: good
    ;; TEST: bad
     ;;   again, need to generate two variables to do this right
    ((utt (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_EVALUATE) (constraint (& (content ?v))))) (var **)
      (uttword +) ;; we set the UTTWORD feature here to allow an evaluate to prefix another utterance
      (punctype (? x imp decl)))
     -evaluate1>
     (head (ADJP (WH -) (var ?v) (arg (% *PRO* (VAR *) (CLASS ONT::ANY-SEM))) ;; changing this from ont::situation, which is too restrictive for cardiac domain
      (lf (% PROP (CLASS ONT::ACCEPTABILITY-VAL))) (gap -))))

   ;; TEST: good,
   ;; TEST: ok,
   ;;   again, need to generate two variables to do this right
   ((utt (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_EVALUATE) (constraint (& (content ?v))))) (var **)
     (punctype (? x imp decl)))
    -evaluate1b>
    (head (ADJP (WH -) (var ?v) (arg (% *PRO* (VAR *) (CLASS ONT::ANY-SEM))) ;; changing this from ont::situation, which is too restrictive for cardiac domain
	   (lf (% PROP (CLASS ONT::ACCEPTABILITY-VAL))) (gap -)))
    (punc (lex punc-comma) (var ?v1)))
   
   
    ;;; Special rules for VPs that require post-modifying adverbials
   ;;; (e.g. be). If the adverbial was not specified, we will create a
   ;;; gap Which will prevent this VP from participating in any
   ;;; constructions except for those that know how to fill that gap
   
   ;; Add tense information (past, present, or future) for VP that requires and adverbial
     ((vp (LF (% prop (class ?c) (var ?var) (constraint ?constraint) (tma ?newtma) 
	         (transform ?transf) (sem ?sem)))
          (class ?c) (constraint ?constraint) (tma ?newtma)  (sem ?sem) (transform ?transf)
        (vform (? vf PAST PRES FUT))
       (gap (% advbl))
       (subj ?subj) (subjvar ?subjvar) (var ?var)
       )
      -vp-tns+-gap> ;; 1 because we are simply adding tense info, not really using more rules
      (head
       (vp- (class ?c)  (constraint ?constraint) (tma ?tma1) (sem ?sem) ;; I added the SEM feature to get right restrictions in "i know where the loop is", but maybe it wasn't there for some other reason JFA 6/04
	 (vform (? vf PAST PRES FUT)) (var ?var)
	(advbl-needed +) (gap -) (subj ?subj) (subj (% np (var ?subjvar)))
	)) 
      (add-to-conjunct (val (TENSE ?vf)) (old ?tma1) (new ?newtma))
      )
   
   ;; untensed vp that requires a post-modifying adverbial
   ((vp (LF (% prop (class ?c) (var ?var) (constraint ?constraint) (transform ?transf) (tma ?tma) (sem ?sem)))
     (class ?c) (var ?var) (constraint ?constraint) (tma ?tma) (sem ?sem) (transform ?transf) 
     (gap (% advbl)) (subj ?subj) (subjvar ?subjvar)
     )
    -vp-tns--gap> 
    (head
     (vp- (class ?c) (constraint ?constraint) (tma ?tma) 
      (subj ?subj) (subj (% np (var ?subjvar))) (sem ?sem)   ;; see vp-tns+-gap> comment
      
      (vform (? vf BASE PASSIVE ING PASTPART))
      (advbl-needed +) 
      (var ?var)
      )))   
   
    ))

;; aux rules which violate VP sem inheritance constraint
(parser::augment-grammar
  '((headfeatures
     (vp vform agr cont comp3 postadvbl aux modal auxname lex headcat transform subj-map template)     
     (s vform neg comp3 cont  lex headcat transform )
     )
    
     ;; rules for auxiliaries with elided VP complement: will I?, can I?, do I? 
     ;; TEST: Does he?
    ((s (stype ynq) (gap -) (wh -) (tag +)
      (subj ?subj) (subjvar ?subjvar) (subj-map ?lsubj-map)
      (var ?v) (lex ?lx)
       (LF (% prop  (var ?v) (class (:* ont::ellipsis ))  (sem ?sem)
	     (constraint (& (LSUBJ ?subjvar) (?lsubj-map ?subjvar)))(tma ?newtma)
	     ))
       (sem ?sem) (transform ?transform) 
      )
     -ynq-aux-modal-nocomp> .8 ;; only execute if we have to so we don't explode the search space
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +) 
	    (contraction -)
	    (sem ?sem) (vform ?vf) (vform (? vf PRES PAST FUT))
	    (var ?v) (tma ?tma1) (agr ?a)
	     ;; Myrosia 2007/02/23 added agr ?a restriction to subjects to prevent cases such as "does you" from parsing this way
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?a) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map)
	     (transform ?transform)
	    (advbl-needed -)
	    ))
     ?subj
     ;; We need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (append-conjuncts (conj1 (& (TENSE ?vf))) (conj2 ?ntma) (new ?newtma))
     ) 
        
    ;;  rule for modal auxiliaries can, might, may, should, could, would: CAN the train arrive at 5:30?
    ;; this is the same as ynq-fut-aux except the Aspect and Time-span features change to those of the modal
    ;; 8/02/2004 build an explicit NP for the subject instead of trying to match it with ?subj to get around a problem with
    ;; the unifier not passing up the subject specifications from the verb
    ;; TEST: Does the dog bark?
    ;; TEST: Does the dog chase the cat?
    ((s (stype ynq) (gap ?gap) (lex ?lx)
       (subjvar ?subjvar) (dobjvar ?dobjvar) (var ?v)
      (LF (% prop  (var ?v) (class ?class) (sem ?newsem)
	     (constraint ?con) (tma ?newtma)
	     ))
      
      (sem ?newsem)
  ;;    (subj ?subj)
      (subj (% NP (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
      )
     -ynq-modal-aux>
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -)
	    (contraction -)
	    ;;(lex (? lx w::can w::might w::may w::should w::could w::would)) ;; only full forms here
	    (vform (? vform PRES PAST FUT)) (agr ?a)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
            (comp3 ?comp)
	    (comp3 (% ?s4 (class ?class)
		      (var ?v)
		      (case (? ccase obj -)) (subj-map ?rsubjmap) 
		      (constraint ?con) (subjvar ?subjvar) (tma ?tma1)
		      (var ?compvar) (sem ?compsem) (gap ?gap) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar)
									(sem ?subjsem) (gap -)))
		      (dobj ?dobj)
		      (advbl-needed -)
		      ))
            (comp3-map ?comp3-map)
            (subj-map ?lsubj-map)
            ))
;;     ?subj
     (NP (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an NP to get around unifier bug
     ?comp
     ;; We need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (append-conjuncts (conj1 (& (TENSE ?vform))) (conj2 ?ntma) (new ?newtma))
     ;; change the temporal values in SEM to be consistent with the aux
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
     )

    ((s (stype ynq) (gap ?gap) (lex ?lx)
      (subjvar ?subjvar) (dobjvar ?dobjvar) (var ?v)
      (LF (% prop  (var ?v) (class ?class) (sem ?newsem)
	     (constraint ?con) (tma ?newtma)
	     ))
      
      (sem ?newsem)
      ;;    (subj ?subj)
      (subj (% NP (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
      )
     -ynq-modal-neg-aux>
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -)
	    
	    ;;(lex (? lx w::can w::might w::may w::should w::could w::would)) ;; only full forms here
	    (vform (? vform PRES PAST FUT)) (agr ?a)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
            (comp3 ?comp)
	    (comp3 (% ?s4 (class ?class)
		      (var ?v)
		      (case (? ccase obj -)) (subj-map ?rsubjmap) 
		      (constraint ?con) (subjvar ?subjvar) (tma ?tma1)
		      (var ?compvar) (sem ?compsem) (gap ?gap) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar)
									(sem ?subjsem) (gap -)))
		      (dobj ?dobj)
		      (advbl-needed -)
		      ))
            (comp3-map ?comp3-map)
            (subj-map ?lsubj-map)
            ))
     (neg)
     (NP (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an NP to get around unifier bug
     ?comp
     ;; We need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (append-conjuncts (conj1 (& (TENSE ?vform) (NEGATION +))) (conj2 ?ntma) (new ?newtma))
     ;; change the temporal values in SEM to be consistent with the aux
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
     )

   #|| 
    ;; negated yes/no questions with modals (not be)
    ;;  TEST: Will not he arrive? Won't he arrive
    ((s (stype ynq) (gap ?gap) 
      (subjvar ?subjvar) (dobjvar ?dobjvar) (var ?v) ;; NB: values taken from VP
      (LF (% prop (sem ?sem) (var ?v) (class ?class)
             (constraint (& (LSUBJ ?subjvar) (DOBJ ?dobj) (dobj-map ?dobjvar)
			    (?comp-map ?compvar)
                            ;;(LCOMP ?compvar)
                            (?rsubjmap ?subjvar)))
                            ;;(?comp3-map ?compvar)))
             (tma ?newtma)
	     (transform ?transform)
	     ))
      (sem ?argsem) ;; 
      (lex ?l)
      (subj ?subj) ;;(% NP (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
      )
  
     -ynq-negated-modal-aux>
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -);; (contraction -)  
	    (var ?v) (sem ?sem)
	    ;; (argsem ?argsem) 
	    (vform (? vform PRES PAST FUT))
	    (agr ?a) (lex ?l)
            (subj ?subj)
            (subj (% ?s1 (lex ?subjlex) (case sub) (sem ?subjsem) (var ?subjvar) (gap -)))
	    (dobj (% -)) (part (% -)) (iobj (% -))
            ;;(comp3 ?comp) 
	    (comp3 (% vp- (sem ?compsem)));;  (class ?class) (var ?compvar)  (subj-map ?rsubjmap)
		     ;; (gap -) (subj ?subj) (tma ?tma1) (advbl-needed -) (sem ?compsem)
		     ;; ))
            ;;(comp3-map ?comp3-map)
            ;;(subj-map ?lsubj-map)
	    (transform ?transform)
	    ))
     (neg)
     (NP (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an NP to get around unifier bug
     (vp- (class ?class) (var ?compvar)  (subj-map ?rsubjmap) (dobjvar ?dobjvar) (dobj-map ?dobj-map) (comp3-map ?comp-map)
		      (gap -) (subj ?subj) (tma ?tma1) (advbl-needed -) (sem ?compsem))
     ;; We need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((TENSE (? vform PRES PAST FUT)) (NEGATION +))))
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
     )
    ||#
    ;; rules for auxiliaries with elided VP complement and negation: won't I?, can't I?, do I? 
    ;; TEST: Doesn't he?
    ((s (stype ynq) (gap -) (wh -) (tag +)
      (subj ?subj) (subjvar ?subjvar) (subj-map ?lsubj-map)
      (var ?v) (lex ?lx)
       (LF (% prop  (var ?v) (class (:* ont::ellipsis ))  (sem ?sem)
	      (constraint (& (LSUBJ ?subjvar) (?lsubj-map ?subjvar)))
	      (tma ?newtma)
	     ))
       (sem ?sem) (transform ?transform) 
      )
     -ynq-aux-modal-nocomp-neg> .8 ;; only execute if we have to so we don't explode the search space
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +) (contraction -)
	    (sem ?sem) (vform ?vf) (vform (? vf PRES PAST FUT))
	    (var ?v) (tma ?tma1) (agr ?a)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map)
	     (transform ?transform)
	    (advbl-needed -)
	    ))
     (neg)
     (NP (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an NP to get around unifier bug
     ;; We need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((TENSE ?vf) (NEGATION +))))
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))     
     )
    ))


(parser::augment-grammar
 '((headfeatures
    (vp agr neg iobj dobj comp3 part cont  tense-pro gap subj subjvar aux modal auxname headcat advbl-needed template)
    (vp- lex headcat)
    )
   
   ;; AUX rule for auxilliaries that change the sem features of the phrase
   ;; e.g. he has left; she is leaving; she can work
   ;; TEST: The dog can bark.
   ;; TEST: The dog has chased the cat.
   ((vp- (subj ?subj) (main -) (subjvar ?subjvar) (dobjvar ?dobjvar) (lex ?l)
     (var ?var) (class ?class) (gap ?gap) 
     (constraint ?con1)
     (tma ?newtma)
     (postadvbl +) (agr ?a)
     (sem ?newsem)
     (vform ?vform) (transform ?transform)
     (aux +) (auxname ?auxname) 
     (subj-map ?lsubj-map)
     )
    -vp-changesem-aux> 1.0
    ;; propagate class, semantics, constraint up from main verb
    (head (aux 
	   (auxname ?auxname) ;; MD 2009/02/11 it's important to propagate up AUXNAME, because templates for "must" and "could" depend on it
	   (tma-contrib ?tma-contrib)
	   (sem-contrib ?sem-contrib)
	   (ellipsis -) ;; (contraction -)
	   (lex ?l)
	   (lf ?lf)
	   (vform ?vform)
	   (agr ?a)
	   (sem ($ f::situation (f::aspect ?aspect) (f::time-span ?time)))
	   (subj ?subj) (subj (% ?s1 (var ?subjvar) (sem ?subjsem) (gap -)))
	   (iobj (% -)) (part (% -)) (dobj (% -))
	   (comp3 ?comp) 
	   (comp3 (% vp- (class ?class)  (constraint ?con1) (tma ?tma1) (var ?var)
		     (case (? ccase obj -)) (var ?compvar)  
		     (gap ?gap)		     
		     (sem ?compsem) ;; (sem ($ f::situation (aspect (? !asp f::indiv-level))))  ;; constraints are in LF
		     (subjvar ?subjvar) (dobjvar ?dobjvar) (transform ?transform)
		     (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -)))
		     (advbl-needed -) (subj-map ?lsubj-map)
		     (auxname ?compauxname)
		     ))
	    (comp3-map ?comp3-map)
	   ))
    ?comp
    (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?newtma)) ;; add aux feature to TMA
    ;; change the temporal values in SEM to be consistent with the aux
    (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Negation rules (for non-elided Vs):   AUX -> AUX not
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   ;; Rule for negated modals that don't change sem features (will and do)
   ;; Add (MODALITY ONT::XX) & (NEGATION +) to TMA
   ;; TEST: The dog won't bark.
   ;; TEST: The dog won't chase the cat.
    ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar) (lex ?l)
      (var ?v) (class ?cl) (gap -) 
      (constraint ?con1)
      (tma ?newtma)
      (postadvbl +) (agr ?a) (sem ?compsem)
      (vform ?vform) (transform ?trans)
      (aux +)  (modal +)
      (auxname ?auxname)
     )
    -vp-modal-neg> 1.0
     (head (aux 
	    (auxname ?auxname) ;; MD 2009/02/11 it's important to propagate up AUXNAME, because templates for "must" and "could" depend on it
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -) (neg -)
	    (lex ?l)
	    (vform ?vform) (vform (? vf PAST PRES FUT)) ;; double matching for 'do', since tense not marked in lexicon
	    (agr ?a) 
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -)))
	    (dobj (% -)) (part (% -)) (iobj (% -))
	    (comp3 ?comp) 
	    (comp3 (% vp- (class ?cl) (var ?v)  (constraint ?con1) (tma ?tma1) 
		      (case (? ccase obj -)) (var ?compvar)  ;;(sem ?compsem) -- needed for bug in unifier
		      (gap -) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (gap -)))
		      (subjvar ?subjvar) (dobjvar ?dobjvar) (transform ?trans)
		      (advbl-needed -)
		      ))
	    (subj-map ?lsubj-map) (comp3-map ?comp3-map)
	    ))
     (neg)
     ?comp
     (unify (pattern (% vp- (sem ?compsem))) (value ?comp))    ;; need this to get around a bug in unifier 
     ;; We need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((NEGATION +)))) 
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
     ;;     (append-conjuncts (conj1 (& (MODALITY ?auxlf) (NEGATION +))) (conj2 ?tma1) (new ?newtma))
     )

   ))


;; CONJUNCTIONS


(parser::augment-grammar 
 '((headfeatures
    (s vform neg sem subjvar dobjvar cont  lex headcat transform subj advbl-needed)
    (sseq vform neg sem subjvar dobjvar cont  lex headcat transform subj advbl-needed)
    (vbarseq sem)
    (vpseq sem)
    ;;(vp- sem)
    )
   
   ;; conjoined VPs w/ same subject
   ;; TEST: The dog barked and chased the cat.
   ((s (stype ?st) (var ?v3) (LF (% prop (var ?v3) 
				    (class ONT::S-CONJOINED)
				    (constraint (& (OPERATOR ?lex) (SEQUENCE (?v1 ?v2)))) (tma ?tma)))
     )
    -s-conj1>
    (head (s (stype (? st decl)) (subj ?subj) (var ?v1) 
	   (lf (% prop (tma ?tma)))
	   (advbl-needed -)
	   ))
    (CONJ (lex (? lex and or but)) (var ?v3))
    (vp (subj ?subj) (var ?v2) (tma ?tma) (advbl-needed -)
     ))    
   
   ;; Sentential conjunction
   ;; both Ss must be of the same type, decl or imperative
   ;; he did this and/or/but he did that; do this and/or/but do that
   ;; TEST: Bark and chase the cat.
   ((s (sseq +) (stype ?st) (var *) (LF (% prop (var *) (class ont::s-conjoined) 
				  (constraint (&  (OPERATOR ?lx) (SEQUENCE (?v1 ?v2))))))
     )
    -s-conj2>
    (head (s (stype (? st decl imp)) (subj ?subj1) (var ?v1) 
	   (lf (% prop (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (CONJ (lex (? lx or but however plus otherwise so and)))
    (s (stype (? st decl imp)) (subj ?subj2) (var ?v2) 
     (advbl-needed -)
     (lf (% prop (tma ?tma2))))
    )

   ;; S1, S2 and S3
   ;;  starting the SSEQ (need two as -s-conj2> above handles the binary conjunctive case)
   ((SSEQ (stype ?st) (var *);; (LF (% prop (var *) (class ont::s-conjoined) (constraint (& 
     (SEQUENCE (?v1 ?v2))
     )
    -s-conj-seq1>
    (head (s (stype (? st decl imp)) (subj ?subj1) (var ?v1)  (sseq -)
	   (lf (% prop (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (punc (lex (? x W::punc-comma W::Punc-semicolon)))
    (s (stype (? st decl imp)) (subj ?subj2) (var ?v2) (sseq -)
	   (lf (% prop (tma ?tma2)))
	   (advbl-needed -)
	   )
    )
   ;; extending the SSEQ
   ((SSEQ (stype ?st) (var *) ;;(LF (% prop (var *) (class ont::s-conjoined) 
     ;; (constraint (& (operator ?conjlf) 
     (SEQUENCE ?newlf))
    -s-conj-seq2>
    (head (sseq (var ?v1) 
	   (sequence ?lf)
	   (stype ?st)
	   ))
    ;;(CONJ (lex (? lx W::and W::OR)) (lf ?conjlf))
    (s (stype ?st) (subj ?subj2) (var ?v2) (sseq -)
     (lf (% prop (tma ?tma2)))
     (advbl-needed -)
     )
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )

   ;; Ending the SSEQ 
   ;; both Ss must be of the same type, decl or imperative
   ((s (stype ?st) (var *) (sseq +)
     (LF (% prop (var *) (class ont::s-conjoined) 
	    (constraint (&  (OPERATOR ?lx) (SEQUENCE ?newlf)))
	    ))
     )
    -s-conj-seq> 1.0
    (head (sseq (var ?v1) 
		(sequence ?lf)
		(stype ?st)
		))
    (CONJ (lex (? lx w::and w::or)))
    (s (stype ?st) (subj ?subj2) (var ?v2) (sseq -)
     (advbl-needed -)
     (lf (% prop (tma ?tma2))))
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )
    

   ;; either S1 or S2
   ;; both S1 and S2
   ;; applies to declarative or imperative sentences only
   ;; TEST: Either the dog barks or the dog chases the cat.
   ((s (stype ?st) (var *) (LF (% prop (var *) (class ont::s-conjoined) (constraint (&  (OPERATOR ?clf) (SEQUENCE (?v1 ?v2))))))
     )
    -s-double-conj>
    (conj (SUBCAT1 S) (SUBCAT2 ?wlex) (SUBCAT3 S) 
     (var ?v) (lf ?clf))
    (head (s (stype (? st decl imp)) (subj ?subj1) (var ?v1) (sseq -)
	   (lf (% prop (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (word (lex ?wlex))
    (s (stype (? st decl imp)) (subj ?subj2) (var ?v2) (sseq -)
     (advbl-needed -)
     (lf (% prop (tma ?tma2))))
    )

   ;; and the VP version e.g., to either stay or go
   ((VP (var *) (LF (% prop (var *) (class ont::s-conjoined) 
		       (constraint (&  (OPERATOR ?clf) (SEQUENCE (?v1 ?v2))))))
     (agr ?agr) (vform ?vform) (gap ?g) (subj ?subj) (sem ?sem)
		       
     )
    -either-vp>
    (conj (SUBCAT1 VP) (SUBCAT2 ?wlex) (SUBCAT3 VP) 
     (var ?v) (lf ?clf))
    (head (vp (subj ?subj) (var ?v1) (agr ?agr) (vform ?vform) (gap ?g) (sem ?sem)
	   (lf (% prop (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (word (lex ?wlex))
    (vp (subj ?subj) (var ?v2) (agr ?agr) (vform ?vform) (gap ?g)
     (advbl-needed -)
     (lf (% prop (tma ?tma2))))
    )

;; VP conjunction
   ;; both VP must be of the same vform
   ;; he had eaten and slept, to puncture or penetrate or pierce
   ((vp- (seq +) (vform ?vf) (var *)  (subjvar ?subj)  (subj ?subject) (agr ?agr) (gap ?gap)(subj-map ?subjmap)
     (class ont::vp-conjoined) (sem ?sem)
     (constraint (&  (OPERATOR ?lx) 
		     (SEQUENCE 
		      ((% *PRO* (var ?v1) (status F) (class ?c1) (tma ?tma1) (sem ?sem1) (constraint ?con1))
		       (% *PRO* (var ?v2) (status F) (class ?c2) (tma ?tma2) (sem ?sem2) (constraint ?con2)))))))
     
    -vbar-conj>
    (head (vp- (vform ?vf) (subjvar ?subj)  (subj ?subject) (var ?v1) (seq -)  (agr ?agr) (gap ?gap)
	       (advbl-needed -) (class ?c1) (constraint ?con1) (tma ?tma1) (sem ?sem1) (subj-map ?subjmap)
	   ))
    (CONJ (lex ?lx)) ;;(? lx or but however plus otherwise so and)))
    (vp- (vform ?vf) (var ?v2) (subjvar ?subj) (subj ?subject) (agr ?agr) (gap ?gap)
     (Advbl-needed -) (class ?c2) (constraint ?con2)  (tma ?tma2) (sem ?sem2))
    (sem-least-upper-bound (in1 ?sem1) (in2 ?sem2) (out ?sem))
    
    )

   ;; they had come, seen and conquered
  ;;  starting the VBARSEQ 
   ((VBARSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
     (class ont::vp-conjoined)
     (constraint (& 
		     (SEQUENCE 
		      ((% *PRO* (var ?v1) (status F) (class ?c1) (tma ?tma1) (constraint ?con1))
		       (% *PRO* (var ?v2) (status F) (class ?c2) (tma ?tma2) (constraint ?con2)))))))
    -vbar-conj-seq1>
    (head (vp- (vform ?vf) (subjvar ?subj) (subj ?subject) (var ?v1) (seq -) (gap ?gap) (agr ?agr)
	       (advbl-needed -) (class ?c1) (constraint ?con1) (tma ?tma1)
	   ))
    (punc (lex (? x W::punc-comma)))
    (vp- (vform ?vf) (var ?v2) (seq -) (subjvar ?subj)  (subj ?subject)  (gap ?gap) (agr ?agr)
     (advbl-needed -) (class ?c2) (constraint ?con2)  (tma ?tma2))
    )
    
    ;; extending the VBARSEQ
   ((VBARSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
     (class ont::vp-conjoined)
     (constraint (& (SEQUENCE ?newlf))))
    -vbar-conj-seq2>
    (head (vbarseq (var ?v1) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
		   (constraint (& (sequence ?lf)))
		   (vform ?vf) (punc ?punc)
		   ))
    (punc (lex ?punc))
    (vp- (vform ?vf) (var ?v2) (seq -)  (gap ?gap)  (agr ?agr)
     (advbl-needed -) (subjvar ?subj) (subj ?subject) (class ?c2) (constraint ?con2) (tma ?tma2))
    (add-to-end-of-list (list ?lf) (val (% *PRO* (var ?v2) (class ?c2) (constraint ?con2) (tma ?tma2))) (newlist ?newlf))
   )

   ;; ending the VBARSEQ 
   ((VP- (vform ?vf) (var *) (seq +) (subjvar ?subj) (subj ?subject)  (gap ?gap)  (agr ?agr) (sem ?sem)
     (class ont::vp-conjoined)
     (constraint (&  (OPERATOR ?lx) 
		     (SEQUENCE ?newlf)))
     )
    -vbar-conj-seq> 1.0
    (head (vbarseq (var ?v1) (subjvar ?subj) (subj ?subject) (gap ?gap) (sem ?sem)
		   (constraint (& (sequence ?lf))) (agr ?agr)
		   (vform ?vf)
		   ))
    (CONJ (lex (? lx w::and w::or)))
    (vp- (vform ?vf) (subjvar ?subj) (subj ?subject)(var ?v2) (seq -)  (gap ?gap)
     (advbl-needed -)  (agr ?agr) (class ?c2) (constraint ?con2) (tma ?tma2))
    (add-to-end-of-list (list ?lf) (val (% *PRO* (var ?v2) (class ?c2) (constraint ?con2) (tma ?tma2))) (newlist ?newlf))
    )

     ;; ending the VBARSEQ, with comma 
   ((VP- (vform ?vf) (var *) (seq +) (subjvar ?subj) (subj ?subject)  (gap ?gap)  (agr ?agr) (sem ?sem)
     (class ont::vp-conjoined)
     (constraint (&  (OPERATOR ?lx) 
		     (SEQUENCE ?newlf)))
     )
    -vbar-conj-seq-comma> 1.0
    (head (vbarseq (var ?v1) (subjvar ?subj) (subj ?subject) (gap ?gap) (sem ?sem)
		   (constraint (& (sequence ?lf))) (agr ?agr)
		   (vform ?vf)
		   ))
    (punc  (lex (? x W::punc-comma)))
    (CONJ (lex (? lx w::and w::or)))
    (vp- (vform ?vf) (subjvar ?subj) (subj ?subject)(var ?v2) (seq -)  (gap ?gap)
     (advbl-needed -)  (agr ?agr) (class ?c2) (constraint ?con2) (tma ?tma2))
    (add-to-end-of-list (list ?lf) (val (% *PRO* (var ?v2) (class ?c2) (constraint ?con2) (tma ?tma2))) (newlist ?newlf))
    )

   ;;  starting the VPSEQ (need two as -s-conj2> above handles the binary conjunctive case)
   ((VPSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr);; (LF (% prop (var *) (class ont::s-conjoined) (constraint (& 
     (SEQUENCE (?v1 ?v2)) (punc (? x W::punc-comma W::Punc-semicolon))
     )
    -vp-conj-seq1>
    (head (vp (vform ?vf) (subjvar ?subj) (subj ?subject) (var ?v1) (seq -) (gap ?gap) (agr ?agr)
	   (lf (% prop (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (punc (lex (? x W::punc-comma W::Punc-semicolon)))
    (vp (vform ?vf) (var ?v2) (seq -) (subjvar ?subj)  (subj ?subject)  (gap ?gap) (agr ?agr)
     (advbl-needed -)
     (lf (% prop (tma ?tma2))))
    )
    
    ;; extending the VPSEQ
   ((VPSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr);;(LF (% prop (var *) (class ont::s-conjoined) 
     ;; (constraint (& (operator ?conjlf) 
     (SEQUENCE ?newlf))
    -vp-conj-seq2>
    (head (vpseq (var ?v1) 
	   (sequence ?lf) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
	   (vform ?vf) (punc ?punc)
	   ))
    (punc (lex ?punc))
    (vp (vform ?vf) (var ?v2) (seq -)  (gap ?gap)  (agr ?agr)
     (advbl-needed -) (subjvar ?subj) (subj ?subject)
     (lf (% prop (tma ?tma2)))
     )
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )

   ;; Ending the VPSEQ 
   ;; both Ss must be of the same type, decl or imperative
   ((VP (vform ?vf) (var *) (seq +) (subjvar ?subj) (subj ?subject)  (gap ?gap)  (agr ?agr)
     (LF (% prop (var *) (class ont::vp-conjoined)  
	    (constraint (&  (OPERATOR ?lx) (SEQUENCE ?newlf)))
	    ))
     )
    -vp-conj-seq> 1.0
    (head (vpseq (var ?v1) (subjvar ?subj) (subj ?subject) (gap ?gap)
		(sequence ?lf) (agr ?agr)
		(vform ?vf)
		))
    (CONJ (lex (? lx w::and w::or)))
    (vp (vform ?vf) (subjvar ?subj) (subj ?subject)(var ?v2) (seq -)  (gap ?gap)
     (advbl-needed -)  (agr ?agr)
     (lf (% prop (tma ?tma2))))
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )
   ))
