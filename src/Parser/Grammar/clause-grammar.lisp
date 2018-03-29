;;;;
;;;; clause-grammar.lisp
;;;;

(in-package w)

(parser::addlexicalcat 'aaux)
(parser::addlexicalCAT 'PART)
(PARSER::ADDLEXICALCAT 'UTTWORD)

;;(define-foot-feature 'gap)

(parser::augment-grammar
 '((headfeatures
    (vp vform var neg sem iobj dobj comp3 part cont aux tense-pro gap subj subjvar modal auxname lex headcat transform subj-map template complex)
    )

 ;; untensed vp
   ;; test: the dog chased the cat to scare it.
   ((vp (lf (% prop (class ?c) (var ?v) (constraint ?constraint) (transform ?transf) (tma ?newtma) (sem ?sem)))
       (agr ?agr) (class ?c) (var ?v) (constraint ?constraint) (tma ?newtma) (sem ?sem) (transform ?transf) )
     -vp-tns-> 1.0
     (head
      (vp- (class ?c) (constraint ?constraint) (tma ?tma) (subj ?subj) 
       (vform (? vf base passive ing pastpart))
       (advbl-needed -)
       ))
    (add-to-conjunct (val (vform ?vf)) (old ?tma) (new ?newtma))
    )))


(parser::augment-grammar
 '((headfeatures
    (vp vform var agr neg sem iobj dobj comp3 part cont aux tense-pro gap subj subjvar modal auxname lex headcat transform subj-map template complex ellipsis)
    (vp- vform var agr neg sem iobj dobj comp3 part cont   tense-pro aux modal auxname lex headcat transform subj-map advbl-needed
	 passive passive-map template result) 
    (s vform var neg sem subjvar dobjvar cont  lex headcat transform ellipsis)
    (cp vform var neg sem subjvar dobjvar cont  transform subj-map subj lex)
    (v lex sem lf neg var agr cont aux modal auxname ellipsis tma transform headcat)
    (aux vform var agr neg sem subj iobj dobj comp3 part cont  tense-pro lex headcat transform subj-map advbl-needed
	 passive passive-map ellipsis contraction auxname) 
    ;; (pp var)
    ;; (pps var)
    (utt neg qtype sem subjvar dobjvar cont lex headcat transform)
    (pred qtype focus gap filled transform headcat lex)
    )
   
   ;;  handling start and end of utterance
   

   ;; if utterance begins with a filled pause  (retrict to having no punc on end yet to reduce spurious ambiguity)
   ;; test: um the dog barked
   ((utt (var ?v) (lf ?lf) (focus ?foc)
      (punc ?punc) (punctype ?p)
     (started  +)
     )
   -utt-fp-start> 1 ;; utterances frequently start with silence. but only start the utterances that go through the end of the input
    (fp)
    (head (utt (focus ?foc) (started -) (var ?v)
               (lf ?lf) (punctype ?p)))
   )
   
   ;;  here's  the end of utterance marker
   ((utt (var ?v1) (lf ?lf) (focus ?foc)
      (punc +) (punctype ?p)
     (ended +)
     )
    -utt-end-only>
    (head (utt (focus ?foc) (ended -) (var ?v1)
               (lf ?lf) (punctype ?p)))
    (punc (lex end-of-utterance)))

   ;; utts ending with a silence
   ;; test: the dog barked um.
   ((utt (var ?v1) (lf ?lf) (focus ?foc)
      (punc +) (punctype ?p)
     (ended +)
     )
    -utt-fp-end> 1
    (head (utt (focus ?foc) (ended -) (var ?v1)
               (lf ?lf) (punctype ?p)))
    (fp))

   
   ;; top-level utterance rules

   ;; hello calo; we did it, calo
   ;; any utterance can be addressed to someone
   ;; the vocative feature prevents multiple vocative constructs, esp one at beginning and one at end
   ((utt (var ?v) (vocative +) (lf (% speechact (var ?sv) (class ?cl) (constraint ?constraint))))
    -vocative-utt> .95    ;; nb: lowered below np-conj rule to eliminate bad interps of utterances like 'john and mary"
    (head (utt (focus ?foc) (var ?v) (vocative -) (lf (% speechact (var ?sv) (class (? cl ont::sa_tell ont::sa_yn-question ont::sa_request)) (constraint ?con)))
	  (punctype -)))
    (np (var ?nv) (lf (% description (status (? nm ont::gname ont::name))))
	(sem ($ f::phys-obj (f::intentional +) (f::object-function f::occupation)))
	)
    (add-to-conjunct (val (vocative ?nv)) (old ?con) (new ?constraint))
    )

  ;; calo find me a computer
   ;; any utterance can be addressed to someone
   ((utt (var ?v) (lf (% speechact (var ?sv) (class ?cl) (constraint ?constraint)))
	 (vocative +))
    ;; lowering this rule allows many bad interpretations in coordops
    -vocative-utt2>  .95
    (np (var ?nv) (lf (% description (status (? nm ont::gname ont::name))))
	(sem ($ f::phys-obj (f::intentional +) (f::object-function f::occupation)))
	)
    (head (utt (punctype -) (focus ?foc) (var ?v) (vocative -) 
	       (lf (% speechact (var ?sv) 
		      (class (? cl ont::sa_tell ont::sa_yn-question ont::sa-request)) (constraint ?con))))
     )
    (add-to-conjunct (val (vocative ?nv)) (old ?con) (new ?constraint))
    )

   ;;there are oranges at corning   
   ((utt (var *) (punctype decl) (lf (% speechact (var *) (class ont::sa_tell) (constraint (& (content ?v)))))) 
    -utt-s1>
    (head (s (stype decl) (gap -) (var ?v) (wh -) (lf ?lf) (advbl-needed -))))
   
   ;;  punctuation: for typed input. punctype must agree 
   ;; myrosia 19/10/06 -- punctype need not always agree in typed dialogue
   ;; rather than trying to track the speechact via punctype, we are storing it
   ;; for further analysis in the future
   
   ((utt  (var ?v) (focus ?foc)  ;; i changed the var from the punc to the utt so that the lf is printed properly (why was it the other way?
     (punc +) (punctype ?p) (uttword ?uw)
     (lf (% speechact (var ?sv) (class ?cl) (constraint ?constraint)))
     )
   -utt-punctuation>
   (head (utt (focus ?foc) (ended -) (var ?v) (punc -) (uttword ?uw)
	  (lf (% speechact (var ?sv) (class ?cl) (constraint ?con)))
	  ))
    (punc (punctype ?p) (lex (? lex w::punc-exclamation-mark w::punc-period w::punc-question-mark w::punc-colon w::ellipses w::punc-comma)))
    (add-to-conjunct (val (punctype ?p)) (old ?con) (new ?constraint))
   )

    #||((utt (var *) (punctype ?stype) (lf (% speechact (var *) (class ont::sa_tell) (constraint (& (content ?v)))))) 
    -utt-seq1>
    (head (sseq (stype ?stype) (gap -) (var ?v) (wh -) 
		(lf ?lf) (advbl-needed -))))||#
   
   ;; questions
   
   ;; e.g., is the engine coming from corning?
   ;; test: is the dog barking?
   ;; test: is the dog chasing the cat?
   ((utt (lf (% speechact (class ont::sa_yn-question) 
		(constraint (& (content ?s-v))) (var *)))
     (var *) (punctype ?p)
     ) 
    -utt-ynq1>
    (head (s (stype ynq) (var ?s-v)  (gap -) (wh -) (advbl-needed -))))
   
   
   ;; tag questions
   ;; note that the rules need to match tense in the tma, not just the vform
   ;; because vform is hierarchical and so "past" and "pres" match in the vform,
   ;; but not in the tma tense
   ;; test: the dog doesn't bark, does it?
   ;; test: the dog doesn't chase the cat, does it?
   ((utt (lf (% speechact (class ont::sa_tag-question) 
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

   ;; test: the dog barks, doesn't it?
   ;; test: the dog chases the cat, doesn't it?
   ((utt (lf (% speechact (class ont::sa_tag-question) 
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
   ;; test: did the dog bark or what?
   ;; test: did the dog chase the cat or what?
   ((utt (lf ?lf)
         (var ?v) (punctype ?p))
    -utt-ynq-or-what>
    (head (utt (lf (% speechact (class ont::sa_yn-question))) (lf ?lf) (var ?v)
	       (punctype ?p)))
    (word (lex or))
    (word (lex what)))

   ;;  "or not" tag on questions
   ;; test: did the dog bark or not?
   ;; test: did the dog chase the cat or not?
   ((utt (lf ?lf)
         (var ?v) (punctype ?p))
    -utt-ynq-or-not>
    (head (utt (lf (% speechact (class ont::sa_yn-question))) (lf ?lf) (var ?v)
	       (punctype ?p)))
    (word (lex or))
    (word (lex not)))
   
     ;;  e.g., hello with punctuation
   ((utt (lf (% speechact (var *) (class ?sa) (constraint (& (content (?lf :content ?lex))))))
         (var *) (uttword +))
    -utt4b>
    (head (uttword (lf (?lf)) (lex ?lex) (sa ?sa)))
    (punc (lex punc-comma) (var ?v1)))

     ;;  e.g., thanks for the gift, sorry that I interrupted
   ((Utt (lf (% SPEECHACT (VAR *) (CLASS ?sa) (constraint (& (content (?lf :content ?lex :reason ?sc))))))
         (var *) (uttword +))
    -utt4b-with-subcat>
    (head (uttword (lf (?lf)) (lex ?lex) (sa ?sa) (subcat ?!subcat) (subcat (% ?xx (car ?sc)))))
    ?!subcat
    (punc (lex punc-comma) (var ?v1)))

     
   ;;  compound utt rule - allows uttword+ utterance to preceed other utts (once)
   ;; test: hello hello
   ((utt (sa-seq +) (lf (% sa-seq (var *) (class ont::sa-seq) (constraint (& (acts (?v1 ?v2))))))
         (var *))
    -uttword-utt> .96
    (utt (lf ?lf1) (var ?v1) (uttword +) (sa-seq -))
    (head (utt (lf ?lf2) (var ?v2) (sa-seq -))))


   ;;  tag sentences, allowing uttword
   ;; test: the dog barks doesn't it hello
   ((utt (sa-seq +) (lf (% sa-seq (var *) (class ont::sa-seq) (constraint (& (acts (?v1 ?v2))))))
         (var *))
    -utt-tag> .90 ;; lowering this from .97 because it was preferred over predicate adjectives, e.g that looks good 
    (utt (lf ?lf1) (var ?v1) (sa-seq -))
    (head (utt (lf ?lf2) (var ?v2) (uttword +) (sa-seq -))))
   

 
   ;; what do you think the red x means? that the battery is damaged
   ;;
   ((utt (var *) (punctype decl) (no-post-adv +) (lf (% speechact (var *) (class ont::sa_pred-fragment) (constraint (& (content ?v)))))) 
    -utt-s-that> 0.9
    (head (cp (ctype s-that-overt) (gap -) (var ?v) (wh -) (lf ?lf) (advbl-needed -))))

   ;;  request and suggestions
   
    ;; e.g., how about from avon to corning? how about going through corning?
   ;; test: how about chasing the cat?
   ((utt (lf (% speechact (var *) (class ont::sa_request-comment) (constraint ?c))) (var *)
     ;; myrosia commented out (lex how-about) 2006/21/02
     ;; lex is a head feature and if we specify it here, it clashes with the lex coming up from the s, where it is determined
     ;; either by an adverbial or an np head.
     ;;(lex how-about) ;; aug-trips -- we need a lex feature here -- can we get it from the s
     (punctype ?p))
    -how-about-utt>
    (head (s (stype how-about) (var ?v) (lf (constraint ?c)) (advbl-needed -))
          ))

   ;; test: how about chasing the cat? How about the dog?
   ((s (stype how-about) (lf (constraint (& (content ?v)))))
    -how-about-np>
    (word (lex (? x how what)) (var ?v1))
    (word (lex about)) 
    (head (np (gap -) (var ?v) (lf ?lf) (case (? case obj -)))))

   ;; test: how about you do it
   ((s (stype how-about) (lf (constraint (& (content ?v)))))
    -how-about-s>
    (word (lex (? x how what)) (var ?v1))
    (word (lex about)) 
    (head (S (gap -) (stype decl) (vform pres)
	     (var ?v) (lf ?lf) (case (? case obj -)))))

    ;; test: how about horizontally?
    ;; test: what about from the cat?
    ((s (stype how-about) (lf (constraint (& (content ?v)))))
    -how-about-advbl>
    (word (lex (? x how what)) (var ?v1))
    (word (lex about)) 
    (head (advbl (arg (% *pro* (var *) (class ont::referential-sem)))
		 (gap -) (var ?v) (lf ?lf) (case (? case obj -)))))
   
   
   
   ;; e.g., what if we use the helicopter instead
   ;; test: what if the dog barks?
   ;; test: what if the dog chases the cat?
   ;; test: what if the dog chases the cat instead?
   ((utt (lf (% speechact (var *) (class ont::sa_request-comment) (constraint (& (content ?v))))) 
         (var *)(punctype ?p))
    -what-if1> 
    (word (lex what))
    (word (lex if))
    (head (s (gap -) (stype decl) (var ?v) (lf ?lf) (advbl-needed -))))
      
    ;; typical assertions
   
   ;; main s rule. we exclude the pp-word subjects because they have special rule
    
   ;; e.g., the boxcar is at bath.
   ;; test: the dog barked.
   ;; test: the dog chased the cat.
   ((s (stype decl) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (lf ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex)
		       (pp-word -) (changeagr -) (gap -))
	       )

     (advbl-needed ?avn)
     )
    -s1>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex) (sort pred) ;; lex needed for expletives? 
      (pp-word -) (changeagr -) (gap -) (expletive ?exp))
    (head (vp (lf ?lf) (gap ?g)
              (template (? !x  lxm::propositional-equal-templ))
	      (subjvar ?npvar)
	      (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex)
		       (pp-word -) (changeagr -) (gap -) (expletive ?exp)))
	      (var ?v) (vform fin) (agr ?a)
	      (advbl-needed ?avn)
	      (neg ?neg)
	      )
	  )
    )

   ;;  special case s1 rule for abstract objects that can be equivalent to propositions (e.g., fact, belief, etc)
   ;;    these all take a subcat-map to ont::formal - only verb that can do this so far is ont::be.
   ;;    e.g., the fact is he left angry

   ((s (stype decl) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (lf ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
		       (pp-word -) (changeagr -))
		    )

     (advbl-needed ?avn)
     )
    -s1-be-prop1>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex) ;; lex needed for expletives?
     (pp-word -) (changeagr -) (subcat-map ont::formal))
    (head (vp (lf ?lf) (gap ?g) (template lxm::propositional-equal-templ)
	      (subjvar ?npvar)
	      (subj (% np (sem ?npsem) (agr ?a) (var ?npvar) (lex ?lex)))
	      (var ?v) (vform fin) (agr ?a)
	      (advbl-needed ?avn)
	      (neg ?neg)
	      )
     )
    )

   ;;  special case s1 rule for nominalizations that have a comp-map of ont::formal 
   ;;    e.g., the belief is he left angry

   ((s (stype decl) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (lf ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
		       (pp-word -) (changeagr -))
		    )

     (advbl-needed ?avn)
     )
    -s1-be-prop2>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) (lex ?lex) ;; lex needed for expletives?
     (pp-word -) (changeagr -) (comp3-map ont::formal))
    (head (vp (lf ?lf) (gap ?g) (template lxm::propositional-equal-templ)
	      (subjvar ?npvar)
	      (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (lex ?lex)))
	      (var ?v) (vform fin) (agr ?a)
	      (advbl-needed ?avn)
	      (neg ?neg)
	      )
     )
    )

    ;; commands with subjects: you/someone take a picture of me
   ;; test: someone bark.
   ;; test: you chase the cat.
   ((s (stype imp) (var ?v) (subjvar ?npvar) (gap ?g) 
     (lf ?lf) (subj (% np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
		       (pp-word -) (changeagr -))
	       )
     (advbl-needed ?avn)
     )
    -command-imp-subj>
    (np (sem ?npsem) (var ?npvar) (agr ?a) (case (? case sub -)) 
     (lex (? l someone somebody)) ;; only someone, you allowed as subjects of imperatives
     (pp-word -) (changeagr -)
     )
    (head (vp (lf ?lf) (gap ?g)
	   
	   (subj (% np (sem ?subjsem) (var ?npvar) (agr ?a) (lex ?lex))) 
	   (var ?v) (vform base)
	   (advbl-needed ?avn)
	   )
     )
    (unify (pattern ?subjsem) (value ?npsem))
    )
   
   ;; wh question utterances
   ;;

   ;; e.g., when will the engine arrive at dansville? and "where will the engine arrive",  but not "where will the engine arrive at"
   ;; test: when will the dog bark?
   ;; test: when will the dog chase the cat?
   ;; test: where will the dog chase the cat?
   ;; test: what will the dog chase?
   ((utt (lf (% speechact (var *) (class ont::sa_wh-question) (constraint (& (content ?v) (focus ?foc)))))
         (var *) (qtype ?type) (punctype ?p))
    -wh-question1>
    (head (s (stype whq) (wh-var ?foc) (var ?v) (qtype (? type when where q how-x -))(advbl-needed -))))

   
   ;; e.g., the engine will arive where?     put it where?
   ;; test: the dog will chase what?
   ;; test: what will chase the cat?
   ((utt (lf (% speechact (var *) (class ont::sa_wh-question) (constraint (& (content ?v) (focus ?foc)))))
         (var *) (qtype ?type) (punctype ?p))
    -decl-wh-question1> .98
    (head (s (stype (? st decl imp)) (wh q) (gap -) (wh-var ?foc) (var ?v) (advbl-needed -))))
   
   
   ;; e.g., when?
   ((utt (lf (% speechact (var *) (class ont::sa_wh-question) (constraint (& (content ?v) (focus ?foc)))))
          (var *) (qtype ?type) (punctype ?p))
    -wh-question2> .95 ;; stop using in cca's
    (head (advbl (wh q) (wh-var ?foc) 
                 (gap -) (var ?v) (qtype (? type when where q how-x)))))
   
    ;; "presentational" here: here are the pictures
   ;; test: here is the dog.
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
     (argument (% s (sem ($ f::situation ))))
     (argument ?a)
     (role ?advrole) 
     (var ?advv)
     (arg ?v) (lf ?lf1)
     ;;(lf (% prop (class (:* ont::spatial-loc here)))) ;; this is here to exclude the ont::to-loc, which otherwise comes up 1st
     (lf (% prop (class (:* ont::here here)))) ;; this is here to exclude the ont::to-loc, which otherwise comes up 1st
     )
    ;; use a vp which expects "there" in the sense ont::exists
    (head (vp (lf ?lf) (gap -)
              
	   (subj (% np (sem ?subjsem) (var ?npvar) (agr ?vpagr) (lex there))) 
	   (subj ?subj)
	   (var ?v) (vform fin) (agr ?vpagr)
	   (advbl-needed -)
	   (lf (% prop (transform ?transform) (sem ?ssem) (class ?c) (class ont::exists) (constraint ?con) (tma ?tma)))
	   )
     )
    (add-to-conjunct (val (mods ?advv)) (old ?con) (new ?constraint))
    )

   ;; to the left are the pictures
   ;; test: to the left is the dog.
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
     (argument (% s (sem ($ f::situation ))))
     (argument ?a)
     (role ?advrole) 
     (var ?advv)
     (arg ?subjvar) (lf ?lf1)
     (lf (% prop (class ont::spatial-loc)))
     )
    (head (s (stype ynq) (lf ?lf) (var ?v)	     
	     (advbl-needed -)
	     (subjvar ?subjvar) (dobjvar ?dobjvar)
	     (subj ?subj)
	     (gap (% pred (sem ?predsem) (var ?advv)))
	     (lf (% prop (class ?c) (class ont::have-property) (sem ?ssem) (constraint ?con) (transform ?transform) (tma ?tma)))
	     )
       )
    (add-to-conjunct (val (mods ?advv)) (old ?con) (new ?constraint))
    )
   
   ;; test: why did the dog bark?
   ;; test: why did the dog chase the cat?
   ((s (stype whq) (wh q) (qtype ?qtype) (var ?v) (gap -) (wh-var ?wvar)
     (lf (% prop (var ?v) (class ?c) (constraint ?constraint)
	    (sem ?sem) (tma ?tma) (transform ?transform)
	    ))
     (preadvbl +) (subj ?subj) 
     (advbl-needed -)
     )
     -wh-advbl-q1> .98 ;; really don't want this to apply before gaps
    (advbl
     ;;(argument (% s (sem ?sem) (sem ($ f::situation (f::type f::eventuality)))))
     (var ?advv)
     (arg ?v) (wh q) (wh-var ?wvar) (qtype ?qtype) (lf ?lf1)
     )
    (head (s (stype ynq) (var ?v) (gap -) (subj ?subj) (tag -)
	   (lf (% prop (sem ?sem) (class ?c) (constraint ?con) (tma ?tma) (transform ?transform)))))
    (add-to-conjunct (val (mods ?advv)) (old ?con) (new ?constraint)))

   ;; what time did it arrive?
   ;; test: what time did the dog bark?
   ;; test: what time did the dog chase the cat?
   ((s (stype whq) (wh q) (qtype ?qtype) (wh-var ?tv) (var ?v) (gap -)
       (lf (% prop (var ?v) (class ?c) (constraint ?constraint)
	      (sem ?sem) (tma ?tma) (transform ?transform)
	      ))
     (preadvbl +) (subj ?subj) 
     (advbl-needed -)
       )
    -wh-advbl-q2> .98 ;; don't want this to apply before gaps
    (np (wh q) (var ?tv) (sem ($ f::time (f::time-scale f::point))))  ;; should only be time points, not intervals
    (head (s (stype ynq) (var ?v) (gap -) (subj ?subj) (tag -)
	   (lf (% prop (sem ?sem) (class ?c) (constraint ?con) (tma ?tma) (transform ?transform)))
	   (advbl-needed -)
	   )
     )
    (add-to-conjunct (val (mods 
			   (% *pro* (status ont::f) (var *)
			      (class ont::event-time-rel)
			      (constraint (& (figure ?v) (ground ?tv))))))
			  (old ?con) (new ?constraint))
     )

   ;; to feed into s-that-subjunctive: he recommends that the dog bark
   ;; test: the dog bark
   ;; test: the dog chase the cat.
   ((s (stype subjunctive) (var ?v) (subjvar ?npvar) (gap ?g) (focus ?npvar)
     (lf ?lf) (subj (% np (sem ?npsem) (var ?npvar) ;(agr ?a)
		       (case (? case sub -)) 
		       (pp-word -) (changeagr -))
		    )
     (advbl-needed ?avn)
     )
    -s-subjunctive>  .94 ;; only use if necessary 
    (np (sem ?npsem) (var ?npvar) ;(agr ?a)
	(case (? case sub -)) (lex ?lex) ;; lex needed for expletives?
      (pp-word -) (changeagr -))
    (head (vp (lf ?lf) (gap ?g)
              
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
   ;;   relative clauses

   ;; e.g., (the route) which went from corning to avon
   ;; test: the dog that barked.
   ;; test: the dog that chased the cat.
   ((cp (ctype relc) (arg ?arg) (argsem ?argsem) (var ?v) (gap -) ;; (gap ?g)
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
    (pro (wh (? wh r)) ;(sem ?argsem)
	 (lex ?plex)
     (agr ?agr) (case sub) (headcat ?vcomp))
    (head (vp (subj (% np (var ?arg) (sem ?argsem) ;(sem ($ ?!type))
		       )) (agr ?agr) 
	      (gap -)  ;; my guess is there are some eliiptical glosses that motivated allowing a gap here, but i'm deleting it for now(gap ?g) 
	      (wh -)
	      (class ?c) (constraint ?con) (vform fin) (tma ?tma)
	      (sem ?sem) (lf ?lf)
	      (transform ?transform) (lex ?vlex)
	      (advbl-needed -)
	      (headcat ?hcat) ;; aug-trips
	   )))
   
   ;; test: (the man) whose dog barked
   ((cp (ctype rel-whose) ;(arg ?arg) (argsem ?argsem)
	(gap -) (lf ?lf) (wh r) (wh-var ?whv)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -rel-whose>
    (np (sem ?argsem) (agr ?agr) (case sub) (wh r) (wh-var ?whv) (var ?arg)
     )
    (head (vp (subj (% np (var ?arg) (sem ?argsem) (sem ($ ?!type)) (lf ?nplf) )) (agr ?agr) (gap -) ;;(wh -)
	   (class ?c) (constraint ?con) (vform fin) (tma ?tma)
	   (sem ?sem) (lf ?lf) 
	   (transform ?transform) (lex ?vlex)
	   (advbl-needed -)
	   (headcat ?hcat) ;; aug-trips
	   )))
   
   ;; e.g., (the train) that i moved
   ;; test: the cat that the dog chased.
   ((cp (ctype relc) (arg ?arg) (argsem ?argsem) (gap -)
     (lf ?lf)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     (agr ?agr)
     )
    -rel2> ;; .95
    (pro (wh r) (case ?case) (sem ?argsem) (agr ?agr) 
     (lex ?l) ;;(lex (? l that which who whom)) 
     (headcat ?vcomp))
    (head (s (gap (% np (var ?arg) (sem ?argsem) (case ?case) (agr ?agr))) 
	      (adj-s-prepost -)
	   (wh -) (stype decl)
	   (vform fin) (lf ?lf)
	   (preadvbl -)
	   (advbl-needed -)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   )))

   ;; test: the person on whom he relies
   ((cp (ctype relc) (arg ?arg) (argsem ?argsem) (gap -)
     (lf ?lf)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -rel3> ;;.95
    (prep (lex ?pt))
    (pro (wh r) (sem ?argsem) (agr ?agr) (lex ?l) (headcat ?vcomp))
    ;; myrosia: note that a better representation would be a pp
    ;; (pp (wh r) (ptype ?pt) (sem ?argsem) (agr ?agr) (lex ?l) (headcat ?vcomp))
    ;; but wh-pro1 rule only allows (wh q) for pronouns, throwing the relative pronouns away
    ;; we may want to reconsider this in the future
    (head (s (gap (% pp (ptype ?pt) (var ?arg) (sem ?argsem) (agr ?agr))) 
	      (adj-s-prepost -)
	   (wh -) (stype decl)
	   (vform fin) (lf ?lf)
	   (preadvbl -)
	   (advbl-needed -)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   )))
   
   ;; test: the city in which i live
   ;; fixme:: the argsem isn't being propagated for (wh r) adverbials. so 
   ((cp (ctype relc) (arg ?srole) (argsem ?srolesem) (gap -)
     (lf ?newlf) (agr ?newagr)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -rel4> .92
    ;; fixme -- this is really hacked to transfer the role from the wh pronoun to the np it will eventually modify
    ;; wh r should only come out of advbl-binary-relative
    (advbl-r (atype pre) (argument (% s (sem ?argsem))) 
     (gap -) (wh r)   
     (arg ?v) (var ?mod)
     (subcatsem ?srolesem)
     (arg2 ?srole)
     )
    (head (s (var ?v) (lf ?lf) (lf (% prop (constraint ?con))) (sem ?argsem) (aux -)
	      (adj-s-prepost -)
	    (tma ?tma) (stype (? stp decl imp how-about))
	   (wh -) (stype decl) (vform fin) (preadvbl -) (lex ?hlex) (headcat ?hcat)
	   )
     )
    (add-to-conjunct (val (mods ?mod)) (old ?con) (new ?newcon))
    (change-feature-values (old ?lf) (new ?newlf) (newvalues ((constraint ?newcon))))
    )
  
   ;;  wh-term as setting in an s structure
  
   ((cp (ctype relc) (arg ?arg) (argsem ?advsem) (gap -) (wh -) (agr ?newagr)
     (lf (% prop (var ?v) (class ?c) 
	    (constraint ?constraint) (tma ?tma)
	    (sem ?sem) (transform ?transform)
;;	    (lex ?l) (headcat ?advlh)   ;; non-aug-trips settings
	    (lex ?hlex) (headcat ?hcat) ;; aug-trips
	    )))
   -relc-setting-norole> ;;0.95
   (advbl-r
	  (var ?npvar) (arg2 ?arg) (arg ?v)
	  (argument (% s (sem ?argsem))) (wh q)
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
   ;;(role-unify (arg0 ?advrole) (arg1 ?advsem) (arg2 ?roles) (arg3 ?con))
   (add-to-conjunct (val (mods ?npvar)) (old ?cons) (new ?constraint)))

   
   
   ;; e.g., (the train) i moved
   ;; test: the cat the dog chased.
   ((cp (ctype relc) (arg ?arg) (argsem ?argsem) (gap -) (reduced +)
     (lf ?lf) (agr ?newagr)
     (lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -rel-reduced1-that> .92
    (head (s (gap (% np (var ?arg) (sem ?argsem) (case ?case) (agr ?agr))) 
	     (adj-s-prepost -)
           (wh -) (stype decl)
           (vform fin) (lf ?lf)
           (preadvbl -)
           (advbl-needed -)
           (lex ?hlex) (headcat ?hcat) ;; aug-trips
           )))
   
   ;;  e.g., (the train) going to avon, (the train) loaded with oranges 
   ;; test: the dog barking.
   ;; test: the dog chasing the cat.
   ((cp (ctype relc) (arg ?arg) (argsem ?argsem) (gap -) (reduced +)
	(lf ?lf) (agr ?agr)
	(lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -rel-reduced2> .97
    (head (vp (var ?v)
	   (subj (% np (lex ?lex) (var ?arg) (sem ?argsem) (sem ($ ?!type)))) 	   
	   (agr ?agr) 
           (gap -)
	   (wh -)
	   (class ?c) 
	   (constraint ?con) (vform (? vform ing passive))
	   (sem ?sem) (tma ?tma)
	   (transform ?transform)
	   (lf ?lf)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   )))

;;  e.g., (the train) not going to avon, (the train) not loaded with oranges 
   ;; test: the dog not barking.
   ;; test: the dog not chasing the cat.
   ((cp (ctype relc) (arg ?arg) (argsem ?argsem) (gap -) (reduced +)
; this i think also works, part 1
;	(lf (% prop (var ?v) (constraint ?con) (class ?c) (tma ?newtma)))
	(lf ?newlf) (agr ?agr)
	(lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -rel-reduced2-neg> .97
    (neg (lex w::not))
    (head (vp (var ?v)
	   (subj (% np (lex ?lex) (var ?arg) (sem ?argsem) (sem ($ ?!type)))) 	   
	   (agr ?agr) 
           (gap -)
	   (wh -)
	   (class ?c) 
	   (constraint ?con) (vform (? vform ing passive))
	   (sem ?sem) (tma ?tma)
	   (transform ?transform)
	   (lf ?lf)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   ))
; this i think also works, part 2
;    (append-conjuncts (conj1 (& (negation +))) (conj2 ?tma) (new ?newtma))
     (add-to-conjunct (val (negation +)) (old ?tma) (new ?newtma))
     (change-feature-values (old ?lf) (new ?newlf) (newvalues ((tma ?newtma))))
    )
   
   ;; "that" and "if" sentences, used embedded, as complements.

   ;; e.g., that the train arrives.
   ;; test: i know that the dog barks.
   ;; test: i know that the dog chased the cat.
   ((cp (ctype s-that-overt) (gap -) 
     (lf (% prop (var ?v) (constraint ?con) (class ?lf) (tma ?newtma)))  ;; (lf ?lf) ;; (lex ?lex) (headcat ?vcomp)) ;; non-aug-trips settings
	(lex ?hlex) (headcat ?hcat)) ;; aug-trips
    -s-that-overt>
    (word (lex (? lx that)) ;; if whether))   these are handled by ctype s-if rule   jfa 3/07
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

   ;; test: i prefer that the dog bark.
   ((cp (ctype s-that-subjunctive) (gap -)
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
   
   
   ;; i guess that if the train arrives, we go
    ((cp (ctype s-that-overt) (gap -) 
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
   ;; test: i know the dog barked.
   ;; test: i know the dog chased the cat.
   ((cp (ctype s-that-missing) (gap -) 
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
     (subj (% (? cat np pp) (ptype (? ptype for -)) (case (? case sub obj -)) 
	      (agr ?subjagr) (sem ?subjsem) (var ?subjvar) (transform ?subjtransform)
	;;      (lex ?lex) ;; non-aug-trips settings
	      ))
     (gap ?g)
     (lf ?lf))
     -sand>
     (word (lex and))
     (head (vp (lf ?lf) 
	    (aux -)
	    (gap ?g)
	    (subj (% np (sem ?subjsem) ;; (lex ?lex) ;; non-aug-trips setting
		     (sem ($ ?!type)) (var ?subjvar) (agr ?subjagr) (transform ?subjtransform)))
	    (vform base)
	    (advbl-needed -)
	    (lex ?lex) (headcat ?hcat) ;; aug-trips
	    )))

   ;;aug-trips: non-aug-trips settings have lex and hcat in subj-np
   ;;   infinitive sentences

   ;; e.g., to know the train arrived.
   ;; aux - to exclude cases like "to can ..."
   ;; test: the dog wants to bark
   ((cp (ctype s-to) (var ?v) ;;(subj ?subj) 
     #||(subj (% (? cat np pp) (ptype (? ptype for -)) (case (? case sub obj -)) (lex ?lex)
	      (agr ?subjagr) 
	      (sem ?subjsem) (var ?subjvar) (transform ?subjtransform)
	      ))||#
     (subj ?subj)
     (dobj ?dobj)
     (gap ?g)
     (lf ?lf) ;; (lex to) (headcat ?vinf)) ;; non-aug-trips settings
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
   
   ;; we need a special rule for cases like "to be taken", because it is aux be, but for passives it's fine
   ;; test: they appear to be broken
    ((cp (ctype s-to) (var ?v) (subj ?subj) (gap ?g)
     (lf ?lf)
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
   
   ;; e.g., for the train to arrive
   ((cp (ctype s-to) (var ?v) (subj ?subj) (gap ?g)
     (lf ?lf)
 ;;    (lex to) (headcat ?vinf) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     )
    -for-to1>
    (word (lex for))
    (np (sem ?subjsem) (var ?npvar) (case (? case obj -)))
    (infinitival-to (lex to) (headcat ?vinf) )
    (head (vp (lf ?lf) (gap ?g)
	      (sem ?sem)
	      (subj ?subj) (subj (% np (lex ?lex) (sem ?subjsem) (sem ($ ?!type)) (var ?npvar)))
	      (vform base)
	      (lex ?hlex) (headcat ?hcat) ;; aug-trips
	      (be-there -)
	      )))

      ; test: tell him to not bark
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
    (append-conjuncts (conj1 (& (vform base) (negation +))) (conj2 ?tma) (new ?newtma))
    )

   ;; v from xing, e.g. prevent, prohibit, forbid where we want the objcontrol role on the ing form
   ;; test: he prevented the dog from barking.
   ((cp (ctype s-from-ing) (var ?v) (ptype ?ptype)
     (subj ?subj)
     (dobj ?dobj)
     (gap ?g)
     (lf ?lf) ;; (lex to) (headcat ?vinf)) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat)) ;; aug-trips     
     -prep-ing>
     (prep (lex ?ptype) (headcat ?ving))
     (head (vp (lf ?lf)
	       (advbl-needed -)
	       (gap ?g)
	       (subj ?subj)
	       (vform ing)
	       (dobj ?dobj) 
	       (lex ?hlex) (headcat ?hcat) ;; aug-trips
	       )))
   
     ;;  pred rules: 
     ;;  preds are used for be verbs, and create a constituent that it similar
     ;;    to a lambda experssion in the logical form.
     ;;   adjectives, adverbials, indefinite descriptions or vps can form preds
     ;;  nb: we must put a var feature in the lf of pred's if it it to be picked up in the objects
     ;;     slot of the logical form  - and it can't be the same as the var of an np or 
     ;;      any other major constituent
     ;; e.g., (the train) is orange
     ;; test: the dog is short.
     ((pred (lf ?lf) (arg ?arg) (argument ?argument) (var ?v) (agr ?agr) (sem ?sem)    
            (filled -) (adjectival +) (how ?how)
            )
      -pred-adj> 1
      (head (adjp (lf ?lf) (sem ?sem)
		  (var ?v) (arg ?arg) (argument ?argument) (argument (% ?argcat (var ?arg))) ;;(wh -) eliminated to allow "how red"
	     (set-modifier -) ;; numbers are set-modifier +, and they don't behave as normal adjps in predicates
	     (atype (? atp central predicative-only))
	     ;; md 2008/17/07 eliminated cases with positive post-subcat, they should only happen when an adjective is looking for an argument after an np, not possible in the pred situation
	     (post-subcat -) (how ?how)
	     ))
      )

   ;; test: the dog is from the store.
     ((pred (lf ?lf) (arg ?arg) (argument ?argument) (var ?v) (agr ?agr) (sem ?sem)    
       (filled -) (adjectival -) (gap ?gap)
       (wh ?wh)
       )
      -pred-advbl> 0.98 ;; don't use it if a regular advbl interpretation is possible
      ;; md: argument should be % np, if an adverbial does not apply to nps, don't use it
      (head (advbl (lf ?lf) (var ?v) (arg ?arg) (argument ?argument) 
		   (argument (% np (var ?arg))) (sem ?sem)
		   (wh (? wh - q)) (sort (? srt binary-constraint pp-word pred))
		   (gap ?gap)
	     ))
      )

     ;; e.g., role nps can be predicates "we remained friends"
     
     ((pred (arg ?arg) (var ?v)  (sem ?sem)
            (lf (% prop (status ont::f) (arg ?arg) (var ?v) 
		   (class ?c) (constraint ?newcon)))
            (argument ?argument)
            (filled -)
            )
      -pred4> .97
      (head (np (sem ?sem) (var ?v) (sort pred) (case (? case obj -))
		(derived-from-name -) (gerund -)
		(lf (% description (status (? x ont::indefinite ont::bare ont::indefinite-plural)) 
		       (sem ($ f::phys-obj (f::type ont::role-reln)))
		       (class ?c) (constraint ?constr)))))
      (add-to-conjunct (val (Figure ?arg)) (old ?constr) (new ?newcon))
      )

   ;; a construction that is limited to pred of emotional state 
   ;;  e.g., he is happy/sad/amused that the dog barked
   
   ((pred (arg ?arg) (var ?v)  (sem ?sem)
            (lf (% prop  (var ?v) 
		   (class (? c ont::emotional-val ont::evoke-emotion))
		   (constraint ?constraint)))
            (argument ?argument)
            (filled -)
            )
     -emotional-state-reason>
     (head (pred (var ?v) (arg ?arg) (sem ?sem)
            (lf (% prop (var ?v) 
		   (class (? c ont::emotional-val ont::evoke-emotion))
		   (constraint ?con)))
            (argument ?argument)
            (filled -)
            ))
    (cp (ctype s-that) (var ?reason))
    (add-to-conjunct (val (reason ?reason)) (old ?con) (new ?constraint)))
        
     ;; add tense information (past, present, or future)
     ;; test: the dog barked.
     ;; test: the dog barks.
     ;; test: the dog will bark.
     ((vp (lf (% prop (class ?c) (var ?v) (constraint ?constraint) (tma ?newtma) 
	         (transform ?transf) (sem ?sem)))
          (class ?c) (var ?v) (constraint ?constraint) (tma ?newtma)  (sem ?sem) (transform ?transf)
           (vform (? vf past pres fut)) (dobjvar ?dobjvar)
          )
      -vp-tns+> 1.0 ;; 1 because we are simply adding tense info, not really using more rules
     (head
      (vp- (class ?c)  (constraint ?constraint) (tma ?tma1) 
        (vform (? vf past pres fut)) (subjvar ?sv) (dobjvar ?dobjvar)
       (advbl-needed -)
       )) 
     (add-to-conjunct (val (tense ?vf)) (old ?tma1) (new ?newtma))
     ) 
   
  
    
     ;; vp rules 
     ;; test: he said the dog barked.
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
	    ;; we allow a possible gap in the dobj np e.g., "what did he thwart the passage of"
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    (restr ?prefix)
	   ))
     ?iobj     
     ?dobj
     ?part
     ?comp
     (append-conjuncts (conj1 ?prefix) (conj2 (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) (lcomp ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       ))
		       (new ?newc))
     
     )
    
   ;; case with a gap in the comp3
   ;;  e.g., what did he start to cook
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?!gap) 
      (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) (lcomp ?compvar)
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
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap ?!gap)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   ))
     ?iobj     
     ?dobj
     ?part
     ?comp
     )

   ;; the indirect object as recipient construction
  
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?gap) 
      (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) (lcomp ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (affected-result (% *pro* (status ont::f)
				(class (:* ont::recipient iobj)) (var *) (sem ?subjsem) (constraint (& (ground ?iobjvar) (figure ?v)))))
		       (?comp3-map ?compvar)
		       
                       ))
      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-recipient-iobj> .97   ;; prefer subcategorized readings if possible
     (head (v (aux -) ;; main verbs only
	    (lf ?c)
	    (sem ($ f::situation (f::type ont::giving) (f::iobj f::recipient)))
	    (vform ?vf)
	    (tma ?tma)
	    ;; (subj (? subj (% ?s1 (var ?subjvar))))
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    ;;(iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	    (part (% -)) 
	    (dobj ?dobj) (dobj (% np (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))	    
	            ;; we allow a possible gap in the dobj np e.g., "what did he thwart the passage of"
	    (comp3 (% -))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   ))
     (np (case (? icase obj -)) (var ?iobjvar) (gerund -) (generated -) (sem ($ (? rcp f::phys-obj f::abstr-obj) (f::intentional +))) (gap -))
     ?dobj
     )

    ;; the indirect object beneficiary construction
    ;; eat me a sandwich
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap ?gap) 
      (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) ;;(beneficiary ?iobjvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (beneficiary (% *pro* (status ont::f)
				(class (:* ont::beneficiary iobj)) (var *) (sem ?subjsem) (constraint (& (ground ?iobjvar) (figure ?v)))))
		       (?comp3-map ?compvar)
		       
                       ))
      (tma ?tma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-beneficiary-iobj> .98
     (head (v (aux -) ;; main verbs only
	    (lf ?c)
	    (sem ($ f::situation (f::type ont::event-of-causation)))
	    (vform ?vf)
	    (tma ?tma)
	    ;; (subj (? subj (% ?s1 (var ?subjvar))))
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?subjagr) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    ;;(iobj ?iobj) (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	    (part (% -)) 
	    (dobj ?dobj) (dobj (% np (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap) (gerund -)))	    
	            ;; we allow a possible gap in the dobj np e.g., "what did he thwart the passage of"
	    (comp3 (% -))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   ))
     (np (case (? icase obj -)) (var ?iobjvar) (sem ($ f::phys-obj (f::origin f::living) (f::intentional +) (f::type ont::person))) 
      (generated -) (gap -))
     ?dobj
     )

      
   ;; rule for modal aux's with no vp complements
   ;; test: the dog should.
    ((vp- (subj ?subj)  (subjvar ?subjvar) (lex ?l)
      (var ?v) (class (:* ont::ellipsis ))
      (gap -) 
      (constraint (& (lsubj ?subjvar) (?lsubj-map ?subjvar)))
      (tma ?newtma) (ellipsis +)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-modal-nocomp> .96 ;; prefer aux's with complements, only execute if needed
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +)
	    (contraction -)
	    (lex ?l)
	    (sem ?sem)
	    (vform ?vf)
	    (tma ?tma1)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr)
				  (sem ?subjsem) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map) 
	    ))
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?newtma))
     )
   
    ;; rule for negated modal aux's with no vp complements
    ;; test: the dog shouldn't.
    ((vp- (subj ?subj)  (subjvar ?subjvar) (lex ?l)
      (var ?v) (class (:* ont::ellipsis ))
      (gap -) 
      (constraint (& (lsubj ?subjvar) (?lsubj-map ?subjvar)))
      (tma ?newtma)
      (postadvbl -) (vform ?vf)
      )
     -vp1-role-neg-modal-nocomp> .96 ;; prefer aux's with complements, only execute if needed
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +) ;; should there be (contraction -) there???	    
	    (lex ?l)
	    (sem ?sem)
	    (vform ?vf)
	    (tma ?tma1)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) 
				  (agr ?subjagr) (sem ?subjsem) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map) 
	    ))
     (neg)
     
     ;; we need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((negation +)))) 
     )
   
   
    ;; for negation after contracted forms of main verb be --disallow n't
    ;; test: i'm not short.
    ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -) 
      (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) (lcomp ?compvar)
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
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -))) ;; note double matching required
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
    ;; test: he said yesterday that the dog barked.
    ;; test: he saw on his left a dog barking.
    ; I wrote on the paper my name.
    ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -)  (complex +)
      (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) (lcomp ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       (mods ?mod)
                       ))
      (tma -)
      (postadvbl -) (vform ?vf)
      )
     -vp1-pre-arg-adv> .96 ;; only use if needed; prefer predicate adverbial modification 
     ;; require non-null dobj to avoid using with intransitive verbs (maybe also need a non-null comp rule??)
     (head (v (aux -)
	    (lf ?c)
	    (lex ?lx)
	    (sem ?argsem) (sem ($ f::situation (f::type ont::situation-root)))
	    (vform ?vf)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr)
				  (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj (% -))
	    (part ?part) 
	    (dobj ?!dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     
	    ))
     (advbl (atype pre-vp) (gap -)
      (argument (% s (sem ?argsem)))
      (arg ?v) (var ?mod) (role ?advrole) (subcat -))      
     ?!dobj  
     ?part
     ?comp
     )
   
     ;; for pre-adverbials after main verb be
     ;; test: dogs are always short.
    ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -) 
      (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) (lcomp ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       (mods ?mod)
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
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj) (dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     
	    ))
     (advbl (atype pre-vp) (gap -) (lf (% prop (class ont::frequency))) (np-advbl -)  ;; don't allow time nps here, e.g., "three years"
      (argument (% s (sem ?argsem)))
      (arg ?v) (var ?mod) (role ?advrole) (subcat -))      
     ?dobj
     ?part
     ?comp
     )

   
   ;; for pre-adverbials after main verb be
   ;; test: 5 is also not a short circuit
    ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (var ?v) (class ?c) (gap -) 
      (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		       (liobj ?iobjvar) (lcomp ?compvar)
		       (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		       (mods ?mod)
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
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr)  (sem ?subjsem) (gap -))) ;; note double matching required
	    (iobj (% -))
	    (part ?part) 
	    (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	    (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	    (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     
	    ))
     (advbl (atype pre-vp) (gap -)
      (argument (% s (sem ?argsem)))
      (arg ?v) (var ?mod) (role ?advrole) (subcat -))      
     (neg (lex not))
     ?dobj
     ?part
     ?comp
     )

   
     
   ;; vp rule with dobj gap
   ;; test: who did he see
   ((vp- (subj ?subj) (subjvar ?subjvar) (dobjvar ?gapvar) ;(dobjvar ?dobjvar)
     (main +) (gap (% ?!cat (var ?gapvar) (sem ?gapsem) (agr ?gapagr) (arg ?arg) (gap -) 
		      (case ?dcase) (ptype ?ptype)
		      ))
     (var ?v) 
     (class ?c)
     (constraint ?newc)
     (postadvbl -)
     )
    -vp-dobj-gap-role> ;;.97
    (head (v (aux -)  (be-there -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (vform ?tense-pro)
             (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -))) ;; note double matching required
	     (iobj ?iobj) (iobj (% ?s2 (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))
	     (part ?part) 
	     (dobj ?!dobj) (dobj (% ?!cat (case (? dcase obj -)) (var ?gapvar) (arg ?arg) (sem ?gapsem) (agr ?gapagr) (ptype ?ptype))) ;; must have a possibility of np dobj
	     (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -))) 
	     (subj-map ?lsubj-map) (dobj-map ?!dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	     (restr ?prefix)
	     ))
    ?iobj     
    ?part
    ?comp
    (append-conjuncts (conj1 ?prefix)
		      (conj2 (& (lsubj ?subjvar) (lobj ?gapvar)
			       (liobj ?iobjvar) (lcomp ?compvar)
			       (?lsubj-map ?subjvar) (?!dobj-map ?gapvar)
			       (?iobj-map ?iobjvar) (?comp3-map ?compvar)
			       ))
		       (new ?newc))

    )
   
   ;; vp rules where the object pp or pred has a gap
   ;; test: who did he talk to?
   ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
     (var ?v) (class ?c) (gap ?!gap) (be-there -)
     (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		    (liobj ?iobjvar) (lcomp ?compvar)
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
	   (dobj ?dobj) (dobj (% (? s3 pp pred) (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?!gap)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    (be-there -)
	   ))
    
    ?iobj 
    ?dobj
    ?part
    ?comp
    )
   
   ;; vp rules where the object pp or pred has a gap
   ((vp- (subj ?subj)  (subjvar ?subjvar) (dobjvar ?dobjvar)
     (var ?v) (class ?c) (gap ?!gap) 
     (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		    (liobj ?iobjvar) (lcomp ?!compvar)
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
	   (comp3 ?!comp) (comp3 (% (? s4 pp pred) (case (? ccase obj -)) (var ?!compvar) (sem ?compsem) (gap ?!gap)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	    
	   (be-there -)
	   ))
    
    ?iobj     
    ?dobj
    ?part
    ?!comp
    )
   
   ;; vp rule with comp3 gap  (typically a path from a "where" question or a prepositional phrase)
   ;; where did he come from
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar)
     (main +) (gap (% ?s4 (var ?!gapvar) (case ?ccase) (agr ?gapagr) (sem ?compsem) (ptype ?ptype) (arg ?arg)
		      ))
     (var ?v) 
     (class ?c) (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
				 (liobj ?iobjvar) (lcomp ?!gapvar)
				 (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
				 (?iobj-map ?iobjvar) (?comp3-map ?!gapvar)
				 ))
     (postadvbl -)
     )
    -vp-comp-gap1-role> .97
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (vform ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -))) ;; note double matching required
	   ;;(subjvar ?subjvar)
	   (iobj ?iobj)  (iobj (% ?s2 (var ?iobjvar) (sem ?iobjsem) (gap -) (case (? icase obj -))))
	   (part ?part) 
	   (dobj ?dobj)			
	   (dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (gap -) (case (? dcase obj -))))
	   (comp3 ?!comp) (comp3 (% ?s4 (var ?!gapvar) (sem ?compsem) (agr ?gapagr) (case (? ccase obj -)) (ptype ?ptype) (arg ?arg)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   
	   (be-there -)
	   ))
     ?iobj     
     ?dobj
     ?part
    )

   ;; vp rule with comp3 gap when a verb has a particle
   ;; the difference from comp-gap1-role is that part is bound to be non-empty by double-matching on (part (% word))
   ;; and then the direct object can come after the particle
   ;; the double matching is necessary to differentiate from the cases where there is a non-empty particle
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar)
     (main +) (gap (% ?s4 (var ?!gapvar) (case ?ccase) (agr ?gapagr) (sem ?compsem) (ptype ?ptype) (arg ?arg)
		      ))
     (var ?v) 
     (class ?c) (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
				 (liobj ?iobjvar) (lcomp ?!gapvar)
				 (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
				 (?iobj-map ?iobjvar) (?comp3-map ?!gapvar)
				 ))
     (postadvbl -)
     )
    -vp-comp-gap1-part-role> .97
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (vform ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -))) ;; note double matching required
	   (iobj ?iobj)  (iobj (% ?s2 (var ?iobjvar) (sem ?iobjsem) (gap -) (case (? icase obj -))))
	   (part ?!part) ;;(part (% part))
	   (dobj ?dobj)	(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (gap -) (case (? dcase obj -))))
	   (comp3 ?!comp) (comp3 (% ?s4 (var ?!gapvar) (sem ?compsem) (agr ?gapagr) (case (? ccase obj -)) (ptype ?ptype) (arg ?arg)))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   
	   (be-there -)
	   ))
     ?iobj     
    ?!part
    ?dobj
    )

    
   ;; vp rule with a vp comp3 with a gap inside it
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar)
    (main +) (gap ?!gap)
    (var ?v) 
     (class ?c) (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
				 (liobj ?iobjvar) (lcomp ?!compvar)
				 (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
				 (?iobj-map ?iobjvar) (?comp3-map ?!compvar)
				 ))
     (postadvbl -)
     )
    -vp-comp-gap2-role> 0.97
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root))) (vform ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -))) ;; note double matching required
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
    (bound (arg ?!gap))  ;; remember, we are parsing bottom up, so this must be bound from below
    )
      
   ;; particles with no independent semantics
   ;;  test: load up the oranges  ;; note: example isobsolete as up will be compositional!!!
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar) (main +)
      (class ?c) (var ?v) 
     (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		      (lcomp ?compvar)
		      (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		      (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		      ))
     (postadvbl -)
     )
    -vp-particle-role> 1  ;; boost this as finding the particle is good evidence for the interpretation
    (head (v (aux -)
	   (lf ?c) (sem ($ f::situation (f::type ont::situation-root)))  (vform ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -) )) ;; note double matching required
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

   
   ;;  test: move up the blocks (i.e., move the blocks up)
   ;;  push up the window
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar) (main +)
      (class ?c) (var ?v) 
     (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		      (lcomp ?compvar)
		      (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		      (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		      (result ?adv-v)
		      ))
     (postadvbl -)
     )
    -vp-compositional-particle-role> 
    (head (v (aux -) (var ?v)
	   (lf ?c) (sem ?sem) (sem ($ f::situation (f::type ont::event-of-change)))  (vform ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -) )) ;; note double matching required
	   (iobj (% -))
	   (part (% -)) ;;(part (% part))
	   (dobj ?dobj)			(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (case (? dcase obj -)) ))
	   (comp3 ?comp)		(comp3 (% ?s4 (var ?compvar) (sem ?compsem) (case (? ccase obj -))))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   (result ?asem)
	   ))
    ;(advbl (particle +) (var ?adv-v)  (arg ?v) (argument (% s (sem ?sem))) (gap -))
    (advbl (particle +) (particle-role-map result) (var ?adv-v) (arg ?dobjvar) 
	   (argument (% np (sem ?dobjsem))) (gap -)
	   (sem ?asem))
    ?dobj
    ?comp
    )

   ;;  test: clean up your room
   ;;  wash up the dishes
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar) (main +)
      (class ?c) (var ?v) 
     (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		      (lcomp ?compvar)
		      (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		      (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		      (manner ?adv-v)
		      ))
     (postadvbl -)
     )
    -vp-compositional-particle-manner-role> 
    (head (v (aux -) (var ?v)
	   (lf ?c) (sem ?sem) (sem ($ f::situation (f::type ont::event-of-change)))  (vform ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -) )) ;; note double matching required
	   (iobj (% -))
	   (part (% -)) ;;(part (% part))
	   (dobj ?dobj)			(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (case (? dcase obj -)) ))
	   (comp3 ?comp)		(comp3 (% ?s4 (var ?compvar) (sem ?compsem) (case (? ccase obj -))))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   (result ?asem)
	   ))
    ;(advbl (particle +) (var ?adv-v)  (arg ?v) (argument (% s (sem ?sem))) (gap -))
    (advbl (particle +) (particle-role-map manner) (var ?adv-v) (arg ?v) 
	   (argument (% s (sem ?sem))) (gap -)
	   (sem ?asem))
    ?dobj
    ?comp
    )
   
   ;; either this rule or we need a version of -vp1-pre-arg-adv> whose advbl takes an NP as argument
   ;;  test: I moved to the table the box
   ((vp- (subj ?subj) (subjvar ?subjvar)  (dobjvar ?dobjvar) (main +)
      (class ?c) (var ?v) 
     (constraint (& (lsubj ?subjvar) (lobj ?dobjvar)
		      (lcomp ?compvar)
		      (?lsubj-map ?subjvar) (?dobj-map ?dobjvar)
		      (?iobj-map ?iobjvar) (?comp3-map ?compvar)
		      (result ?adv-v)
		      ))
     (postadvbl -)
     )
    -vp-compositional-result-role> 
    (head (v (aux -) (var ?v)
	   (lf ?c) (sem ?sem) (sem ($ f::situation (f::type ont::event-of-change)))  (vform ?tense-pro)
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (agr ?subjagr) (sem ?subjsem) (gap -) )) ;; note double matching required
	   (iobj (% -))
	   (part (% -)) ;;(part (% part))
	   (dobj ?dobj)			(dobj (% ?s3 (var ?dobjvar) (sem ?dobjsem) (case (? dcase obj -)) ))
	   (comp3 ?comp)		(comp3 (% ?s4 (var ?compvar) (sem ?compsem) (case (? ccase obj -))))
	   (subj-map ?lsubj-map) (dobj-map ?dobj-map) (iobj-map ?iobj-map) (comp3-map ?comp3-map)
	   (result ?asem)
	   ))
    ;(advbl (particle +) (var ?adv-v)  (arg ?v) (argument (% s (sem ?sem))) (gap -))
    (advbl (result-only +) (var ?adv-v) (arg ?dobjvar) 
	   (argument (% np (sem ?dobjsem))) (gap -)
	   (sem ?asem))
    ?dobj
    ?comp
    )
 
   
   ;; passive transformations

   ;; lexical transformation rules for passives.  
   ;; there are rules for moving the direct object and the indirect object,
   ;; each case consists of one rule with and one without the "by" pp.

   ;;  these rules aren't ideal.  they don't allow the "by" pp
   ;; (logical subject) in vps that also have other pps.  they also
   ;; don't allow the object of one pp from a pps to move.

   ;; test: the dog was given (me)
   ((v (vform passive) (passive +)
     (subj ?!dobj) (subj-map ?dobj-map) 
     (dobj (% -)) (agent-map ?subj-map) (agent-sem ?subjsem)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
;     (prefix ?prefix)
     (restr ?prefix)
     )
    -v-passive> 1.0 
    (head (v (vform pastpart) (lex (? !lx been)) ;; exclude be
	   (subj ?subj) (subj-map ?subj-map) ;; please don't remove - this is needed for trips-tflex conversion
	   (subj (% ?s (sem ?subjsem)))
	   (dobj ?!dobj) (dobj-map ?dobj-map) (exclude-passive -)
	   (iobj ?iobj) (iobj-map ?iobj-map)
	   (comp3 ?comp3) (comp3-map ?comp-map)
	   (part ?part)
;	   (prefix ?prefix)
	   (restr ?prefix)
	   )))

   ;; test:  the dog is taken care of
   ((v (vform passive)  (passive +)
     (subj (% np (lex ?dobjlex) (sem ?dobjsem) (var ?dobjvar) (agr ?dobjagr)))
     (subj-map ?dobj-map) 
     ;(dobj (% prep (lex ?pt)))
     (dobj (% -)) 
     (dobj-map -)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
     )
    -v-passive-pp> 1.0
    (head (v (vform pastpart) (lex (? !lx been)) (exclude-passive -);; exclude be
	   (subj (% np (lex ?subjlex) (sem ($ ?!type))))
	   (dobj (% pp (ptype ?pt) (lex ?dobjlex) (sem ?dobjsem) (var ?dobjvar) (agr ?dobjagr)))
	   (dobj-map ?dobj-map)
	   (iobj ?iobj) (iobj-map ?iobj-map)
	   (comp3 ?comp3) (comp3-map ?comp-map)
	   (part ?part)))
    (prep (lex ?pt))
    )

   ;; test: the dog was given me by him
   ;; e.g. these are separated from the battery by a gap
   ;; note that comp3 is promoted to dobj
   ((v (vform passive)  (passive +)
       (subj ?!dobj) (subj-map ?dobj-map) 
     ;;       (dobj (% -)) 
     (dobj ?comp3) (dobj-map ?comp3-map)     
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 (% pp (ptype by) (gerund -) (sem ?!subj-sem))) (comp3-map ?subj-map)
     (part ?part)) 
    -v-passive-by> 1.0
    (head (v (vform pastpart) (lex (? !lx been)) (exclude-passive -);; exclude be
	     (subj (% np (lex ?subjlex) (sem ?!subj-sem) (agr ?subjagr) (sem ($ ?!type))))
	     (subj-map ?subj-map) (iobj-map ?iobj-map)
	     (dobj ?!dobj) (dobj-map ?dobj-map)
	     (iobj ?iobj) 	  
	     (comp3 ?comp3) (comp3-map ?comp3-map)
	     (part ?part)))
    )
   
   ;; test: i was given the dog
   ;;  passive form with indirect object
   ((v (vform passive)  (passive +)
       (subj ?!iobj) (subj-map ?iobj-map) 
       (iobj-passive +)
       (dobj ?!dobj) (dobj-map ?dobj-map)
       (iobj (% -)) 
       (comp3 ?comp3)  (comp3-map ?comp3-map)
       (part ?part)) 
    -v-passive-iobj> 1.0
    (head (v (vform pastpart) (lex (? !lx been)) ;; exclude be
	   (subj ?subj) ;; please don't remove - this is a dummy unrestricted var needed for trips-tflex conversion
	   (dobj ?!dobj) (dobj-map ?dobj-map)
	   (iobj ?!iobj) (iobj-map ?iobj-map) 
	   (comp3 ?comp3) (comp3-map ?comp3-map)
	   (part ?part))))
   
   ;; test: i was given the dog by him

   ((v (vform passive)  (passive +) (lex (? !lx been)) ;; exclude be
       (subj ?!iobj) (subj-map ?iobj-map) 
       (iobj-passive +)
       (dobj ?!dobj) (dobj-map ?dobj-map)
       (iobj (% -)) (iobj-map -)
       (comp3 (% pp (ptype by) (sem ?lsubj-sem)))  (comp3-map ?subj-map)
       (part ?part)) 
    -v-passive-iobj-by>
    (head (v (vform pastpart)
	   (subj ?subj) ;; please don't remove - this is a dummy unrestricted var needed for trips-tflex conversion
	   (dobj ?!dobj) (dobj-map ?dobj-map)
	   (iobj ?!iobj) 
	   (comp3 ?comp3) (comp3-map (% -))
	   (part ?part))))

   ;; the dog was computer generated.  It is FDA approved.
   ((vp- (vform passive)  (passive +)
     (subj-map ?subj-map) (subj ?subj)
     ;;(dobj (% np (sem ?sem) (var ?v-n) (agr ?a) (lex ?lex) (gap -) (RESTR ?nr))) 
     ;;(dobj-map ?subj-map)
     (class ?class)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (subjvar ?subjvar)
     ;;(comp3 ?comp3) (comp3-map ?comp3-map)
     (constraint (& (?ag-map ?v-n) ;(ont::agent ?v-n)
			   ;;(% *PRO* (class ?nc) (status ont::bare) (constraint ?nr) (var ?v-n)))
		    (?subj-map ?subjvar)
		    (?dobj-map  ?dobjvar)
		    (?iobj-map ?iobjvar)))
		    
     ;(part (% -))
     ) 
    -v-passive+prenom-subj> 
    (np (sort ?sort) (LF (% description (status (? x ont::bare ont::name)))) ;;(RESTR ?nr) (status ?status) 
     (complex -) (gerund -) (var ?v-n) 
     (sem ?sem) (relc -) (abbrev -) (gap -) (agr ?a) (lex ?lex)
     )
    (head (v (vform passive) (lex (? !lx been)) (exclude-passive -);; exclude be
	     (lf ?class) (agent-map ?ag-map) (agent-sem ?sem)
	     (subj (% np (lex ?subjlex) (sem ?!subj-sem) (agr ?subjagr) (var ?subjvar) (sem ($ ?!type))))
	     (subj-map ?subj-map) (iobj-map ?iobj-map)
	     ;;(dobj ?!dobj) 
	     (dobj-map ?dobj-map) (dobj (% ?s3 (agr ?dobjagr) (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))
	     (iobj ?iobj)  (iobj (% ?s2  (case (? icase obj -)) (var ?iobjvar) (sem ?iobjsem) (gap -)))     
	     (comp3 (% -));; (comp3-map ?comp3-map)
	     (part ?part)
	     (restr ?con)
	     ))
    ?part
    )
    
   
   ;;;   ;;  e.g., terminal 1 is separated by a gap from a positive terminal
   ;;; "by" comes before a subcategorized pp argument
   ((v (vform passive)  (passive +)
       (subj ?!dobj) (subj-map ?dobj-map) 
       (comp3 ?!comp3) (comp3-map ?comp-map)
       (iobj ?iobj) (iobj-map ?iobj-map)
       (dobj (% pp (ptype by) (gerund -) (sem ?!subj-sem))) (dobj-map ?subj-map)
       (part ?part)) 
    -v-passive-by-reversed>
    
    (head (v (vform pastpart) (lex (? !lx been)) ;; exclude be
	   (subj (% np (lex ?subjlex) (sem ?!subj-sem) (agr ?subjagr) (sem ($ ?!type))))
	   (subj-map ?subj-map) (iobj-map ?iobj-map)
	   (dobj ?!dobj) (dobj-map ?dobj-map)
	   (iobj ?iobj) 
	   (comp3 ?!comp3) (comp3-map ?comp-map)
	   (part ?part))
     )
    )
#||  tp be completed
   ;;  non-verbal expression construction, e.g., He barked the orders
    ((v (vform ?vf)
       (subj ?!dobj) (subj-map ?dobj-map) 
       (dobj (% np (sem ?!subj-sem))) (dobj-map :neutral)
       (part ?part)) 
    -nonverbal-expression-manner>
     (head (v (vform ?vf) 
	   (subj (% np (lex ?subjlex) (sem ?!subj-sem) (sem ($ ?!type))))
	   (subj-map ?subj-map) (iobj-map ?iobj-map)
	   (dobj -) 
	   (comp3 -)
	   (part ?part))
     )
    )
   ||#

   ;;  prefixes on verbs
      
   ((v (vform ?vf) 
     (subj ?subj) (subj-map ?subj-map) 
;     (dobj ?!dobj) (dobj-map ?dobj-map)
     (dobj ?dobj) (dobj-map ?dobj-map)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
;     (prefix (:mod (% *pro* (status ont::f) (class ?qual)
     (restr ?newc)
     )
    -v-prefix-hyphen> 1.01 
    (adv (prefix +)
;     (lf ?qual) (arg ?v) (var ?adv-v) (wh -)
     (lf ?qual) (var ?adv-v) (wh -)
     (argument (% s (sem ?argsem))) 
     )
    (word (lex w::punc-minus))
    (head (v (var ?v) (vform ?vf) (lex (? !lx been)) ;; exclude be
	     (subj ?subj) (subj-map ?subj-map) ;; please don't remove - this is needed for trips-tflex conversion
;	     (dobj ?!dobj) (dobj-map ?dobj-map) (exclude-passive -)
	     (dobj ?dobj) (dobj-map ?dobj-map) (exclude-passive -)
	     (iobj ?iobj) (iobj-map ?iobj-map)
	     (comp3 ?comp3) (comp3-map ?comp-map)
	     (part ?part) (restr ?prefix)
	     (passive -)))  ;; we do the prefix first, then the passive
     (append-conjuncts (conj1 ?prefix) (conj2 (& (mod (% *pro* (status ont::f) (class ?qual)
				   (var ?adv-v) (constraint (& (figure ?v)))))))
		       (new ?newc))
    )

   ((v (vform ?vf) 
     (subj ?subj) (subj-map ?subj-map) 
     (dobj ?dobj) (dobj-map ?dobj-map)
     (iobj ?iobj) (iobj-map ?iobj-map)
     (comp3 ?comp3) (part ?part) (comp3-map ?comp-map)
;     (prefix (:mod (% *pro* (status ont::f) (class ?qual)
     (restr ?newc)
     )
    -v-prefix> 1.01 
    (adv (prefix +)
;     (lf ?qual) (arg ?v) (var ?adv-v) (wh -)
     (lf ?qual) (var ?adv-v) (wh -)
     (argument (% s (sem ?argsem))) 
     )
    (head (v (var ?v) (vform ?vf) (lex (? !lx been)) ;; exclude be
	     (subj ?subj) (subj-map ?subj-map) ;; please don't remove - this is needed for trips-tflex conversion
	     (dobj ?dobj) (dobj-map ?dobj-map) (exclude-passive -)
	     (iobj ?iobj) (iobj-map ?iobj-map)
	     (comp3 ?comp3) (comp3-map ?comp-map)
	     (part ?part) (restr ?prefix)
	     (passive -)))  ;; we do the prefix first, then the passive
     (append-conjuncts (conj1 ?prefix) (conj2 (& (mod (% *pro* (status ont::f) (class ?qual)
				   (var ?adv-v) (constraint (& (figure ?v)))))))
		       (new ?newc))
    )

   
   ;; ditransitive transformation 
   
      ;;  this rule makes the iobj optional
      ;; test: give the dog
   ((v (iobj (% -)) (subj ?subj) (dobj ?dobj) (comp3 ?comp3) (part ?part) (vform ?vform)
       (subj-map ?subjvar) (dobj-map ?dobjvar)
       (comp3-map ?compvar))
     -iobj-delete> .98 ;; make sure you don't delete real iobj
     (head (v (iobj (% np)) (subj ?subj) (dobj ?dobj) (comp3 ?comp3) (iobj-passive -) (part ?part) (vform ?vform)
              (subj-map ?subjvar) (dobj-map ?dobjvar)
	    (comp3-map ?compvar))))

   
   ;; there transformation - be and exist forms can take there as a subject
   ;; for objects
   ;; test: there exists a dog
    ((v (vform ?vform) (be-there +)
      (subj (% np (lex there) (agr ?agr) (sem ($ -)))) (subj-map -)
      (subjvar -)
      (dobj ?!subj) (dobj-map ?subj-map)
      (iobj (% -)) (iobj-map -)
      (comp3 (% -)) (comp3-map -)      
      (part (% -)) 
      )
     -be-there-sense> .97
     (head (v (vform ?vform) (be-there -)
	    (agr ?agr)
	    (lf ont::exists) (transform ?transform)
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
      (subj (% np (lex there) (agr ?agr) (sem ($ -)))) (subj-map -)
     (subjvar -)
      (dobj ?!subj) (dobj-map ?subj-map)
      (iobj (% -)) (iobj-map -)
      (comp3 ?dobj) (comp3-map ?dobj-map)
      (part (% -))
      )
     -be-there-sense1> .97
     (head (v (vform ?vform) (be-there -)
	    (agr ?agr)
	    (lf ont::have-property) (transform ?transform)
	    (lex (? lex be was were been is ^s are))
	    (sem ?sem)
	    (subj ?!subj) (subj-map ?subj-map)
	    (subj (% np (agr ?agr)))
	    (dobj ?dobj) (dobj (% pred)) (dobj-map ?dobj-map)
	    (iobj (% -)) (iobj-map -)
	    (comp3 (% -)) (comp3-map -)
	    (part (% -))
	    )))   

   
   ;; a transformation on be to take expletive + filled predicate
   ;; It is...
    ((v (vform ?vform) (be-there +)
      (subj (% np (lex it) (agr 3s) (expletive +) (sem ($ -)))) (subj-map -)
      (dobj ?dobjnew) (dobj-map ?dobj-map)
      (iobj (% -)) (iobj-map -)
      (comp3 (% -)) (comp3-map -)
      (part (% -))
      )
     -be-expletive-sense> .97 
     (head (v (vform ?vform) (be-there -)
	    (agr 3s)
	    (lf ont::have-property) (transform ?transform)
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
   
   ;; test: the dog does bark.
   ((aux
     (tma-contrib (& (modality ?auxlf) (negation ?neg)))
     (sem-contrib *nochange*)
     )
    -aux-from-modal-v-nochangesem> 1.0
    (head (v (aux +) (modal +) (lex ?l)
	   (changesem -)
	   (lf ?auxlf)
	   (neg ?neg)
	   (tma ?tma1)
	   ))
    )

   ;; test: the dog can bark.
   ((aux
     (tma-contrib (& (modality ?auxlf) (negation ?neg)))
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

   ;; test: the cat was chased.
   ((aux
     (tma-contrib (& (?auxname +) (negation ?neg)))
     (sem-contrib *nochange*)
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

   ;; test: the dog is barking.
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


 ;;== new passive treatment ====
(parser::augment-grammar
 '((headfeatures
    (vp- vform var agr neg sem iobj dobj comp3 part cont   tense-pro aux modal auxname lex headcat transform subj-map advbl-needed passive subjvar template result)
    (v lex sem lf neg var agr cont aux modal auxname ellipsis tma transform headcat)
    (cp vform neg sem subjvar dobjvar cont  transform)
    )


   ;; i know if/whether it's real
   ((cp (ctype s-if) (var *) (gap ?g) (condition ?lf)
     ;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     (lf (% prop (var *) (class (:* ont::clause-condition ?lex))
	    (constraint (& (content ?v)))
	    ))
     (clex ?lex)
     )
    -s-ifa> 
    (word (lex (? lex if whether)))
    (head (s (stype decl) (var ?v) (main -) (gap ?g) (vform fin)
	   (advbl-needed -) (adj-s-prepost -)
	   (lf (% prop (var ?v) (class ?c) (constraint ?con)))
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   ))
    )

   
   ;; e.g., tell him whether to leave
   ;; test: if the dog barks.
   ;; test: unless the dog chases the cat.
   ((cp (ctype s-if) (var *) (gap ?g) (condition ?lf)
	;; (lex ?l) (headcat ?vcomp) ;; non-aug-trips settings
     (lex ?hlex) (headcat ?hcat) ;; aug-trips
     (lf (% prop (var *) (class (:* ont::clause-condition ?lex)) 
	    (constraint (& (content ?s-v)))
	    ))
     (clex ?lex)
     )
    -s-ifa-to> .97 ;; set this below wh-descg1a, but above adv-vp-post
    (word (lex (? lex if whether)))
    (head (cp (ctype s-to) (sem ?argsem) (var ?s-v) (lf ?lf-s) (gap -)
	   (lex ?hlex) (headcat ?hcat) ;; aug-trips
	   (lf (% prop (transform ?transform) (sem ?argsem) (class ?c) (constraint ?con))))
     )
    )

    ; test: tell him not to bark
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
	   (lf (% prop (transform ?transform) (sem ?argsem) (class ?c) (constraint ?con) (tma ?tma))))
     )
    (append-conjuncts (conj1 (& (negation +))) (conj2 ?tma) (new ?newtma))
    )
  
   ))

;;  a few rules with exceptional head features
(parser::augment-grammar
  '((headfeatures
     (vp vform agr neg sem var subjvar dobjvar cont  lex headcat tma transform subj-map advbl-needed template)
    (s vform neg cont  lex headcat transform advbl-needed)
    (v lex sem lf neg var agr cont lex transform aux modal tma advbl-needed headcat)
    (utt subjvar dobjvar cont lex headcat transform))  ;; I removes the SEM as i don't think it makes sense to move up to an S/UTT

    ;; ynq questions with be because subjvar must be set properly
    
    
   ;;  these ynq forms occus only with the verb to be.
   ;; they are hard to capture using the general mechanisms for the be serves as the aux and the main verb
   ;; test: ; is the train late?
   ;;       is there a train?
   ((s (stype ynq) (main +) (aux -) (gap ?gap)
     (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar) (agr ?subjagr)))
     (sort pred) 
     
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
    (head (v (aux -)

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
	   (subj ?subj) (subj (% ?s1 (var ?subjvar) (sem ?subjsem) (agr ?subjagr) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	   (part (% -));; (part ?part) 
	   (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem)))
	    
	   )
     
     )
    ;;?subj
    (np (var ?subjvar) (agr ?subjagr) (case (? npcase sub -)) (sem ?subjsem) (wh -) (sort (? !sort wh-desc)) (gap -) (lex ?subjlex) )  ;; lots of restrictions on this np to eliminate sentences like "is where the people"
    ?dobj
    ?comp
    (add-to-conjunct (val (tense (? vf past pres))) (old ?tma) (new ?newtma))
    )
   
   
   ;; test: ; isn't the train late?
   ;;       isn't there a train?
     ((s (stype ynq) (main +) (aux -) (gap ?gap)
       (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar) (agr ?subjagr)))
       (sort pred) 
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
      (head (v (sem ?sem) (aux -) (var ?v)
	   (advbl-needed ?avn)
	   (lf ?belf)
	   (subj-map ?subj-map) (dobj-map ?dobj-map) (comp3-map ?comp3-map)
	   (agr ?subjagr) 
	   (tma ?tma) (vform ?vf) (transform ?transform)
	   ;; no uniform & unique lf's for main verb be uses, so have to match the lex
	   ;; unless we can match the lf-form be
	   (lex (? lx am are is was were ^s))
	   (subj ?subj) (subj (% ?s1 (var ?subjvar) (sem ?subjsem) (agr ?subjagr) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	    (part (% -)) ;; (part ?part) 
	   (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) ))
	    
	   )
     
       )
      (neg (lex n^t))
    ;;?subj
    (np (var ?subjvar) (case (? npcase sub -)) (sem ?subjsem) (wh -) (sort (? !sort wh-desc)) (gap -) (lex ?subjlex) )  ;; lots of restrictions on this np to eliminate sentences like "is where the people"
    ?dobj
      ?comp
      (append-conjuncts (conj1 (& (tense (? vf past pres)) (negation +))) (conj2 ?tma) (new ?newtma))
      ;;    (add-to-conjunct (val (tense (? vf past pres fut))) (old ?tma) (new ?newtma))
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
     (sort pred) 
     (lf (% prop (var ?v) (class ?belf)
	    (constraint (& (lsubj ?subjvar)
			   (lobj ?dobjvar) 
			   (?subj-map ?subjvar) (?dobj-map ?dobjvar)
			   ))
	    (sem ?sem) (tma ?tma)
	    (transform ?transform)
	    ))
     (gap (% ?!s3 (case ?dcase) (agr ?dagr) (var ?dobjvar) (sem ?dobjsem) (gap -)))
     (advbl-needed ?avn)
     )
    -s-ynq-be-gap> 0.96  ;; downgrade this until we find an example that needs this rule!!  Why do we not require the V to be ONT::BE or ONT::EQUAL????
    (head (v (aux -)
	   
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
	   (subj ?subj) (subj (% ?s1 (var ?subjvar) (sem ?subjsem) (agr ?subjagr) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	   (part (% -))		 
	   (dobj ?!dobj) (dobj (% ?!s3 (case (? dcase obj -)) (agr ?dagr) (var ?dobjvar) (sem ?dobjsem) (gap ?gap)))
	   (comp3 (% -))
	   ;;	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) ))
	    
     ))
    ;;?subj
    (np (var ?subjvar) (case sub) (sem ?subjsem) (agr ?subjagr) (wh -) (sort (? !sort wh-desc)) (gap -) (lex ?subjlex) )  ;; lots of restrictions on this np to eliminate sentences like "is where the people"
    ;;?!dobj
    (add-to-conjunct (val (tense (? vf past pres fut))) (old ?tma) (new ?newtma))
    )

   
   
   ;;  these ynq forms occus only with the verb to be.
   ;; they are hard to capture using the general mechanisms for the be serves as the aux and the main verb
   ;; e.g., is the train late?
   ; Is the pizza quickly cold?
   ((s (stype ynq) (main +) (aux -) (gap -)
     (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar) (agr ?subjagr)))
     (sort pred) 
     
     (var ?v) ;; propagate up explicitly because not a head feature	   
     (agr ?subjagr) ;; propagate up explicitly because not a head feature
     (sem ?sem) ;; propagate up explicitly because not a head feature
     (transform ?transform) ;; propagate up explicitly because not a head feature
      ;; propagate up explicitly because not a head feature	
     (subjvar ?subjvar) (dobjvar ?dobjvar)
     
     (lf (% prop (var ?v) (class ?belf)
	    (constraint (& (lsubj ?subjvar)
			   (lobj ?dobjvar) 
			   (comp3 ?compvar)
			   (?subj-map ?subjvar) (?dobj-map ?dobjvar)
			   (?comp3-map ?compvar) 
			   (mods ?mod)
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
	   (subj ?subj) (subj (% ?s1 (case sub) (var ?subjvar) (sem ?subjsem) (agr ?subjagr) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	   (part (% -))
	   (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	   (advbl-needed ?avn)
	   )
     
     )
    (add-to-conjunct (val (tense (? vf past pres fut))) (old ?tma) (new ?newtma))
    ?subj
    (advbl (atype pre-vp) (gap -)  ; pre-vp: Is the block eventually melted?
     (argument (% s (sem ?argsem)))
     (arg ?v) (var ?mod) (role ?advrole) (subcat -))           
    ?dobj
    ?comp) 

   ; Is the pizza cold quickly?
   ((s (stype ynq) (main +) (aux -) (gap -)
     (subj (% np (lex ?subjlex) (sem ?subjsem) (var ?subjvar) (agr ?subjagr)))
     (sort pred) 
     
     (var ?v) ;; propagate up explicitly because not a head feature	   
     (agr ?subjagr) ;; propagate up explicitly because not a head feature
     (sem ?sem) ;; propagate up explicitly because not a head feature
     (transform ?transform) ;; propagate up explicitly because not a head feature
      ;; propagate up explicitly because not a head feature	
     (subjvar ?subjvar) (dobjvar ?dobjvar)
     
     (lf (% prop (var ?v) (class ?belf)
	    (constraint (& (lsubj ?subjvar)
			   (lobj ?dobjvar) 
			   (comp3 ?compvar)
			   (?subj-map ?subjvar) (?dobj-map ?dobjvar)
			   (?comp3-map ?compvar) 
			   (mods ?mod)
			   ))
	    (sem ?sem) (tma ?tma)
	    (transform ?transform)
	    ))
     (advbl-needed ?avn)
     )
    -s-ynq-be-adv2>
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
	   (subj ?subj) (subj (% ?s1 (case sub) (var ?subjvar) (sem ?subjsem) (agr ?subjagr) (lex ?subjlex) (gap -))) ;; note double matching required
	   (iobj (% -))
	   (part (% -))
	   (dobj ?dobj)	(dobj (% ?s3 (case (? dcase obj -)) (var ?dobjvar) (sem ?dobjsem) (gap -)))
	   (comp3 ?comp) (comp3 (% ?s4 (case (? ccase obj -)) (var ?compvar) (sem ?compsem) (gap -)))
	   (advbl-needed ?avn)
	   )
     
     )
    (add-to-conjunct (val (tense (? vf past pres fut))) (old ?tma) (new ?newtma))
    ?subj
    ?dobj
    ?comp
    (advbl (atype post) (gap -)  ; post: Is the block melted eventually?
     (argument (% s (sem ?argsem)))
     (arg ?v) (var ?mod) (role ?advrole) (subcat -))           
    ) 
   
   
    ;; conditionals
    ;;; md commented out 2008/16/06 because conditionals are now handled as regular adverbials.
;;;    ;; test: if the train arrives, then we can load oranges.
;;;   ((s (stype ?st) (gap -) (lf (% prop (class ?op) (var *) (constraint (&  (condition ?v1) (content ?v2)))))
;;;       (var *))
;;;    -s-if-then>
;;;    (cp (ctype s-if) (gap -) (var ?v1) (condition ?op))
;;;    (word (lex then))
;;;    (head (s (gap ?gap) (stype ?st) (vform (? x fin base)) (var ?v2)
;;;	   (advbl-needed -) 	   
;;;	   )))
;;;   
;;;   ;; test: if the train arrives, we can load oranges.
;;;   ((s (stype ?st) (lf (% prop (class ?op) (var *) (constraint (& (condition ?v1) (content ?v2))))) (gap -) (var *))
;;;    -s-ifb>
;;;    (cp (ctype s-if) (var ?v1) (gap -) (condition ?op))
;;;    (head (s (gap ?gap) (stype ?st) (var ?v2) (vform (? x fin base))
;;;	   (advbl-needed -)
;;;	   )))
;;;
;;;    ;; test: we can load oranges if the train arrives
;;;   ((s (stype ?st) (gap -) (lf (% prop (class ?op) (var *) (constraint (& (condition ?v1) (content ?v2)))))
;;;       (var *))
;;;    -s-ifc>
;;;    (head (s (gap ?gap) (stype ?st) (vform (? x fin base))  (var ?v2)
;;;	   (advbl-needed -)
;;;	   ))
;;;    (cp (ctype s-if) (gap -) (var ?v1) (condition ?op))
;;;    )
   
   #||  ;; ditransitive transformation 

    ;;  this would be better in code above, but we have to disable 
    ;;  comp3-map and iobj-map as head features 
   ;; as for the passive transformations, this rule is not ideal,
   ;; because if there's already a comp3 then you can't put the iobj
   ;; there.

   ;; this rule also overgenerates: e.g.
   ;; tell him to go -> *tell to go to him
   ;; that leaves us enough time -> *?that leaves enough time to us
   ;; test: give that to me
   ((v (vform ?vform) (subj ?subj) (dobj ?dobj) (iobj (% -)) (subj-map ?subj-map) (dobj-map ?dobj-map)
     (comp3 (% pp (ptype to) (var ?compvar) (sem ?iosem))) (comp3-map ?map) (part ?part)) 
    -ditransitive-to>
    (head (v (vform ?vform) (subj ?subj) (dobj ?dobj) (subj-map ?subj-map) (dobj-map ?dobj-map)
	   (iobj (% np (sem ?iosem))) (comp3 (% -)) (iobj-map ?map) (part ?part)
	   (advbl-needed -)
	   )))||#
 

    ;;  rejections
    
    ;;  note, utterances like "not avon, bath" are now treated as two speech acts
     

;;;    ;;  e.g., not avon
;;;    ((utt (var *) (lf (% speechact (var *) (class ont::sa_reject) (constraint (& (content ?v1))))))
;;;     -reject-np>
;;;     (word (lex not))
;;;     (head ((? cat np) (var ?v1))))
;;;
;;;    ;;  not via bath
;;;    ((utt (var **) (lf (% speechact (var ?nv) (class ont::sa_reject) (constraint (& (content ?v))))))   
;;;      -reject-advbl>
;;;     (adv (lex not) (var ?nv))
;;;     (head (advbl (wh -) (var ?v) (argument (% ?x (sem ?sem))) (arg (% *pro* (var *) (gap -) (sem ?sem))))))

;;;    ;;  e.g., not a closed path
;;;    ((utt (var *) (lf (% speechact (var **) (class ont::sa_identify) (constraint (& (content ?v1))))))
;;;     -not-np-utt>
;;;     (word (lex not))
;;;     (head ((? cat np) (var ?v1))))
    
    
    
       ;;  np utterances
   ;;   e.g.,  the train
   ;; myrosia 2005/01/25 added a sem restriction to prevent expletives from coming up here
   ;; test: the dog.
   ((utt (lf (% speechact (var *) (class ont::sa_identify) (constraint (& (content ?v))))) (var *))
    -np-utt-simple> .98
    (head (np (wh -) (sort (? x pred unit-measure)) (complex -) (var ?v) (sem ($ ?!type)))))

   ; The dog?
   ((utt (lf (% speechact (var *) (class ont::SA_YN-QUESTION) (constraint (& (content ?v) (punctype ?p))) )) (var *)
	 (punc +) (punctype ?p)) 
    -np-utt-simple-q> .98
    (head (np (wh -) (sort (? x pred unit-measure)) (complex -) (var ?v) (sem ($ ?!type))))
    (punc (punctype ?p) (lex w::punc-question-mark))
    )
   
   ; What next?  What color?
   ((utt (lf (% speechact (var *) (class ont::SA_WH-QUESTION) (constraint (& (content ?v) (focus ?v) (punctype ?p))) )) (var *)
	 (punc +) (punctype ?p))
    -np-utt-simple-whq> .98
    (head (np (wh Q) (sort (? x pred unit-measure)) (complex -) (var ?v) (sem ($ ?!type))))
    (punc (punctype ?p) (lex w::punc-question-mark))
    )
   
   ; How big?
   ((utt (lf (% speechact (var *) (class ont::SA_WH-QUESTION) (constraint (& (content ?v) (focus ?v) (punctype ?p))) )) (var *)
	 (punc +) (punctype ?p)) 
    -how-adj-utt-simple-q> .98
    (head (adjp (wh Q) (sort (? x pp-word)) (complex -) (var ?v) (sem ($ ?!type))))
    (punc (punctype ?p) (lex w::punc-question-mark))
    )

   ;; complex nps (e.g., disjunctions, conjunctions) are dispreferred over parses with disjunction more deeply attached
    ((utt (lf (% speechact (var *) (class ont::sa_identify) (constraint (& (content ?v))))) (var *))
     -np-utt> .97
     (head (np (wh -) (sort (? x pred unit-measure)) (complex +) (var ?v) (sem ($ ?!type)))))

    ;; test: not the dog.
    ((s (stype elided-verb) (var *) (lf (% prop (class (:* ont::ellipsis))
					   (var *)
					   (constraint (& (:theme ?v)))
					   (tma (& (negation +)))
					   ))
     )
     -not-np-s> .98
     (neg)
     (head (np (wh -) (sort (? x pred unit-measure)) (gerund -) (var ?v) (sem ?sem) (sem ($ ?!type))))
     )

    ;; test: not red
    ((s (stype elided-verb) (var *) (lf (% prop (class (:* ont::ellipsis))
					   (var *)
					   (constraint (& (:property ?v)))
					   (tma (& (negation +)))
					   ))
      )
     -not-adjp-s> .94
     (neg)
     (head (adjp  (var ?v) (argument (% ?x (sem ?sem))) ;; (wh -)  i eliminated this to allow the question  "how red?"
	    (set-modifier -)  ;; disallows numbers as adjp fragments - they already have a number interpretation 
	    (arg (% *pro* (var *) (gap -) (sem ?sem) (constraint (& (context-rel utt-frag)))))))
     )
     

    
    
     ;;  here a question mark forces a y/n question interp 
    ;; test: ok?
    ((utt (var *) (lf (% speechact (var *) (class ont::sa_prompt-for) (constraint (& (content ?sa))))))
     -utt5>
     (head (uttword (sa ?sa) (lf ?lf)))
     (punc (punctype ynq)))
    
    ;; test: from which city?
    ((utt (var *) (lf (% speechact (var *) (class ont::sa_wh-question) (constraint (& (content ?v) (focus ?foc)))))) 
     -utt4>
     (head (advbl (wh q) (var ?v) (wh-var ?foc) (gap -)
                  (argument (% ?x (sem ?sem)))
                  (arg (% *pro* (var *) (gap -) (sem ?sem)))))
     (punc))

;;;      still can't figure out a good way to do this
;;;    ;; e.g., reduced ynq, e.g., can we? don't they?
;;;
;;;    ((utt (var *) (lf (% speechact (var *) (class ont::sa_ynq-question)
;;;			 (constraint (& (content ?v) (focus ?foc))))))
;;;     -utt-ynq-reduced>
;;;     (s (type ynq) (reduced +) (var ?v))
;;;     (punc))
    
       
    ;;   gap questions
    ;; who/what-dog/whose-dog did you see at the store?
    ;; test: who did you see?
    ;; test: who did you see at the store?
    ;; test: whose dog did you see at the store?
    ((s (stype whq) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj) (sem ?sem)
      (qtype q) (lf ?lf) (var ?v))
     -wh-q2>
     (np (var ?npvar) (sem ?npsem) (wh q) (agr ?a) (case ?case))
     (head (s (stype ynq) (lf ?lf) (var ?v) (sem ?sem)
	    (advbl-needed -)
	    (subj ?subj)
	    (subjvar ?subjvar) (dobjvar ?dobjvar)
	    (gap (% np (sem ?npsem) (case ?case) (var ?npvar) (agr ?a))))
      )
     )

    ;; test: the dog, he said.
    ;; test: the dog, he told me.
    ;; test: the dog? he asked.
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

    ;; test: he barked, he said.
    ;; test: he barked, he told me.
    ;; test: did he bark? he asked.
    ((s (stype decl) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj)
	(lf ?lf) (var ?v))
     -utt-role-fronting> .97
     (utt (var ?uttvar) (sem ?uttsem))
     (head (s (stype ?st) (main -) (lf (% prop (class ont::report-speech))) (var ?v) (sem ?sem)
	      (gap (% utt (sem ?uttsem) (var ?uttvar)))
	      ))
     )
  
    ;; gap questions with gapped pps
    ; Of what do you think?
     ((s (stype whq) (subjvar ?subjvar) (dobjvar ?dobjvar)
      (qtype q) (lf ?lf) (var ?v))
     -wh-q-ppgap>
     (pp (var ?npvar) (sem ?npsem) (wh q) (agr ?a) (case ?case) (ptype ?ptp))
      (head (s (stype ynq) (lf ?lf) (var ?v) 
	     (advbl-needed -)
	     (subjvar ?subjvar) (dobjvar ?dobjvar)
	     (gap (% pp (sem ?npsem) (case ?case) (var ?npvar) (agr ?a) (ptype ?ptp))))
       )
      )

    ;; mod gap questions with a gapped pred -- e.g. "where is it", "how far is it"
    ;; presumably "how large should i make it" should also parse, but this is untested
    ;; test: where is it?
    ((s (stype whq) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj)
      (qtype q) (lf ?lf) (var ?v))
     -wh-q-predgap>
     (pred (var ?predvar) (sem ?predsem) (wh q) (arg ?subjvar)
	    (SEM ($ f::abstr-obj
				 (F::type (? !ttt ont::path ont::conventional-position-reln ont::direction ont::complex-ground-reln ont::back ont::front ont::left-of ont::off ont::orients-to ont::right-of ;ont::pos-as-containment-reln ; we allowed "in" for some reason, but I don't remember the example!
					     ont::pos-directional-reln ont::pos-distance ont::pos-wrt-speaker-reln ont::resulting-object))))
	   )
     (head (s (stype ynq) (lf ?lf) (var ?v) 
	    (advbl-needed -)
	    (subjvar ?subjvar) (dobjvar ?dobjvar)
	    (subj ?subj)
	    (gap (% pred (sem ?predsem) (var ?predvar))))
       )
      )
    
     ; Where can I put the block?
    ((s (stype whq) (subjvar ?subjvar) (dobjvar ?dobjvar) (subj ?subj)
      (qtype q) (lf ?lf) (var ?v))
     -wh-q-predgap2>
     (advbl (var ?predvar) (sem ?predsem) (wh q) (arg ?dobjvar)
	    (SEM ($ f::abstr-obj
				 (F::type (? ttt ont::path ont::conventional-position-reln ont::direction ont::complex-ground-reln ont::back ont::front ont::left-of ont::off ont::orients-to ont::right-of ;ont::pos-as-containment-reln ; we allowed "in" for some reason, but I don't remember the example!
					     ont::pos-directional-reln ont::pos-distance ont::pos-wrt-speaker-reln ont::resulting-object))))
	    )
     (head (s (stype ynq) (lf ?lf) (var ?v) 
	    (advbl-needed -)
	    (subjvar ?subjvar) (dobjvar ?dobjvar)
	    (subj ?subj)
	    (gap (% advbl (sem ?predsem) (var ?predvar) (arg ?dobjvar))))
       )
      )
    
    ))

(parser::augment-grammar
  '((headfeatures
     (s vform var neg sem dobjvar cont  lex headcat transform advbl-needed)
     (pred qtype focus arg var sem wh lf lex headcat transform)
     (vp- vform var agr neg sem iobj dobj comp3 part cont  tense-pro aux modal lex headcat transform tma subj-map advbl-needed template result)
     )
    

    ;; myrosia 01/24/02 to get the dummy sentences with predicates to
    ;; walk, i introduced a new structure: a predicate with filled
    ;; argument we usually only want unfilled arguments. note that we
    ;; assume that adjective the reason to do it this way rather than
    ;; the previous rules (commented out at the end) is that we want
    ;; to take advantage of selectional restrictions on arguments, and
    ;; it does not seem possible in the other setup
    ;; test: is it good to bark?
    ((pred (filled +) (argument -) (gap -) (adjectival +)
      )
     -fill-argument-pred>
     (head (pred (filled -) (argument ?argument) (argument (% np (var ?v) (sem ($ f::situation))))
	    (arg ?v) (wh -) (adjectival +)
	    ))
     (cp (ctype (? ctp s-to s-that-overt s-if))
      (var ?v) (gap -))  ;; note: is it good to eat is not expletive - the it refers and fills a gap in the pred.
     )

    ;; a special rule for expletive questions - what is possible to do --
    ;; test: what is good to chase?
    ((s (stype whq) (main +) (aux -) (gap -) (subjvar -)
      (qtype q)
      (subj -)
      (var ?v)
      (advbl-needed ?avn)
      (lf (% prop (var ?v) (class ?belf) 
	     (constraint (& ;; subject not included in the new constraint
			  (lobj ?dobjvar) 
			  (comp3 ?compvar)
			  (?dobj-map ?dobjvar)
			  (?comp3-map ?compvar) 
			  ))
	     (sem ?sem) (tma ?newtma)
	     (transform ?transform)
	     )))
     -s-whq-expletive>
     (np (var ?npvar) (sem ?npsem) (wh q) (agr ?gagr) (case ?gcase))
     (head (v (sem ?sem) (aux -)
	    (advbl-needed ?avn)
	    (lf ?belf)
	    (subj-map ?subj-map) (dobj-map ?dobj-map) (comp3-map ?comp3-map)
	    (agr ?gagr) 
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
     (add-to-conjunct (val (tense (? vf past pres fut))) (old ?tma) (new ?newtma))
     )

   ))

;; putting the sem in this rule violates sem feature inheritance, so i had to move this rule here 
;; ideally, we should put the sem in the lexicon, but it's not currently possible for non-hierarchy lfs
;; which is what uttwords have
(parser::augment-grammar
  '((headfeatures
    (utt cont lex headcat transform))

 ;; test: hello
 ;; test: yes
 ;; test: maybe
 ;; test: no
   ((utt (var *) (sem ($ f::proposition)) (uttword +)
         ;(lf (% speechact (var *) (class ?sa) (constraint (& (content (?lf :content ?lex))))))
         (lf (% speechact (var *) (class ?sa) (constraint (& (content ?lf)))))
         (punctype (? x decl imp)))
    -utt4a>
    (head (uttword (lf (?lf)) (lex ?lex) (var ?v) (sa ?sa))))

    ((utt (var *) (sem ($ f::proposition)) (uttword +)
      (lf (% speechact (var *) (class  ont::sa_apologize) (constraint (& (content ?reason)))))
      (punctype (? x decl imp)))
     -utt-sa-cp>
     (head (uttword (lf (?lf)) (lex ?lex) (var ?v) (sa ont::sa_apologize)))
     (cp (var ?reason)))

    ((utt (var *) (sem ($ f::proposition)) (uttword +)
      (lf (% speechact (var *) (class (? sa ont::sa_apologize ont::sa_thank)) (constraint (& (reason ?reason)))))
      (punctype (? x decl imp)))
     -utt-sa-for>
     (head (uttword (lf (?lf)) (lex ?lex) (var ?v) (sa (? sa ont::sa_apologize ont::sa_thank))))
     (pp (ptype w::for) (var ?reason)))

    )
  )

(parser::augment-grammar
 '((headfeatures
    (vp vform agr comp3 cont postadvbl  aux modal lex headcat tma transform subj-map advbl-needed template)
    (s vform var neg sem subjvar dobjvar comp3 cont  lex headcat transform subj advbl-needed)
    (utt sem subjvar dobjvar cont lex headcat transform))
   
     ;;   basic commands
    
    ;; e.g., tell me the plan
    ;; test: bark.
    ;; test: chase the cat.
    ((utt (lf (% speechact (var *) (class ont::sa_request) (constraint (& (content ?v))))) (var *)
          )
     -command-imp1> 1.0
     (head (s (stype imp) (wh -) (var ?v) (sem ($ F::SITUATION (F::type ONT::EVENT-OF-change)))
	      (gap -) (advbl-needed -))))
      
    ;; test: bark.
    ;; test: chase the cat.
    ((s (stype imp)
	(lf (% prop (var ?v) (class ?c) (constraint ?con)
	       (sem ?sem) (tma ?newtma)
	       (transform ?transform)
	       ))
      )
     -command-imp2>
     (head (vp (gap  -) 
	    (sem ?sem)
	    (sem ($ f::situation (f::aspect (? aspc f::dynamic f::stage-level)
					    )
		    (f::type ont::event-of-change)
		    ))
	    (var ?v) (aux -) (tma ?tma)
	    (constraint ?con)
	    (subj (% np (var (% *pro* (class ont::person) (var *) (sem ?subjsem) (constraint (& (proform *you*)))
				))
		     (sem ($ f::phys-obj (f::form f::solid-object) (f::spatial-abstraction f::spatial-point)
			     (f::information -) (f::trajectory -) (f::container -) (f::group -)
			     (f::mobility f::self-moving) (f::origin f::human) (f::intentional +)))
		     (sem ?subjsem)))
	    (subjvar (% *pro* (class ont::person) (var *) (constraint (& (proform *you*)))
			(sem ?subjsem)))
	    (subj-map (? xxx ont::agent ont::arg0 ont::neutral))
	    ;;($ f::phys-obj (f::form f::solid-object) (f::spatial-abstraction f::spatial-point)
	    ;;			(f::information -) (f::trajectory -) (f::container -) (f::group -)
	    ;;			(f::mobility f::self-moving) (f::origin f::human) (f::intentional +)))))
		
	    (class ?c) (lf ?lf)
	    (vform base) (postadvbl ?pa) (main ?ma)
	    (transform ?transform)
	    (advbl-needed -)
	    )
      )
     (append-conjuncts (conj1 (& (tense pres))) (conj2 ?tma) (new ?newtma))
     )

    ;; e.g., let's go  nb: not the same as "let us go" - which is a request for permission

    ;; test: let's bark.
    ;; test: let's chase the cat.
    ((s (stype imp) 
	(lf (% prop (var ?v) (class ?c) (constraint ?con)
	    (sem ?sem) (tma ?newtma)
	    (transform ?transform)
	    ))
	)
     -lets-imp> 1.0 ;; get let's to go through this rule instead of command-imp2
     (word (lex let))
     ;(np (lex us) (var ?npvar))
     (word (lex ^s))
     (head (vp (gap  -) (sem ?sem)
	       (sem ($ f::situation (f::aspect (? aspc f::dynamic f::stage-level))))
	       (var ?v) (aux -) (tma ?tma)
	       (constraint ?con)
	       ;(subj (% np (var ?npvar)
			;(sem ?subjsem)))
	       ;(subjvar ?npvar)
	       (subj (% np (var (% *pro* (status ont::pro-set) (class ont::person) (var *) (sem ?subjsem) (constraint (& (proform w::us)))
				))
			(sem ($ f::phys-obj (f::form f::solid-object) (f::spatial-abstraction f::spatial-point)
				(f::information -) (f::trajectory -) (f::container -) (f::group -)
				(f::mobility f::self-moving) (f::origin f::human) (f::intentional +)))
			(sem ?subjsem)))
	       (subjvar (% *pro* (status ont::pro-set) (class ont::person) (var *) (constraint (& (proform w::us)))
			   (sem ?subjsem)))
	       (class ?c)
	       (vform base) (postadvbl ?pa) (main ?ma)
	    (transform ?transform)
	    (advbl-needed -)
	    )
      )
     (append-conjuncts (conj1 (& (tense pres))) (conj2 ?tma) (new ?newtma))
     )
     
    ;; negative commands, e.g., don't tell me the plan
    ;; test: don't bark.
    ;; test: don't chase the cat.
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
	       (subj-map (? xx ont::agent ont::arg0))
	       (sem ($ f::situation (f::aspect (? aspc f::dynamic f::stage-level))))
	       (var ?v) (aux -) (tma ?tma)
	       (constraint ?con)
	       (subj (% np (var (% *pro* (class ont::person) (var *) (sem ?subjsem) (constraint (& (proform *you*)))))
			(sem ($ f::phys-obj (f::form f::object) (f::intentional +)))
			(sem ?subjsem)))
	       (subjvar (% *pro* (var *) (class ont::person) (sem ?subjsem) (constraint (& (proform *you*)))))
	       (class ?c)
	       (vform base) (postadvbl ?pa) (main ?ma)
	       (transform ?transform)
	       (advbl-needed -)
	       ))
     (change-feature-values (old ?tma) (new ?newtma) (newvalues ((tense pres) (modality ont::do) (negation +)))) 
     )
	
  ;;  default rule for non-action vps (useful when subject is deleted)
    ;;   e.g., have to go to elmira
    ;; test: have to bark.
    ;; test: have to chase the cat.
     ((utt (lf (% speechact (var **) (class ont::sa_tell) (constraint (& (content ?v))))) (var **)
           (punctype (? x imp decl)))
      
      -vp-utt-inform> .95
      (head (vp (gap -) (sem ?sem) (sem ($ f::situation (f::cause (? cs f::stimulating f::phenomenal f::mental -))))
	     (var ?v)
	     (constraint ?constraint) (class ?class)
	     (vform (? vform pres past fut)) (agr (? agr 1s 1p))
	     (transform ?transform)
	     (advbl-needed -)
	     (subj (% np (var (% *pro* (class ont::referential-sem) (var *) (constraint (& (proform subj)))))
			(sem ($ f::phys-obj))
			))
	     (subjvar (% *pro* (class ont::referential-sem) (var *) (constraint (& (proform subj)))
			 ))
	     )))

    ;; test: good
    ;; test: bad
     ;;   again, need to generate two variables to do this right
    ((utt (lf (% speechact (var **) (class ont::sa_evaluate) (constraint (& (content ?v))))) (var **)
      (uttword +) ;; we set the uttword feature here to allow an evaluate to prefix another utterance
      (punctype (? x imp decl)))
     -evaluate1>
     (head (adjp (wh -) (var ?v) (arg (% *pro* (var *) (class ont::any-sem))) ;; changing this from ont::situation, which is too restrictive for cardiac domain
      (lf (% prop (class ont::acceptability-val))) (gap -))))

   ;; test: good,
   ;; test: ok,
   ;;   again, need to generate two variables to do this right
   ((utt (lf (% speechact (var **) (class ont::sa_evaluate) (constraint (& (content ?v))))) (var **)
	 (uttword +)
	 (punctype (? x imp decl)))
    -evaluate1b>
    (head (adjp (wh -) (var ?v) (arg (% *pro* (var *) (class ont::any-sem))) ;; changing this from ont::situation, which is too restrictive for cardiac domain
	   (lf (% prop (class ont::acceptability-val))) (gap -)))
    (punc (lex punc-comma) (var ?v1)))
   
   
    ;;; special rules for vps that require post-modifying adverbials
   ;;; (e.g. be). if the adverbial was not specified, we will create a
   ;;; gap which will prevent this vp from participating in any
   ;;; constructions except for those that know how to fill that gap
   
   ;; add tense information (past, present, or future) for vp that requires and adverbial
     ((vp (lf (% prop (class ?c) (var ?var) (constraint ?constraint) (tma ?newtma) 
	         (transform ?transf) (sem ?sem)))
          (class ?c) (constraint ?constraint) (tma ?newtma)  (sem ?sem) (transform ?transf)
        (vform (? vf past pres fut))
       (gap (% advbl))
       (subj ?subj) (subjvar ?subjvar) (var ?var)
       )
      -vp-tns+-gap> ;; 1 because we are simply adding tense info, not really using more rules
      (head
       (vp- (class ?c)  (constraint ?constraint) (tma ?tma1) (sem ?sem) ;; i added the sem feature to get right restrictions in "i know where the loop is", but maybe it wasn't there for some other reason jfa 6/04
	 (vform (? vf past pres fut)) (var ?var)
	(advbl-needed +) (gap -) (subj ?subj) (subj (% np (var ?subjvar) (agr ?subjagr) (sem ?subjsem) ))
	)) 
      (add-to-conjunct (val (tense ?vf)) (old ?tma1) (new ?newtma))
      )
   
   ;; untensed vp that requires a post-modifying adverbial
   ((vp (lf (% prop (class ?c) (var ?var) (constraint ?constraint) (transform ?transf) (tma ?tma) (sem ?sem)))
     (class ?c) (var ?var) (constraint ?constraint) (tma ?tma) (sem ?sem) (transform ?transf) 
     (gap (% advbl)) (subj ?subj) (subjvar ?subjvar)
     )
    -vp-tns--gap> 
    (head
     (vp- (class ?c) (constraint ?constraint) (tma ?tma) 
      (subj ?subj) (subj (% np (var ?subjvar) (agr ?subjagr) (sem ?subjsem))) (sem ?sem)   ;; see vp-tns+-gap> comment
      
      (vform (? vf base passive ing pastpart))
      (advbl-needed +) 
      (var ?var)
      )))   
   
    ))

;; aux rules which violate vp sem inheritance constraint
(parser::augment-grammar
  '((headfeatures
     (vp vform agr cont comp3 postadvbl aux modal auxname lex headcat transform subj-map template)     
     (s vform neg comp3 cont  lex headcat transform )
     )
    
     ;; rules for auxiliaries with elided vp complement: will i?, can i?, do i? 
     ;; test: does he?
    ((s (stype ynq) (gap -) (wh -) (tag +)
      (subj ?subj) (subjvar ?subjvar) (subj-map ?lsubj-map)
      (var ?v) (lex ?lx)
       (lf (% prop  (var ?v) (class (:* ont::ellipsis ))  (sem ?sem)
	     (constraint (& (lsubj ?subjvar) (?lsubj-map ?subjvar)))(tma ?newtma)
	     ))
       (sem ?sem) (transform ?transform) 
      )
     -ynq-aux-modal-nocomp> .96 ;; only execute if we have to so we don't explode the search space
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +) 
	    (contraction -)
	    (sem ?sem) (vform ?vf) (vform (? vf pres past fut))
	    (var ?v) (tma ?tma1) (agr ?a)
	     ;; myrosia 2007/02/23 added agr ?a restriction to subjects to prevent cases such as "does you" from parsing this way
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (agr ?a) (var ?subjvar) (sem ?subjsem) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map)
	     (transform ?transform)
	    (advbl-needed -)
	    ))
     ?subj
     ;; we need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (append-conjuncts (conj1 (& (tense ?vf))) (conj2 ?ntma) (new ?newtma))
     ) 
        
    ;;  rule for modal auxiliaries can, might, may, should, could, would: can the train arrive at 5:30?
    ;; this is the same as ynq-fut-aux except the aspect and time-span features change to those of the modal
    ;; 8/02/2004 build an explicit np for the subject instead of trying to match it with ?subj to get around a problem with
    ;; the unifier not passing up the subject specifications from the verb
    ;; test: does the dog bark?
    ;; test: does the dog chase the cat?
    ((s (stype ynq) (gap ?gap) (lex ?lx)
       (subjvar ?subjvar) (dobjvar ?dobjvar) (var ?v)
      (lf (% prop  (var ?v) (class ?class) (sem ?newsem)
	     (constraint ?con) (tma ?newtma)
	     ))
      
      (sem ?newsem)
  ;;    (subj ?subj)
      (subj (% np (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
      )
     -ynq-modal-aux>
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -)
	    (contraction -)
	    ;;(lex (? lx w::can w::might w::may w::should w::could w::would)) ;; only full forms here
	    (vform (? vform pres past fut)) (agr ?a)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
            (comp3 ?comp)
	    (comp3 (% ?s4 (class ?class)
		      (var ?v)
		      (case (? ccase obj -)) (subj-map ?rsubjmap) 
		      (constraint ?con) (subjvar ?subjvar) (tma ?tma1)
		      (var ?compvar) (sem ?compsem) (gap ?gap) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar)
									(sem ?subjsem) (agr ?a) (gap -)))
		      (dobj ?dobj) (dobjvar ?dobjvar)
		      (advbl-needed -)
		      ))
            (comp3-map ?comp3-map)
            (subj-map ?lsubj-map)
            ))
;;     ?subj
     (np (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an np to get around unifier bug
     ?comp
     ;; we need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (append-conjuncts (conj1 (& (tense ?vform))) (conj2 ?ntma) (new ?newtma))
     ;; change the temporal values in sem to be consistent with the aux
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
     )

    ((s (stype ynq) (gap ?gap) (lex ?lx)
      (subjvar ?subjvar) (dobjvar ?dobjvar) (var ?v)
      (lf (% prop  (var ?v) (class ?class) (sem ?newsem)
	     (constraint ?con) (tma ?newtma)
	     ))
      
      (sem ?newsem)
      ;;    (subj ?subj)
      (subj (% np (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
      )
     -ynq-modal-neg-aux>
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -)
	    
	    ;;(lex (? lx w::can w::might w::may w::should w::could w::would)) ;; only full forms here
	    (vform (? vform pres past fut)) (agr ?a)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
            (comp3 ?comp)
	    (comp3 (% ?s4 (class ?class)
		      (var ?v)
		      (case (? ccase obj -)) (subj-map ?rsubjmap) 
		      (constraint ?con) (subjvar ?subjvar) (tma ?tma1)
		      (var ?compvar) (sem ?compsem) (gap ?gap) (subj (% ?s1 (lex ?subjlex) (case sub) (var ?subjvar)
									(sem ?subjsem) (agr ?a) (gap -)))
		      (dobj ?dobj)
		      (advbl-needed -)
		      ))
            (comp3-map ?comp3-map)
            (subj-map ?lsubj-map)
            ))
     (neg)
     (np (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an np to get around unifier bug
     ?comp
     ;; we need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (append-conjuncts (conj1 (& (tense ?vform) (negation +))) (conj2 ?ntma) (new ?newtma))
     ;; change the temporal values in sem to be consistent with the aux
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
     )

   #|| 
    ;; negated yes/no questions with modals (not be)
    ;;  test: will not he arrive? won't he arrive
    ((s (stype ynq) (gap ?gap) 
      (subjvar ?subjvar) (dobjvar ?dobjvar) (var ?v) ;; nb: values taken from vp
      (lf (% prop (sem ?sem) (var ?v) (class ?class)
             (constraint (& (lsubj ?subjvar) (dobj ?dobj) (dobj-map ?dobjvar)
			    (?comp-map ?compvar)
                            ;;(lcomp ?compvar)
                            (?rsubjmap ?subjvar)))
                            ;;(?comp3-map ?compvar)))
             (tma ?newtma)
	     (transform ?transform)
	     ))
      (sem ?argsem) ;; 
      (lex ?l)
      (subj ?subj) ;;(% np (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
      )
  
     -ynq-negated-modal-aux>
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -);; (contraction -)  
	    (var ?v) (sem ?sem)
	    ;; (argsem ?argsem) 
	    (vform (? vform pres past fut))
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
     (np (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an np to get around unifier bug
     (vp- (class ?class) (var ?compvar)  (subj-map ?rsubjmap) (dobjvar ?dobjvar) (dobj-map ?dobj-map) (comp3-map ?comp-map)
		      (gap -) (subj ?subj) (tma ?tma1) (advbl-needed -) (sem ?compsem))
     ;; we need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((tense (? vform pres past fut)) (negation +))))
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
     )
    ||#
    ;; rules for auxiliaries with elided vp complement and negation: won't i?, can't i?, do i? 
    ;; test: doesn't he?
    ((s (stype ynq) (gap -) (wh -) (tag +)
      (subj ?subj) (subjvar ?subjvar) (subj-map ?lsubj-map)
      (var ?v) (lex ?lx)
       (lf (% prop  (var ?v) (class (:* ont::ellipsis ))  (sem ?sem)
	      (constraint (& (lsubj ?subjvar) (?lsubj-map ?subjvar)))
	      (tma ?newtma)
	     ))
       (sem ?sem) (transform ?transform) 
      )
     -ynq-aux-modal-nocomp-neg> .96 ;; only execute if we have to so we don't explode the search space
     (head (aux 
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis +) (contraction -)
	    (sem ?sem) (vform ?vf) (vform (? vf pres past fut))
	    (var ?v) (tma ?tma1) (agr ?a)
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -))) ;; note double matching required
	    (subj-map ?lsubj-map)
	     (transform ?transform)
	    (advbl-needed -)
	    ))
     (neg)
     (np (lex ?subjlex) (case sub) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)) ;; build an np to get around unifier bug
     ;; we need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((tense ?vf) (negation +))))
     (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))     
     )
    ))


(parser::augment-grammar
 '((headfeatures
    (vp agr neg iobj dobj comp3 part cont  tense-pro gap subj subjvar aux modal auxname headcat advbl-needed template)
    (vp- lex headcat template result)
    )
   
   ;; aux rule for auxilliaries that change the sem features of the phrase
   ;; e.g. he has left; she is leaving; she can work
   ;; test: the dog can bark.
   ;; test: the dog has chased the cat.
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
	   (auxname ?auxname) ;; md 2009/02/11 it's important to propagate up auxname, because templates for "must" and "could" depend on it
	   (tma-contrib ?tma-contrib)
	   (sem-contrib ?sem-contrib)
	   (ellipsis -) ;; (contraction -)
	   (lex ?l)
	   (lf ?lf)
	   (vform ?vform)
	   (agr ?a)
	   (sem ($ f::situation (f::aspect ?aspect) (f::time-span ?time)))
	   (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
	   (iobj (% -)) (part (% -)) (dobj (% -))
	   (comp3 ?comp) 
	   (comp3 (% vp- (class ?class)  (constraint ?con1) (tma ?tma1) (var ?var)
		     (case (? ccase obj -)) (var ?compvar)  
		     (gap ?gap)		     
		     (sem ?compsem) ;; (sem ($ f::situation (aspect (? !asp f::indiv-level))))  ;; constraints are in lf
		     (subjvar ?subjvar) (dobjvar ?dobjvar) (transform ?transform)
		     (subj ?subj)
		     (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
		     (advbl-needed -) (subj-map ?lsubj-map)
		     (auxname ?compauxname)
		     ))
	    (comp3-map ?comp3-map)
	   ))
    ?comp
    (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?newtma)) ;; add aux feature to tma
    ;; change the temporal values in sem to be consistent with the aux
    (change-feature-values (old ?compsem) (new ?newsem) (newvalues ?sem-contrib))
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; negation rules (for non-elided vs):   aux -> aux not
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   ;; rule for negated modals that don't change sem features (will and do)
   ;; add (modality ont::xx) & (negation +) to tma
   ;; test: the dog won't bark.
   ;; test: the dog won't chase the cat.
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
	    (auxname ?auxname) ;; md 2009/02/11 it's important to propagate up auxname, because templates for "must" and "could" depend on it
	    (tma-contrib ?tma-contrib)
	    (sem-contrib ?sem-contrib)
	    (ellipsis -) (neg -)
	    (lex ?l)
	    (vform ?vform) (vform (? vf past pres fut)) ;; double matching for 'do', since tense not marked in lexicon
	    (agr ?a) 
	    (subj ?subj) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
	    (dobj (% -)) (part (% -)) (iobj (% -))
	    (comp3 ?comp) 
	    (comp3 (% vp- (class ?cl) (var ?v)  (constraint ?con1) (tma ?tma1) 
		      (case (? ccase obj -)) (var ?compvar)  ;;(sem ?compsem) -- needed for bug in unifier
		      (gap -) (subj (% ?s1 (lex ?subjlex) (var ?subjvar) (sem ?subjsem) (agr ?a) (gap -)))
		      (subjvar ?subjvar) (dobjvar ?dobjvar) (transform ?trans)
		      (advbl-needed -)
		      ))
	    (subj-map ?lsubj-map) (comp3-map ?comp3-map)
	    ))
     (neg)
     ?comp
     (unify (pattern (% vp- (sem ?compsem))) (value ?comp))    ;; need this to get around a bug in unifier 
     ;; we need to have a sequence of additions to conjunct, to avoid parser warnings
     (add-to-conjunct (old ?tma1) (val ?tma-contrib) (new ?ntma))
     (change-feature-values (old ?ntma) (new ?newtma) (newvalues ((negation +)))) 
     (change-feature-values (old ?COMPSEM) (NEW ?NEWSEM) (NEWVALUES ?SEM-CONTRIB))
     ;;     (APPEND-CONJUNCTS (CONJ1 (& (MODALITY ?AUXLF) (NEGATION +))) (CONJ2 ?TMA1) (NEW ?NEWTMA))
     )

   ))


;; conjunctions


(parser::augment-grammar 
 '((headfeatures
    (s vform neg sem subjvar dobjvar cont  lex headcat transform subj advbl-needed)
    (sseq vform neg sem subjvar dobjvar cont  lex headcat transform subj advbl-needed)
    (vbarseq sem)
    (vpseq sem)
    ;;(vp- sem)
    (vp- result)
    )
   
   ;; conjoined vps w/ same subject
   ;; test: the dog barked and chased the cat.
   ((s (stype ?st) (var ?v3) (sem ?sem)
     (lf (% prop (var ?v3) 
	    (class ?class)
	    (constraint (& (operator ?lex) (sequence (?v1 ?v2)))) (tma ?tma)))
     )
    -s-conj1>
    (head (s (stype (? st decl)) (subj ?subj) (var ?v1) (sem ?s1)
	     (lf (% prop (class ?c1) (tma ?tma))) (gap -)
	     (advbl-needed -)
	     ))
    (conj (lf (? lex ont::and ont::or ont::but)) (var ?v3))
    (vp (subj ?subj) (gap -) (var ?v2) (tma ?tma) (advbl-needed -)
     (class ?c2) (sem ?s2))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )

  ;; conjoined vps w same subject AND object!
   ;; test: the dog chased and caught the cat.
   ((s (stype ?st) (var ?v3) (sem ?sem)
     (lf (% prop (var ?v3) 
        (class ?class)
        (constraint (& (operator ?lex) (sequence (?v1 ?v2)))) (tma ?tma)))
     )
    -s-conj1a> 
    (head (s (stype (? st decl)) (subj ?subj) (var ?v1) (sem ?s1) (gap ?!dobj)
         (lf (% prop (class ?c1) (tma ?tma)))
         (advbl-needed -)
         ))
    (conj (lf (? lex ont::and ont::or ont::but)) (var ?v3))
    (vp (subj ?subj) (gap -) (var ?v2) (tma ?tma) (advbl-needed -) (dobj ?!dobj)
     (class ?c2) (sem ?s2))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )

   
   ;; sentential conjunction
   ;; both ss must be of the same type, decl or imperative
   ;; he did this and/or/but he did that; do this and/or/but do that
   ;; test: bark and chase the cat.
   ((s (sseq +) (stype ?st) (var *)  (sem ?sem)
     (lf (% prop (var *) (class ont::?class) 
	    (constraint 
	     (&  (operator (? lx ont::or ont::and ont::but ont::however ont::plus ont::otherwise ont::so))
		 (sequence (?v1 ?v2))))))
     )
    -s-conj2>
    (head (s (stype (? st decl imp)) (subj ?subj1) (var ?v1) (sem ?s1)
	   (lf (% prop (class ?c1) (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (conj (lf (? lx ont::or ont::and ont::but ont::however ont::plus ont::otherwise ont::so)))
    (s (stype (? st decl imp)) (subj ?subj2) (var ?v2) 
     (advbl-needed -) (sem ?s2)
     (lf (% prop (class ?c2) (tma ?tma2))))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )

   ;; s1, s2 and s3
   ;;  starting the sseq (need two as -s-conj2> above handles the binary conjunctive case)
   ((sseq (stype ?st) (var *)
     (sequence (?v1 ?v2))
     (class ?class)
     (sem ?sem)
     )
    -s-conj-seq1>
    (head (s (stype (? st decl imp)) (subj ?subj1) (var ?v1)  (sseq -) (sem ?s1)
	   (lf (% prop (class ?c1) (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (punc (lex (? x w::punc-comma w::punc-semicolon)))
    (s (stype (? st decl imp)) (subj ?subj2) (var ?v2) (sseq -) (sem ?s2)
	   (lf (% prop (class ?c2) (tma ?tma2)))
	   (advbl-needed -)
	   )
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )
   ;; extending the sseq
   ((sseq (stype ?st) (var *) (class ?class) (sem ?sem)
     (sequence ?newlf))
    -s-conj-seq2>
    (head (sseq (var ?v1) 
	   (sequence ?lf)
	   (stype ?st)
	   (class ?c1)
	   (sem ?s1)
	   ))
    ;;(CONJ (lex (? lx W::and W::OR)) (lf ?conjlf))
    (s (stype ?st) (subj ?subj2) (var ?v2) (sseq -) (sem ?s2)
     (lf (% prop (class ?c2) (tma ?tma2)))
     (advbl-needed -)
     )
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )

   ;; Ending the SSEQ 
   ;; both Ss must be of the same type, decl or imperative
   ((s (stype ?st) (var *) (sseq +) (sem ?sem)
     (LF (% prop (var *) (class ?class) 
	    (constraint (&  (OPERATOR ?lx) (SEQUENCE ?newlf)))
	    ))
     )
    -s-conj-seq> 1.0
    (head (sseq (var ?v1) 
		(sequence ?lf)
		(stype ?st)
		(class ?c1)
		(sem ?s1)
		))
    (CONJ (lf (? lx ont::and ont::or)))
    (s (stype ?st) (subj ?subj2) (var ?v2) (sseq -) (sem ?s2)
     (advbl-needed -)
     (lf (% prop (class ?c2) (tma ?tma2))))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )
    

   ;; either S1 or S2
   ;; both S1 and S2
   ;; applies to declarative or imperative sentences only
   ;; TEST: Either the dog barks or the dog chases the cat.
   ((s (stype ?st) (var *) 
     (sem ?sem)
     (LF (% prop (var *) (class ?class) (constraint (&  (OPERATOR ?clf) (SEQUENCE (?v1 ?v2))))))
     )
    -s-double-conj>
    (conj (SUBCAT1 S) (SUBCAT2 ?wlex) (SUBCAT3 S) 
     (var ?v) (lf ?clf))
    (head (s (stype (? st decl imp)) (subj ?subj1) (var ?v1) (sseq -) (sem ?s1)
	   (lf (% prop (class ?c1) (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (word (lex ?wlex))
    (s (stype (? st decl imp)) (subj ?subj2) (var ?v2) (sseq -) (sem ?s2)
     (advbl-needed -)
     (lf (% prop (class ?c2) (tma ?tma2))))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )

   ;; and the VP version e.g., to either stay or go
   ((VP (var *) (LF (% prop (var *) (class ?class) 
		       (constraint (&  (OPERATOR ?clf) (SEQUENCE (?v1 ?v2))))))
     (agr ?agr) (vform ?vform) (gap ?g) (subj ?subj) (sem ?sem)
		       
     )
    -either-vp>
    (conj (SUBCAT1 VP) (SUBCAT2 ?wlex) (SUBCAT3 VP) 
     (var ?v) (lf ?clf)) 
    (head (vp (subj ?subj) (var ?v1) (agr ?agr) (vform ?vform) (gap ?g) (sem ?s1)
	   (lf (% prop (class ?c1) (tma ?tma1)))
	   (advbl-needed -)
	   ))
    (word (lex ?wlex))
    (vp (subj ?subj) (var ?v2) (agr ?agr) (vform ?vform) (gap ?g)
     (advbl-needed -) (sem ?s2)
     (lf (% prop (class ?c2) (tma ?tma2))))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    
    )

;; VP conjunction
   ;; both VP must be of the same vform
   ;; he had eaten and slept, to puncture or penetrate or pierce, to fight but accept, 
   ; This uses this rule: The mouse is caught by the dog and caught by the cat.
   ((vp- (seq +) (vform ?vf) (var *)  (subjvar ?subj)  (subj ?subject) (agr ?agr) (gap ?gap)(subj-map ?subjmap)
     (class ?class) (sem ?sem)
     (constraint (&  (OPERATOR ?lx) 
		     (SEQUENCE 
		      ((% *PRO* (var ?v1) (status ont::f) (class ?c1) (tma ?tma1) (sem ?sem1) (constraint ?con1))
		       (% *PRO* (var ?v2) (status ont::f) (class ?c2) (tma ?tma2) (sem ?sem2) (constraint ?con2)))))))
     
    -vbar-conj>
    (head (vp- (vform ?vf) (subjvar ?subj)  (subj ?subject) (var ?v1) (seq -)  (agr ?agr) (gap ?gap)
	       (advbl-needed -) (class ?c1) (constraint ?con1) (tma ?tma1) (sem ?sem1) (subj-map ?subjmap)
	   ))
    (CONJ (lf ?lx) (but-not -)) ;;(? lx or but however plus otherwise so and)))
    (vp- (vform ?vf) (var ?v2) (subjvar ?subj) (subj ?subject) (agr ?agr) (gap ?gap)
     (Advbl-needed -) (class ?c2) (constraint ?con2)  (tma ?tma2) (sem ?sem2))
    (sem-least-upper-bound (in1 ?sem1) (in2 ?sem2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    
    )

   ; (the dog that) chased and ate the cat
   ((vp- (seq +) (vform ?vf) (var *)  (subjvar ?subj)  (subj ?subject) (agr ?agr) (gap ?gap)(subj-map ?subjmap)
     (class ?class) (sem ?sem)
     (constraint (&  (OPERATOR ?lx) 
		     (SEQUENCE 
		      ((% *PRO* (var ?v1) (status ont::f) (class ?c1) (tma ?tma1) (sem ?sem1) (constraint ?con1))
		       (% *PRO* (var ?v2) (status ont::f) (class ?c2) (tma ?tma2) (sem ?sem2) (constraint ?con2)))))))
     
    -vbar-conj-dobj>
    (head (vp- (vform ?vf) (subjvar ?subj)  (subj ?subject) (var ?v1) (seq -)  (agr ?agr) (gap ?!dobj)
	       (advbl-needed -) (class ?c1) (constraint ?con1) (tma ?tma1) (sem ?sem1) (subj-map ?subjmap)
	   ))
    (CONJ (lf ?lx) (but-not -)) ;;(? lx or but however plus otherwise so and)))
    (vp- (vform ?vf) (var ?v2) (subjvar ?subj) (subj ?subject) (agr ?agr) (gap -) (dobj ?!dobj)
     (Advbl-needed -) (class ?c2) (constraint ?con2)  (tma ?tma2) (sem ?sem2))
    (sem-least-upper-bound (in1 ?sem1) (in2 ?sem2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    
    )
   
   ;; they had come, seen and conquered
  ;;  starting the VBARSEQ 
   ((VBARSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
     (class ?class) (sem ?sem)
     (constraint (& 
		     (SEQUENCE 
		      ((% *PRO* (var ?v1) (status ont::f) (class ?c1) (tma ?tma1) (constraint ?con1))
		       (% *PRO* (var ?v2) (status ont::f) (class ?c2) (tma ?tma2) (constraint ?con2)))))))
    -vbar-conj-seq1>
    (head (vp- (vform ?vf) (subjvar ?subj) (subj ?subject) (var ?v1) (seq -) (gap ?gap) (agr ?agr)
	       (advbl-needed -) (class ?c1) (constraint ?con1) (tma ?tma1) (sem ?s1)
	   ))
    (punc (lex (? x W::punc-comma)))
    (vp- (vform ?vf) (var ?v2) (seq -) (subjvar ?subj)  (subj ?subject)  (gap ?gap) (agr ?agr)
     (advbl-needed -) (class ?c2) (constraint ?con2)  (tma ?tma2) (sem ?s2))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )
    
    ;; extending the VBARSEQ
   ((VBARSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
     (class ?class) (sem ?sem)
     (constraint (& (SEQUENCE ?newlf))))
    -vbar-conj-seq2>
    (head (vbarseq (var ?v1) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
		   (constraint (& (sequence ?lf))) (class ?c1) (sem ?sem1)
		   (vform ?vf) (punc ?punc)
		   ))
    (punc (lex ?punc))
    (vp- (vform ?vf) (var ?v2) (seq -)  (gap ?gap)  (agr ?agr) (sem ?s2)
     (advbl-needed -) (subjvar ?subj) (subj ?subject) (class ?c2) (constraint ?con2) (tma ?tma2))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    (add-to-end-of-list (list ?lf) 
     (val (% *PRO* (var ?v2) (class ?c2) (constraint ?con2) (tma ?tma2))) (newlist ?newlf))
   )

   ;; ending the VBARSEQ 
   ((VP- (vform ?vf) (var *) (seq +) (subjvar ?subj) (subj ?subject)  (gap ?gap)  (agr ?agr) (sem ?sem)
     (class ?class) 
     (constraint (&  (OPERATOR ?lx) 
		     (SEQUENCE ?newlf)))
     )
    -vbar-conj-seq> 1.0
    (head (vbarseq (var ?v1) (subjvar ?subj) (subj ?subject) (gap ?gap) (sem ?s1) (class ?c1)
		   (constraint (& (sequence ?lf))) (agr ?agr)
		   (vform ?vf)
		   ))
    (CONJ (lf (? lx ont::and ont::or)))
    (vp- (vform ?vf) (subjvar ?subj) (subj ?subject)(var ?v2) (seq -)  (gap ?gap) (sem ?s2)
     (advbl-needed -)  (agr ?agr) (class ?c2) (constraint ?con2) (tma ?tma2))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    (add-to-end-of-list (list ?lf) 
     (val (% *PRO* (var ?v2) (class ?c2) (constraint ?con2) (tma ?tma2))) (newlist ?newlf))
    )

   ;; ending the VBARSEQ - WITH BUT-NOT - ONLY WORKS FOR only workd of -ing and BARE forms
   ((VP- (vform (? vf w::ing w::base)) (var *) (seq +) (subjvar ?subj) 
     (subj ?subject)  (gap ?gap)  (agr ?agr) (sem ?sem)
     (class ?class)
     (constraint (&  (OPERATOR ?conj) 
		     (SEQUENCE ?seq)
		     (except  (% *PRO* (var ?v2) (class ?c2) (constraint ?con2) (tma ?tma2)))))
     )
    -vbar-conj-seq-but-not> 1.0
    (head (vbarseq (var ?v1) (subjvar ?subj) (subj ?subject) (gap ?gap) (sem ?s1) (class ?c1)
		   (constraint (& (sequence ?seq))) (agr ?agr)
		   (vform (? vf w::ing w::base))
		   ))
    (CONJ (lf ?conj) (but-not +))
    (vp- (vform (? vf w::ing w::base)) (subjvar ?subj) (subj ?subject)(var ?v2) (seq -)  (gap ?gap)
     (advbl-needed -) (sem ?s2) (agr ?agr) (class ?c2) (constraint ?con2) (tma ?tma2))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )

     ;; ending the VBARSEQ, with comma 
   ((VP- (vform ?vf) (var *) (seq +) (subjvar ?subj) (subj ?subject)  (gap ?gap)  (agr ?agr) (sem ?sem)
     (class ?class)
     (constraint (&  (OPERATOR ?lx) 
		     (SEQUENCE ?newlf)))
     )
    -vbar-conj-seq-comma> 1.0
    (head (vbarseq (var ?v1) (subjvar ?subj) (subj ?subject) (gap ?gap) (sem ?s1) (class ?c1)
		   (constraint (& (sequence ?lf))) (agr ?agr)
		   (vform ?vf)
		   ))
    (punc  (lex (? x W::punc-comma)))
    (CONJ (lf (? lx ont::and ont::or)))
    (vp- (vform ?vf) (subjvar ?subj) (subj ?subject)(var ?v2) (seq -)  (gap ?gap) (sem ?s2)
     (advbl-needed -)  (agr ?agr) (class ?c2) (constraint ?con2) (tma ?tma2))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    (add-to-end-of-list (list ?lf) (val (% *PRO* (var ?v2) (class ?c2) (constraint ?con2) (tma ?tma2))) (newlist ?newlf))
    )

   ;;  starting the VPSEQ (need two as -s-conj2> above handles the binary conjunctive case)
   ((VPSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
     (SEQUENCE (?v1 ?v2)) (class ?class) (sem ?sem) (punc (? x W::punc-comma W::Punc-semicolon))
     )
    -vp-conj-seq1>
    (head (vp (vform ?vf) (subjvar ?subj) (subj ?subject) (var ?v1) (seq -) (gap ?gap) (agr ?agr)
	   (lf (% prop (class ?c1) (tma ?tma1)))
	   (advbl-needed -) (sem ?s1)
	   ))
    (punc (lex (? x W::punc-comma W::Punc-semicolon)))
    (vp (vform ?vf) (var ?v2) (seq -) (subjvar ?subj)  (subj ?subject)  (gap ?gap) (agr ?agr)
     (advbl-needed -) (sem ?s2)
     (lf (% prop (class ?c2) (tma ?tma2))))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    )
    
    ;; extending the VPSEQ
   ((VPSEQ (vform ?vf) (var *) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
     (class ?class) (sem ?sem)
     ;; (constraint (& (operator ?conjlf) 
     (SEQUENCE ?newlf))
    -vp-conj-seq2>
    (head (vpseq (var ?v1) (class ?c1) (sem ?s1)
	   (sequence ?lf) (subjvar ?subj)  (subj ?subject) (gap ?gap)  (agr ?agr)
	   (vform ?vf) (punc ?punc)
	   ))
    (punc (lex ?punc))
    (vp (vform ?vf) (var ?v2) (seq -)  (gap ?gap)  (agr ?agr)
     (advbl-needed -) (subjvar ?subj) (subj ?subject) (sem ?s2)
     (lf (% prop (class ?c2) (tma ?tma2)))
     )
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )

   ;; Ending the VPSEQ 
   ;; both Ss must be of the same type, decl or imperative
   ((VP (vform ?vf) (var *) (seq +) (subjvar ?subj) (subj ?subject)  (gap ?gap)  (agr ?agr) (sem ?sem)
     (LF (% prop (var *) (class ?class)  
	    (constraint (&  (OPERATOR ?lx) (SEQUENCE ?newlf)))
	    ))
     )
    -vp-conj-seq> 1.0
    (head (vpseq (var ?v1) (subjvar ?subj) (subj ?subject) (gap ?gap)
		(sequence ?lf) (agr ?agr) (class ?c1) (sem ?s1)
		(vform ?vf)
		))
    (CONJ (lf (? lx ont::and ont::or)))
    (vp (vform ?vf) (subjvar ?subj) (subj ?subject)(var ?v2) (seq -)  (gap ?gap)
     (advbl-needed -)  (agr ?agr) (sem ?s2)
     (lf (% prop (class ?c2) (tma ?tma2))))
    (sem-least-upper-bound (in1 ?s1) (in2 ?s2) (out ?sem))
    (class-least-upper-bound (in1 ?c1) (in2 ?c2) (out ?class))
    (add-to-end-of-list (list ?lf) (val ?v2) (newlist ?newlf))
    )
   ))


;;  construction for create a CAUSE event as in "He sneezed the box off the table"

(parser::augment-grammar
 '((headfeatures
    (vp vform agr comp3 cont postadvbl  aux modal lex headcat tma transform subj-map advbl-needed template subj subjvar))
    
        ;;  The dog barked the cat up the tree.
      ((vp (lf (% prop (class ONT::CAUSE-EFFECT) (var *) 
        (constraint (& (agent ?ag) (affected ?npvar) (method ?v) (formal ?mod)))
        (tma ?tma) 
        ;(transform ?transf) 
        (sem ?newsem)
        ))
     (tma ?tma) (class ONT::CAUSE-EFFECT) (var *)
         ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
;      (advbl-needed -) (complex +) (result-present +) (GAP ?gap)
      ;(SUBJ (% NP (Var ?npvar) (sem ?sem) (lex ?lex)))
      ;(subjvar ?npvar)
      (advbl-needed -) (complex +) (GAP ?gap)
      )
     -vp-result-advbl-intransitive-to-transitive> 0.98  ; prefer the transitive sense if there is one
     (head (vp (VAR ?v) 
        (seq -)  ;;  post mods to conjoined VPs is very rare
        ;(DOBJVAR -)  ; This doesn't work because it could unify with a dobjvar not yet instantiated
	(dobj (% -)) ; cannot use (dobj -) because dobj is (% - (W::VAR -))
        ;(SUBJ (% NP (Var ?npvar) (sem ?sem) (lex ?lex)))  
        ;(subjvar ?npvar)
        (constraint ?con) (tma ?tma) (result-present -)
	(subjvar ?ag)
	(subj-map ONT::AGENT)
	(COMP3 (% -))
        ;;(aux -) 
        (gap ?gap)
        (ellipsis -)
        ))
     (np (Var ?npvar) (sem ?sem))
     (advbl (ARGUMENT (% NP ;; (? xxx NP S)  ;; we want to eliminate V adverbials, he move quickly  vs he moved into the dorm
             (sem ?sem) (var ?npvar)))
      (GAP -) 
      ;; (subjvar ?subjvar)
      (SEM ($ f::abstr-obj (F::type (? ttt ont::path ont::position-reln)))) ;(F::type (? !ttt1 ont::position-as-extent-reln ont::position-w-trajectory-reln ))))
;      (SEM ($ f::abstr-obj (F::type (? ttt ont::position-reln ont::goal-reln ont::direction-reln))))
      (SET-MODIFIER -)  ;; mainly eliminate numbers 
      (ARG ?npvar) (VAR ?mod)
      ;;(role ?advrole) 
      )
     (compute-sem-features (lf ONT::CAUSE-EFFECT) (sem ?newsem))
     )

        ;;  He talked me deaf.
      ((vp (lf (% prop (class ONT::CAUSE-EFFECT) (var *) 
        (constraint (& (agent ?ag) (affected ?npvar) (method ?v) (formal ?mod)))
        (tma ?tma) 
        ;(transform ?transf) 
        (sem ?newsem)
        ))
     (tma ?tma) (class ONT::CAUSE-EFFECT) (var *)
         ;;(LF (% PROP (constraint ?new) (class ?class) (sem ?sem) (var ?v) (tma ?tma)))
;      (advbl-needed -) (complex +) (result-present +) (GAP ?gap)
      ;(SUBJ (% NP (Var ?npvar) (sem ?sem) (lex ?lex)))
      ;(subjvar ?npvar)
      (advbl-needed -) (complex +) (GAP ?gap)
      )
     -vp-result-adj-intransitive-to-transitive> 0.98  ; prefer the transitive sense if there is one
     (head (vp (VAR ?v) 
        (seq -)  ;;  post mods to conjoined VPs is very rare
        ;(DOBJVAR -)  ; This doesn't work because it could unify with a dobjvar not yet instantiated
	(dobj (% -)) ; cannot use (dobj -) because dobj is (% - (W::VAR -))
        ;(SUBJ (% NP (Var ?npvar) (sem ?sem) (lex ?lex)))  
        ;(subjvar ?npvar)
        (constraint ?con) (tma ?tma) (result-present -)
	(subjvar ?ag)
	(subj-map ONT::AGENT)
	(COMP3 (% -))
        ;;(aux -) 
        (gap ?gap)
        (ellipsis -)
        ))
     (np (Var ?npvar) (sem ?sem))
     (adjp (ARGUMENT (% NP (sem ?sem) (var ?npvar))) 
      (SEM ($ f::abstr-obj (F::type (? ttt ONT::position-reln ont::domain-property))))
      (GAP -)
      ;; (subjvar ?subjvar)
      (SET-MODIFIER -)  ;; mainly eliminate numbers 
      (ARG ?npvar) (VAR ?mod)
      ;;(role ?advrole) 
      )
     (compute-sem-features (lf ONT::CAUSE-EFFECT) (sem ?newsem))
     )

      )     
 )
