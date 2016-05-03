;;;;
;;;; speech-grammar.lisp
;;;;

;; additional rules that parse common phenomena that only occur in speech

(in-package w)

(parser::augment-grammar
 '((headfeatures
    (utt neg qtype sem subjvar dobjvar cont lex headcat transform)
    )

    ;; Various fragments such as "not a dog", "not red" etc. that are parsed with elided verb
   ((UTT (no-post-adv +) (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_PRED-FRAGMENT) (constraint (& (content ?v))))) (var *))
    -elided-verb-utt> 
    (head (S (STYPE elided-verb) (var ?v)))
    )
      
   ;;  ADVBL UTTERANCES
   ;; TEST: In the corner
   ;; TEST: sometimes
   ((UTT (no-post-adv +) (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_PRED-FRAGMENT) (no-post-adv +) (constraint (& (content ?v))))) (var **))
    -advbl-utt> .96
    (head (advbl (WH -) (SORT (? !sort DISC)) (VAR ?v) (ARGUMENT (% ?x (SEM ?sem))) 
		 (ARG (% *PRO* (VAR *) (gap -) (sem ?sem)))
	   (gap -))))
   
   ;; multiple advbl frags (in answers to questions), e.g.,  never in the morning
   ;; e.g., Never in the morning.
   ((UTT (no-post-adv +) (lf (% SPEECHACT (VAR **) 
		(CLASS ONT::SA_CONDITION) 
		(constraint (& (mods ?nv) (condition ?cond) (content ?v)))))
     (var **))
    -advbl-advbl-utt> .96
    (ADVBL (VAR ?nv) (ARG *) (ARGUMENT (% ?x (SEM ?sem))))
    (head (cp (WH -) (VAR ?v) (ctype s-if) (condition ?cond))))

   ;; e.g., In what corner?
   ((UTT (no-post-adv +) (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_QUERY) (constraint (& (content ?v) (focus ?foc))))) (var **))
    -advbl-utt-wh> .96
    (head (advbl (WH Q) (SORT (? !sort DISC)) (VAR ?v) (wh-var ?foc) (ARGUMENT (% ?x (SEM ?sem))) 
	   (ARG (% *PRO* (VAR *) (gap -) (sem ?sem)))
	   (gap -))))

   ;; creates too many spurious interpretations, so treat these as fragments
   ;; TEST: anyways
;   ((UTT (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_TELL) (constraint (& (MODS ?v))))) (var *))
;    -advbl-disc-utt> .96
;    (head (advbl (WH -) (SORT DISC) (VAR ?v) (ARG *) (gap -))))

   ;;  e.g., which train
   ;; TEST: Which dog?
   ((UTT (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_QUERY) (constraint (& (content ?npvar) (focus ?npvar))))) (var *) (FOCUS ?npvar))
    -np-utt-q> .95
    (head (NP (WH Q) (VAR ?npvar))))

   ;; adjective utts
   ;; TEST: Horizontal.
   ((UTT (no-post-adv +) (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_PRED-FRAGMENT) (constraint (& (content ?v))))) (var **))
    -adjp-utt> .96
    (head (adjp  (VAR ?v) (ARGUMENT (% ?x (SEM ?sem))) ;; (WH -)  I eliminated this to allow the question  "how red?"
		 (set-modifier -)  ;; disallows numbers as ADJP fragments - they already have a number interpretation 
		(ARG (% *PRO* (VAR *) (gap -) (sem ?sem) (constraint (& (CONTEXT-REL UTT-FRAG))))))
     ))

   ;; conditionals as utterances, e.g., if we saw the train.
   ;; TEST: If the dog barked.
   ;; TEST: If the dog chased the cat.
   ((UTT (lf (% SPEECHACT (VAR **) (CLASS ONT::SA_CONDITION) (constraint (& (content ?v) (condition ?cond))))) (var **))
    -conditional-utt> .96
    (head (cp (WH -) (VAR ?v) (ctype s-if) (condition ?cond))))


 ;; e.g., Why two boxcars of oranges?
   ;; Myrosia 2005/01/25 Added a sem restriction to prevent expletives from coming up here
   ;; TEST: Why the dog?
   ((Utt  (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_WHY-QUESTION) (constraint (& (content ?v) (focus ?v))))) 
          (qtype ?type) (punctype ?p))
    -why-np> 
    (word (LEX why)) 
    (head (NP (var ?v) (gap -) (sem ($ ?!type)))))

   ;;  e.g., why go to the store
   ;; TEST: Why bark?
   ;; TEST: Why chase the cat?
   ((Utt  (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_WHY-QUESTION) (constraint (& (content ?v) (focus ?v))))) 
          (qtype ?type) (punctype ?p))
    -why-vp> 0.87 ;; myrosia lowered the probability a little so that this does not interfere with lattices			        
    ;; this rule is really unfairly prejudiced because it uses a word, not a constituent
    ;; so it misses on penalties it would have if a real "why" constituent was built
    (word (LEX why)) 
    (head (VP (var ?v)
	   ;;(SEM ACTION)
	   (SEM ($ f::situation (f::cause (? c F::AGENTIVE F::MENTAL))))
	   (VFORM BASE) (gap -))))
   
   ;; e.g., Why not three boxcars of oranges?
   ;; Why not the dog?
   ((Utt (lf (% SPEECHACT (VAR *) (CLASS ONT::SA_REQUEST-COMMENT) (constraint (& (content ?v))))) 
         (punctype ?p))
     -why-not-pp> 
     (word (LEX why)) 
     (word (LEX not)) 
    (head ((? cat np vp) (gap -) (var ?v))))


;;  robust rules for speech

   ;; TO is often missed 
    ((cp (ctype s-to) (var ?v) (subj ?subj) (gap ?g)
      (LF (% prop (var ?v) (class ?class) (constraint ?lf))))
     -to-missing1> .9
     (head (vp (constraint ?lf) (class ?class) (gap ?g)
	       (subj ?subj) (gap -)
	       (vform base))))

))
