;; This file contains all the variables that control the behavior of the parser

(in-package :parser)

;;=======================================

;;  Basic Parser Control Parameters
;;(setf *number-interps-desired* 1)    ;; number of speech act interpretations desired
(setf (number-parses-desired *chart*) 1)     ;; number of speech acts to return
(setf (number-parses-to-find *chart*) 1)   ;; the number of parsers to find before stopping from which to try to build speech acts 
(setf (stopwhensolutionsfound *chart*) t)    ;; enable early termination when number of parses found - I wouldn't consider turning it off!
(setf *wait-for-speech-end* t)      ;; set t to make parser wait until speech recognition completes rather than parsing incrementally
                                     ;;         (helps a lot if they are both on the same machine)
(setq *process-lattices* t)          ;; if T, we will parse the lattice if we don't find one from the one-best
(setq *max-utterance-length* 2000)   ;; accepts utterance up to 20 seconds (good luck parsing it!!)
(setmaxnumberentries 3000)           ;; higher # entries to allow more complex utterances
;; (setmaxnumberentries 800)            ;; Max number of constituents before bailing out - set low here for CALO demo

(setq *skip-consituent-penalty* .97)   ;;  penalty for skipping over punctuation elements note: silences are not penalized)

;; OUTPUT  CONTROL
;; Controlling output from the test-lattice function
(setq *symbol-prefix* 'V)
(setq *print-package-flag* nil) ;; if non-nil, the terminal output routines will not suppress package prefixes
(setq *simplify-numbers* t)          ;; simplifies number logical forms (e.g., "three hundred and seven" -> 307.
(setf (standalone *chart*) nil)              ;; suppress messages such as turn taking when running standalone (does it do this anymore?)
(setq *filter-sa-on* nil)            ;; supress low probability and empty speech acts
(setq *reliability-threshold* 0)    ;; minimum weight for a speech act to be considered
;;  the costs that drive the search for the best constituent coverage in the chart
(setq *cost-table* '((w::PUNC 5)  ;; Puncs should be attached to their utts
                     (w::UTT 1)  ;; making UTT one prefers sequences with minimal number of UTTs
		     (w::NP 2) (w::PATH 2) ;; These are very common fragments (NP prob 1 == UTT with prob .5)
		     (w::ADVBL 2) (w::ADJP 2)  ;; other high level constits
		     ))

(setq *default-text-cost-table* '((ont::SA_TELL 1) (ont::SA_QUERY 1.2) (ont::IDENTIFY 2) (ont::SA_pred-fragment 2) 
				  (ont::SA_request 1.2) (ont::SA_YN-QUESTION 1.2)
				  (ont::SA_CONFIRM 1.3) (ont::SA_WH-QUESTION 1.2) (w::CP 2) (w::VP 2) 
				  (w::punc .5)))

(setq *default-speech-cost-table* 
      '((w::PUNC 5)  ;; Puncs should be attached to their utts
	(w::UTT 1)  ;; making UTT one prefers sequences with minimal number of UTTs
	(w::NP 2) (w::PATH 2) ;; These are very common fragments (NP prob 1 == UTT with prob .5)
	(w::ADVBL 2) (w::ADJP 2)  ;; other high level constits
	))

(setq *default-definition-cost-table*
      '((w::definition 1) (ont::SA_QUERY 3) (ont::IDENTIFY 1.5) (ont::SA_pred-fragment 3) 
	(ont::SA_request 3) (ont::SA_YN-QUESTION 3)
	(ont::SA_CONFIRM 3) (ont::SA_WH-QUESTION 3) (ont::TELL 3)(w::CP 3) (w::VP 3) (w::ADJP 3) (w::advbl 3)
	(w::punc .5)))

(setq *threshold-factor* 1)  ;; This is used to compute a threshold to stop search when the cost of a path

;;  Grammar Controls
;; This  Specifies the features that should be inserted in any GAP constituent of the category that is first in the list (e.g., W::NP)
(setq *default-gap-cat-feat-lists* '((w::NP w::VAR w::AGR w::SEM w::CASE)))
(setf (flexible-semantic-matching *chart*) t)  ;; set to T to allow unification with semantic mismatches
(setq *sem-failure-penalty* .9)  ;; the multiplier in cases of semantic mismatch (only used if *flexible-semantic-matching* is t)
(setq *default-rule-probability* 1.0)   ;; The default probabilty used for rules that don't specify a probability
(defvar *default-word-length* 6) ;; ideally, word length is computed form the UTTERANCE message, but if not, about 6 is good for text,
                                 ;;  and say around 30 for speech

;; Detailed Parser Algorithm Control for Evaluations
(setq *test-mode* 'print)   ;; PRINT - it prints the answer, TIMED - it gives the timing statistics
(setq *ignore-binding-loops* t)  ;; if set to NIL, parser will break if it finds a binding list that contains a loop (of course, this should never happen ....)
(setq *reference-filter-enabled* nil) ;; When set to T, the parser incrementally calls reference at each NP built

(setq  *wait-for-speech-end* nil)   ;; set t to stop parser running while speech recognition is running

;;    Variables to control the scoring

(setq *score-length-multiplier* .8)   ;; proportion of score that is calculated based on the length of the constituent
(setq *score-corner-multipler* 0)     ;; proportion of score that is calculated based on how far from the starting position this constituent begins
(setq *score-probability-threshold* 0)   ;;  an old way of normalizing the probabilities within a range - it increases the boosted terms, so makes the search more chaotic
(setq *word-length* 8)  ;; number of time units for the average word
(setq *rule-multiplier* .99)   ;; average decrease in probability per word

(parser::define-lf-preferences
  '(((ONT::PUT :agent :theme) .9)   ;; inhibit put/place commands without modifiers i.e., the location complement
    ((ONT::ALLOW :effect :theme :agent :mods) .9) ;; inhibit attachments to "let"
    ((ONT::SHOW :addressee :theme :agent :mods) .9) ;; inhibit attachments to "show"
    ((ONT::TRANSFER-INFORMATION :theme :addressee :agent :mods) .9)
    )
  )

(setq *no-positions-in-lf* nil)  ;; set to nil if we have positional info in the LF
(setq *include-parse-tree-in-messages* nil)  ;;  set to a list of feature names  if the parse tree (with the specified features) is also desired in the UTT messages
(setq *promote-tmas* t)   ;; set to t if the TMA features should be promoted to the top level rather than being embedded in the TMA feature
(setq *lf-roles-to-suppress*  '(w::LSUBJ w::LOBJ w::LIOBJ w::LCOMP -))  ;; generally don't want the syntactic roles in the LF

;; incremental parser controls

(setq *incremental-parsing-enabled* nil)
(setq  *vp-filter-enabled* nil)   ;; incremental VP filtering
(setq *VP-filter-interpolation-value* 0.1) ;; influence of VP filter on constituent score

(setq *reference-filter-enabled* nil)   ;; incremental NP filtering
(setq *reference-interpolation-value* 0.1)   ;; influence of reference feedback on constituent score

;; variable for application specific stuff
(defvar *in-system* nil)

(setq *use-tags-as-filter* nil) ;;  determines whether POS tags are used to filter lexicon
(setq *bad-tag-multiplier* 0)  ;; prob multiplier for bad tags  (0 = eliminate it)

;; Using advice on constituent boundaries
(setq *skeleton-boost-factor* 1.05)
(setq *skeleton-constit-cats* '(W::NP W::S W::CP W::VP W::ADVBL W::PP W::ADJP))   ;; set them to all useful so grammar is processed on initial loading
                                                                                  ;; and then the system file for the app may downsize the list
(setq *skeleton-penalty-factor* 1)  ;; default is no penalty as wit generally hurts good constits not predicted

(setq *use-senses-as-filter* nil)
(setq *bad-sense-multiplier* .97)

(setq *wn-wsd-enabled* t)
(setq *kr-type-info-desired* nil)

(setq *filter-texttagger-input* t) ;;  if set to T we remove tagged subparts of stretches that are subsumed by a constituent with domain info
