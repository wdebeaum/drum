;;;
;;; After-load customizations for this TRIPS system
;;;

;; DRUM uses text-tagger
(setq *use-texttagger* t)

;;;; Parser options
(setq parser::*parser-init-settings*
      '((parser::*parser-display-enabled* nil)
	(parser::*in-system* :drum)
	;; average number of letters in a word (not critical)
	(parser::*word-length* 8)
	;; boost factor based on length of input covered
;	(parser::*score-length-multiplier* .4)
	(parser::*score-length-multiplier* .2)
	;; not clear this is helpful
	(parser::*score-corner-multiplier* 0)
	;; indicate we should use POS information
	(parser::*use-tags-as-filter* t)
	;; penalty multiplier for lex entries that do not match POS tags
	(parser::*bad-tag-multiplier* .9)
	;; penalty for referential-sem lex items
	(parser::*referential-sem-penalty* .96)
	;; constituents that we expect in the skeleton
	(parser::*skeleton-constit-cats* '(W::NP W::CP W::VP W::ADVBL W::PP W::S))
	;; boost constituents that match the skeleton (from stat. parser)
;	(parser::*skeleton-boost-factor* 1.1)
;	(parser::*skeleton-boost-factor* 1.04)
	(parser::*skeleton-boost-factor* .2)   ;; this is the percent of the difference from prob to 1 that is the boost
	;; penalty for crossing skeleton constituent boundaries
	((setf (parser::barrier-penalty parser::*chart*) .99))
	;;
	(parser::*use-senses-as-filter* t)
	;;
	(parser::*bad-sense-multiplier* .96)
	;;
	(parser::*no-positions-in-lf* nil)
	;;
	(parser::*beam-pruning-on* t) 
	;;
;	(parser::*pruning-frequency* 1000)
	(parser::*pruning-frequency* 1000)
	;;
	(parser::*beam-width* 50)
	;; max number of constituents built before stopping
	((parser::setmaxnumberentries 6000))
	;;
	((parser::setmaxchartsize 8000))
	;;
	(parser::*kr-type-info-desired* '(:WNsense :DRUM :wordnet))
	;;
	((setf (parser::flexible-semantic-matching parser::*chart*) t))
	;; boost content words that have domain specific info attached
	(parser::*domain-boosting-factor* .2)  ;; 20% increase on difference from 1
	;; have the parser remove tagged constituents that are subparts of other terms with domain info
	(parser::*filter-texttagger-input* t)
	;; number of interpretations to obtain before stopping
	((setf (parser::number-parses-to-find parser::*chart*) 4))
	;; number of interpretations returned
	((setf (parser::number-parses-desired parser::*chart*) 1))
	;; disfavor speech acts not common in text
	(parser::*filter-and-preparse-input* t)   ;; enable preparsing (e.g., for sequences)
	;; required for WebParser
	(parser::*include-parse-tree-in-messages* '(w::lex))
	((parser::customize-cost-table 
	  '((ont::SA_TELL 1)
	    (ont::SA_identify 1.3)
	    (ont::SA_CONFIRM 1.3)
	    (ont::SA_QUERY 2)
	    (ont::SA_pred-fragment 2)
	    (ont::SA_request 2.5)
	    (ont::SA_YN-QUESTION 2.5)
	    (ont::SA_WH-QUESTION 2.5)
	    (w::ISOLATED-CP 1.2)
	    (w::CP 1.5)  
	    (w::VP 2)
	    (w::punc .5))))
	))

(parser::initialize-settings)   ;; do it here to set the parser for testing (s o it matches the settings of the parser component)

;;;; IM options
;; dialogue manager, eg: textIM, simpleIM, extractIM...
(setq im::*current-dialog-manager* #'im::extractsequenceIM)
;;(setq im::*current-dialog-manager* #'im::extractIM)

;(setq im::*substitute-types-in-pros* t)
(setq im::*substitute-types-in-pros* nil)

(setq im::*compute-force-from-tmas* t)
;; allow highly fragmented parses
(setq im::*max-allowed-utts-in-turn* 30)
;; no domain-specific reasoner
(setq im::*external-name-resolution* nil)
;; system running without a behavioral agent; we do discourse processing only
(setq im::*no-BA-mode* t)
;; include the domain semantic info into the LF output
;(setq im::*include-domain-info* t)
;; don't add all the non-matched features of extracted terms (they are retrievable from the context)
;(setq im::*add-features-to-entailment* nil)
;; pass on the LF structures utterance by utterance (rather than by paragraph)
(setq im::*lf-output-by-utterance* t)
;; generate LF graphs? (turn on for debugging)
(setq im::*show-lf-graphs* nil)
;; we want to extract multiple events even when they share arguments
(setq im::*max-cover-with-strict-disjoint-extractions* nil)
;; we want to emit all events
(setq im::*eliminate-subevents* nil)

(setq im::*allow-optional-lfs* t) ;; set to t for optional term matching
(setq im::*output-format* 'im::lf-term)

;(setq im::*symbol-map* nil)


;; this looks obsolete
;(setq ex::*extraction-ids* '(ex::drum-extraction))

(setq *print-pretty* t)

(setq *texttagger-split-mode*
  :split-clauses
;  :split-sentences
  )

;;;; LxM options
;; use WordFinder?
(setq lxm::*use-wordfinder* t)
;; we are trying to really depend on the Stanford parser (for now!)
(setq lxm::*use-tagged-senses-only* t)
;; don't use wordnet if we have domain-specific info from TextTagger
(setq lxm::*no-wf-senses-for-words-tagged-with-ont-types* t)
;; don't use wordnet if we have TRIPS entries  
(setq  lxm::*use-trips-and-wf-senses* nil) 
;; lower bound
(setq lxm::*domain-score-lower-bound* 0.95)

;;;; LOGGING options
(setq logging::*logging-enabled* nil)
(setq logging2::*logging-enabled* nil)

;;;; extractor rules
(setq im::*extraction-sequence* '((im::preprocessRules) (im::drumterms) (im::drumtermsAdd) (im::drum) (im::drum_ev_add) (im::drummod) (im::drumCC) (im::drumMisc)))
(setq im::*substitute-terms-in-extraction* t)

; lg
;(parser::traceon 2)
