;; This file contains all the variables that control the behavior of the parser

(in-package :parser)

;;=======================================
;; Grammar/Parsing control

;;  Basic Parser Control Parameters
(setq *number-interps-desired* 1)    ;; number of speech act interpretations desired
(setq *number-parses-desired* 1)     ;; number of parses from which to try to build speech acts
(setf (stopwhensolutionsfound *chart*) t)  ;; enable early termination when number of parses found - I won't consider turning it off!
(setq *process-lattices* t)          ;; if T, we will try to find an interpretation in the lattice if we don't find one in the incremental input
(setq *simplify-numbers* t)

(setq *max-utterance-length* 2000)  ;; accepts utterance up to 20 seconds (good luck parsing it!!)

(setmaxnumberentries 1500)    ;; Max number of constituents before bailing out - set low here for CALO demo

;; Controlling output from the test-lattice function
(setq *test-mode* 'print)   ;; PRINT - it prints the answer, TIMED - it gives the timing statistics

;;  The printer typically suppresses the packages. 
(setq *print-package-flag* nil) ;; if non-nil, the terminal output routines will not suppress package prefixes


;; This  Specifies the features that should be inserted in any GAP constituent of the category that is first in the list (e.g., W::NP)
(setq *default-gap-cat-feat-lists* '((w::NP w::VAR w::AGR w::SEM w::CASE)))

;; IF NIL, this disables selectional restrictions (unlikely this really works, must check)
(setq *use-sem* t) ;; Set to nil to stop generating selelctional restrictions

;; The default probabilty used for rules that don't specify a probability
(setq *default-rule-probability* 1.0)   

;; Detailed Parser Algorithm Control
(setq *ignore-binding-loops* t)  ;; if set to NIL, parser will break if it finds a binding list that contains a loop (of course, this should never happen ....)

(setq *standalone* nil)              ;; suppress messages such as turn taking

;; When set to T, the parser incrementally calls reference at each NP built

(setq *reference-filter-enabled* nil)

;; Controls for extracting Speech acts from the Chart

(setq *filter-sa-on* nil)            ;; supress low probability and empty speech acts
(setq *reliability-threshold* 0)    ;; minimum weight for a speech act to be considered
;;  the costs that drive the search for the best constituent coverage of the input
(setq *cost-table* '((w::PUNC 5)  ;; Puncs should be attached to their utts
                     (w::UTT 1)  ;; making UTT one prefers sequences with minimal number of UTTs
		     (w::NP 2) (w::PATH 2) ;; These are very common fragments (NP prob 1 == UTT with prob .5)
		     (w::ADVBL 2) (w::ADJP 2)  ;; other high level constits
		     ))
(setq *threshold-factor* 1)  ;; This is used to compute a threshold to stop search when the cost of a path


(setq *parser-is-online* t) ;; If T, messages are processed normally by the parser. If NIL, then most
;; messages are ignored, at least those that would cause parsing to happen (as
;; opposed to control messages). This variable is toggled by the ONLINE and
;; OFFLINE requests.

(setq  *wait-for-speech-end* t)   ;; set t to stop parser running while speech recognition is running

;;=======================================
;; Lexicon/Domain specialization control
;; the first four of these vars are used in myrosia's kr transform handling, which is still used in monroe and medadvisor domains, but not in calo -- for these we use nate's transform code
(setq lxm::*keep-nonspecialized-entries* nil) ;; If set to t, will cause non-specialized entries for which specializations exist to be retained in the lexicon with a lower probability"
(setq lxm::*no-kr-probability* 1) 
(setq lxm::*kr-in-lexicon* t) 
(setq lxm::*kr-sense-boost* 1.03) 
(setq lxm::*find-unknown-words* nil)
(setq lxm::*use-domain-senses* nil) ;; if t the lxm boosts senses if they are used in nate's new transforms
