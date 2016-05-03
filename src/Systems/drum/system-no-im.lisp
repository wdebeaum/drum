;;;;
;;;; File: Systems/drum/system-no-im.lisp
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :drum
  (:old-trips-component :lxm               #!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser            #!TRIPS"src;Parser;")
  (:dfc-component       :channelkb         #!TRIPS"src;ChannelKB;")
  (:dfc-component       :discourse-context #!TRIPS"src;DiscourseContext;")
  (:dfc-component       :deepsemlex	   #!TRIPS"src;DeepSemLex;code;lib;")
)

;; This bogosity semi-approved by George
(defpackage :wordfinder)
(setq wordfinder::*use-wordfinder* t)

;; DRUM uses text-tagger
(setq *use-texttagger* t)
  
;; Now load the system
(trips:load-trips-system)
;; this isn't part of a lisp component so we load it separately
(load #!TRIPS"src;TextTagger;drum-dsl-resources.lisp")

;;;
;;; After-load customizations for this TRIPS system
;;;

(setq parser::*parser-init-settings*
      '((parser::*parser-display-enabled* nil)
	(parser::*in-system* :drum)
	(parser::*word-length* 8)      ;; average number of letters in a word (not critical)
	(parser::*score-length-multiplier* .6)   ;;  boost factor based on length of input covered
	(parser::*score-corner-multiplier* 0)    ;; not clear this is helpful
	(parser::*use-tags-as-filter* t)        ;;  indicate we should use POS information
	(parser::*bad-tag-multiplier* .9)  ;;  penalty multiplier for lex entries that do not match POS tags
	(parser::*skeleton-constit-cats* '(W::NP W::CP W::VP W::ADVBL W::PP W::S))  ;; constituents that we expect in the skeleton
	(parser::*skeleton-boost-factor* 1.1)   ;;  boost if we build a constituent that matches the skeleton (from stat. parser)
	((setf (parser::barrier-penalty parser::*chart*) .8))
	(parser::*use-senses-as-filter* t)
	(parser::*bad-sense-multiplier* .96)
	(parser::*no-positions-in-lf* nil)
	(parser::*beam-pruning-on* t) 
	(parser::*pruning-frequency* 500)
	(parser::*beam-width* 20)
	((parser::setmaxnumberentries 6000))    ;;  max 3000 constituents built before stopping
	((parser::setmaxchartsize 8000))
	(parser::*kr-type-info-desired* '(:WNsense :DRUM))
	((setf (parser::flexible-semantic-matching parser::*chart*) t))
	(parser::*domain-boosting-factor* 1.01)   ;;  boost content words that have domain specific info attached
	((setf (parser::number-parses-desired parser::*chart*) 4))  ;; get 4 interpretations before stopping
	((parser::customize-cost-table '((ont::SA_TELL 1) (ont::SA_identify 1.1)
					 (ont::SA_QUERY 2) (ont::SA_pred-fragment 2) (ont::SA_request 2.5) (ont::SA_YN-QUESTION 2.5)
					 (ont::SA_CONFIRM 1.3) (ont::SA_WH-QUESTION 2.5) (w::CP 1.3)  ;; disfavor speech acts not common in text
					 (w::VP 2) (w::punc .5))))
	))

(parser::initialize-settings)   ;; do it here to set the parser for testing (s o it matches the settings of the parser component)

(setq *print-pretty* t)

(setq *texttagger-split-mode*
  :split-clauses
;  :split-sentences
  )

;; LxM options
;; use WordFinder?
(setq lxm::*use-wordfinder* t)
(setq lxm::*use-tagged-senses-only* t)   ;; we are trying to really depend on the Stanford parser (for now!)
(setq lxm::*no-wf-senses-for-words-tagged-with-ont-types* t)
(setq  lxm::*use-trips-and-wf-senses* nil) ;; don't use wordnet if we have TRIPS entries  

;; LOGGING options
(setq logging::*logging-enabled* nil)
(setq logging2::*logging-enabled* nil)

