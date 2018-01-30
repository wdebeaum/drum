;;;;
;;;; File: Systems/drum/system.lisp
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :drum
  (:dfc-component	:lxm               #!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser            #!TRIPS"src;Parser;")
  (:dfc-component       :im                #!TRIPS"src;NewIM;")
  (:dfc-component       :channelkb         #!TRIPS"src;ChannelKB;")
  ;;(:dfc-component       :discourse-context #!TRIPS"src;DiscourseContext;")
  (:dfc-component       :deepsemlex	   #!TRIPS"src;DeepSemLex;code;lib;")
)

;; add WebParser to the system when we have it
(when (probe-file #!TRIPS"src;WebParser;defsys.lisp")
  (nconc (assoc :drum trips::*trips-systems*)
	 (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

;; Now load the system
(trips:load-trips-system)
;; ... and files that aren't part of a component

;; DeepSemLex resource definitions from TextTagger's data
(load #!TRIPS"src;TextTagger;drum-dsl-resources.lisp")

;; a few additional grammar rules useful for robust text processing
(load "robust-grammar.lisp")

;;;; extractor rules
#|
(load "preprocessRules.lisp")
(load "DRUMtermRules.lisp")
(load "DRUMtermRules_add.lisp")
(load "DRUMRules_ev.lisp")
(load "DRUMRules_ev_add.lisp")
(load "DRUMRules_mod.lisp")
(load "DRUMRules_CC.lisp")
(load "DRUMRules_misc.lisp")

(load "symbolmapping.lisp")
|#

(load "preprocessRules.lisp")
(load "cwmsRules.lisp")
(load "DRUMRules_ev.lisp")
(load "DRUMRules_mod.lisp")
(load "cwmsRules_CC.lisp")
(load "symbolmapping.lisp")
(setq im::*symbol-map* (append im::*symbol-map* '((ONT::QUANTITY-ABSTR ONT::QTY)
						  (ONT::OBJECTIVE-INFLUENCE ONT::INFLUENCE im::-rule5_3_AGENT_AFFECTED)
						  )))

;; domain preferences
(load "domain-sense-preferences")
(load "domain-words.lisp")

(defun parse-eval (x)
  (im::send-msg `(request :receiver parser :content (eval ,x))))

