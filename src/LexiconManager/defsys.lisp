;;;;
;;;; defsys.lisp for LexiconManager
;;;;
;;;; $Id: defsys.lisp,v 1.25 2015/08/13 19:18:29 wdebeaum Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :logging)
  (load #!TRIPS"src;Logging;defsys"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :om)
  (load #!TRIPS"src;OntologyManager;defsys"))

(unless (find-package :w)
  (load #!TRIPS"src;LexiconManager;w-pkg"))

;; Used to cache PARSER package in *parser-package* (below)
(unless (find-package :parser)
  (load #!TRIPS"src;Parser;parser-pkg"))

(unless (find-package :lxm)
  (load #!TRIPS"src;LexiconManager;lxm-pkg"))

(in-package :lexiconmanager)

;; the first four of these vars are used in myrosia's kr transform handling, which is still used in monroe and medadvisor domains, but not in calo -- for these we use nate's transform code
(defvar *keep-nonspecialized-entries* t "If set to t, will cause non-specialized entries for which specializations exist to be retained in the lexicon with a lower probability")
(defvar *no-kr-probability* .99) 
(defvar *kr-in-lexicon* nil) 
(defvar *kr-sense-boost* 1.03) 
(defvar *use-domain-senses* nil) ;; if t the lxm boosts senses if they are used in nate's new transforms
(defvar *lexicon-data*)  ;; all words defined in TRIPS lexicon data files

(defvar *parser-package* (find-package :parser)
  "Cached value of the PARSER package, used by MAKE-LEXICON-ENTRY.")

;; Bring in wordfinder defs after W package defined
(unless (find-package :wf)
  (load #!TRIPS"src;WordFinder;defsys"))

;  "Whether to call WordFinder to lookup unknown words. Default value depends
;on configured setting in WordFinder package."
(defvar *use-wordfinder* wf::*use-wordfinder*)
(defvar *request-is-from* nil
  "When LXM processes a request, it binds this to the :sender of the request,
   who is likely waiting for a reply and won't respond to any requests made to
   it until LXM replies, resulting in a deadlock. Specifically, WordFinder
   (which is called for get-word-def requests) will avoid making requests to
   TextTagger when this is set to lxm::textagger.")

(mk:defsystem :scenario-code
    :source-pathname #!TRIPS"src;LexiconManager;"
    :load-only t
    :components ("scenario" 
		 "messages"))

(mk:defsystem :lexicon-code
    :source-pathname #!TRIPS"src;LexiconManager;Code;"
    :components ("structures"
		 "warning"
		 "make-templates"
		 "lf-functions"
		 "make-vocab"
		 "lexicon-functions"
		 "lexicon-DB"
		 "make-new-lex"
		 "dynamic-lexicon"
		 "wordfinder-interface"
		 "lexicon-interface"
		 "nomlex-verbnoms"
		 ))

(mk:defsystem :lxm
    :package lxm
    :depends-on (:comm :logging :util :om :scenario-code
		 :lexicon-code))

;; Only want to load wordfinder if we're going to use it
;; Can't conditionally include a dependency in a defsystem declaration,
;; so we do it here by modifying the system's dependency list directly
(when *use-wordfinder*
  (let ((system (mk:find-system :lxm)))
    (setf (mk::component-depends-on system)
	  (append (mk::component-depends-on system) (list :wordfinder)))))

(defmethod mk:operate-on-system :after ((name (eql :lxm)) (operation (eql :load)) &rest args)
  (format *trace-output* "~&~%;;; lxm: initializing lexicon~%~%")
  (initialize-lexicon)
)

(defun initialize-lexicon (&key (kr-in-lexicon *kr-in-lexicon*)
			   (kr-boost *kr-sense-boost*)
			   (no-kr-probability *no-kr-probability*)
			   (keep-nonspecialized *keep-nonspecialized-entries* )
			   (use-domain-senses *use-domain-senses*)
			   )
  "Initialize and load the lexion.
The kr variables are for myrosia's kr transforms, which are still used
in monroe and medadvisor domains.
In calo use-domain-senses controls whether we check for in-domain senses
via nate's transforms."
  (setq *lexicon-data* (init-lexicon-db))
  (load-lexicon-data)
  (setq *kr-in-lexicon* kr-in-lexicon)
  (setq *kr-sense-boost* kr-boost)
  (setq *no-kr-probability* no-kr-probability)
  (setq *keep-nonspecialized-entries* keep-nonspecialized)
  (setq *use-domain-senses* use-domain-senses)
  (load-new-scenario trips::*scenario*)
  )

(defun load-lexicon-data (&key (version :new))
  ;; Templates
  (load #!TRIPS"src;LexiconManager;Data/templates/templates.lisp")
  (load #!TRIPS"src;LexiconManager;Data/templates/noun-templates.lisp")
  (ecase version
    (:old
     ;; Old-style lexicon was (mk:load-system :lexicon-data)
     (load #!TRIPS"src;LexiconManager;Data/old/vocab.lisp")
     (load #!TRIPS"src;LexiconManager;Data/old/nouns-vocab.lisp")
     (load #!TRIPS"src;LexiconManager;Data/old/pronouns.lisp")
     (load #!TRIPS"src;LexiconManager;Data/old/adj-vocab.lisp")
     (load #!TRIPS"src;LexiconManager;Data/old/fnwords.lisp")
     (load #!TRIPS"src;LexiconManager;Data/old/time-and-space-adverbs.lisp")
     (load #!TRIPS"src;LexiconManager;Data/old/other-adverbs.lisp")
     (load #!TRIPS"src;LexiconManager;Data/old/values.lisp"))
    (:new
     ;; New-style lexicon loads directories (after templates)
     (format *trace-output* "~&;; lxm: loading per-lemma data files...~%")
     (let ((*package* (find-package :lxm))
	   (*load-verbose* nil))
       (mapc #'load (directory #!TRIPS"src;LexiconManager;Data;new;**;*.lisp")))
     (format *trace-output* "~&;; lxm: done loading per-lemma data files~%"))
    (:all
     ;; Or all entries in one file (if  such a file exists)
     (let ((*package* (find-package :lxm)))
       (load #!TRIPS"src;LexiconManager;Data/new/all-lex.lisp")))))

(defun load-lexicon ()
  (mk:load-system :lxm)
  )

(defun load-lexicon-code ()
  (mk:compile-system :lexicon-code)
  )

(defun lexicon-words (&optional (lexicon-data *lexicon-data*))
  "See also commented out lexicon-entries in Code/lexicon-functions.lisp."
  (loop for word being each hash-key of (lexicon-DB-word-table lexicon-data)
       collecting word))
