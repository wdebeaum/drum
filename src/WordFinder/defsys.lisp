;;;;
;;;; defsys.lisp: Defsystem for Dave's Word Finder
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 19 Mar 2003
;;;; Time-stamp: <Wed Apr 16 16:20:55 EDT 2008 swift>
;;;;
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :logging)
  (load #!TRIPS"src;Logging;defsys"))

(unless (and (find-package :ont) (find-package :f))
  (load #!TRIPS"src;OntologyManager;defsys"))

(unless (find-package :w)
  (load #!TRIPS"src;LexiconManager;defsys"))

(defpackage :wordfinder
  (:use :common-lisp :util)
  (:nicknames :wf))

(in-package :wordfinder)

(mk:defsystem :wordfinder
    :source-pathname #!TRIPS"src;WordFinder;"
    :components ("overhead"
		 "messages"
		 "lookup"
		 ;; no longer using cmu-names -- using NER instead
;		 "cmu-names/name-database"
;		 "comlex/comlex" ;phasing out comlex
		 "wordnet/stoplist"
		 "wordnet/wordnet"
		 "wordnet/wordnet-to-trips"
		 )
    :depends-on (:util)
    :finally-do (init-wordfinder)
    )

;; Get auto-configured filenames for lexical resources
(load #!TRIPS"src;config;WordFinder;defs")

;; Possible run-time overrides
;; no longer using cmu-names
;(defvar *cmu-names-filename-override*
;  #!TRIPS"etc;WordFinder;cmu-names;cmu-list-of-names")
(defvar *comlex-filename-override*
  #!TRIPS"etc;WordFinder;comlex;comlex_synt_1.1.1")
(defvar *wordnet-basepath-override*
  #!TRIPS"etc;WordFinder;wordnet;dict;")

(if *use-wordfinder*

(defun init-wordfinder (&key (verbose t))

;; If we're using cmu-names...
;  (when *cmu-names-filename*
;    ;; If there's a run-time version of the resource...
;    (when (probe-file *cmu-names-filename-override*)
;      ;; Then override the compiled-in default
;      (setq *cmu-names-filename* *cmu-names-filename-override*))
;    ;; Trace msg
;    (if verbose 
;        (format t "wordfinder: loading CMU names data: ~A~%" *cmu-names-filename*))
;    ;; Initialize it (from cmu-names/name-database.lisp)
;    (build-name-hashtable *cmu-names-filename*))

  ;; If we're using comlex...
;  (when *comlex-filename*
;    ;; If there's a run-time version of the resource...
;    (when (probe-file *comlex-filename-override*)
;      ;; Then override the compiled-in default
;      (setq *comlex-filename* *comlex-filename-override*))
;    ;; Trace msg
;    (if verbose
;        (format t "wordfinder: loading COMLEX data: ~A~%" *comlex-filename*))
;    ;; Initialize it (from comlex/comlex.lisp)
;    (build-comlex-hashtable *comlex-filename*))

  ;; If we're using wordnet...
  (when *wordnet-basepath*
    ;; If there's a run-time version of the resource...
    (when (probe-file *wordnet-basepath-override*)
      ;; Then override the compiled-in default
      (setq *wordnet-basepath* *wordnet-basepath-override*))
    ;; Trace msg
    (if verbose
	(format t "wordfinder: loading WordNet data: ~A~%" *wordnet-basepath*))
    ;; Initialize it (from wordnet/wordnet-to-trips.lisp)
    (setup-wordnet *wordnet-basepath*)))

;; Else !*use-wordfinder*
(defun init-wordfinder (&key (verbose t))
  nil)
)

