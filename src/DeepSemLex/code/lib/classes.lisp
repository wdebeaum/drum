;;;; class declarations (must be loaded before any other code using them, to avoid breaking some Lisps)

(in-package :dsl)

(locally (declare (optimize safety))

(defclass-simple provenance ()
  "Where a concept, relation, or input-text came from."
  (symbol name "the name of the annotator or resource (not including version)")
  ((maybe string) version "the version of the resource" nil)
  ((maybe string) filename
   "the name of the specific file within the resource (should be nil if there
    is only one file)"
   nil)
  ((maybe integer) record-number
   "the index of the record within the file (byte number, line number, sentence
    number, etc.; resource-specific)"
   nil)
  ((list-of provenance) provenance
   "further provenance info, specifying where the named resource got its information from"
   nil)
  )

;; unfortunately equalp doesn't work for class instances the way it does for
;; structures
;; TODO make this more generic
(defun provenance-equalp (p1 p2)
  (or (eq p1 p2)
      (and (eq (name p1) (name p2))
      	   (equalp (version p1) (version p2))
      	   (equalp (filename p1) (filename p2))
      	   (equalp (record-number p1) (record-number p2))
	   (= (length (provenance p1)) (length (provenance p2)))
	   ;; TODO ignore ordering?
	   (every #'identity
	          (mapcar #'provenance-equalp (provenance p1) (provenance p2)))
	   )))

(defclass-simple input-text ()
  "A chunk of text (usually a sentence) to be used as input to the parser."
  (string text "the text itself")
  ((maybe provenance) provenance "" nil)
  (list lattice "a list of TextTagger-like messages to the parser" nil)
  (symbol lf-root "the ID of the root term in lf-terms." nil)
  (list lf-terms "the list of LF terms output by the gloss system." nil)
  (list syntax-tree "the syntax tree output by the gloss system." nil)
  )

(defclass-simple concept ()
  "An abstract top-level concept class. Different aspects of lexical concepts
   are their own subclasses, which may be aggregated or inherited from to form
   completely defined concepts and (with morphs) senses."
  (symbol name "the name of the concept"
   (intern (symbol-name (gensym "C")) :lexicon-data))
  ((list-of symbol) aliases "alternative names for the concept" nil)
  ((list-of (or (cons concept t) standard-object)) references
   "conses and non-relation class instances that contain references to this
    concept (used when merging concepts for newly-discovered aliases)"
   nil)
  ((list-of input-text) definitions "textual definitions of the concept" nil)
  ((list-of input-text)	examples "examples of the concept in context" nil)
  ((list-of relation) out "the list of relations where this is the source" nil)
  ((list-of relation) in "the list of relations where this is the target" nil)
  ((list-of provenance) provenance "" nil)
  )

(defclass-simple relation ()
  "A labeled, directed link between two concepts."
  ((maybe-disj concept) source)
  (symbol label) ; NOTE: may be :inherit, :overlap, :subtype-of, :nominalization... maybe this should be in symbol-types.lisp...
  ((maybe-disj concept) target)
  ((maybe provenance) provenance "" nil)
  )

(defclass-simple role-restr-map ()
  "A semantic role and a restriction of the concepts that may play that role."
  ((list-of sem-role) roles "the semantic role name(s)")
  ((disj-conj concept) restriction
   "the concept restricting what may play that role")
  (boolean optional "t if a player of this role is not required to be present"
           t)
  )

(defclass-simple sem-frame (concept)
  "A semantic frame."
  ((list-of role-restr-map) maps "" nil)
  )

(defclass-simple sem-feats (concept)
  "Legacy semantic features."
  ((feats sem-feat) features "" nil))

(defclass-simple entailments (concept)
  "A list of entailment rules applicable to a concept, as strings."
  ((list-of string) rules "" nil))

(defclass-simple semantics (concept)
  "A grouping of semantic concept parts with no syntactic parts."
  ((maybe-disj sem-frame) sem-frame)
  ((maybe-disj sem-feats) sem-feats)
  ((maybe-disj entailments) entailments)
  )

(defclass-simple syn-sem-map ()
  "(see slot docs)"
  (syn-arg syn-arg "the syntactic argument name")
  ((maybe-disj syn-cat) syn-cat "the POS/phrase tag of the argument")
  ((maybe (maybe-disj symbol)) head-word
   "the head word of the argument (often the preposition when syn-cat is PP)"
   nil)
  ((maybe sem-role) sem-role "the semantic role played by the argument" nil)
  (boolean optional "t if this argument is not required to be present" t)
  )

(defclass-simple syn-sem (concept)
  "A syntactic frame with maps from syntactic arguments to semantic roles"
  ((list-of syn-sem-map) maps "a single map from an argument to a role" nil)
  )

(defclass-simple syn-feats (concept)
  "a simple feature/value map used by the grammar"
  ((feats syn-feat) features "" nil))

(defclass-simple syntax (concept)
  "A grouping of syntactic concept parts with no semantic parts."
  ((maybe-disj syn-sem) syn-sem)
  ((maybe-disj syn-feats) syn-feats)
  (list template-call "the template name and arguments that returned this syntax, if any" nil)
  )

(defclass-simple word ()
  "A word or multiword expression."
  (symbol first-word "the first or only word")
  ((list-of symbol) remaining-words "all the words except the first and the particle" nil)
  ((maybe symbol) particle "the separable part of certain verbs" nil)
  )

(defclass-simple morph-map ()
  "(see slot docs)"
  (syn-feats syn-feats "syntactic features implied by this form of the word" nil)
  (word morphed "the inflected form")
  )

(defclass-simple morph ()
  "A set of inflected forms for a particular word and part of speech."
  (pos pos)
  ((list-of morph-map) maps "" nil)
  ((list-of (cons symbol list)) irregularities
   "Lisp forms describing how this morph is different from the default for this POS and base word."
   nil)
  )

(defun base-word (m)
  "Get the 'morphed' word that is actually the base form (assuming there is one)."
    (declare (type morph m))
  (let ((required-feats
	  `((pos ,(pos m)) 
	    ,(case (pos m)
	      (N '(agr |3S|))
	      (V '(vform base))
	      (otherwise '(form none))
	      ))))
    (morphed
      (find-if
        (lambda (mm)
	  (when (syn-feats mm)
	    (null (first (nth-value 2
		(unify-feats required-feats (features (syn-feats mm))))))))
	(maps m)
	))))

(defclass-simple sense (concept)
  "A concept with all the parts, plus a morph."
  (morph morph)
  ((maybe-disj syntax) syntax)
  ((maybe-disj semantics) semantics)
  )

(defclass-simple lexicon-and-ontology ()
  "A database of words, concepts, and relationships among them."
  ((hash :to concept) concepts
   "Map from concept names to instances of the concept class."
   (make-hash-table :test #'eq)
   )
  ((hash :from (list-of symbol) :to (list-of sense)) senses
   "Map from lists of word symbols to senses of those words. Senses are mapped
    from their first words as well as their full multiwords, and particle verbs
    are also mapped from a version without the particle. Morphed versions of
    all of these are also mapped."
   (make-hash-table :test #'equalp)
   )
  ((hash :from word :to (list-of morph)) morphs
   "Map from base-form word objects to default morphs for those base forms."
   (make-hash-table :test #'equalp)
   )
  )

) ; end (optimize safety)
