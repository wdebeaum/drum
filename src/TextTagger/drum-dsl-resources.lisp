(in-package :dsl)

(defun get-ncit-files-for-symbol (rv sym)
  (with-slots (base-dir) rv
    (let* ((symname (symbol-name sym))
	   (int (parse-integer (subseq symname 1)))
	   (filename (format nil "~5,'0d" (floor int 10))))
      (list (make-pathname :defaults base-dir :name filename :type "lisp")))))

(defresource (NCIT NCI-Thesaurus) (
  :version "16.05e"
  :base-dir (pathname-directory #!TRIPS"src;TextTagger;drum-dsl;NCIT;")
  :get-files-for-symbol #'get-ncit-files-for-symbol
  ))

(defun get-obo-files-for-symbol (rv sym)
  (with-slots (base-dir) rv
    (let* ((symname (symbol-name sym))
	   (int (parse-integer symname))
	   (filename (format nil "~6,'0d" (floor int 10))))
      (list (make-pathname :defaults base-dir :name filename :type "lisp")))))

;; wrapper for defresource with defaults specific to OBO ontologies
(defmacro def-obo-resource (names &rest versions)
  `(defresource ,names
    ,@(mapcar
      (lambda (v)
        `(
	  :base-dir ',(pathname-directory (trips::make-trips-pathname (format nil "src;TextTagger;drum-dsl;~a;foo.lisp" (first names))))
	  :get-files-for-symbol #'get-obo-files-for-symbol
	  ,@v
	  ))
      (if (null versions) '(nil) versions))))

;; bioentities is an OBO file, but it uses non-numeric IDs, so instead we go by
;; the first character (which happens to always match /\w/)
(defun get-bioentities-files-for-symbol (rv sym)
  (with-slots (base-dir) rv
    (let* ((symname (symbol-name sym))
	   (filename (subseq symname 0 1)))
      (list (make-pathname :defaults base-dir :name filename :type "lisp")))))

(defresource (BE BioEntities) (
  :base-dir (pathname-directory #!TRIPS"src;TextTagger;drum-dsl;BE;")
  :get-files-for-symbol #'get-bioentities-files-for-symbol
  ))

(defresource (BEL Biological-Expression-Language)) ; also used in bioentities

(def-obo-resource (BTO brenda-tissue-ontology) (:version "119"))
(def-obo-resource (CHEBI chemical-entities-of-biological-interest))
(def-obo-resource (CO cell-ontology)) ; NOTE: can't use CL because that's Common Lisp
(def-obo-resource (GO gene-ontology) (:version "releases/2014-09-02"))
(def-obo-resource (MI PSI-MI molecular-interaction) (:version "2.5"))
(def-obo-resource (MOD PSI-MOD protein-modification))
(def-obo-resource (SO sequence-ontology))
(def-obo-resource (EFO experimental-factor-ontology))
(def-obo-resource (PR PRO protein-ontology))
(def-obo-resource (UO units-of-measurement-ontology))
(def-obo-resource (Orphanet))
(def-obo-resource (HP human-phenotype))
;; these don't have DSL files associated with them because the original
;; resources don't define type hierarchies, but we define them anyway in order
;; to get the symbol packages defined
(defresource (HGNC HUGO HUGO-gene-nomenclature-comittee human-genome-organization))
(defresource (UP UniProt))
(defresource (XFAM))
(defresource (MESH))
(defresource (CVCL Cellosaurus))
(defresource (FA neXtProt-Family))
;; this one is really big so we don't get it
(defresource (PC PubChem))

(require-resource-version :ont)
(require-dsl-file #!TRIPS"src;TextTagger;drum-mappings.lisp")

