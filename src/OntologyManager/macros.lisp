
(in-package :ontologymanager)

;; Single macro for LFMapper transform rules
(defmacro define-transform (&rest args)
  `(apply #'add-lftransform ',args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros used in data files
;;

;; An extra helper for interactive file loading
(defmacro define-frame-to-frame-transform (&rest args)
  `(apply #'krontology-define-frame-to-frame-transform ',args))

(defmacro define-frame-to-predicate-transform (&rest args)
  `(apply #'krontology-define-frame-to-predicate-transform ',args))

(defmacro define-slot-transform (&rest args)
  `(apply #'krontology-define-slot-transform ',args))


(defmacro define-predicate-to-predicate-transform (&rest args)
  `(apply #'krontology-define-slot-transform ',args))

(defmacro define-class (&rest args)
  `(apply #'krontology-define-class ',args))

(defmacro define-operator (&rest args)
  `(apply #'krontology-define-operator ',args))

(defmacro define-coerce-operator (&rest args)
  `(apply #'krontology-define-coerce-operator ',args))

(defmacro define-kr-predicate (&rest args)
  `(apply #'krontology-define-predicate ',args))

(defmacro define-type (&rest args)
  `(apply #'lfontology-define-type ',args))

(defmacro define-feature (&rest args)
  `(apply #'lfontology-define-feature ',args))

(defmacro define-feature-arguments (&rest args)
  `(apply #'lfontology-define-feature-arguments ',args))

;; An extra helper for interactive file loading
(defmacro define-feature-rule (&rest args)
  `(apply #'lfontology-add-implied-feature-rule ',args))

(defmacro define-feature-list-type (&rest args)
  `(apply #'lfontology-define-feature-list-type ',args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These are macros which are in objects.txt, and should just be ignored
(defmacro define-scenario (&rest args)
  (declare (ignore args)))

(defmacro define-map (&rest args)
  (declare (ignore args)))

(defmacro define-place (&rest args)
  (declare (ignore args)))

(defmacro define-connection (&rest args)
  (declare (ignore args)))

(defmacro define-connection-status (&rest args)
  (declare (ignore args))) 

(defmacro define-object (&rest args)
    (declare (ignore args)))

;;
;; Used by macro definitions when reading the data files
;;

(defun lfontology-define-type
    (type &key (parent nil) (sem nil) (arguments nil) (coercions nil) (wordnet-sense-keys nil) (comment nil) (definitions nil) (entailments nil))
  (add-linguistic-type type *lf-ontology* :parent parent :semantics sem :arguments arguments :coercions coercions :wordnet-sense-keys wordnet-sense-keys :comment comment :definitions definitions)
  )

(defun lfontology-define-feature (name &key name-only values)
  (add-feature name (ling-ontology-feature-table *lf-ontology*) :values values :nameonly name-only)
  )

(defun lfontology-define-feature-arguments (&key feature arguments)
  (add-feature-arguments (first feature) (second feature) arguments (ling-ontology-feature-table *lf-ontology*))
  )

(defun lfontology-add-implied-feature-rule (name &key feature implies)
  (declare (ignore name))
  (add-implied-rule (first feature) (second feature) 
			       (make-typed-sem implies) 
			       (ling-ontology-feature-table *lf-ontology*)
			       ))

(defun lfontology-define-feature-list-type (name &key features defaults)  
  (add-feature-type name *lf-ontology* :features features :defaults defaults)
  )

(defun krontology-define-class (class &key (isa nil) (slots nil))
  (declaim (special *LF-Ontology*))
  (add-kr-type class *kr-ontology* :isa isa :slots slots :lfontology *lf-ontology*)
  )

(defun krontology-define-predicate (class &key (isa nil) (arguments nil))
  (declaim (special *LF-Ontology*))
  (add-predicate class (kr-ontology-predicate-table *kr-ontology*) :isa isa :arguments arguments) ;; :lfontology *lf-ontology*)
  )

(defun krontology-define-operator (operator &key (isa nil) (arguments nil) (result nil))
  (declaim (special *LF-Ontology*))
  (add-operator operator (kr-ontology-operator-table *kr-ontology*) :isa isa :arguments arguments :result result :coerce nil)
  )

(defun krontology-define-coerce-operator (operator &key (isa nil) (argument nil) (result nil) (lf nil))
  (declaim (special *LF-Ontology*))
  (add-operator operator (kr-ontology-operator-table *kr-ontology*) :isa isa :arguments (list argument) 
			 :result result :coerce t :lf lf)
  )


(defun krontology-define-frame-to-frame-transform (name &key parent preds arguments defaults sem abstract)
  (declaim (special *LF-Ontology*))
  (add-frame-to-frame-transforms name *kr-ontology* *lf-ontology* :parent parent :preds preds :sem sem 
				   :arguments arguments :default defaults :abstract abstract)
  )


(defun krontology-define-frame-to-predicate-transform (name &key preds sem)
  (declaim (special *LF-Ontology*))
  (add-frame-to-predicate-transforms name *kr-ontology* *lf-ontology* :preds preds :sem sem)
  )

(defun krontology-define-slot-transform (name &key pattern sem)
  (declaim (special *LF-Ontology*))
  (add-slot-transforms name *kr-ontology* *lf-ontology* :preds pattern :sem sem)
  )

(defun krontology-define-predicate-to-predicate-transform (name &key pattern sem)
  (declaim (special *LF-Ontology*))
  (add-predicate-to-predicate-transforms name *kr-ontology* *lf-ontology* :preds pattern :sem sem)
  )
