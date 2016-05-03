;;;; generic function declarations (must be loaded before any other code using them, to avoid breaking some Lisps)

(in-package :dsl)

#| old and busted? see query.lisp
(defgeneric dnf (c) (:documentation
  "Apply inheritance relations and merge concepts in order to put a concept in
   disjunctive normal form (an W::OR of W::ANDs, even if it's just one item
   (W::OR (W::AND foo)))."))

(defgeneric merge-concepts (dst src) (:documentation
  "Add the information in concept src to concept dst, destructively. Signal an
   error if the two conflict."))
|#

(defgeneric listify (o) (:documentation
  "Turn an object into a pure nested list representation for printing."))

