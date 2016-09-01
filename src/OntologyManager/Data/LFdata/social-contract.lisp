;;; Rik -- August 2015

(in-package :om)

;; Working on the social-contract hierarchy,
;; I ended up with `essential-contract` and `nonessential-contract` under
;; `social-imperative`, but also `external/internal-authority` under `governing-principle`.
;; However, it seems that we can do away with those nodes if we introduce a feature or
;; something like `authority-source` which can have values such as `internal, external`
;; and even `religious`.

;;; ont::social-contract contains a set of concepts that refer to
;;; interactions between individuals and society

(define-type ont::social-contract
    :arguments ((:OPTIONAL ONT::FIGURE (f::situation)))
    :parent ont::mental-construction
)


;;; ont::social-imperative represents concepts which
;;; apply to general contract types and/or divisions.
;;; Roughly, this contains the concepts which are necessary
;;; to describe contracts and
;;;
;;; for actual contracts or concepts refering to ideologies
;;; see ont::governing-principle

(define-type ont::social-imperative
  :parent ont::social-contract
  :wordnet-sense-keys ("freedom%1:26:01" "slavery%1:26:00")
)

(define-type ont::social-judgement
  :parent ont::social-imperative
)

;; Maybe there is too much overlap between children in governing-principle and
;; judgement-val.  The goal is to extricate the actual judgement from the expression
;; which represents it, but that may be too lofty a goal.

(define-type ont::judgement-val
  :parent ont::social-judgement
  :wordnet-sense-keys ("justice%1:07:00" "injustice%1:07:00" "moral%3:00:00::" "immoral%3:00:00::" "immoral%3:00:00:wrong:01")
)

(define-type ont::essential-contract
  :parent ont::social-imperative
  :wordnet-sense-keys ("right%1:07:00" "absolute%1:09:00" "absolute%3:00:00:inalienable:00")
)


;; While the sense of freedom%1:26:00 falls under nonessential-contract, its children
;; should fall under a child of governing-principle
;; Wordnet doesn't seem to have the correct sense of oppression.
;; maybe status should fall under the part of the tree with responsibility

(define-type ont::nonessential-contract
  :parent ont::social-imperative
  :wordnet-sense-keys ("freedom%1:26:00" "privilege%1:07:02" "status%1:26:00::")
)

;; There could be a distinction between a guideline and a motivation --
;; Specifically, the entity that this principle is relative to.
;; In particular for something like ethic%1:10:00
;; external-authority and internal-authority could be merged back into governing-principle

(define-type ont::governing-principle
  :parent ont::social-contract
  :wordnet-sense-keys ("morality%1:07:00" "morality%1:16:00" "ethic%1:09:00" "ethic%1:10:00")
)

(define-type ont::external-authority
  :parent ont::governing-principle
  :wordnet-sense-keys ("law%1:14:00" "treaty%1:10:00")
)

(define-type ont::internal-authority
  :parent ont::governing-principle
  :wordnet-sense-keys ("conscience%1:16:00")
)

;; This falls directly under governing-principle rather than external-authority
;; since we don't want to prescribe the authority.  Instead, a criminal-activity
;; gets its status with respect to an authority

(define-type ont::criminal-activity
  :parent ont::governing-principle
  :wordnet-sense-keys ("transgression%1:04:00")
)

(define-type ont::responsibility
  :parent ont::social-contract
  :wordnet-sense-keys ("duty%1:04:00" "duty%1:04:02" "obligation%1:04:00" "responsibility%1:04:00")
)
