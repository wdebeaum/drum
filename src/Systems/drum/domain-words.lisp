(in-package :lxm)

#|
An example of defining a new sense (ONT::PERSON) for a word ("character").  The word does not have to exsit currently in the TRIPS lexicon, or anywhere else. 

:pos indicates the part-of-speech of the word sense.  Some common categories are
   W::n (noun)
   W::v (verb)
   W::adj (adjective)
   W::adv (adverb)

LF-PARENT indicates the ontology type.  This type must exist in the TRIPS ontology.  

TEMPL indicates the syntactic template of the word sense.  It denotes how this word sense can be used in a sentence.  The most common ones are
   countable nouns: COUNT-PRED-TEMPL (e.g., three chairs)
   transitive verbs: AGENT-AFFECTED-XP-TEMPL (e.g., I ate the cookie)
   intransitive verbs: AGENT-TEMPL (e.g., I walked)

The templates are in templates.lisp and noun-templates.lisp in .../src/LexiconManager/Data/templates/

An easy (or easier) way to identify an appropriate template is to look up the existing lexical entry for a word that behaves similarly to the target word.  The templates are shown in the left panel of the online ontology browser.

Note that there can be multiple entries for a word sense, each with a different template, indicating different usages.

|#

#|
(define-words :pos W::n  
:words (
 (w::character
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL COUNT-PRED-TEMPL)
     )
    )
   )
))

|#

(define-words :pos W::n  
:words (
 ((w::El w::Nino)
   (SENSES
    ((LF-PARENT ONT::natural-phenomenon)
     (TEMPL name-templ)
     )
    )
   )
))

(define-words :pos W::n  
:words (
 ((w::El w::Niño)
   (SENSES
    ((LF-PARENT ONT::natural-phenomenon)
     (TEMPL name-templ)
     )
    )
   )
))

(define-words :pos W::n  
:words (
 ((w::La w::Nina)
   (SENSES
    ((LF-PARENT ONT::natural-phenomenon)
     (TEMPL name-templ)
     )
    )
   )
))

(define-words :pos W::n  
:words (
 ((w::La w::Niña)
   (SENSES
    ((LF-PARENT ONT::natural-phenomenon)
     (TEMPL name-templ)
     )
    )
   )
))



