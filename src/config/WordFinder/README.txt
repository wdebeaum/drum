
config/WordFinder/README.txt

George Ferguson, ferguson@cs.rochester.edu, 20 Jul 2005
Time-stamp: <Wed Jul 20 14:09:33 EDT 2005 ferguson>

Configure setup for WordFinder to be able to locate lexical
resources.

Running "./configure" generates "defs.lisp" with settings for some
lisp globals (defvars) for use by WordFinder:

  *use-wordfinder*
	Initial setting for whether to use WordFinder

  *wordnet-basepath*
  *comlex-filename*
  *cmu-names-filename*
	Paths to the directories or files containing the various
        resources. Exactly which files are used and how they are
        arranged depends on the WordFinder code, and this configure
        setup does not try to verify that the right files are there.

No installation is needed with this arrangement, but also the paths
are hardcoded once configure is run. I will arrange the WordFinder
code to always look in TRIPS_BASE/etc/WordFinder first, to allow an
override and to allow us to symlink or otherwise bundle the resources
with a TRIPS distribution.
 
Configure options:

  --enable-wordfinder
	The default arrangement: wordfinder is enabled by default and
	we look for all the resources

  --disable-wordfinder
	Makes initial value of *use-wordfinder* be nil, and prevents
	searching for paths to resources

  --with-wordnet=PATH|no
  --with-comlex=PATH|no
  --with-cmu-names=PATH|no
	If "no", then disable use of appropriate resource (set path to
	nil in "defs.lisp") and don't bother looking for it now, else
	use given PATH (which we check and will warn about if not
	found)
