WordFinder is used to augment the lexicon, by looking up unknown words
at parse time in various external resources.

To enable WordFinder, you first have to install the resources on your machine.

It will then look for the various lexical resources (wordnet, comlex) 
in some "standard" places.  If you are at URCS, the resources are available
on the fileserver at
  /p/nl/wordnet/WordNet-3.0/dict
  /p/nl/comlex/comlex_synt_1.1.1

If you are not at URCS, WordNet 3.0 can be downloaded 
from http://wordnet.princeton.edu/
Comlex is not freely available (distributed through LDC, but costs more than $1200).
If you don't have access to it, the system can still run without it
(and according to James, its performance shouldn't be affected that much).

If you're running on Linux at URCS, you don't need to do anything: the
resources will be found in their locations listed above.

If you have access to the URCS fileserver, just copy the directories shown above.
If you put them in the following locations, configure will find them
automagically:
  wordnet: /usr/local/share/wordnet/WordNet-3.0/dict
  comlex: /usr/local/share/comlex/comlex_synt_1.1.1

Otherwise, you can put these wherever you want, but you'll have to run
configure with the options described in
in src/config/WordFinder/README.txt 


For more complete WordFinder documentation, see src/WordFinder/docs/README.xhtml
