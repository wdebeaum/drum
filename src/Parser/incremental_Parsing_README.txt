Scott C. Stoness
Mar 30, 2005

Incremental Parsing Changes to the Parser

Modifications made are listed below.  

(Same Version) 

indicates that the same version of the code works for both incremental
and non-incremental versions of the parser, and the changes made to
make it incremental should be completely seamless and unnoticed by a 
non-incremental parser (e.g. no flags were used, nothing tricky, just
an extension to the general functioning of the program that depends on 
other code behaving slightly differently under incremental conditions).

(Flagged)

The variable *Incremental-Parsing-Enabled* will determine which bit of 
code should be run.

=============================
Core-Parser/onlineParser.lisp
=============================

ADD-ENTRY-TO-CHART  (Same Version)

Changed add-entry-to-chart into two functions.  Shouldn't change
behaviour for non-incremental code.  Isn't variable-guarded; depends
for behaviour on whether or not procedural-filter returns an entry or
a nil.  If it returns an entry, we add it, as before.  If it returns
nil, we ignore it, as before.  The incremental code in the procedural
filter is responsible for calling the add-entry-to-chart-continued
continuation should the entry need to be added later.  

===========================
Core-Parser/attachment.lisp
===========================

PROCEDURAL-FILTER (Same Version)

Changed it so that if an attachment returns 'DELAY-ENTRY, it will
avoid adding the constituent to the chart.  The attachment itself, 
of course, will need to call add-entry-to-chart-continued.  Shouldn't
change behaviour for non-incremental code, 


==========================
Trips-Parser/messages.lisp
==========================

SEND-WITH-CONTINUATION (Same Version)

Code changed slightly, but should work in the same way as before.

SEND-WITH-SYNCHRONOUS-CONTINUATION (Same Version)

Allows syncrhonous continuations to be piggy-backed onto the 
existing continuation system, so that some continuations will
have the side-effect of stopping the parser.

RECEIVE-REPLY (Same Version)

Modified receive-reply to handle synchronous continuations;  should
work exactly like before so long as user is only calling 
send-with-continuation.

GET-NEXT-EVENT (Flagged)

*Incremental-Parsing-Enabled* flag determines whether this calls
get-next-event-nonincremental or get-next-event-incremental.

get-next-event-nonincremental is the exact old code, while 
get-next-event-incremental is the new, incremental version of it.

Note that it is probably a good idea to harmonize the processing so 
that the main messages.lisp loop handles the parser control for all 
operations at some point, but for now, this is much simpler, and 
doesn't require changes to the existing code-base.


=============================
Trips-Parser/attachments.lisp
=============================

BESTFIRSTTEST (Flagged)

The *Incremental-Parsing-Enabled* flag determines how the 
parser is stopped if the right number of spanning utterances have 
been found.


=============================
Trips-Parser/tripsParser.lisp
=============================

STARTNEWUTTERANCE (Same Version)

Sets *utterance-parsing-complete* and
*end-of-utterance-processing-performed* to nil; shouldn't affect
non-incremental parsing, as those variables are ignored in that mode.


PI (Different Version)

Added PI as the incremental version of P, so that we can do keyboard
tests directly in lisp.

PARSE (Flagged)

The *incremental-parse-enabled* flag makes sure that continue-BU-parse
is not called, nor is (get-parse) when the end of the sentence is seen.















========================
ISSUES TO CONSIDER LATER
========================

Is 'DELAY-ENTRY the best method to guard the procedural filter code?

Not sure that (trace-entry) does the right thing in the
Add-Entry-To-Chart code, since it traces whatever is sent to the
function, whether it is actually added or not...

Messages.lisp should probably control all Parser functioning, rather
than the ignore messages loop continue-BU-parse; it should be possible
to harmonize this control, but at the moment it is simpler not to.

UTTLIST has to be used to capture all utterances, because it is quite
possible incrementally to find an UTTERANCE that in fact is spanning, 
before the END message comes in.

