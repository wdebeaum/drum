
File: README-defsystem.txt
Creator: George Ferguson
Created: Wed Oct 28 11:16:38 2009
Time-stamp: <Fri Jan 15 11:39:57 EST 2010 ferguson>

15 Jan 2009:

- Updated to 3.6i since I thought it would fix a bunch of compiler
  warnings for deprecated EVAL-WHEN constructs, but it didn't, so
  I just changed them by hand
  - Of course, since defsystem is supposed to work on systems that
    predate CLtL2, this could be considered a regression...


28 Oct 2009:

- Attempt to wrap the old defsystem3.5i code in a CLOS wrapper to allow
  :before and :after methods to alter is behavior on a per-module basis.
  - I previously tried upgrading to the "experimental" (since 2006)
    defsystem4, but it had many issues. I resolved most of them
    eventually, but decided it probably wasn't worth messing with if this
    works. So let's see...

