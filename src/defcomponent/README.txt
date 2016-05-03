

Time-stamp: <Wed Apr 23 09:50:59 EDT 2008 ferguson>

23 Apr 2008:

Some day I should really change the message handlers code. Something
like the option (1) below would make more sense, supported by a
function like the following:

(defun invoke-if-exists (msg name &rest args)
  "Lookup NAME in current component's package. If it exists and is FBOUNDP,
then invoke it on MSG (for replying, etc.) and ARGS. Else do nothing and
return NIL."
  (let ((fname (intern name (slot-value *component* 'package))))
    (when (fboundp fname)
      (apply fname msg args))))

I started to put this in, but decided it wasn't the most important
thing to do, and anyway that I shouldn't rush into it. But really,
agents should have to worry too much about messages. Would have to see
how this interacted with defcomponent-handler also, since we need to
be able to register method for handling messages *other* than the
built-in ones. Maybe you just write the function and it gets called if
it exists? But what if you want one function to handler multiple
messages and do its own `parsing' (eg., parser currently). Then
defcomponent-handler could override the standard behavior for certain
message types.

------------------------------------------------------------------------
9 Feb 2004:

Thoughts on defmodule, a library for defining TRIPS modules in Lisp

We need to support the following functionality:

 - compile-time information for compiling the component

 - load-time information for loading the component

 - run-time information for running the component

 - indication of message handlers

The first three replace the defsys information presently. The fourth
will rationalize the messages.lisp file that otherwise gets copied
around.

For the message handlers, there are two possibilities:

  1. Create message handler name from parameters of the message (eg.,
     RECEIVE-<VERB>-<SUBVERB>), then call it if it exists, passing it
     the message. This is relatively fast to do (could even be cached,
     I suppose). It requires the message be reparsed by the handler
     though.

  2. Use some kind of pattern matching, perhaps based on the code in
     the lisp facilitator, to dispatch messages. Then the handlers
     would be specified by giving a pattern and a function to call. It
     would be good to have a way to refer to bits of the pattern and
     get them passed as parameters to the function, otherwise they'll
     just have to reparsed.
