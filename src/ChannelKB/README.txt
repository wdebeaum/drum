
Time-stamp: <Mon Feb  9 20:21:26 EST 2004 ferguson>

Monitors channel status:

- Listens for events indicating a change in channel state:

    (tell
     :content (channel-event
     	       :content (channel-opened :channel <chan> :id <id>)))

    (tell
     :content (channel-event
     	       :content (channel-closed :channel <chan>)))

- Keeps track of channel state and can answer queries of the following
  form:

    (ask :content (channel-query :content <query>))

- Replies with:

    (tell
     :content (answer
     	       :content <answer>
	       :bindings <bindings>
	       :query <query>))

  where <answer> is one instance of the <query> that can be proven,
  <bindings> is a list of (<variable> . <value>) bindings, and
  <query> is the original query.

- The <answer> and <bindings> will be NIL if the query cannot be
  proven.

- If ASK-ALL is used instead of ASK, the <answer> is a list of
  instances and the <bindings> is a list of binding lists.

- Possible queries include:

    (channel-status <channel> <status>)

  and other predicates defined in "db.lisp".

- Queries can include variables, indicated by a symbol whose name
  starts with a question mark.

- Also reports changes in user communication status (communicado or
  incommunicado), by monitoring the events involving LOCATION-OF from
  physical awareness, and reporting changes by means of:

    (tell
     :content (comm-event
     	       :content (comm-status :who <user> :what communicado
	                             :channel <channel>)))

    (tell
     :content (comm-event
     	       :content (comm-status :who <user> :what incommunicado)))
