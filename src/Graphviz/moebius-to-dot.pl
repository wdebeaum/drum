#!/usr/bin/perl

# moebius-to-dot.pl - convert Moebius system output to an LF-graph-like dot file

my $next_value_id_num = 1;

sub get_next_value_id
{
  return "W" . ($next_value_id_num++);
}

print "digraph LFishGraph {\n  node [shape=none]\n";

my $prev_id = undef;

while (<>) {
  if (/^\d+\.\s+/) {
    # the text of the sentence
    print "  \"" . quotemeta($') . "\"\n";
  } elsif (/ is a kind of /) {
    # ignore
  } elsif (/^\s*[a-z]{1,2}\.\s+_(([a-zA-Z_-]+)\d+)(?:-(\S+?))?(?:_[a-z]+\d+)?\s+\(a (.*?)\)(?:\s+\[\])?\s*$/) {
    # a node
    my ($id, $specifier, $word, $type) = ($1, $2, $3, $4);
    if ($word) {
      print "  \"$id\" [label=\"($specifier (:* \Q$type\E \Q$word\E))\"]\n";
    } else {
      print "  \"$id\" [label=\"($specifier \Q$type\E)\"]\n";
    }
    $prev_id = $id;
  } elsif (/^\s*[a-z]{1,2}\.\s+(\*([a-zA-Z_-]+))\s+\(a (.*?)\)(?:\s+\[\])?\s*$/) {
    # a name
    my ($id, $name, $type) = ($1, $2, $3);
    print "  \"$id\" [label=\"(the \Q$type\E)\"]\n";
    print "  \"$id\" -> \"$name\" [label=\":name-of\"]\n";
    $prev_id = $id;
  } elsif (/^\s*[ivx]+\.\s+(.*?):_([a-zA-Z_-]+\d+)(?:-\S+?)?(?:_[a-z]+\d+)?(?:\s+\[\])?\s*$/) {
    # an edge to a term node
    my ($label, $to_id) = ($1, $2);
    print "  \"$prev_id\" -> \"$to_id\" [label=\"\Q$label\E\"]\n";
  } elsif (/^\s*[ivx]+\.\s+(.*?):(\*.*?)(?:\s+\[\])?\s*$/) {
    # an edge to a name
    my ($label, $to_id) = ($1, $2);
    print "  \"$prev_id\" -> \"$to_id\" [label=\"\Q$label\E\"]\n";
  } elsif (/^\s*[ivx]+\.\s+(.*?):(.*?)(?:\s+\[\])?\s*$/) {
    # an edge to a value
    my ($label, $to_label) = ($1, $2);
    my $to_id = get_next_value_id();
    print "  \"$prev_id\" -> \"$to_id\" [label=\"\Q$label\E\"]\n";
    print "  \"$to_id\" [label=\"\Q$to_label\E\"]\n";
  } else {
    print STDERR "odd looking line, ignoring: $_";
  }
}

print "}\n";
