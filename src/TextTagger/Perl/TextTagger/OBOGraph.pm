# OBOGraph.pm
#
# Time-stamp: <Sun Mar 19 22:33:20 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 28 Aug 2015
#

#----------------------------------------------------------------
# Description:
# This package implements a class providing various tools for
# reasoning with OBO ontologies. It encodes the hierarchy as a
# doubly linked hash, which can provide enormous speedups for
# certain operations over large ontologies.
# Note: The construction of the underlying graph is done at
# initialization. If, for some reason, this is removed, many
# operations would still work (by defaulting to linear search
# over the list of terms), but at considerable cost.
 
#----------------------------------------------------------------
# History:
# 2015/08/28 lgalescu
# - Created (as OBOTools.pm).
# 2017/03/16 lgalescu
# - Changed to an OO implementation; renamed to OBOGraph.pm.
# 2017/03/19 lgalescu
# - Added to Trips/TextTagger component (changed package name).

require 5.10.1;

###----------------------------------------------------------------
## Package summary
#

package TextTagger::OBOGraph;


###----------------------------------------------------------------
## Modules & Features
#

use TextTagger::OBOParser qw(read_obo);

use Data::Dumper;
use Set::Scalar;
use List::Util qw(first);

###----------------------------------------------------------------
## Globals
#

###----------------------------------------------------------------
## Subroutines
#

# OBO object
sub new {
  my $class = shift;
  my $self = {
	      file => shift,
	      header => undef,
	      stanzas => undef,
	      graph => undef,
	     };
  bless $self, $class;

  $self->read_obo_file();
  $self->make_id_graph();

  return $self;
}

# read obo from file into a list representation (first item is the header
# and the rest are stanzas)
sub read_obo_file {
  my $self = shift;
  my $oboFile = $self->{file};
  unless (-f $oboFile) {
    return "Can't find OBO file $oboFile\n" ;
    return;
  }
  my $fh;
  open $fh, "<$oboFile" 
    or die "Can't open OBO file $oboFile: $!";
  my @result = read_obo($fh);
  $self->{header} = shift @result;
  $self->{stanzas} = \@result;
  close $fh;
}

# creates a double-linked graph structure over the OBO terms
# this structure is useful for search, since each node (hashed by 
# its ID) has links to both parents and children
sub make_id_graph {
  my $self = shift;

  my $result;
  my $n = 0;
  foreach my $term ($self->terms) {
    $n++;
    my $sid = term_id($term);
    $result->{$sid}{term} = $term;
    next unless exists $term->{is_a};
    $result->{$sid}{is_a} = $term->{is_a};
    foreach my $pid (@{$term->{is_a}}) {
      push @{$result->{$pid}{superclass_of}}, $sid;
    }
  }
  warn "Processed $n terms\n";
  
  $self->{graph} = $result;
  
  1; # we don't want to return the whole graph
}

### getters for various structural elements

# returns graph
sub graph {
  my $self = shift;
  return $self->{graph};
}

# checks if an id is in the graph
# returns undef if no graph 
sub in_graph {
  my ($self, $x) = @_;
  return undef unless $self->{graph};
  return exists $self->{graph}{$x};
}

# returns list of non-obsolete terms
sub terms {
  my $self = shift;
  grep { ($_->{stanza_name} eq 'Term') and
	 (!exists($_->{is_obsolete}) or ($_->{is_obsolete} eq 'false')) }
    @{ $self->{stanzas} };
}

# returns list of all term IDs
sub ids {
  my $self = shift;
  return keys %{ $self->graph } if defined $self->graph;
  return map { term_id($_) } $self->terms;
}

# returns list of parents of a term, given the term's id
sub parents {
  my ($self, $x) = @_;
  unless ($self->in_graph($x)) {
    warn "Error: parents() requires the graph -- none was found!\n";
    return undef;
  }
  return () unless exists $self->{graph}{$x}{is_a};
  return @{ $self->{graph}{$x}{is_a} };
}

# returns list of children of a term, given the term's id
sub children {
  my ($self, $x) = @_;
  unless ($self->in_graph($x)) {
    warn "Error: children() requires the graph -- none was found!\n";
    return undef;
  }
  return () unless exists $self->{graph}{$x}{superclass_of};
  return @{ $self->{graph}{$x}{superclass_of} };
}

# checks if a node is a direct child of another one
sub is_a {
  my ($self, $x, $y) = @_;
  return undef unless $self->in_graph($x) and $self->in_graph($y);
  return 0+(grep {$y eq $_} $self->parents($x));
}

# checks if node with id x is equal to or has as ancestor node with id y
sub is_descendant_of {
  my ($self, $x, $y) = @_;
  return undef unless $self->in_graph($x) and $self->in_graph($y);
  return 1 if $x eq $y;
  foreach my $p ($self->parents($x)) {
    return 1 if $self->is_descendant_of($p, $y);
  }
  return 0;  
}

# returns the list of id's for all descendants of a node (not including self)
sub descendants {
  my ($self, $x) = @_;

  return undef unless $self->in_graph($x);
  my @children = $self->children($x); 
  my $result = Set::Scalar->new(@children);
  foreach my $c (@children) {
    $result = $result->union(Set::Scalar->new($self->descendants($c)));
  }
  return $result->members;
}

# returns the list of id's for all ancestors of a node (not including self)
sub ancestors {
  my ($self, $x) = @_;

  return undef unless $self->in_graph($x);
  my @parents = $self->parents($x);
  my $result = Set::Scalar->new(@parents);
  foreach my $p (@parents) {
    $result = $result->union(Set::Scalar->new($self->ancestors($p)));
  }
  return $result->members;
}

# returns term with given id
sub get_term {
  my ($self, $x) = @_;
  return $self->{graph}{$x}{term}
    if defined($self->graph) and $self->in_graph($x);
  return first { term_id($_) eq $x } $self->terms;
}

# returns value of a tag for the term with given id
sub get_term_tag {
  my ($self, $x, $tag) = @_;
  my $s = $self->get_term($x);
  return exists($s->{$tag}) ? $s->{$tag} : undef;
}

# returns value of name tag for the term with given id
sub get_term_name {
  my ($self, $x) = @_;
  return term_name($self->get_term($x));
}

# returns list of synonyms for the term with given id
# options can specify which scope is intended; if given, returns only
# synonyms of the specified scope(s); if absent, returns all synonyms.
sub get_term_synonyms {
  my ($self, $x, %opts) = @_;
  my $syns = $self->get_term_tag($x, 'synonym') // [];
  my @result;
  foreach my $syn (@$syns) {
    push @result, $syn->{synonym}
      if !%opts or $opts{$syn->{scope}};
  }
  return @result;
}

# finds nodes by name or synonym (equality is case-insensitive)
# options: synonyms => bool
sub find_by_name {
  my ($self, $name, %opts) = @_;

  # lowercase $name
  $name = lc($name);
  my $result = Set::Scalar->new();
  foreach my $id ($self->ids) {
    my $term_name = $self->get_term_name($id);
    if ($name eq lc($term_name)) {
      $result->insert($id);
      next;
    }
    if ($opts{synonyms}) {
      my @syns = $self->get_term_synonyms($id, 'EXACT' => 1);
      my $syns_set = Set::Scalar->new( map { lc($_) } @syns);
      $result->insert($id) if ($syns_set->contains($name));
    }
  }
  return $result->members;
}

# find the least common ancestors of two terms, identified by their IDs
sub lca {
  my ($self, $x, $y) = @_;

  # get all ancestors
  my $ancestors = Set::Scalar->new($self->common_ancestors($x, $y));

  # look for ancestors with no children among themselves
  my @result;
  foreach my $a (@$ancestors) {
    push @result, $a
      if $ancestors->is_disjoint(Set::Scalar->new($self->children($a)));
  }
  
  return @result;
}

# returns all common ancestors for a set of ids (inclusive)
sub common_ancestors {
  my ($self, @ids) = @_;

  return () unless scalar(@ids);

  my $result = Set::Scalar->new($ids[0], $self->ancestors($ids[0]));
  shift @ids;
  foreach my $y (@ids) {
    my $yAncestors = Set::Scalar->new($y, $self->ancestors($y));
    $result = $result->intersection($yAncestors);
    return () if $result->is_empty;
  }
  return $result->members;
}

# returns all common descendants for a set of ids (inclusive)
sub common_descendants {
  my ($self, @ids) = @_;

  return () unless scalar(@ids);

  my $result = Set::Scalar->new($ids[0], $self->descendants($ids[0]));
  shift @ids;
  foreach my $y (@ids) {
    my $yAncestors = Set::Scalar->new($y, $self->descendants($y));
    $result = $result->intersection($yAncestors);
    return () if $result->is_empty;
  }
  return $result->members;
}

### Two utility function dealing with an inconsistency b/w OBO, which
### requires id and name tags to be unique, and the perl representation
### of stanzas in TextTagger::OBOParser, which puts the values of these
### required tags inside arrays

sub term_id {
  return $_[0]->{id}[0];
}
sub term_name {
  return $_[0]->{name}[0];
}

1;
