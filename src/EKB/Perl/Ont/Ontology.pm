#!/usr/bin/perl

# Ontology.pm
#
# Time-stamp: <Thu Sep  3 16:23:50 CDT 2020 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 27 May 2016
#

#----------------------------------------------------------------
# Description:
# This class implements a simple ontology.

#----------------------------------------------------------------
# History:
# 2016/05/27 v1.0	lgalescu
# - Created, from other scripts.
# - Currently this can represent a simple, single-inheritance ontology.
# 2016/05/27 v2.0	lgalescu
# - Modified to allow multiple inheritance.
# - N.B.: constructor has changed!
# 2017/02/06 v3.0	lgalescu
# 2020/09/03 v4.0	lgalescu
# - added more functionality: lcp, ancestors, comp

#----------------------------------------------------------------
# Usage:
# 

#----------------------------------------------------------------
# TODO:
# - maybe add partOf relations?


package Ont::Ontology;

$VERSION = '3.0';

use strict 'vars';
use warnings;

use List::Util qw(any uniq);

use util::Log;

sub new {
  my $class = shift;
  my $self = {};
  bless $self, $class;
  $self->_init(@_);
  return $self;
}

sub _init {
  my $self = shift;
  my @data = @_;
  while (@data) {
    my $c = shift @data;
    my $p = shift @data;
    unless ($p) {
      FATAL "Incorrect specification: No parent for class $c";
    }
    $self->add($c, $p);
  }
}

# list of types
sub classes {
  my $self = shift;

  return keys %{$self};
}

# check if type is in ontology
sub has {
  my $self = shift;
  my ($a) = @_;
  ERROR "%s: %s not found in ontology", ref($self), $a
    unless $a eq "1" or exists $self->{$a};
  return exists $self->{$a};
}

# add type to ontology
sub add {
  my $self = shift;
  my ($c, $p) = @_;
  $self->{$c}{$p} = 1;
  unless ($self->has($p)) {
    $self->{$p} = undef;
  }
}

# check if a IS-A b (recursive)
sub is_a {
  my $self = shift;
  my ($a, $b) = @_;

  return 0 unless $self->has($a);
  return 1 if $a eq $b;
  return any { $self->is_a($_, $b) } $self->parents($a);
}

# comparison operator
sub comp {
  my $self = shift;
  my ($a, $b) = @_;
  return 0 if $a eq $b;
  return scalar($self->ancestors($b)) <=> scalar($self->ancestors($a));
}

# parents
sub parents {
  my $self = shift;
  my ($a) = @_;
  return () unless $self->has($a);
  return () unless defined $self->{$a}; # skip dummy root
  #DEBUG 0, "parents($a): (%s)", join(", ", keys %{$self->{$a}});
  return keys %{$self->{$a}};
}

# ancestors along the longest path to (a) root, including self
sub ancestors {
  my $self = shift;
  my ($a) = @_;
  return () unless $self->has($a);
  return () unless defined $self->{$a}; # skip dummy root
  my @ancestors = uniq map { $self->ancestors($_) } $self->parents($a);  
  #DEBUG 0, "ancestors(%s)=(%s)", $a, join(", ", @ancestors);
  return $a, @ancestors;
}

# lowest common parent (recursive)
sub lcp {
  my $self = shift;
  my @types = @_;
  #WARN "lcp(%s)", join(",", @types);
  if (scalar(@types) < 1) {
    ERROR "insufficient number of arguments";
    return;
  }
  # bad type? got to the top with no match?
  return unless $self->has($types[0]);
  # found it?
  return $types[0] if scalar(@types) == 1; 
  # reduce
  my $a = shift @types;
  my $b = shift @types;
  if ($self->is_a($a, $b)) {
    return $self->lcp($b, @types);
  }
  if ($self->is_a($b, $a)) {
    return $self->lcp($a, @types);
  }
  if (@types) {
    return $self->lcp($self->lcp($self->parents($a), $b), @types);
  }
  return $self->lcp($self->parents($a), $b);
}

1;
