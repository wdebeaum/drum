#!/usr/bin/perl

# Ontology.pm
#
# Time-stamp: <Fri Feb  3 15:02:48 CST 2017 lgalescu>
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

#----------------------------------------------------------------
# Usage:
# 

#----------------------------------------------------------------
# TODO:
# - maybe add partOf relations?


package Ont::Ontology;

$VERSION = '2.0';

use strict 'vars';
use warnings;

use List::Util qw(any);

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

sub add {
  my $self = shift;
  my ($c, $p) = @_;
  $self->{$c}{$p} = 1;
  unless ($self->has($p)) {
    $self->{$p} = undef;
  }
}

sub is_a {
    my $self = shift;
    my ($a, $b) = @_;

    return 0 unless $self->has($a);
    return 1 if $a eq $b;
    return any { $self->is_a($_, $b) } keys %{$self->{$a}};
}

sub has {
    my $self = shift;
    my ($a) = @_;

    return exists $self->{$a};
}

sub classes {
    my $self = shift;

    return keys %{$self};
}

1;
