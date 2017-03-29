# IndexCards.pm
#
# Time-stamp: <Tue Jan 17 22:23:11 CST 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 15 Jun 2016
#

#----------------------------------------------------------------
# Description:
# 

#----------------------------------------------------------------
# History:
# 2016/06/15 v0.0	lgalescu
# - Started. Importing stuff from old script(s).

#----------------------------------------------------------------
# Usage:
# 

package EKB::Reasoner::IndexCards;

$VERSION = '0.0';

use strict 'vars';
use warnings;

use Data::Dumper;
use XML::LibXML;
use List::Util qw(all any first min max uniq);

use EKB;
use EKB::Match;
use Ont::BioEntities;
use Ont::BioEvents;
use EKB::Reasoner::Rule;
use EKB::Reasoner;
use Local::Log;

our @ISA = qw(EKB::Reasoner);

my @rules;

my $ont_events = Ont::BioEvents->new();
my $ont_bioents = Ont::BioEntities->new();

sub new {
  my $class = shift;
  my $self = {
	      ekb => shift,
	      rules => [],
	      # log for which rules applies to what elements
	      log => {},
	     };
  bless $self, $class;
  $self->_init();
  return $self;
}

sub _init {
  my $self = shift;
  $self->SUPER::_init();

  push @{ $self->{rules} },
    map { EKB::Reasoner::Rule->new($self, $_) } @rules;
    
}

## Rules

@rules =
  (
   # < fixEventArguments: obsolete
   # < 
   {
   }
  );

1;
