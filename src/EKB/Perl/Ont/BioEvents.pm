#!/usr/bin/perl

# BioEvents.pm
#
# Time-stamp: <Tue Jan 17 22:21:29 CST 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 27 May 2016
#

#----------------------------------------------------------------
# Description:
# 

#----------------------------------------------------------------
# History:
# 2016/05/27 v1.0	lgalescu
# - Created, from other scripts.

#----------------------------------------------------------------
# Usage:
# 

package Ont::BioEvents;

$VERSION = '1.0';

use strict 'vars';
use warnings;

use Ont::Ontology;

our @ISA = qw(Ont::Ontology);


# TRIPS event ontology fragment
my @data = ( "ONT::PTM" => 1,
	     "ONT::ACETYLATION" => "ONT::PTM",
	     "ONT::FARNESYLATION" => "ONT::PTM",
	     "ONT::GLYCOSYLATION" => "ONT::PTM",
	     "ONT::HYDROXYLATION" => "ONT::PTM",
	     "ONT::METHYLATION" => "ONT::PTM",
	     "ONT::PHOSPHORYLATION" => "ONT::PTM",
	     "ONT::RIBOSYLATION" => "ONT::PTM",
	     "ONT::SUMOYLATION" => "ONT::PTM",
	     "ONT::UBIQUITINATION" => "ONT::PTM",
	     "ONT::MODULATE" => 1,
	     "ONT::INCREASE" => 1,
	     "ONT::DECREASE" => 1,
	     "ONT::STIMULATE" => "ONT::MODULATE",
	     "ONT::INHIBIT" => "ONT::MODULATE",
	     "ONT::PRODUCE" => "ONT::INCREASE",
	     "ONT::CONSUME" => "ONT::DECREASE",
	     "ONT::ACTIVATE" => "ONT::STIMULATE",
	     "ONT::DEACTIVATE" => "ONT::INHIBIT",
	     "ONT::CATALYZE" => "ONT::STIMULATE",
	     "ONT::BIND" => 1,
	     "ONT::BREAK" => 1,
	     "ONT::EXPRESS" => 1,
	     "ONT::TRANSCRIBE" => 1,	  # FIXME: we had it as => EXPRESS
	     "ONT::TRANSLATE" => 1,	  # FIXME: we had it as => EXPRESS
	     "ONT::TRANSLOCATE" => 1,
	     "ONT::HYDROLYZE" => 1,
	     "ONT::PPEXPT" => 1,
	     "ONT::SIGNALING" => 1,
	     # uncertain ont status
	     "ONT::ACTIVITY" => 1,
	     "ONT::NO-CHANGE" => 1,
	     "ONT::TRANSFORM" => 1,
	     "ONT::INTERACT" => 1
	   );

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@data);
    bless $self, $class;
}

1;
