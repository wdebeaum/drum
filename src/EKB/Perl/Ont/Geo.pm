#!/usr/bin/perl

# Geo.pm
#
# Time-stamp: <Mon Jun 10 10:54:51 MST 2019 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 27 May 2016
#

#----------------------------------------------------------------
# Description:
# Ontology of geographical entities.

#----------------------------------------------------------------
# History:
# 2019/06/10 v1.0	lgalescu
# - Created.

#----------------------------------------------------------------
# Usage:
# 

package Ont::Geo;

$VERSION = '1.0';

use strict 'vars';
use warnings;

use Ont::Ontology;

our @ISA = qw(Ont::Ontology);


# TRIPS ontology for bio entities, adapted to some extent
my @data = (
	    'ONT::GEOGRAPHIC-REGION' => 1,
	    'ONT::POLITICAL-REGION' => 'ONT::GEOGRAPHIC-REGION',
	    'ONT::COUNTRY' => 'ONT::POLITICAL-REGION',
	    'ONT::STATE' => 'ONT::POLITICAL-REGION',
	    'ONT::DISTRICT' => 'ONT::POLITICAL-REGION',
	    'ONT::COUNTY' => 'ONT::POLITICAL-REGION',
	    'ONT::CITY' => 'ONT::POLITICAL-REGION',
	   );

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@data);
    bless $self, $class;
}

1;
