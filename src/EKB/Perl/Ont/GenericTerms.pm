#!/usr/bin/perl

# GenericTerms.pm
#
# Time-stamp: <Fri May 19 22:21:04 CST 2017 rcarff>
#
# Author: Roger Carff <rcarff@ihmc.us>, 27 May 2016
#

#----------------------------------------------------------------
# Description:
# 

#----------------------------------------------------------------
# History:
# 2017/05/19 v1.0	rcarff
# - Created, to handle generic objects not contained in BioEntities.

#----------------------------------------------------------------
# Usage:
# 

package Ont::GenericTerms;

$VERSION = '1.0';

use strict 'vars';
use warnings;

use Ont::Ontology;

our @ISA = qw(Ont::Ontology);


# TRIPS ontology for generic terms.  These are not covered by BioEntities
my @data = (
    	'ONT::BIOLOGICAL-PROCESS' => 1,
	    'ONT::MUTATION' => 1,
	    'ONT::SIGNALING-PATHWAY' => 1,

        # see when using 'by means of'
		'ONT::PS-OBJECT' => 1,

        # see with EPI's
		'ONT::PERSON' => 1,
		'ONT::INFO-MEDIUM' => 1,
		'ONT::MATERIAL' => 1,
		'ONT::SHAPE-OBJECT' => 1,
		'ONT::RESPONSE' => 1,
		);

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@data);
    bless $self, $class;
}

1;
