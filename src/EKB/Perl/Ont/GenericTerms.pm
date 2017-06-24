#!/usr/bin/perl

# GenericTerms.pm
#
# Time-stamp: <Fri Jun 23 13:16:12 CDT 2017 lgalescu>
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
# 2017/06/23 v2.0	lgalescu
# - Added a 'disease' branch.

#----------------------------------------------------------------
# Usage:
# 

package Ont::GenericTerms;

$VERSION = '2.0';

use strict 'vars';
use warnings;

use Ont::Ontology;

our @ISA = qw(Ont::Ontology);


# TRIPS ontology for generic terms.  These are not covered by BioEntities
my @data = (
	    'ONT::BIOLOGICAL-PROCESS' => 1,
	    'ONT::MUTATION' => 1,
	    'ONT::SIGNALING-PATHWAY' => 1,

	    # seen when using 'by means of'
	    'ONT::PS-OBJECT' => 1,

	    # seen with EPI's
	    'ONT::PERSON' => 1,
	    'ONT::INFO-MEDIUM' => 1,
	    'ONT::MATERIAL' => 1,
	    'ONT::SHAPE-OBJECT' => 1,
	    'ONT::RESPONSE' => 1,

	    # diseases
	    'ONT::MEDICAL-DISORDERS-AND-CONDITIONS' => 1,
	    'ONT::DISEASE' => 'ONT::MEDICAL-DISORDERS-AND-CONDITIONS',
	    'ONT::CANCER' => 'ONT::DISEASE',
	   );

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@data);
    bless $self, $class;
}

1;
