#!/usr/bin/perl

# BioEntities.pm
#
# Time-stamp: <Sun Oct 28 16:39:50 CDT 2018 lgalescu>
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

package Ont::BioEntities;

$VERSION = '1.0';

use strict 'vars';
use warnings;

use Ont::Ontology;

our @ISA = qw(Ont::Ontology);


# TRIPS ontology for bio entities, adapted to some extent
my @data = (# biomolecular branch
	    'ONT::MOLECULAR-PART' => 1,
	    'ONT::MOLECULE' => 'ONT::MOLECULAR-PART',
	    'ONT::GENE-PROTEIN' => 'ONT::MOLECULE', # changed
	    'ONT::PROTEIN' => 'ONT::GENE-PROTEIN',
	    'ONT::PROTEIN-FAMILY' => 'ONT::GENE-PROTEIN',
	    'ONT::GENE' => 'ONT::GENE-PROTEIN',
	    'ONT::MACROMOLECULAR-COMPLEX' => 'ONT::MOLECULE', # changed
	    'ONT::AMINO-ACID' => 'ONT::MOLECULE', # changed
	    'ONT::RNA' => 'ONT::MOLECULE', # changed
	    'ONT::MRNA' => 'ONT::RNA',
	    'ONT::MOLECULAR-DOMAIN' => 'ONT::MOLECULAR-PART',
	    'ONT::MOLECULAR-SITE' => 'ONT::MOLECULAR-PART',
	    'ONT::PROMOTER' => 'ONT::MOLECULAR-PART',
	    'ONT::RESIDUE' => 'ONT::MOLECULAR-PART',
	    'ONT::TERMINUS' => 'ONT::MOLECULAR-PART',
	    # chemical substance branch
	    'ONT::SUBSTANCE' => 1,
	    'ONT::GAS-SUBSTANCE' => 'ONT::SUBSTANCE',
	    'ONT::LIQUID-SUBSTANCE' => 'ONT::SUBSTANCE',
	    'ONT::SOLID-SUBSTANCE' => 'ONT::SUBSTANCE',
	    'ONT::CHEMICAL' => 'ONT::SUBSTANCE',
	    'ONT::MINERALS' => 'ONT::SUBSTANCE',
	    'ONT::CHEMICAL' => 'ONT::MOLECULE', # added
	    'ONT::PHARMACOLOGIC-SUBSTANCE' => 'ONT::CHEMICAL',
	    'ONT::MEDICATION' => 'ONT::PHARMACOLOGIC-SUBSTANCE',
	    # cell part
	    # N.B: this is everything that's not specifically mapped to
	    # a cell component subclass
	    'ONT::CELL-PART' => 1,
	    'ONT::CELL' => 1,
	   );

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@data);
    bless $self, $class;
}

1;
