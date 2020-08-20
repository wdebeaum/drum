#!/usr/bin/perl

# make-ont-file.pl
#
# Time-stamp: <Tue Jul 28 00:27:41 CDT 2020 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 27 Jul 2020
#

#----------------------------------------------------------------
# Description:
# This script
# - ingests the TRIPS ontology in XML format.
# - given an input file (an ontology definition file), it looks
#   for lines containing strings in the format ##ont::thing##, and 
#   replaces them with the list of ISA relations in the EKB ontology
#   format (child => parent) derived from the TRIPS sub-ontology
#   rooted in ont::thing.
# - by convention, the roots are assigned the parent '1'.
# Note: ##ont::thing## cannot be preceded by anything other than
# whitespace or it will be considered a simple comment. If it is
# followed by other characters, they will be copied in the output.
# The following is valid syntax in the input file:
# my @data = (
#    ##ont::time-object##, # temporal concepts
#    ##ont::geo-object##   # geo locations
#    );

#----------------------------------------------------------------
# History:
# 2020/07/27 v1.0	lgalescu
# - Created.

#----------------------------------------------------------------
# Usage:
# 
my $usage;

BEGIN {
    # Just use basename of program, rather than whole path.
    $0 =~ s:.*/::;

    $usage = "usage: $0 Ontfile.pm.in
";
}

BEGIN {
    $TRIPS_BASE_DEFAULT = ".";
    $TRIPS_BASE = $ENV{'TRIPS_BASE'} || $TRIPS_BASE_DEFAULT;
    warn "TRIPS_BASE=$TRIPS_BASE\n";
  }

# local (TRIPS) Perl libraries
use lib "$main::TRIPS_BASE/etc/";

$VERSION = '1.0';

use strict 'vars';
use warnings;

use XML::LibXML;
use Getopt::Long;
use Data::Dumper;
use File::Spec::Functions;

use util::Log;

# default root
my $root = 'ONT::ROOT';

my $help;
my $debugLevel;

GetOptions(
           'r=s' => \$root,
           'h|help' => \$help,
           'd|debug=i' => \$debugLevel,
          );

die $usage if $help;

$debugLevel = $debugLevel // 0;
local $util::Log::DebugLevel = $debugLevel;
local $util::Log::CallerInfo = ($debugLevel > 1);

my $infile = shift
  or die $usage;

-f $infile
  or FATAL "Cannot find input file: $infile";

# ont hierarchy: parent => [child, ...]
my %ont; 
read_ont();

open(IN, "<$infile")
  or die "Cannot read input file: $infile";
while(my $line = <IN>) {
  if ($line =~ m/^(\s*)##(ont::[-\w]+)##(.*)$/i) {
    my ($indent, $root, $coda) = ($1, uc($2), $3);
    INFO "$0: Expanding: $root";
    my @relns = get_isa_relns($root);
    INFO "$0: Found %d relns", scalar(@relns);
    printf "%s%s%s\n", $indent, join(",\n$indent", @relns), $coda;
  } else {
    print $line;
  }
}
close(IN);


sub read_ont {
  # the source file must exist in $TRIPS_BASE/src/DeepSemLex/data/TRIPS/trips-ont-dsl.xml
  my $ont_file =
    catfile($main::TRIPS_BASE, "src", "DeepSemLex", "data", "TRIPS", "trips-ont-dsl.xml");

  # read ont file
  unless (-f $ont_file) {
    FATAL "Cannot find trips-ont-dsl.xml";
  }

  my $ont_doc;

  eval {
    $ont_doc = XML::LibXML->load_xml(location => $ont_file);
  } || FATAL "Error reading TRIPS ontology from $ont_file: %s", $@;

  my @concepts = $ont_doc->findnodes('/dsl/concept');
  INFO "$0: Read %d concepts from %s", scalar(@concepts), $ont_file;

  for my $c (@concepts) {
    DEBUG 3, "%s", $c;
    my $n = $c->findvalue('@name'); 
    my $p = $c->findvalue('relation[@label="inherit"]')
      or next;			# should only apply to ont::root
    $p =~ s/^\s+|\s+$//g;
    push @{ $ont{uc($p)} }, uc($n); # FIXME: check if already defined?
  }
}

sub get_isa_relns {
  my $root = shift;

  my @outputs = ( $root );
  my @relns;
  
  unless ($root eq 'ONT::ROOT') {
    push @relns, sprintf("'%s' => '%s'", $root, 1);
  }

  while (@outputs) {
    my $next = shift @outputs;
    next unless exists $ont{$next};
    # we sort outputs so 
    my @children = sort @{ $ont{$next} };
    foreach my $c (@children) {
      push @relns, sprintf("'%s' => '%s'", $c, ($next eq 'ONT::ROOT') ? 1 : $next);
    }
    push @outputs, @children;
  }

  return @relns;
}
