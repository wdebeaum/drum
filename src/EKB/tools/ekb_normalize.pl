#!/usr/bin/env perl

# ekb_normalize.pl
#
# Time-stamp: <Mon Jun 12 18:42:39 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 12 Jun 2017

# Description
# Tool for normalizing an EKB.

# History
# 2017/06/12 v1.0	lgalescu
# - Created.

#----------------------------------------------------------------
# Usage:
# 
my $usage;

BEGIN {
    # Just use basename of program, rather than whole path.
    $0 =~ s:.*/::;

    $usage = "usage: $0 EKB";
}

BEGIN {
    $TRIPS_BASE_DEFAULT = ".";
    $TRIPS_BASE = $ENV{'TRIPS_BASE'} || $TRIPS_BASE_DEFAULT;
    warn "TRIPS_BASE=$TRIPS_BASE\n";
  }

# local (TRIPS) Perl libraries
use lib "./Perl/";
use lib "../../etc/";
use lib "$main::TRIPS_BASE/etc/";
use lib "$main::TRIPS_BASE/etc/EKBAgent";

$VERSION = '1.0';

use Data::Dumper;
use Getopt::Long;

use EKB;

use util::Log;

our (
     $ekb_file,
     $help,
     $debugLevel,
    );

# last arg must be the ekb file
$ekb_file = pop;
unless (-f $ekb_file) {
  FATAL "Cannot find EKB file: $ekb_file";
}

GetOptions(
	   'h|help' => \$help,
	   'd|debug=i' => \$debugLevel,
	  );

die $usage if $help;

$debugLevel = $debugLevel // 0;
local $util::Log::DebugLevel = $debugLevel;
local $util::Log::CallerInfo = ($debugLevel > 1);

my $ekb = EKB->new($ekb_file);

$ekb->normalize();

print $ekb->toString();
