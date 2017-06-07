#!/usr/bin/perl

# ekb_filter.pl
#
# Time-stamp: <Sat Apr  1 19:00:04 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  1 Apr 2017

# Description
# Tool for filtering an EKB, to keep only desired sentences and/or paragraphs
# and the assertions derived from them.

# History
# 2017/04/01 v1.0	lgalescu
# - Created.

#----------------------------------------------------------------
# Usage:
# 
my $usage;

BEGIN {
    # Just use basename of program, rather than whole path.
    $0 =~ s:.*/::;

    $usage = "usage: $0 [-p PID1 ...] [-s SID1 SID2 ...] EKB";
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
     @pids,
     @sids,
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
	   'p=s{1,}' => \@pids,
	   's=s{1,}' => \@sids,
	   'h|help' => \$help,
	   'd|debug=i' => \$debugLevel,
	  );

die $usage if $help;

$debugLevel = $debugLevel // 0;
local $util::Log::DebugLevel = $debugLevel;
local $util::Log::CallerInfo = ($debugLevel > 1);

my $ekb = EKB->new($ekb_file);

$ekb->filter( { paragraphs => \@pids,
		sentences => \@sids } );

print $ekb->toString();
