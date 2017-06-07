#!/usr/bin/perl -CSD

# test.pl
#
# Time-stamp: <Sat May 20 09:47:10 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  5 Feb 2016
#

#----------------------------------------------------------------
# Description:
# Tool to compare two EKBs.

#----------------------------------------------------------------
# History:
# 

#----------------------------------------------------------------
# Usage:
# 

# Get TRIPS_BASE from environment
BEGIN {
    $TRIPS_BASE_DEFAULT = ".";
    $TRIPS_BASE = $ENV{'TRIPS_BASE'} || $TRIPS_BASE_DEFAULT;
    warn "TRIPS_BASE=$TRIPS_BASE\n";
  }

use strict 'vars';
use warnings;

# local (TRIPS) Perl libraries
use lib "$main::TRIPS_BASE/etc/";
use lib "$main::TRIPS_BASE/etc/EKBAgent";

use Data::Dumper;
use Getopt::Long;

use EKB;
use EKB::Compare;

use util::Log;

our (
     $debugLevel,
     %cmp_opts,
    );

GetOptions(
	   'no-norm' => \&opt_handler,
	   'ignore-text' => \&opt_handler,
	   'ignore-sentence-diffs' => \&opt_handler,
	   'ignore-dbid' => \&opt_handler,
	   'ignore-pred-type' => \&opt_handler,
	   'attributes=s' => \&opt_handler,
	   'uttnums=s' => \&opt_handler,
	   'debug|d=i' => \$debugLevel,
	  ) or die "Something's wrong";

sub opt_handler {
    my ($opt_name, $opt_value) = @_;

    if ($opt_name eq 'no-norm') {
	$cmp_opts{normalize} = 0;
    } elsif ($opt_name eq 'ignore-text') {
	$cmp_opts{ignore_text} = 1;
    } elsif ($opt_name eq 'ignore-sentence-diffs') {
	$cmp_opts{ignore_sentence_diffs} = 1;
    } elsif ($opt_name eq 'ignore-dbid') {
	$cmp_opts{ignore_dbid} = 1;
    } elsif ($opt_name eq 'ignore-pred-type') {
	$cmp_opts{ignore_pred_type} = 1;
    } elsif ($opt_name eq 'attributes') {
	$cmp_opts{attributes} = [ split(/,/, $opt_value) ];
    } elsif ($opt_name eq 'uttnums') {
	$cmp_opts{uttnums} = [ split(/,/, $opt_value) ];
    }
}

$debugLevel = $debugLevel // 0;
$util::Log::DebugLevel = $debugLevel;
$util::Log::CallerInfo = ($debugLevel > 1);

my $ekb_file1 = shift;
my $ekb_file2 = shift;

my $ekb1 = EKB->new($ekb_file1);
my $ekb2 = EKB->new($ekb_file2);

my @opts = %cmp_opts;

my $comp = EKB::Compare->new($ekb1, $ekb2, @opts);
FATAL "Cannot set up comparison due to errors"
  unless $comp;
$comp->compare();

INFO "EKBs are %s", $comp->result ? "identical" : "different";

printf "%s %s %s\n", $0, $ekb_file1, $ekb_file2;
print '-'x80 . "\n";
print $comp->diffs_as_string;

exit 0;
