#!/usr/bin/perl

# ekr_drum.pl
#
# Time-stamp: <Sun Mar  5 23:55:39 CST 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  6 Jun 2016
#

#----------------------------------------------------------------
# Description:
# Wrapper for the DRUM reasoner.

#----------------------------------------------------------------
# History:
# 2016/06/06 v1.0	lgalescu
# - Created, from older reasoner script.
# 2017/02/06 v1.0.1	lgalescu
# - adapted to work as part of EKB TRIPS component
# 2017/03/04 v1.0.2	lgalescu

#----------------------------------------------------------------
# Usage:
# 
# Get TRIPS_BASE from environment
BEGIN {
    $TRIPS_BASE_DEFAULT = ".";
    $TRIPS_BASE = $ENV{'TRIPS_BASE'} || $TRIPS_BASE_DEFAULT;
    warn "TRIPS_BASE=$TRIPS_BASE\n";
  }

my $VERSION = "1.0.2";

# local (TRIPS) Perl libraries
use lib "$main::TRIPS_BASE/etc/";
use lib "$main::TRIPS_BASE/etc/EKBAgent";

use feature 'unicode_strings';

use Getopt::Long;
use Data::Dumper;

use EKB;
use EKB::Compare;
use XML::LibXML;
use Test::More;

use util::Log;

use EKB::Reasoner::Drum;
use EKB::Reasoner::Rule;

our (
     $opt_pub,
     $opt_ekr_options,
     $debugLevel,
     $help
    );

GetOptions(
	   'pub' 		=> \$opt_pub,
	   'ko|ekr-options=s' 	=> \$opt_ekr_options,
	   'd|debug=i' 		=> \$debugLevel,
	   'h|help' 		=> \$help,
	  ) or die "Something's wrong";

$util::Log::DebugLevel = $debugLevel // 0;
$util::Log::CallerInfo = 1;

my $ekb_file = shift
  or FATAL "No ekb specified";

my %ekb_options;

if ($opt_pub) {
    $ekb_options{pub} = 1;
}

my $ekb = EKB->new($ekb_file, \%ekb_options);
INFO "Done: Read EKB";
$ekb->info();

$ekb->normalize();
INFO "Done: Normalize EKB";
$ekb->info();

my %ekr_options;
if ($opt_ekr_options) {
  %ekr_options = parse_options($opt_ekr_options);
}

DEBUG 3, "options in: %s", Dumper(\%ekr_options);

my $reasoner = EKB::Reasoner::Drum->new($ekb, %ekr_options);

DEBUG 3, "options out: %s", Dumper(\%{$reasoner->options()});

$reasoner->run();

INFO "Done: Drum inference";
$ekb->info();

$ekb->print();

0;


# input: comma-separated options string
# output: list of key => value pairs
# conventions:
#   opt=val ~~> opt => val
#   no-opt ~~> opt => 0
#   opt ~~> opt => 1
sub parse_options {
  my $opt_string = shift;

  my @opts;

  foreach my $opt (split(/,\s*/, $opt_string)) {
    my ($o, $v) = split(/=/, $opt);
    if (defined($v)) {
      push @opts, $o => $v;
    } elsif ($o =~ m/^no-(.+)/) {
      push @opts, $1 => 0;
    } else {
      push @opts, $o => 1;
    }
  }
    
  return @opts;
}
