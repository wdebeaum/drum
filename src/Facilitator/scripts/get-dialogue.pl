#!/usr/bin/env perl

# get-dialogue.pl
#
# Time-stamp: <Thu May 30 17:13:37 CDT 2019 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 12 Apr 2018
#
# Extracts a readable dialogue out of a facilitator log.
# N.B.: System alerts are filtered out!

# Output is in TSV format:
# - col1: speaker (USR | SYS)
# - col2: uttnum (only for USR utterances)
# - col3: utterance

# This script works best in conjunction with find-messages. Running it directly
# on a log will likely result in warnings and duplicate lines in the output.

# Example of usage:
# cat facilitator.log \
# | ${TRIPS_BASE}/bin/find-messages -nom -from texttagger:utterance,speech-out:spoken \
# | ${TRIPS_BASE}/bin/get-dialogue \
# > dialogue.tsv

BEGIN {
  my $TRIPS_BASE_DEFAULT = "/usr/local";
  my $TRIPS_BASE = $ENV{TRIPS_BASE} || $TRIPS_BASE_DEFAULT;
  $path = "$TRIPS_BASE/etc";
}

use warnings;
use open qw(:utf8 :std);

use lib $path;
use KQML::KQML;
use Data::Dumper;

# read all messages from the buffer
while (my $msg_lol = KQML::KQMLRead(\*STDIN))  {
  my $msg = KQML::KQMLKeywordify($msg_lol);
  next unless defined $msg;
  my $verb = lc($msg->{'verb'});
  my $content_lol = $msg->{':content'};
  my $content = KQML::KQMLKeywordify($content_lol);
  next unless defined $content;
  my $content0 = lc($content->{'verb'});
  if ($content0 eq "spoken") {
    my $text = KQML::KQMLStringAtomAsPerlString($content->{':what'});
    $text =~ s/ *\n/ /g;
    $text =~ s/\n\Z//;
    # we skip SYS utts that just signal waiting (!!)
    next if $text eq "I'm waiting for you.";
    printf "SYS\t\t%s\n", $text;
  }
  elsif ($content0 eq "utterance") {
    my $uttnum = $content->{':uttnum'};
    my $text = KQML::KQMLStringAtomAsPerlString($content->{':text'});
    $text =~ s/ *\n/ /g;
    $text =~ s/\n\Z//;
    printf "USR\t%d\t%s\n", $uttnum, $text;
  } 
}
