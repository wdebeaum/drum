#!/usr/bin/perl

# ekb_join.pl
#
# Time-stamp: <Mon Dec 16 16:32:04 CST 2019 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 11 Dec 2019
#

#----------------------------------------------------------------
# Description:
# Tool to join EKBs. 

#----------------------------------------------------------------
# History:
# 20191210 v0.1 lgalescu
# - Started working on it.
# 20191215 v1.0 lgalescu
# - Tested. Ready.
# 20191216 v1.1 lgalescu
# - Tweak: added offset attribute to paragraphs (a bit redundant).

#----------------------------------------------------------------
# Usage:
# 
my $usage;

BEGIN {
    # Just use basename of program, rather than whole path.
    $0 =~ s:.*/::;

    $usage = "usage: $0 -id ID EKB ..."
}


# Get TRIPS_BASE from environment
BEGIN {
    $TRIPS_BASE_DEFAULT = ".";
    $TRIPS_BASE = $ENV{'TRIPS_BASE'} || $TRIPS_BASE_DEFAULT;
    warn "TRIPS_BASE=$TRIPS_BASE\n";
  }

$VERSION = '1.1';

use strict 'vars';
use warnings;

# local (TRIPS) Perl libraries
use lib "$main::TRIPS_BASE/etc/";
use lib "$main::TRIPS_BASE/etc/EKBAgent";

use Data::Dumper;
use Getopt::Long;
use List::Util qw(any all uniq);

use EKB;
use EKB::Compare;

use util::Log;

our (
     $debugLevel,
     $docId,
     $help,
    );

GetOptions(
	   'id=s' =>\$docId,
	   'debug|d=i' => \$debugLevel,
	   'help' => \$help,
	  ) or die $usage;

my @ekb_inputs = @ARGV;

die $usage if $help;

$debugLevel = $debugLevel // 0;
local $util::Log::DebugLevel = $debugLevel;
local $util::Log::CallerInfo = ($debugLevel > 1);

DEBUG 1, "EKB list: [%s]", join(" ", @ekb_inputs);

my @ekb_list;
for my $ekb_file (@ekb_inputs) {
  my $ekb = EKB->new($ekb_file);
  $ekb->normalize();
  push @ekb_list, $ekb;
}

WARN "Got %d inputs", scalar(@ekb_list);

# 1. we create an empty EKB (timestamp is set)
my $ekb = EKB->new();

# 2. set ekb attributes
# 2.1 the id attribute
$ekb->set_attr('id', $docId);  

# 2.2. the complete attribute
my @comps = uniq map { $_->get_attr('complete') } @ekb_list;
my $complete =
  (any { !defined($_) } @comps) ? undef
  : (any { lc($_) eq 'false' } @comps) ? "false"
  : (all { lc($_) eq 'true' } @comps) ? "true"
  : undef;

if ($complete) {
  $ekb->set_attr('complete', $complete);
}

# 2.3. the domain attribute
my @domains = uniq map { $_->get_attr('domain') } @ekb_list;
my $domain = $domains[0];
unless (all { $_ eq $domain } @domains) {
  WARN "Input EKBs are in multiple domains: %s", join(", ", @domains);
  undef $domain;
}
$ekb->set_attr('domain', $domain);

# 3. add the input element 
my $input_elem = $ekb->add_input();

my $nekbs = 0;
my $ndocs = 0;
my $nparas = 0;
my $nsents = 0;
my $textlen = 0;
my %offsets; # for new paragraphs, relative to joined doc
my %aid_map; # assertion ids

foreach my $e (@ekb_list) {
  $nekbs++;

  my %map;

  # 4. add input documents
  
  my @docs = $e->get_docs; # could be more than 1
  INFO "%s: %d docs", $e->get_attr('id'), scalar(@docs);
  foreach my $doc (@docs) {
    # the old
    my $old_docid = $doc->getAttribute('id');
    # the new
    my $new_docid = sprintf("doc%d", ++$ndocs);
    $e->set_docid($old_docid, $new_docid);
    my $new_doc = $doc->cloneNode(1);
    $ekb->add_doc($new_doc);
    # verify
    WARN "docid mapping: %s -> %s",
      $old_docid, $new_doc->getAttribute('id');
  }

  # 5. add input paragraphs
  
  my @paras = $e->get_paragraphs;
  INFO "%s: %d paras", $e->get_attr('id'), scalar(@paras);
  foreach my $p (@paras) {
    # the old
    my $old_pid = $p->getAttribute('id');
    # the new
    my $new_pid = sprintf("p%d", ++$nparas);
    $e->set_pid($old_pid, $new_pid);
    my $new_p = $p->cloneNode(1);
    $ekb->add_paragraph($new_p);
    # store offset
    set_attribute($new_p, 'offset', $textlen);
    $offsets{$new_pid} = $textlen;
    $textlen += length($new_p->textContent);
    # verify
    WARN "pid mapping: %s -> %s",
      $old_pid, $new_p->getAttribute('id');

    ### not needed!!
    # $e->set_offset($textlen);
    # my @a = $e->get_assertions();
    # my $last = $a[-1];
    # my $last_s = $last->getAttribute('start');
    # my $last_e = $last->getAttribute('end');
    # WARN "last assertion text (%d, %d):\n  recorded: %s\n  substr: %s",
    #   $last_s, $last_e,
    #   get_slot_value($last, 'text'),
    #   $e->get_span_text($new_pid, $last_s, $last_e);
    
  }
  INFO "text length (%d): %d", $nparas, $textlen;

  # 6. add sentences
  
  my @sents = $e->get_sentences;
  INFO "%s: %d sents", $e->get_attr('id'), scalar(@sents);
  foreach my $s (@sents) {
    # the old
    my $old_sid = $s->getAttribute('id');
    # the new
    my $new_sid = sprintf("s%d", ++$nsents);
    $e->set_sid($old_sid, $new_sid);
    my $new_s = $s->cloneNode(1);
    $ekb->add_sentence($new_s);
    # verify
    WARN "sid mapping: %s -> %s",
      $old_sid, $new_s->getAttribute('id');
  }

  # 7. add assertions
  # notes:
  # - an EKB may contain multiple paragraphs per document
  # - char positions (start, end) are relative to the concatentation of all
  #   paragraphs in the EKB

  my @assertions = $e->get_assertions;
  INFO "%s: %d assertions", $e->get_attr('id'), scalar(@assertions);
  WARN "%s", $assertions[0];
  # first pass may modify other assertions
  foreach my $a (@assertions) {
    # the old
    my $old_aid = $a->getAttribute('id');
    # the new
    my $new_aid = make_new_aid($old_aid, $a->getAttribute('uttnum'));
    if ($new_aid ne $old_aid) {
      $e->set_aid($old_aid, $new_aid);
      # verify
      WARN "aid mapping: <%s> -> <%s>",
	$old_aid, $a->getAttribute('id');
    }
    # 
  }
  # ready to copy
  foreach my $a (@assertions) {
    my $new_a = $a->cloneNode(1);
    $ekb->add_assertion($new_a);
  }
}

# 8. done
print $ekb->toString();


# make new aid
# heuristic: join old aid with sid
# justification: there should not be multiple assertions with the same aid in
# the same sentence
sub make_new_aid {
  my ($aid, $sid) = @_;
  if (!exists $aid_map{$aid}) {
    $aid_map{$aid}{$sid} = $aid;
    return $aid;
  }
  my $new_aid = sprintf("%s_%s", $aid, $sid);
  $aid_map{$aid}{$sid} = $new_aid;
  return $new_aid;
}
