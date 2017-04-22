# Compare.pm
#
# Time-stamp: <Fri Apr 21 16:44:49 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  4 May 2016
#

#----------------------------------------------------------------
# Description:
# Compare two EKBs

#----------------------------------------------------------------
# History
# 2016/05/04 v0.1	lgalescu
# - Created.
# 2016/05/05 v1.0	lgalescu
# - First working version.
# 2016/05/07 v1.1	lgalescu
# - Fixed bugs, other updates.
# 2016/05/16 v1.2	lgalescu
# - Fully converted to OO interface.
# 2016/05/18 v1.2.1	lgalescu
# - Fixed bug
# 2016/05/26 v1.3	lgalescu
# - Updated to use new OO interface for EKB.pm.
# 2016/05/26 v1.3	lgalescu
# - Updated to use new OO interface for EKB.pm.
# 2016/06/02 v1.4	lgalescu
# - Added option to ignore the predicate type.
# 2016/06/06 v1.5	lgalescu
# - Added option to skip normalization (saves time on big EKBs).
# - Added span defaults when :start/:end are missing.
# - Fixed bugs (corner cases).
# 2016/07/07 v1.6	lgalescu
# - Added ability to perform ont checks (allows match when one type is-a
#   the other). Added option to control this behavior.
# 2016/09/17 v1.7	lgalescu
# - Added assertion counts to summary
# 2016/09/18 v2.0	lgalescu
# - Fixed use of deep matching option, which was buggy.
# - Now using deep matching option for all proper args.
# 2017/02/03 v2.0.1	lgalescu
# - Adapted to work as part of EKB TRIPS component
# 2017/03/04 v2.0.2	lgalescu
# - Fixed: issue causing an uninitialized value warning in &cmp_args.
# 2017/03/24 v2.1.0	lgalescu
# - Added option to ignore sentence diffs (assumes they are identical even
#   when they're not).
# 2017/04/21 v2.1.1	lgalescu
# - Fixed bug.

# TODO (in order of importance):
# - try to find node "substitutions"
# - profile: it can be slow on big EKBs!
# - keep track of why things failed (failure diagnostics)
# - there are a couple FIXME notes below
# - improve how diffs are set and got
# - consider whether it might be appropriate to add a similarity score 
#   and obtain the best alignement through global optimization

package EKB::Compare;

$VERSION = '2.1.1';

use strict 'vars';
use warnings;

use Data::Dumper;
use Algorithm::Diff qw(sdiff);
use List::Util qw(min max all any first);
use XML::LibXML;
use Test::Deep::NoTest qw(eq_deeply set subsetof);

use EKB;
use EKB::Match;
use Ont::BioEntities;
use Ont::BioEvents;
use util::Log;

our @Options = qw/no_normalization 
		  ignore_sentence_diffs
		  ignore_ekb_id 
		  ignore_ekb_complete 
		  ignore_dbid 
		  ignore_pred_type 
		  only_uttnums 
		  strict_ont_match/;

# note about extraction mappings ($self->{x_map})
# 1. this is reset for each sentence!
# idea:
#  $id1 ~~ $id2 iff $x_map{1}{$id1}{$id2}==1 [=> $x_map{2}{$id2}{$id1}==1]
#  $id1 !~ $id2 iff $x_map{1}{$id1}{$id2}==0 [=> $x_map{2}{$id2}{$id1}==0]

# bio ontologies
my $ont_events = Ont::BioEvents->new();
my $ont_bioents = Ont::BioEntities->new();


### constructor

sub new {
  my $class = shift;
  if (@_ < 2) {
    FATAL "Constructor EKB::Compare->new() must have at least two arguments";
  }
  my ($ekb1, $ekb2, @call_opts) = @_;
  # some checks
  my $valid = eval { $ekb1->isa("EKB") && $ekb2->isa("EKB") };
  if (!$valid) {
    ERROR "Failed to create %s object%s.", $class, ($@ ? " ($@)" : "");
    return undef;
  }
  if (@call_opts % 2) {
    FATAL "Options must be name=>value pairs. Got: %s", Dumper(\@call_opts);
  }
  # options: set defaults
  my %options = _default_options();
  # ... and read the caller-deinfed ones
  my %opts = @call_opts;
  while (my($key, $val) = each %opts) {
    my $lkey = lc($key);
    ERROR "Unrecognised option: $key"
      unless (any {$_ eq $lkey} @Options);
    $options{$lkey} = $val;
  }
  DEBUG 2, "options at init: %s", Dumper(\%options);
  my $self = {
	      # ekb1: reference EKB; a EKB object
	      ekb1 => $ekb1,
	      # ekb2: test EKB; a EKB object
	      ekb2 => $ekb2,
	      # options: options for compare()
	      options => \%options,
	      # result: match or no match
	      result => undef,
	      # diffs: structure w/ detailed diffs
	      diffs => {},
	      # summary at assertion level
	      summary => {},
	      # sentence mapping from ekb1 to ekb2
	      s_map => {},
	      # EKB assertion mapping from ekb1 to ekb2 (temporary)
	      x_map => {}
	     };
  bless $self, $class;
}


### getters and setters

sub _default_options {
  return
    ( no_normalization => 0,
      ignore_ekb_id => 0,
      ignore_ekb_complete => 0,
      ignore_dbid => 0,
      ignore_pred_type => 0,
      strict_ont_match => 0,
      only_uttnums => [],
    );
}

# get/set options in bulk
sub options {
  my $self = shift;
  if (@_) {
    my %opts = @_;
    while (my ($opt, $val) = each(%opts)) {
      $self->option($opt, $val);
    }
  }
  my @opts = %{ $self->{options} }
    if $self->{options};
  return @opts;
}

# get/set individual option
sub option {
  my $self = shift;
  return __option($self->{options}, @_);
}

sub ekb1 {
  my $self = shift;
  return $self->{ekb1};
}

sub ekb2 {
  my $self = shift;
  return $self->{ekb2};
}

sub result {
  my $self = shift;
  if (@_) {
    $self->{result} = shift;
  }
  return $self->{result};
}

sub diffs {
  my $self = shift;
  return $self->{diffs};
}

sub summary {
  my $self = shift;
  return $self->{summary};
}


### comparison between two EKBs (main function of this package/class)

sub compare {
  my $self = shift;

  unless ($self->option('no_normalization')) {
    $self->ekb1()->normalize();
    $self->ekb2()->normalize();
  }

  my @tests =
    (
     # ekb meta info
     $self->option('ignore_ekb_id')
     || $self->cmp_ekb_attribute('id'),
     $self->option('ignore_ekb_complete')
     || $self->cmp_ekb_attribute('complete'),

     # inputs (by sentence)
     $self->cmp_ekb_input(),

     # EKB assertions (by sentence)
     $self->cmp_ekb_assertions()
    );

  return $self->result(min @tests);
}


### function for matching EKB attributes

sub cmp_ekb_attribute {
  my $self = shift;
  my ($attr) = @_;

  my $v1 = $self->ekb1()->get_attr($attr);
  my $v2 = $self->ekb2()->get_attr($attr);
  my $result = ($v1) && ($v2) && ($v1 eq $v2) ? 1 : 0;
  if (! $result) {
    $self->diffs()->{"/ekb/\@$attr"}{prev} = $v1 // "";
    $self->diffs()->{"/ekb/\@$attr"}{new} = $v2 // "";
  }
  return $result;
}


### function for matching inputs (sentence mapping)

sub cmp_ekb_input {
  my $self = shift;

  my @sentences1 = $self->ekb1()->get_sentences();
  # apply only_uttnums filter
  my $only_uttnums = $self->option('only_uttnums');
  my $have_uttnums = (scalar(@$only_uttnums) > 0);
  if ($have_uttnums) {
    @sentences1 =
      grep { my $id = $_->getAttribute('id');
	     first { $id eq $_} @$only_uttnums }
      @sentences1;
    DEBUG 2, "Doing only %s", "@sentences1";
  }
  # are we assuming sentences are the same?
  if ($self->option('ignore_sentence_diffs')) {
    foreach my $s (@sentences1) {
      my $sid1 = $s->getAttribute('id');
      $self->{s_map}{$sid1} = $sid1;
    }
    return 1;
  }
    
  my @sentences2 = $self->ekb2()->get_sentences();
  my @s_texts1 = map { $_->textContent } @sentences1;
  my @s_texts2 = map { $_->textContent } @sentences2;
  my @diffs = sdiff( \@s_texts1, \@s_texts2 );
  DEBUG 3, "%s", Dumper(@diffs);
  my ($i1, $i2) = (0, 0);
  my $result = 1;
  foreach my $diff (@diffs) {
    DEBUG 3, "%s", Dumper($diff);
    my $d = $diff->[0];
    my ($sid1, $path1, $sid2, $path2);
    if ($i1 < scalar(@sentences1)) {
      $sid1 = $sentences1[$i1]->getAttribute('id');
      $path1 = sprintf("/ekb/input/sentences/sentence[\@id=%s]", $sid1);
    }
    if ($i2 < scalar(@sentences2)) {
      $sid2 = $sentences2[$i2]->getAttribute('id');
      $path2 = sprintf("/ekb/input/sentences/sentence[\@id=%s]", $sid2);
    }
    if ($d eq 'u') { # unchanged
      $self->{s_map}{$sid1} = $sid2;
      $i1++; $i2++;
    } elsif ($d eq '-') { # deletion
      $self->diffs()->{$path1}{prev} = $diff->[1];
      $i1++;
      $result = 0;
    } elsif ($d eq '+') { # insertion
      if ($have_uttnums) {
	# ignore if we restrict to specific uttnums
      } else {
	$self->diffs()->{$path2}{new} = $diff->[2];
	$i2++;
	$result = 0;
      }
    } elsif ($d eq 'c') { # changed
      $self->diffs()->{$path1}{prev} = $diff->[1];
      $self->diffs()->{$path2}{new} = $diff->[2];
      $i1++;
      $i2++;
      $result = 0;
    }
  }
  return $result;
}


### function for matching all EKB assertions (items)

# N.B.: assertions are compared only for matching sentences
sub cmp_ekb_assertions {
  my $self = shift;

  DEBUG 1, "s_map: %s", Dumper($self->{s_map});

  # sentences to work on
  my @uttnums = keys($self->{s_map});

  # apply only_uttnums filter (and validate)
  my $only_uttnums = $self->option('only_uttnums');
  if (@$only_uttnums) {
    eq_deeply($only_uttnums, subsetof(@uttnums))
      or FATAL "Some requested uttnums cannot be found in the EKB (%s)",
      "@{$only_uttnums}";
    @uttnums = @$only_uttnums;
  }
  
  return
    all { $_ == 1 }
    map { $self->cmp_ekb_assertions_for_utt($_) }
    sort { $a <=> $b }
    @uttnums;
}


### function for matching all EKB assertions (items) for a sentence

# sid1: sentence id in ekb1
sub cmp_ekb_assertions_for_utt {
  my $self = shift;
  my ($sid1) = @_;

  my $sid2 = $self->{s_map}{$sid1};

  my @items1 =
    sort { _span_cmp(_get_span($a), _get_span($b)) }
    $self->ekb1()->get_assertions({ uttnum => $sid1 });
  my @items2 =
    $self->ekb2()->get_assertions({ uttnum => $sid2 });

  my $count1 = scalar(@items1);
  my $count2 = scalar(@items2);

  $self->summary()->{all}{1} += $count1;
  $self->summary()->{all}{2} += $count2;
  
  { 
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 1, "comparing assertions for utt pair <%s, %s>: n=<%d, %d>",
      $sid1, $sid2, $count1, $count2;
  }
    
  $self->reset_x_map();
  my $result = $self->cmp_ekb_items(\@items1, \@items2);
    
  # items unique to ekb1
  my @uniq1 =
    grep { ! $self->has_match($_, 1) }
    @items1;
  foreach my $i1 (@uniq1) {
    my $span1 = _get_span($i1);
    DEBUG 3, "Deletion: [%d, %d] %s ", $span1->[0], $span1->[1], $i1->getAttribute('id');
    push @{ $self->diffs()->{utt}{$sid1}{$span1->[0]}{$span1->[1]}{prev} }, $i1;
    $result = 0;
  }
  $self->summary()->{del} += scalar(@uniq1);
  $self->summary()->{eql} += ($count1 - scalar(@uniq1));

  # items unique to ekb2
  my @uniq2 =
    sort { _span_cmp(_get_span($a), _get_span($b)) }
    grep { ! $self->has_match($_, 2) }
    @items2;
  foreach my $i2 (@uniq2) {
    my $span2 = _get_span($i2);
    DEBUG 3, "Insertion: [%d, %d] %s ", $span2->[0], $span2->[1], $i2->getAttribute('id');
    push @{ $self->diffs()->{utt}{$sid1}{$span2->[0]}{$span2->[1]}{new} }, $i2;
    $result = 0;
  }
  $self->summary()->{ins} += scalar(@uniq2);

  # done!
  {
    DEBUG 3, "x_map:\n %s", Dumper($self->{x_map});
  }
    
  return $result;
}


### generic function for matching two lists of EKB items

# TODO: some form of global optimization. for this i'd have to consider scoring
# matches (just match or no-match is not enough)
sub cmp_ekb_items {
  my $self = shift;
  my ($list1, $list2, $options) = @_;

  my $result = 1;

  # the following is an optimization that's expensive, but has not
  # demonstrated (yet) any benefit, so i'm not using it 
  if (0) {
    # to maximize chances we're getting the easy ones right,
    # we proceed in the order of
    # min{_span_dist(a,b) ; b \in L2}, where a \in \L1
    my @spans2 =  map { _get_span($_) } @$list2;

    my @sorted_list1 =
      map { $list1->[$_->[0]] }
      sort { $a->[1] <=> $b->[1] }
      map { my $i = $_;
	    my $span1 = _get_span($list1->[$i]);
	    [ $i, min (map { _span_distance($span1, $_) } @spans2) ]
	  }
      0..$#$list1;

    DEBUG 2, "sorted: %s", "@sorted_list1";
  }
    
  # we keep track here of temporary matches in @$list2 (ids)
  my %tmp_matched2 = ();
  foreach my $i1 (@$list1) {
    my $span1 = _get_span($i1);
    my $x_type = $i1->nodeName;
    my @candidates =
      # sort candidates in span-match order
      sort { _span_distance($span1, _get_span($a))
	     <=>
	     _span_distance($span1, _get_span($b)) }
      # select candidates with matching spans
      grep { _span_overlap($span1, _get_span($_)) == 1 }
      # eliminate candidates we matched already in @$list1
      grep { ! exists $tmp_matched2{$_->getAttribute('id')} }
      # eliminate candidates we know don't match
      grep { ! $self->not_matched($i1, $_) }
      # eliminate incompatible types
      grep { $x_type eq $_->nodeName }
      @$list2;

    {
      local $Data::Dumper::Terse = 1;
      local $Data::Dumper::Indent = 0;
      my @ids2 =
	map {[$_->getAttribute('id'), _get_span($_)]} @candidates;
      DEBUG 2, "span1: [%d,%d] %s %s => %s",
	$span1->[0], $span1->[1],
	$x_type, $i1->getAttribute('id'), Dumper(\@ids2);
      DEBUG 3, "candidates for %s:\n%s",
	$i1->getAttribute('id'), "@candidates";
    }
	
    # look for a match
    my $match =
      first { $self->cmp_ekb_item($i1, $_, $options) }
      @candidates;
    # short-circuit if we're not interested in a deep match
    if ($match) {
      # keep track of this id
      # TODO: optimization: if item has a match, no need to add it
      $tmp_matched2{$match->getAttribute('id')} = 1;
    } elsif (__option_is($options, 'deep', 0)) {
      return 0;
    }
    $result &&= 1;
  }
  DEBUG 2, "==> %s", $result;
  return $result;
}


### generic function for matching two EKB items

# TODO: if this is called from a different package, we must set %_x_map!
sub cmp_ekb_item {
  my $self = shift;
  my ($i1, $i2, $options) = @_;
    
  # shortcut if we'd looked at any of this before
  return 1 if $self->matched($i1, $i2);
  return 0 if ($self->has_match($i1, 1)
	       || $self->has_match($i2, 2)
	       || $self->not_matched($i1, $i2));

  {
    DEBUG 2, "matching [deep=%s]: %s %s ~~ %s",
      __option($options, 'deep'),
      $i1->nodeName,
      $i1->getAttribute('id'), $i2->getAttribute('id');
  }
    
  my $name = $i1->nodeName;
  my $name2 = $i2->nodeName;

  return 0 unless ($name2 eq $name);

  return $self->cmp_TERM($i1, $i2, $options) if ($name eq TERM);
  return $self->cmp_EVENT($i1, $i2, $options) if ($name eq EVENT);
  return $self->cmp_EPI($i1, $i2, $options) if ($name eq EPI);
  return $self->cmp_CC($i1, $i2, $options) if ($name eq CC);
  return $self->cmp_MODALITY($i1, $i2, $options) if ($name eq MODALITY);
}


### functions for matching extractions

sub cmp_TERM {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "TERM") && __assert_eq($i2->nodeName, "TERM")
    or return 0;

  my @tests = ();

  # overlap test
  push @tests,
    _span_overlap(_get_span($i1), _get_span($i2));

  # type test
  push @tests,
    $self->cmp_ont_slot($i1, $i2, 'type');
  
  unless (__option_is($options, 'deep', 0)) {
    push @tests,
      $self->option('ignore_dbid')
      || $self->cmp_attribute_aslist($i1, $i2, 'dbid', '|'),
      $self->cmp_text_slot($i1, $i2, 'name'),
      $self->cmp_mods($i1, $i2),
      $self->cmp_features($i1, $i2),
      $self->cmp_mutation($i1, $i2),
      $self->cmp_aggregate($i1, $i2),
      $self->cmp_components($i1, $i2),
      $self->cmp_members($i1, $i2);
  }
    
  my $result = min(@tests);
  $self->record_result($i1, $i2, $result)
      unless (__option_is($options, 'deep', 0) and $result);

  DEBUG 2, "==> %s", $result;
  return $result;
}

sub cmp_EVENT {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "EVENT") && __assert_eq($i2->nodeName, "EVENT")
    or return 0;
    
  my @tests = ();
  
  # overlap test
  push @tests,
    _span_overlap(_get_span($i1), _get_span($i2));

  # type test
  push @tests,
    $self->cmp_ont_slot($i1, $i2, 'type');
  
  unless (__option_is($options, 'deep', 0)) {
    push @tests,
      $self->cmp_text_slot($i1, $i2, 'negation'),
      $self->cmp_text_slot($i1, $i2, 'polarity'),
      $self->cmp_text_slot($i1, $i2, 'modality'),
      $self->cmp_pseudo_args($i1, $i2, 'epistemic-modality', { deep => 0 }),
      $self->cmp_mods($i1, $i2),
      $self->cmp_aggregate($i1, $i2),
      $self->cmp_predicate($i1, $i2),
      $self->cmp_args($i1, $i2, { deep => 0 }),
      $self->cmp_pseudo_args($i1, $i2, 'site'),
      $self->cmp_pseudo_args($i1, $i2, 'location'),
      $self->cmp_pseudo_args($i1, $i2, 'from-location'),
      $self->cmp_pseudo_args($i1, $i2, 'to-location'),
      $self->cmp_pseudo_args($i1, $i2, 'cell-line');
  }

  my $result = min(@tests);
  $self->record_result($i1, $i2, $result)
      unless (__option_is($options, 'deep', 0) and $result);

  DEBUG 2, "==> %s", $result;
  return $result;
}

sub cmp_CC {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "CC") && __assert_eq($i2->nodeName, "CC")
    or return 0;

  my @tests = ();

  # overlap test
  push @tests,
    _span_overlap(_get_span($i1), _get_span($i2));

  # type test
  push @tests,
    $self->cmp_ont_slot($i1, $i2, 'type');
  
  unless (__option_is($options, 'deep', 0)) {
    push @tests,
      $self->cmp_text_slot($i1, $i2, 'negation'),
      $self->cmp_text_slot($i1, $i2, 'polarity'),
      $self->cmp_text_slot($i1, $i2, 'modality'),
      $self->cmp_args($i1, $i2, { deep => 0 });
  }

  my $result = min(@tests);
  $self->record_result($i1, $i2, $result)
      unless (__option_is($options, 'deep', 0) and $result);

  DEBUG 2, "==> %s", $result;
  return $result;
}

sub cmp_EPI {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "EPI") or return 0;
  __assert_eq($i2->nodeName, "EPI") or return 0;

  my @tests = ();
  
  # overlap test
  push @tests,
    _span_overlap(_get_span($i1), _get_span($i2));

  # type test
  push @tests,
    $self->cmp_ont_slot($i1, $i2, 'type');
  
  unless (__option_is($options, 'deep', 0)) {
    push @tests,
      $self->cmp_text_slot($i1, $i2, 'negation'),
      $self->cmp_text_slot($i1, $i2, 'polarity'),
      $self->cmp_text_slot($i1, $i2, 'modality'),
      $self->cmp_args($i1, $i2, { deep => 0 });
  }

  my $result = min(@tests);
  $self->record_result($i1, $i2, $result)
      unless (__option_is($options, 'deep', 0) and $result);

  DEBUG 2, "==> %s", $result;
  return $result;
}

sub cmp_MODALITY {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "MODALITY") && __assert_eq($i2->nodeName, "MODALITY")
    or return 0;

  my @tests = ();

  # overlap test
  push @tests,
    _span_overlap(_get_span($i1), _get_span($i2));

  # type test
  push @tests,
    $self->cmp_ont_slot($i1, $i2, 'type');
  
  unless (__option_is($options, 'deep', 0)) {
    push @tests,
      $self->cmp_text_slot($i1, $i2, 'negation'),
      $self->cmp_text_slot($i1, $i2, 'polarity'),
      $self->cmp_args($i1, $i2, { deep => 0 });
  }

  my $result = min(@tests);
  $self->record_result($i1, $i2, $result)
      unless (__option_is($options, 'deep', 0) and $result);

  DEBUG 2, "==> %s", $result;
  return $result;
}


### functions for matching extraction properties

sub cmp_attribute {
  my $self = shift;
  my ($i1, $i2, $attr) = @_;

  my $a1 = $i1->getAttribute($attr);
  my $a2 = $i2->getAttribute($attr);

  DEBUG 2, "matching attribute %s: %s ~~ %s", $attr, $a1, $a2; 
    
  return 1 if ((! $a1) && (! $a2));
  return 0 unless $a1;
  return 0 unless $a2;
  return ($a1 eq $a2);
}

sub cmp_attribute_aslist {
  my $self = shift;
  my ($i1, $i2, $attr, $delim) = @_;
    
  my $a1 = $i1->getAttribute($attr);
  my $a2 = $i2->getAttribute($attr);
    
  DEBUG 2, "matching attribute %s: %s ~~ %s", $attr, $a1, $a2;
    
  return 1 if ((! $a1) && (! $a2));
  return 0 unless $a1;
  return 0 unless $a2;
  my @l1 = split($delim, $a1);
  my @l2 = split($delim, $a2);
  return eq_deeply(\@l1, set(@l2));
}

# compare slot whose value is some text
sub cmp_text_slot {
  my $self = shift;
  my ($i1, $i2, $slot) = @_;
  my $t1 = get_slot_value($i1, $slot);
  my $t2 = get_slot_value($i2, $slot);
  return 1 if ((! $t1) && (! $t2));
  return 0 unless $t1;
  return 0 unless $t2;
  DEBUG 2, "%s: %s ~~ %s => %s", $slot, $t1, $t2, ($t1 eq $t2);
  return ($t1 eq $t2);
}

# FIXME: <frequency> items appear to be polymorphous and this might fail!
#        perhaps the fix should be in the EKB format!
sub cmp_mods {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($mods1) = $i1->findnodes('./mods');
  my ($mods2) = $i2->findnodes('./mods');

  return 1 if ((! $mods1) && (! $mods2));
  return 0 unless $mods1 && $mods2;
  my %mods1 = __get_mods($mods1);
  my %mods2 = __get_mods($mods2);
  DEBUG 2, "\nmods1: %s\nmods2: %s", Dumper(\%mods1), Dumper(\%mods2);
  return eq_deeply(\%mods1, \%mods2);
}

sub cmp_features {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($feats1) = $i1->findnodes('./features');
  my ($feats2) = $i2->findnodes('./features');

  return 1 if ((! $feats1) && (! $feats2));
  return 0 unless $feats1 && $feats2;

  my @tests =
    ( 
     $self->cmp_text_slot($feats1, $feats2, 'active'),
     $self->cmp_pseudo_args($feats1, $feats2, 'location'),
     $self->cmp_features_mutation($feats1, $feats2),
     $self->cmp_features_site($feats1, $feats2),
     $self->cmp_pseudo_args($feats1, $feats2, 'cell-line'),
     $self->cmp_features_inevent($feats1, $feats2),
     $self->cmp_pseudo_args($feats1, $feats2, 'bound-to', { deep => 0 }),
    );
  return min(@tests);
}

# polymorphic:
# - true/false
# - fully specified
sub cmp_mutation {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($mut1) = $i1->findnodes('./mutation');
  my ($mut2) = $i2->findnodes('./mutation');

  return 1 if ((! $mut1) && (! $mut2));
  return 0 unless $mut1 && $mut2;

  # we serialize them and compare as strings
  (my $ser_mut1 = $mut1->toString(0)) =~ s/>\s+</></mg;
  (my $ser_mut2 = $mut2->toString(0)) =~ s/>\s+</></mg;

  return $ser_mut1 eq $ser_mut2;
}

sub cmp_aggregate {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($agg1) = $i1->findnodes('./aggregate');
  my ($agg2) = $i2->findnodes('./aggregate');

  return 1 if ((! $agg1) && (! $agg2));
  return 0 unless $agg1 && $agg2;

  # FIXME: should we perhaps not short-circuit here?
  return
    $self->cmp_attribute($agg1, $agg2, 'operator')
    &&
    $self->cmp_pseudo_args($agg1, $agg2, 'member');
}

sub cmp_components {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($comp1) = $i1->findnodes('./components');
  my ($comp2) = $i2->findnodes('./components');

  return 1 if ((! $comp1) && (! $comp2));
  return 0 unless $comp1 && $comp2;

  return
    $self->cmp_pseudo_args($comp1, $comp2, 'component');
}

# this applies to protein family members, for which the dbid is the defining feature
sub cmp_members {
  my $self = shift;
  my ($i1, $i2) = @_;

  my @elems1 = $i1->findnodes('./members');
  my @elems2 = $i2->findnodes('./members');

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;

  my @dbids1 =
    map { $_->getAttribute('dbid') }
    @elems1;
  my @dbids2 =
    map { $_->getAttribute('dbid') }
    @elems2;
   
  return eq_deeply(\@dbids1, set(@dbids2));
}

sub cmp_predicate {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($pred1) = $i1->findnodes('./predicate');
  my ($pred2) = $i2->findnodes('./predicate');

  return 1 if ((! $pred1) && (! $pred2));
  return 0 unless $pred1 && $pred2;

  return
    $self->option('ignore_pred_type')
    || $self->cmp_text_slot($pred1, $pred2, 'type')

}

sub cmp_args {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  # we group all args together
  my @args1 = $self->ekb1()->assertion_args($i1);
  my @args2 = $self->ekb2()->assertion_args($i2);

  {
    DEBUG 2, "args: %d vs. %s", scalar(@args1), scalar(@args2);
  }
    
  # we sort them by role (assuming unique arg per role)
  my %rolemap1 =
    map { $_->getAttribute('role') => { id => $_->getAttribute('id'), 
					arg => $_ } }
    @args1;
  my %rolemap2 =
    map { $_->getAttribute('role') => { id => $_->getAttribute('id'),
					arg => $_ } }
    @args2;

  return 0 unless eq_deeply([keys %rolemap1], set(keys %rolemap2));
  foreach my $role (keys %rolemap1) {
    my $a1 = (defined $rolemap1{$role}{id}) 
      ? $self->ekb1()->get_assertion($rolemap1{$role}{id})
      : undef;
    my $a2 = (defined $rolemap2{$role}{id})
      ? $self->ekb2()->get_assertion($rolemap2{$role}{id})
      : undef;
    # fix for cases where the role arg is not extracted
    if ((! $a1) && (! $a2)) {
      return
	$self->cmp_text_slot($rolemap1{$role}{arg}, $rolemap2{$role}{arg}, 'type');
    }
    return 0 unless $a1 && $a2;
    return 0 unless $self->cmp_ekb_item($a1, $a2, $options);
  }

  return 1;
}

# these are sub-elements that refer to other EKB items
# TODO: there may be corner cases where either the @id doesn't resolve to any
# EKB item, or there is no @id at all!
# FIXME: this should be renamed, as it used for other things than actual
# pseudo/satellite args!
sub cmp_pseudo_args {
  my $self = shift;
  my ($i1, $i2, $name, $options) = @_;
  my @elems1 = $i1->findnodes("./$name");
  my @elems2 = $i2->findnodes("./$name");

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;
  # this is never a deep match, so we take a shortcut
  return 0 unless scalar(@elems1) == scalar(@elems2);

  my @items1 =
    grep { defined }
    map { $self->ekb1()->get_assertion($_) }
    map { $_->getAttribute('id') }
    @elems1;
  my @items2 =
    grep { defined }
    map { $self->ekb2()->get_assertion($_) }
    map { $_->getAttribute('id') }
    @elems2;

  DEBUG 3, "cmp_pseudo_args:\n\@items1=%s, \@items2=%s",
    Dumper(\@items1), Dumper(\@items2);

  # if there are missing assertions, we shortcut
  return 0 unless scalar(@items1) == scalar(@items2);

  # TODO: pseudo_args that have no referent should be compared, too!
  # TODO: some pseudo-args have other attributes beside id

  return $self->cmp_ekb_items(\@items1, \@items2,
			      __options($options, 'deep' => 0));
}

# polymorphic:
# - true/false
# - pseudo-args
sub cmp_features_mutation {
  my $self = shift;
  my ($f1, $f2) = @_;
    
  # pseudo-args
  return 0 unless $self->cmp_pseudo_args($f1, $f2, 'mutation[@id]');

  # true/false
  my ($mut1) = $f1->findnodes('./mutation[not(@id)]');
  my ($mut2) = $f2->findnodes('./mutation[not(@id)]');

  return 1 if (! $mut1) && (! $mut2);
  return 0 unless $mut1 && $mut2;

  return ($mut1->textContent eq $mut2->textContent);
}

# polymorphic:
# - domain sites are pseudo-args
# - residue sites are fully specified
sub cmp_features_site {
  my $self = shift;
  my ($f1, $f2) = @_;

  # pseudo-args
  return 0 unless $self->cmp_pseudo_args($f1, $f2, 'site[@id]');

  # residues: we serialize them and compare as strings
  my @elems1 =
    map { (my $s = $_) =~ s/>\s+</></mg; $s }
    map { $_->toString(0) }
    $f1->findnodes('./site[not(@id)]');
  my @elems2 = 
    map { (my $s = $_) =~ s/>\s+</></mg; $s }
    map { $_->toString(0) }
    $f2->findnodes('./site[not(@id)]');

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;

  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 2, "sites: %s ~~ %s", Dumper(\@elems1), Dumper(\@elems2);
  }
    
  return eq_deeply(\@elems1, set(@elems2));
}

sub cmp_features_inevent {
  my $self = shift;
  my ($f1, $f2) = @_;
  my @elems1 = $f1->findnodes('./inevent/event');
  my @elems2 = $f2->findnodes('./inevent/event');

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;
  # this is never a deep match, so we take a shortcut
  return 0 unless scalar(@elems1) == scalar(@elems2);

  my @evids1 = 
    map { $_->getAttribute('id') } @elems1;
  my @events1 =
    grep { defined $_ }
    map { $self->ekb1()->get_assertion($_) } @evids1;
  my @evids2 = 
    map { $_->getAttribute('id') } @elems2;
  my @events2 =
    grep { defined $_ }
    map { $self->ekb2()->get_assertion($_) } @evids2;
  # another shortcut
  return 0 unless scalar(@events1) == scalar(@events2);

  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 2, "inevent: %s ~~ %s", Dumper(\@evids1), Dumper(\@evids2);
    DEBUG 3, "inevent: %s ~~ %s", join(", ", @events1), join(", ", @events2);
  }
    
  return $self->cmp_ekb_items(\@events1, \@events2, { deep => 0} );
}

# compare slot whose value is an ont type
# we only check bio ontologies
sub cmp_ont_slot {
  my $self = shift;
  if ($self->option('strict_ont_match')) {
    return $self->cmp_text_slot(@_);
  }
  my ($i1, $i2, $slot) = @_;
  my $t1 = get_slot_value($i1, $slot);
  my $t2 = get_slot_value($i2, $slot);
  return 1 if ((! $t1) && (! $t2));
  return 0 unless $t1;
  return 0 unless $t2;
  if ($ont_events->has($t1)) {
    return $ont_events->is_a($t1, $t2) || $ont_events->is_a($t2, $t1);
  }
  if ($ont_bioents->has($t1)) {
    return $ont_bioents->is_a($t1, $t2) || $ont_bioents->is_a($t2, $t1);
  }
  return ($t1 eq $t2);
}


### operations on match graph (for an utterance)

# TODO: simplify this structure; maybe make it a class?
sub reset_x_map {
  my $self = shift;
  $self->{x_map} = {};
}

# record matching result
sub record_result {
  my $self = shift;
  my ($i1, $i2, $result) = @_;
  if ($result) {
    $self->record_match($i1, $i2);
  } else {
    $self->record_no_match($i1, $i2);
  }
}

# record a match
sub record_match {
  my $self = shift;
  my ($i1, $i2) = @_;
  return 1 if $self->matched($i1, $i2);
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  if ($self->has_match($i1, 1)) {
    ERROR "cannot re-match %s to %s (now: %s ~~ %s)",
      $id1, $id2, $id1, $self->{x_map}{1}{$id1};
    return 0;
  }
  if ($self->has_match($i2, 2)) {
    ERROR "cannot re-match %s to %s (now: %s ~~ %s)",
      $id1, $id2, $self->{x_map}{2}{$id2}, $id2;
    return 0;
  }
  $self->{x_map}{1}{$id1}{$id2} = 1;
  $self->{x_map}{2}{$id2}{$id1} = 1;
  DEBUG 2, "matched: %s ~~ %s", $id1, $id2;
  return 1;
}

# record a no-match
sub record_no_match {
  my $self = shift;
  my ($i1, $i2) = @_;
  return 1 if $self->not_matched($i1, $i2);
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  if ($self->matched($i1, $i2)) {
    ERROR "Cannot unmatch previous match: %s ~~ %s", $id1, $id2;
    return 0;
  }
  $self->{x_map}{1}{$id1}{$id2} = 0;
  $self->{x_map}{2}{$id2}{$id1} = 0;
  DEBUG 2, "No match: %s ~~ %s", $id1, $id2;
  return 1;
}

sub seen_before {
  my $self = shift;
  my ($i1, $i2) = @_;
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  return
    (exists $self->{x_map}{1}{$id1})
    &&
    (exists $self->{x_map}{1}{$id1}{$id2});
}

sub matched {
  my $self = shift;
  my ($i1, $i2) = @_;
  return 0 unless $self->seen_before($i1, $i2);
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  DEBUG 3, "matched(%s, %s) = %s", $id1, $id2, $self->{x_map}{1}{$id1}{$id2};
  return $self->{x_map}{1}{$id1}{$id2};
}

sub not_matched {
  my $self = shift;
  my ($i1, $i2) = @_;
  return 0 unless $self->seen_before($i1, $i2);
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  DEBUG 3, "not_matched(%s, %s) = %s", $id1, $id2, $self->{x_map}{1}{$id1}{$id2};
  return !$self->{x_map}{1}{$id1}{$id2};
}

# nb: $source is 1 or 2
sub has_match {
  my $self = shift;
  my ($item, $source) = @_;
  ($source == 1) || ($source == 2)
    or FATAL "2nd argument must be 1 or 2";
    
  my $id = $item->getAttribute('id');
  return 0 unless (exists $self->{x_map}{$source}{$id});
  DEBUG 3, "has_match(%s, %d) = %s", $id, $source,
    any { $self->{x_map}{$source}{$id}{$_} }
    keys %{ $self->{x_map}{$source}{$id} };
  return
    any { $self->{x_map}{$source}{$id}{$_} }
    keys %{ $self->{x_map}{$source}{$id} };
}


### diff printing

sub diffs_as_string {
  my $self = shift;

  {
    DEBUG 2, "diffs: %s", Dumper($self->diffs);
  }
    
  # FIXME temporary, should do things differently!
  sub s_path_id {
    my $path = shift;
    $path =~ m{\@id=(\d+)};
    return $1;
  }
    
    my $result;

  # options
  $result .= sprintf("Options: %s\n", $self->_options_to_string());

  # summary result
  $result .= sprintf("------\nResult: EKBs are %s\n",
		     $self->result ? "identical" : "different");

  # counts table
  $result .= "------\nAssertion counts:\n";
  my $summary = $self->summary();
  $result .= sprintf(" REF\t%d\n",
		     $summary->{del} + $summary->{eql});
  $result .= sprintf(" HYP\t%d\n",
		     $summary->{ins} + $summary->{eql});
  $result .= sprintf(" DEL\t%d\n",
		     $summary->{del});
  $result .= sprintf(" INS\t%d\n",
		     $summary->{ins});
  $result .= sprintf(" EQL\t%d\n",
		     $summary->{eql});
  
  # if identical, that's it
  return $result if $self->result;
    
  # meta info
  $result .= "------\nDiffs:\n\n";
    
  $result .= $self->_diff_value('/ekb/@id');
  $result .= $self->_diff_value('/ekb/@complete');
    
  # sentences
  my @s_paths =
    sort { s_path_id($a) <=> s_path_id($b) }
  grep {$_ =~ m{^/ekb/input/sentences/sentence}}
  keys %{$self->diffs};
  foreach my $s (@s_paths) {
    $result .= $self->_diff_input($s);
  }
    
  # extractions, by utt and spans
  foreach my $sid (sort {$a<=>$b} keys %{$self->diffs()->{utt}}) {
    my $utt = $self->ekb1()->get_sentence($sid);
    $result .= sprintf("uttnum=%d %s\n", $sid, $utt->textContent);
    foreach my $s (sort {$a<=>$b} keys %{$self->diffs()->{utt}{$sid}}) {
      foreach my $e (sort {$a<=>$b} keys %{$self->diffs()->{utt}{$sid}{$s}}) {
	DEBUG 2, "utt: %d span: [%d, %d]", $sid, $s, $e;
	$result .= $self->_diff_span($sid, $s, $e);
      }
    }
  }
  return $result;
}

sub _options_to_string {
  my $self = shift;
  my $result = "";
  my %options = $self->options();
  return
    join ", ",
    map {
      my $opt = $_;
      my $val;
      if (ref($options{$opt}) eq "ARRAY") {
	$val = sprintf("(%s)", join(",", @{$options{$opt}}));
      } else {			# scalar
	$val = $options{$opt};
      }
      sprintf("%s => %s", $opt, $val);
    }
    sort keys %options;
  return $result;
}

sub _diff_value {
  my $self = shift;
  my ($path) = @_;
    
  if (exists $self->diffs()->{$path}) {
    return
      sprintf("%s\n< %s\n> %s\n",
	      $path,
	      $self->diffs()->{$path}{prev},
	      $self->diffs()->{$path}{new});
  }
  return "";
}

sub _diff_input {
  my $self = shift;
  my ($path) = @_;
    
  my $result = "";
  if (exists $self->diffs()->{$path}) {
    $result = sprintf("%s\n", $path);
    $result .= sprintf("< %s\n", $self->diffs()->{$path}{prev})
      if (exists $self->diffs()->{$path}{prev});
    $result .= sprintf("> %s\n", $self->diffs()->{$path}{new})
      if (exists $self->diffs()->{$path}{new});
  } else {
    WARN "path not found in \%{$self->diffs}: %s", $path;
  }
  return $result;
}

sub _diff_span {
  my $self = shift;
  my ($sid, $start, $end) = @_;
    
  my $result = "";
  if (exists $self->diffs()->{utt}{$sid}{$start}{$end}) {
    $result = sprintf("%d,%d\n", $start, $end);
    $result .=
      join("",
	   map { sprintf("< %s\n", $_->toString(1)) }
	   @{ $self->diffs()->{utt}{$sid}{$start}{$end}{prev} })
      if (exists $self->diffs()->{utt}{$sid}{$start}{$end}{prev});
    $result .=
      join("",
	   map { sprintf("> %s\n", $_->toString(1)) }
	   @{ $self->diffs()->{utt}{$sid}{$start}{$end}{new} })
      if (exists $self->diffs()->{utt}{$sid}{$start}{$end}{new});
  } else {
    WARN "span not found in diffs: [%d, %d]", $start, $end;
  }
  return $result;
}


### operations on spans

# get span from an EKB entry
# TODO: move to EKB.pm
sub _get_span {
  my $x = shift;
  my $start = $x->getAttribute('start');
  my $end = $x->getAttribute('end');
  unless (defined $start and defined $end) {
    ERROR "No :start or :end in %s", $x;
    $start = 0;
    $end = 0;
  }
  return [$start, $end];
}

# TODO: this likely belongs in EKB.pm, though the way it's done here is a bit
# peculiar. also, we're only doing this for mods, which is also peculiar!
# will have to rethink it in a more general way.
sub __get_mods {
  my $mods_elem = shift;
  my @children = $mods_elem->findnodes('./mod|./degree|./frequency');
  DEBUG 3, "mods [%d]: %s", scalar(@children), "@children";
  my %mods;
  map { $mods{$_->nodeName}{get_slot_value($_, 'type')} = 1 }
    $mods_elem->nonBlankChildNodes();
  return %mods;
}

# span comparator
sub _span_cmp {
  my ($span1, $span2) = @_;
  if ($span1->[0] == $span2->[0]) {
    return ($span1->[1] <=> $span2->[1]);
  }
  return -1 if ($span1->[0] <= $span2->[0]);
  return  1;
}

# span overlap
sub _span_overlap {
  my ($span1, $span2) = @_;
  # by convention, a zero-length span is unknown, so assume overlap
  return 1 if ($span1->[0] == $span1->[1] or $span2->[0] == $span2->[1]);
  # span1 left-of span2
  return 0 if ($span1->[1] < $span2->[0]);
  # span1 right-of span2
  return 0 if ($span1->[0] > $span2->[1]);
  # overlap
  return 1;
}

# span distance: the Pompeiu–Hausdorff distance
sub _span_distance {
  my ($span1, $span2) = @_;
  return max ( abs($span2->[0] - $span1->[0]),
	       abs($span2->[1] - $span1->[1]) );
}

### handling options

# opts: hashref
# 
sub __options {
  my $opts = shift;
  my %new_opts = @_;
  while (my ($opt, $val) = each(%new_opts)) {
    __option($opts, $opt, $val);
  }
  return $opts;
}

sub __option {
  my $opts = shift;
  return undef unless (ref($opts) eq "HASH");
  my ($option, $value) = @_;
  if (defined $value) {
    $opts->{$option} = $value;
  }
  return $opts->{$option};
}

sub __option_is {
  my ($opts, $option, $value) = @_;
  return 0 unless (ref($opts) eq "HASH");
  return 0 unless (defined $value);
  return 0 unless (defined $opts->{$option});
  return $opts->{$option} eq $value;
}

### other utilities

sub __assert_eq {
  my ($a, $b) = @_;
  return 1 if ($a eq $b);
  ERROR "assertion failed: (\"%s\" eq \"%s\")", $a, $b;
  return 0;
}

1;
