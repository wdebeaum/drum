# CWMS.pm
#
# Time-stamp: <Mon Jul  1 18:23:41 CDT 2019 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  1 Jun 2016
# $Id: CWMS.pm,v 1.4 2019/07/01 23:25:01 lgalescu Exp $
#

#----------------------------------------------------------------
# Description:
# Reasoner for making inferences from text-extracted EKBs.

#----------------------------------------------------------------
# History:
# 2018/03/05 v0.0	lgalescu
# - Started. Copied a few general-purpose rules from DRUM.
# 2019/06/11 v1.0	lgalescu
# - Significant additions.
# 2019/07/01 v1.1	lgalescu
# - Fixed bugs (getting attribute value from descendants)

#----------------------------------------------------------------
# Usage:
# 

package EKB::Reasoner::CWMS;

$VERSION = '1.1';

use strict 'vars';
use warnings;

use Data::Dumper;
use XML::LibXML;
use List::Util 1.45 qw(all any first min max uniq);

use EKB;
use EKB::Match;
use Ont::BioEntities;
use Ont::BioEvents;
use Ont::Geo;
use EKB::Reasoner::Rule;
use EKB::Reasoner;
use util::Log;

our @ISA = qw(EKB::Reasoner);

my @rules;

my $ont_geo = Ont::Geo->new();

sub new {
  my $class = shift;
  my $ekb = shift;
  my %options = @_;
  my $self = {
	      ekb => $ekb,
	      rules => [],
	      # options
	      options => {},
	      # log for which rules applies to what elements
	      log => {},
	     };
  bless $self, $class;
  $self->_init(%options);
  return $self;
}

sub _init {
  my $self = shift;
  $self->default_options();
  $self->SUPER::_init(@_); # pass options up

  push @{ $self->{rules} },
    map { EKB::Reasoner::Rule->new($self, $_) } @rules;
}

sub default_options {
  my $self = shift;
  $self->options(
		);
}

## Rules

# note: this is an ordered list!
# rules that modify assertions should precede clean-up rules
@rules =
  (

   ## numbers with modifiers, e.g., "over 10,000"
   {
    name => "EKR:NumberMod",
    constraints => ['TERM[type[.="ONT::NUMBER"] and value[@id and @mod]]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
      
      my $t_id = $t->getAttribute('id');

      my ($val) = $t->findnodes('value');
      my $tn_id = $val->getAttribute('id');
      my $tn_term = $ekb->get_assertion($tn_id, "TERM");
      
      match_node( $tn_term,
		  { SX => { 'type' => "ONT::NUMBER",
			    'value' => {} }
		  } )
	or return 0;

      my ($tn_value) = $tn_term->findnodes('value');
      # must have some actual value
      $tn_value->hasChildNodes()
	or return 0;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tn_id;

      # getmod
      my $mod = $val->getAttribute('mod');
      # set value
      my $new_val = clone_node($tn_value);
      $new_val->setAttribute('mod', $mod);
      $t->removeChild($val);
      $ekb->modify_assertion( $t,
      			      { rule => $rule->name, refid => $tn_id },
			      $new_val
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },

   ## e.g., "the year 2016"
   {
    name => "EKR:TimeIdentifiedAs",
    constraints => ['TERM[type[.="ONT::YEAR"] and equals[@id] and scale[.="ONT::TIME-LOC-SCALE"]]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
      
      my $t_id = $t->getAttribute('id');
      
      my $tl_id = getvalue_xpath($t, 'equals/@id');
      my $tl_term = $ekb->get_assertion($tl_id, "TERM");
      match_node( $tl_term,
		  { SX => { 'type' => "ONT::TIME-LOC",
			    'timex' => {} } }
		)
	or return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      # remove scale
      my ($s) = $t->findnodes('scale[.="ONT::TIME-LOC-SCALE"]');
      $t->removeChild($s);
      
      # copy slots
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_node(type => get_slot_value( $tl_term, "type")),
			      clone_node($tl_term->findnodes('timex'))
			    );
      DEBUG 2, "Asserted: %s", $t->toString;
      
      1;
    }
   },
   
   ## e.g., "the month of March"
   {
    name => "EKR:TimeMonthOf",
    constraints => ['TERM[type[.="ONT::MONTH"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # get assoc-with node(s)
      my $tl_id;
      my $assoc;
      my @assocs = $t->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $m = match_node( $ekb->get_assertion($a_id, "TERM"),
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	    $assoc = $a;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");

      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      $new_timex->setAttribute("type", "DURATION");

      # remove assoc
      $t->removeChild($assoc);
      
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_node(type => get_slot_value( $tl_term, "type")),
			      $new_timex
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },

   ## e.g., "the first half of March"
   {
    name => "EKR:TimePart",
    constraints => ['TERM[type[.="ONT::PART"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # get assoc-with node(s)
      my $tl_id;
      my $assoc;
      my @assocs = $t->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $m = match_node( $ekb->get_assertion($a_id, "TERM"),
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	    $assoc = $a;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");

      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      my $m = getvalue_xpath($t, 'text/@normalization');
      $new_timex->setAttribute("mod", $m);
      $new_timex->setAttribute("type", "DURATION");

      # remove assoc
      $t->removeChild($assoc);
      
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_node(type => get_slot_value( $tl_term, "type")),
			      $new_timex
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },

   ## the first 2 weeks of March
   # ...

   ## e.g., "the beginning of 2015", "the start of the week"
   # < EVENT[@id=$t_id and assoc-with[@id=$tl_id] and type=$type]
   #    $type=one-of(ONT::START, ???)
   # < TERM[@id=$tl_id and type=ONT::TIME-LOC]
   # > + TERM[@id=$t_id]/ as near-clone of EVENT[@id=$t_id]
   #     - assoc-with
   #     ~ type=ONT::TIME-LOC
   #     + timex=TERM[@id=$tl_id]/timex
   #     + timex/@mod=$type
   # > - TERM[@id=$tl_id]
   # > - EVENT[@id=$t_id]
   {
    name => "EKR:TimeModStart",
    constraints => ['EVENT[type[.="ONT::START"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');
      my $tl_id;

      # get assoc-with node(s)
      my @assocs = $e->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $m = match_node( $ekb->get_assertion($a_id, "TERM"),
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches event %s (t: %s)",
	$rule->name(), $e_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");
      
      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      $new_timex->setAttribute("mod", "START");

      @assocs = grep { $_->getAttribute('id') ne $tl_id } @assocs;
      
      my $t = $ekb->derive_assertion( $e_id, 'TERM', 1 );
      $ekb->add_assertion($t);
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_nodes(type => get_slot_value( $tl_term, "type"),
					      spec => get_slot_value( $tl_term, "spec")),
			      map { clone_node($_) } @assocs,
			      $new_timex,
			    );
      DEBUG 2, "Asserted: %s", $t->toString;
     
      1;
    }
   },

   ## e.g., "the end of 2015", "the end of April"
   # < TERM[@id=$t_id and assoc-with[@id=$tl_id] and type=$type]
   #    $type=one-of(ONT::ENDPOINT, ???)
   # < TERM[@id=$tl_id and type=ONT::TIME-LOC]
   # > ~ TERM[@id=$t_id]/
   #     - assoc-with
   #     ~ type=ONT::TIME-LOC
   #     + timex=TERM[@id=$tl_id]/timex
   #     + timex/@mod=$type
   # > - TERM[@id=$tl_id]
   {
    name => "EKR:TimeModEnd",
    constraints => ['TERM[type[.="ONT::ENDPOINT"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # get assoc-with node(s)
      my $tl_id;
      my $assoc;
      my @assocs = $t->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $m = match_node( $ekb->get_assertion($a_id, "TERM"),
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	    $assoc = $a;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");

      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      $new_timex->setAttribute("mod", "END");

      # remove assoc
      $t->removeChild($assoc);
      
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_nodes(type => get_slot_value( $tl_term, "type"),
					      spec => get_slot_value( $tl_term, "spec")),
			      $new_timex
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },

   ## fix time links incorrectly parsed as locations
   ## order: after all time-related rules
   {
    name => "EKR:LocationToTime",
    constraints => ['*/location'],
    handler => sub {
      my ($rule, $ekb, $a) = @_;

      my $a_id = $a->getAttribute('id');

      my @locs = $a->findnodes("location");

      my $count = 0;
      foreach my $l (@locs) {
	my $tl_id = $l->getAttribute('id');
	my $tl_term = $ekb->get_assertion($tl_id, "TERM");
	next unless
	  defined($tl_term) &&
	  match_node( $tl_term, { SX => { 'type' => "ONT::TIME-LOC" } } );

	INFO "Rule %s matches assertion %s (t: %s)",
	  $rule->name(), $a_id, $tl_id;

	$a->removeChild($l);
	$ekb->modify_assertion( $a,
				{ rule => $rule->name, refid => $tl_id },
				make_node("time", { id => $tl_id }) );

	$count++;
      }
      
      $count;
    }
   },

   ## ARRIVE location => to-location
   {
    name => "EKR:ArriveIn",
    constraints => ['EVENT[type[.="ONT::ARRIVE"] and location[@id and @mod]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('location[@id]'); 
      my $mod = $s->getAttribute('mod');
      any { $mod eq $_ } qw/ONT::IN-LOC/
	or return 0;
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      # make sure we don't have it already
      match_node( $t, { SX => { 'to-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("to-location", { id => $t_id,
							 mod => $mod }) );

      1;
    }
   },
   
   ## location => from-location
   {
    name => "EKR:ELoc2FromLoc",
    constraints => ['EVENT[location[@id and @mod="ONT::FROM-LOC"]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('location[@id]'); 
      my $mod = $s->getAttribute('mod');
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      # make sure we don't have it already
      match_node( $t, { SX => { 'from-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("from-location", { id => $t_id }) );

      1;
    }
   },
   
   ## location => to-location
   {
    name => "EKR:ELoc2ToLoc",
    constraints => ['EVENT[location[@id and @mod]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('location[@id]'); 
      my $mod = $s->getAttribute('mod');
      any { $mod eq $_ } qw/ONT::TO-LOC ONT::GOAL-AS-CONTAINMENT/
	or return 0;
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      # make sure we don't have it already
      match_node( $t, { SX => { 'to-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("to-location", { id => $t_id }) );

      1;
    }
   },
   
   ## result(TO) => to-location
   {
    name => "EKR:EResult2ToLoc",
    constraints => ['EVENT[result[@id and mod] and not(to-location)]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('result[@id and mod]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      match_node( $t, { SX => { 'to-location' => $t_id } } )
	and return 0;

      my $mod = $s->getAttribute('mod');
      any { $mod eq $_ } qw/TO INTO/
	or return 0;


      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      ## NOT USED -- only issues a warning
      #WARN "Rule %s not applied.", $rule->name();
      #return 0;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("to-location", { id => $t_id,
							 mod => $mod }) );

      1;
    }
   },

   ## source => from-location
   {
    name => "EKR:ESource2FromLoc",
    constraints => ['EVENT[source/@id and not(from-location)]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('source[@id]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      match_node( $t, { SX => { 'from-location' => $t_id } } )
	and return 0;

      my $mod = $s->getAttribute('mod');
      if ($mod) {
	any { $mod eq $_ } qw/FROM/
	  or return 0;
      }
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("from-location", { id => $t_id, mod => $mod }) );

      1;
    }
   },
   
   ## result = to-location
   {
    name => "EKR:EResultIsToLoc",
    constraints => ['EVENT[result/@id and to-location/@id]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('result[@id]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      my $tl_id = getvalue_xpath($e, 'to-location/@id')
	or return 0;
      ($t_id eq $tl_id)
	or return 0;
      # TODO: this would be simpler, but it doesn't work
      # $e->exists('to-location[@id='.$t_id.']')
      #   or return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e, { rule => $rule->name } );

      1;
    }
   },

   ## result = from-location
   {
    name => "EKR:EResultIsFromLoc",
    constraints => ['EVENT[result/@id and from-location/@id]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('result[@id]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      my $tl_id = getvalue_xpath($e, 'from-location/@id')
      	or return 0;
      ($t_id eq $tl_id)
      	or return 0;
      # TODO: this would be simpler, but it doesn't work
      # $e->exists('from-location[@id='.$t_id.']')
      # 	or return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e, { rule => $rule->name } );

      1;
    }
   },

   ## result => from-location
   {
    name => "EKR:EResult2FromLoc",
    constraints => ['EVENT[result[@id and @mod="FROM"] and not(from-location)]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('result[@id]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      match_node( $t, { SX => { 'from-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      my $mod = $s->getAttribute('mod');
      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("from-location", { id => $t_id, mod => $mod }) );

      1;
    }
   },

   ## the rest are generic clean-up rules
   
   {
    ## remove dangling references on relation arguments
    # < X:EVENT()|CC()|EPI()|MODALITY()
    # < X/arg[@id=id]
    # < ! *[@id=id]
    # > - X/arg/@id
    name => "EKR:RemoveDanglingReferences",
    constraints => [],
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      my @args = assertion_args($r);
      my $count = 0;
      foreach my $arg (@args) {
	my $a_id = $arg->getAttribute('id');
	next if $ekb->get_assertion($a_id);
	INFO "Rule %s matches %s (arg: %s)",
	  $rule->name, $r_id, $a_id;
	$arg->removeAttribute('id');
	$count++;
      }

      $count;
    }
   },
   
   {
    ## delete CC relations that don't have two arguments and lack referrers
    # < C:CC(!arg[2])
    # < ! *(*:C)
    # > -C
    # WARNING: removes assertions
    name => "EKR:RemoveIncompleteCC",
    constraints => ['CC[not(arg[2])]', '_DO_NOT_USE_'],
    handler => sub {
      my ($rule, $ekb, $c) = @_;

      my $c_id = $c->getAttribute('id');

      # < C:CC(!arg[2])
      my @args = assertion_args($c);
      return 0 if (scalar(@args) >= 2);

      # < ! *(*:C)
      my @referrers =
	grep { $_->getAttribute('id') ne $c_id }
	$ekb->find_referrers($c_id);
      return 0 unless (scalar(@referrers) == 0);

      INFO("Rule %s matches %s",
	   $rule->name(), $c_id);
	
      # > -C
      $c->parentNode->removeChild($c);

      1;
    }
   },

   {
    ## delete empty relations (no arguments of any kind) with no referrers
    # < X:reln()
    # < ! *(*:X)
    # > - X
    # WARNING: removes assertions
    name => "EKR:RemoveBareReln",
    constraints => [],
    repeat => 1,
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      # < X:reln()
      my @args = ( assertion_args($r), assertion_xargs($r) );
      return 0 unless (scalar(@args) == 0);
	
      # < ! *(*:X)
      my @referrers =
	grep { $_->getAttribute('id') ne $r_id }
	$ekb->find_referrers($r_id);
      return 0 unless (scalar(@referrers) == 0);

      # > - X
      INFO("Rule %s matches %s",
	   $rule->name, $r_id);
	
      $ekb->remove_assertion($r);

      1;
    }
   },
   
   {
    ## Remove dangling 'inevent' features (event doesn't exist or it doesn't
    ## have an arg pointing to the term)
    # < TERM[@id=$t_id and //inevent[@id=$e]]
    #   ! *[@id=$e and arg*[@id=$t_id]]
    # > - TERM//inevent[@id=$e]
    name => "EKR:FixDanglingInevent",
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');
      
      my @e_ids = map { $_->value } $t->findnodes('features/inevent/@id');
      my $count = 0;
      foreach my $e_id (@e_ids) {
	my $e = $ekb->get_assertion($e_id);
	my @args = ( assertion_args($e), assertion_xargs($e) );
	next if any { $_->getAttribute('id') eq $t_id } @args;

	INFO "Rule %s matches term %s (inevent: %s)",
	  $rule->name(), $t_id, $e_id;

	remove_elements($t, 'features/inevent[@id="'.$e_id.'"]');
	    
	$count++;
      }
      $count;
    }
   },

   {
    ## when <mod><type>ONT::NEG</type><value>NOT</value></mod>
    ## flip <negation>
    # < E[mods/mod[type=ONT::NEG and value=NOT]]
    # > - E/mods/mod[type=ONT::NEG and value=NOT]
    # > ~ E[negation=flip()]
    name => 'EKR:ModNegNot',
    constraints => ['*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]'],
    handler => sub {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      INFO("Rule %s matches %s",
	   $rule->name, $r_id);

      # > ~ E[negation=flip()]
      my $neg = get_slot_value($r, 'negation');
      if (defined $neg) {
	if ($neg eq '+') {
	  $r->set_slot_value('negation', '-');
	} else {
	  $r->set_slot_value('negation', '+');
	}
      } else {
	$r->addChild(make_slot_node( negation => '+' ));
      }

      # > - E:/mods/mod[type=ONT::NEG and value=NOT]
      remove_elements($r, 'self::*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]');

      1;
    }
   },
   
   {
    ## obtain localization event from term w/ location info
    # < TERM[@id=$t_id and features/location/@id=$l_id]
    # > EVENT[type=ONT::LOCALIZATION
    #         pred=ONT::BE-AT-LOC
    #         arg[@id=$t_id]
    #         location[@id=$l_id]]
    # WARNING: adds assertions
    name => "EKR:LocateTerm",
    constraints => ['TERM[features/location]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');

      # get location(s)
      my @loc_ids = map {$_->value} $t->findnodes('features/location/@id');

      my @loc_terms =
	grep { defined $_ } map { $ekb->get_assertion($_, "TERM") } @loc_ids;
		     
      my $start = min ( $t->getAttribute('start'),
			map { $_->getAttribute('start') }
			@loc_terms );
      my $end = max ( $t->getAttribute('end'),
		      map { $_->getAttribute('end') }
		      @loc_terms );

      # TODO: perhaps, instead of making multiple events, i should make
      # an aggregate location?? but then why shouldn't the original term have
      # a single location feature pointing to the aggregate??
      foreach my $loc_term (@loc_terms) {
	my $l_id = $loc_term->getAttribute('id');
	$ekb->infer_assertion( 'EVENT',
			       { refid => $t_id,
				 start => $start,
				 end => $end,
				 rule => $rule->name },
			       make_slot_node(type => 'ONT::LOCALIZATION'),
			       make_predicate('ONT::BE-AT-LOC'),
			       make_arg(':NEUTRAL' => $t_id),
			       make_node("location", { id => $l_id }) );
      }
		     
      1;
    }
   },

   {
    ## distribute :EPI info in aggregate event assertions to member events
    # EVENT[type=S-CONJOINED and 
    #       aggregate/member[@id=$eid] and 
    #       epistemic-modality[@id=$mid]]
    # EVENT[@id=$id]
    # => mod: EVENT[@id=$id and epistemic-modality[@id=$mid]]
    # WARNING: adds EKB cross-refs
    name => "EKR:EpiConjoinedEvent",
    constraints => ['EVENT[aggregate and epistemic-modality]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');
	
      # match aggregate
      match_node($e, { SX => { 'type' => [OP_OR,
					  "ONT::S-CONJOINED",
					  "ONT::VP-CONJOINED"],
			       'aggregate' => {},
			       'epistemic-modality' => {} } })
	or return 0;

      # get :epi node(s)
      my @epis = $e->findnodes('epistemic-modality');
    
      # get aggregate member ids
      my @m_ids = map { $_->value } $e->findnodes('aggregate/member/@id');
      return 0 unless @m_ids;

      INFO("Rule %s matches event %s w/ members (%s)", 
	   $rule->name(), $e_id, "@m_ids");

      foreach my $m_id (@m_ids) {
	my $m_e = $ekb->get_assertion($m_id, "EVENT");
	DEBUG(1, "%s", $m_e);
	next unless $m_e;
	my ($pred) = $m_e->findnodes('predicate');
	foreach my $epi (@epis) {
	  my $c_epi = $epi->cloneNode(1);
	  $m_e->insertBefore($c_epi, $pred);
	  DEBUG(1, "%s", $m_e);
	}
      }

      1;
    }
   },

   {
    ## delete terms not referenced elsewhere
    # < T:TERM(id=$t_id)
    # < ! *[...id=$t_id...]
    # > - T
    # WARNING: removes assertions
    name => 'EKR:RemoveOrphanTerm',
    constraints => ['TERM'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # < ! *(...id=X...)
      my @referrers = $ekb->find_referrers($t_id);
      return 0 unless (scalar(@referrers) == 0);
  
      INFO("Rule %s matches term %s",
	   $rule->name(), $t_id);
      
      $ekb->remove_assertion($t);

      1;
    }
   }

  );

1;
