# CWMS.pm
#
# Time-stamp: <Mon Mar  5 14:57:19 CST 2018 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  1 Jun 2016
# $Id: CWMS.pm,v 1.1 2018/03/05 22:06:33 lgalescu Exp $
#

#----------------------------------------------------------------
# Description:
# Reasoner for making inferences from text-extracted EKBs.

#----------------------------------------------------------------
# History:
# 2018/03/05 v0.0	lgalescu
# - Started. Copied a gew general rules from DRUM.

#----------------------------------------------------------------
# Usage:
# 

package EKB::Reasoner::CWMS;

$VERSION = '0.0';

use strict 'vars';
use warnings;

use Data::Dumper;
use XML::LibXML;
use List::Util 1.45 qw(all any first min max uniq);

use EKB;
use EKB::Match;
use Ont::BioEntities;
use Ont::BioEvents;
use EKB::Reasoner::Rule;
use EKB::Reasoner;
use util::Log;

our @ISA = qw(EKB::Reasoner);

my @rules;

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
@rules =
  (
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
    constraints => ['CC[not(arg[2])]'],
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
